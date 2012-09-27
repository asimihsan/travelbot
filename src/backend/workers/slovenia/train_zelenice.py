#!/usr/bin/env python
# -*- coding: utf-8 -*-

# -----------------------------------------------------------------------------
#   Zelenic train times.
# -----------------------------------------------------------------------------
__description__ = "Zelenice train bus times."

import os
import sys
import datetime
import time
import codecs
import pprint
import pytz

# -----------------------------------------------------------------------------
#   Relative imports.
# -----------------------------------------------------------------------------
CURRENT_DIR = os.path.abspath(os.path.join(__file__, os.pardir))
WORKERS_DIR = os.path.abspath(os.path.join(CURRENT_DIR, os.pardir))
assert(os.path.isdir(WORKERS_DIR))
sys.path.append(WORKERS_DIR)
from utilities.browser import BrowserManager
from utilities.api_objects import Location, JourneyLegPoint, JourneyLeg, Journey

BACKEND_DIR = os.path.abspath(os.path.join(WORKERS_DIR, os.pardir))
assert(os.path.isdir(BACKEND_DIR))
sys.path.append(BACKEND_DIR)
from workers.celery import celery
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#   Constants.
# -----------------------------------------------------------------------------
APP_NAME = "backend.workers.slovenia.train_zelenice"
ROOT_URI = "http://www.slo-zeleznice.si/en/"

LOCATIONS_FILEPATH = os.path.join(CURRENT_DIR, "train_zelenice_locations.txt")
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#   Logging.
# -----------------------------------------------------------------------------
import logging
import logging.handlers
logger = logging.getLogger(APP_NAME)
logger.setLevel(logging.DEBUG)
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
formatter = logging.Formatter("%(asctime)s - %(name)s - %(message)s")
ch.setFormatter(formatter)
logger.addHandler(ch)

#fh = logging.handlers.RotatingFileHandler(LOG_FILENAME, maxBytes=10*1024*1024, backupCount=10)
#fh.setFormatter(formatter)
#fh.setLevel(logging.DEBUG)
#fh.setFormatter(formatter)
#logger.addHandler(fh)
# -----------------------------------------------------------------------------

# @celery.task
def get_locations():
    logger = logging.getLogger("%s.get_locations" % APP_NAME)
    logger.debug("entry.")
    with BrowserManager() as browser:
        # -----------------------------------------------------------------
        #   -   Get ROOT_URI.
        #   -   Scrape points from the SELECT list.
        # -----------------------------------------------------------------
        logger.debug("get: %s" % ROOT_URI)
        browser.get(ROOT_URI)
        doc = browser.get_page_source_as_doc()
        select = doc.cssselect("select")[0]
        options = select.iterchildren()
        options.next()
        location_names = (option.text for option in options)
        # -----------------------------------------------------------------

    logger.debug("returning.")
    return [Location(name = location_name) for location_name in location_names]

@celery.task
def get_journeys(from_location, to_location, journey_date=None, journey_time=None):
    logger = logging.getLogger("%s.get_journeys" % APP_NAME)
    logger.debug("entry. from_location: %s, to_location: %s, journey_date: %s, journey_time: %s" %
                 (from_location, to_location, journey_date, journey_time))
    journeys = []
    if journey_date is None:
        tz = pytz.timezone(pytz.country_timezones["si"][0])
        now = datetime.datetime.now(tz)
        journey_date = datetime.date(day = now.day,
                                     month = now.month,
                                     year = now.year)
        logger.debug("journey_date is None so set to %s" % journey_date)
    journey_date_string = journey_date.strftime("%d.%m.%Y")
    with BrowserManager() as browser:
        # -----------------------------------------------------------------
        #   -   Get ROOT_URI.
        #   -   Submit form with date, from, and to field filled in.
        # -----------------------------------------------------------------
        logger.debug("get: %s" % ROOT_URI)
        browser.get(ROOT_URI)

        browser.fill_element_by_xpath(".//*[@id='date']",
                                      journey_date_string)
        browser.select_option_by_xpath(".//*[@id='entrystation']",
                                       from_location)
        browser.select_option_by_xpath(".//*[@id='exitstation']",
                                       to_location)

        # first submit on homepage
        logger.debug("submit on homepage.")
        browser.click_element_by_xpath(".//*[@id='submit_ttable']")

        # -------------------------------------------------------------
        #   The results page for journeys involving a connection is
        #   convoluted. It's a flat table of rows, where a given
        #   journey is started by a line that contains price
        #   and details links.
        # -------------------------------------------------------------
        logger.debug("parse results page.")
        doc = browser.get_page_source_as_doc()
        table = doc.cssselect("table.timetable")[0]
        rows = table.cssselect("tr")[1:]
        blocks = []
        current_block = []
        for row in rows:
            text = row.text_content()
            if "Details" in text and len(current_block) > 0:
                blocks.append(current_block)
                current_block = []
            if text.strip() == '':
                continue
            current_block.append(row)
        blocks.append(current_block)
        # -------------------------------------------------------------

        # -------------------------------------------------------------
        #   current_blocks is a list of lists. Each row in a sublist
        #   is a journey leg.
        # -------------------------------------------------------------
        for block in blocks:
            journey_legs = []
            for row in block:
                transport_identifier = row.cssselect("td")[0].text_content()

                # From location and arrival time.
                actual_from_location = row.cssselect("td")[2].text_content()
                departure_time_string = row.cssselect("td")[3].text_content()
                departure_location = Location(name = actual_from_location)
                departure_time = datetime.time(hour = int(departure_time_string.split(":")[0]),
                                               minute = int(departure_time_string.split(":")[1]))

                # To location and arrival time.
                actual_to_location = row.cssselect("td")[4].text_content()
                arrival_time_string = row.cssselect("td")[5].text_content().strip()
                arrival_location = Location(name = actual_to_location)
                arrival_time = datetime.time(hour = int(arrival_time_string.split(":")[0]),
                                             minute = int(arrival_time_string.split(":")[1]))

                # Legpoint for departure.
                departure_journey_legpoint_datetime = datetime.datetime(year = journey_date.year,
                                                                        month = journey_date.month,
                                                                        day = journey_date.day,
                                                                        hour = departure_time.hour,
                                                                        minute = departure_time.minute)
                departure_journey_legpoint = JourneyLegPoint(location = departure_location,
                                                             datetime = departure_journey_legpoint_datetime)

                arrival_journey_legpoint_datetime = datetime.datetime(year = journey_date.year,
                                                                      month = journey_date.month,
                                                                      day = journey_date.day,
                                                                      hour = arrival_time.hour,
                                                                      minute = arrival_time.minute)
                arrival_journey_legpoint = JourneyLegPoint(location = arrival_location,
                                                           datetime = arrival_journey_legpoint_datetime)

                journey_leg = JourneyLeg(departure = departure_journey_legpoint,
                                         arrival = arrival_journey_legpoint,
                                         mode_of_transport = "train",
                                         transport_identifier = transport_identifier)
                journey_legs.append(journey_leg)

            journey = Journey(legs = journey_legs)
            journeys.append(journey)
        # -------------------------------------------------------------

    logger.debug("returning.")
    return journeys

def write_locations_to_file():
    bus_locations = get_locations()
    f = codecs.open(LOCATIONS_FILEPATH, "w", "utf-8-sig")
    for bus_location in bus_locations:
        f.write(u"%s\n" % bus_location.name)
    f.close()

def main():
    write_locations_to_file()
    journeys = get_journeys(from_location="Ljubljana",
                            to_location="Bled Jezero")
    pprint.pprint(journeys)

if __name__ == "__main__":
    main()
