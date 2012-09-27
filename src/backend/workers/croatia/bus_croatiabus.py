#!/usr/bin/env python
# -*- coding: utf-8 -*-

# -----------------------------------------------------------------------------
#   Croatia-bus bus times.
# -----------------------------------------------------------------------------
__description__ = "Croatia-bus bus times."

import os
import sys
import datetime
import time
import codecs
import pprint
import requests
import json
import string
import time
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
APP_NAME = "backend.workers.croatia.bus_croatiabus"
ROOT_URI = "http://www.croatiabus.hr/index.php?option=com_content&view=article&id=79&Itemid=389&lang=en"
LOCATION_JSON_URI = "http://www.croatiabus.hr/modules/mod_cbusvrc/mod_cbusformatrazi_json.php"

LOCATIONS_FILEPATH = os.path.join(CURRENT_DIR, "bus_croatiabus_locations.txt")
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

    logger.debug("HTTP GET of ROOT_URI: %s" % ROOT_URI)
    r1 = requests.get(ROOT_URI)
    location_names = []
    for letter in string.ascii_lowercase:
        data = {"value": letter}
        logger.debug("HTTP POST of LOCATION_JSON_URI %s with data %s." % (LOCATION_JSON_URI, data))
        r2 = requests.post(LOCATION_JSON_URI, data=data, cookies=r1.cookies)
        locations = json.loads(r2.text)
        location_names.extend(locations)
        time.sleep(0.5)

    logger.debug("returning.")
    return [Location(name = location_name) for location_name in location_names]

@celery.task
def get_journeys(from_location, to_location, journey_date=None, journey_time=None):
    logger = logging.getLogger("%s.get_journeys" % APP_NAME)
    logger.debug("entry. from_location: %s, to_location: %s, journey_date: %s, journey_time: %s" %
                 (from_location, to_location, journey_date, journey_time))
    journeys = []
    if journey_date is None:
        tz = pytz.timezone(pytz.country_timezones["hr"][0])
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

        browser.fill_element_by_xpath(".//*[@id='dpolazak_vrc']",
                                      journey_date_string)
        browser.fill_element_by_xpath(".//*[@id='ipolazak_vrc']",
                                      from_location)
        browser.fill_element_by_xpath(".//*[@id='idolazak_vrc']",
                                      to_location)

        # first submit on homepage
        logger.debug("submit on homepage.")
        browser.click_element_by_xpath(".//*[@id='trazi_vrc']")

        # -------------------------------------------------------------
        #   Results page is unusually simple. There is always
        #   one row per journey, where journey legs are compressed
        #   into the row, so it's not possible to determine the legs.
        # -------------------------------------------------------------
        logger.debug("parse results page.")
        doc = browser.get_page_source_as_doc()
        rows = doc.cssselect(".g_holderlinije2")
        if len(rows) == 0:
            logger.debug("there are no results available.")
            import ipdb; ipdb.set_trace()
            return journeys
        # -------------------------------------------------------------

        # -------------------------------------------------------------
        #   Go through each row and figure out the journeys. Each
        #   journey will only have one leg because the site doesn't
        #   offer that detail.
        # -------------------------------------------------------------
        for row in rows:
            journey_legs = []

            transport_identifier = row.cssselect(".h_lin_tbl")[0].text_content().strip()

            # From location and arrival time.
            actual_from_location = doc.xpath(".//*[@id='headResulta']/b[1]")[0].text_content()
            departure_time_string = row.cssselect(".h_pol_tbl")[0].text_content().strip()
            departure_location = Location(name = actual_from_location)
            departure_time = datetime.time(hour = int(departure_time_string.split(":")[0]),
                                           minute = int(departure_time_string.split(":")[1]))

            # To location and arrival time.
            actual_to_location = doc.xpath(".//*[@id='headResulta']/b[2]")[0].text_content()
            arrival_time_string = row.cssselect(".h_dol_tbl")[0].text_content().strip()
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

            if arrival_time < departure_time:
                logger.debug("arrival_time %s < departure_time %s, so assume arrives the next day." % (arrival_time, departure_time))
                arrival_day = journey_date.day + 1
            else:
                logger.debug("arrival_time %s >= departure_time %s, so assume arrives the same day." % (arrival_time, departure_time))
                arrival_day = journey_date.day
            arrival_journey_legpoint_datetime = datetime.datetime(year = journey_date.year,
                                                                  month = journey_date.month,
                                                                  day = arrival_day,
                                                                  hour = arrival_time.hour,
                                                                  minute = arrival_time.minute)
            arrival_journey_legpoint = JourneyLegPoint(location = arrival_location,
                                                       datetime = arrival_journey_legpoint_datetime)

            journey_leg = JourneyLeg(departure = departure_journey_legpoint,
                                     arrival = arrival_journey_legpoint,
                                     mode_of_transport = "bus",
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
    #write_locations_to_file()

    journeys = get_journeys(from_location="Zagreb",
                            to_location="Dubrovnik")
    pprint.pprint(journeys)

if __name__ == "__main__":
    main()
