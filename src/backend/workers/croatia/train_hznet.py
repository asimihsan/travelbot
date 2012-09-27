#!/usr/bin/env python
# -*- coding: utf-8 -*-

# -----------------------------------------------------------------------------
#   HZNet train times.
# -----------------------------------------------------------------------------
__description__ = "HZNet train times."

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
import lxml.html

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
APP_NAME = "backend.workers.croatia.train_hznet"
ROOT_URI = "http://www.hznet.hr/timetable"

LOCATIONS_FILEPATH = os.path.join(CURRENT_DIR, "train_hznet_locations.txt")
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
    location_names = []

    with BrowserManager() as browser:
        browser.get(ROOT_URI)
        iframe = browser.get_element_by_xpath("//iframe")
        browser.switch_to_frame(iframe)
        select_element = browser.selenium.find_element_by_id("ddList1")
        select_options = select_element.find_elements_by_tag_name("option")[2:]
        for select_option in select_options:
            text = select_option.text
            stripped = text.strip()
            capitalized = stripped.capitalize()
            location_names.append(capitalized)

    logger.debug("returning.")
    return [Location(name = location_name) for location_name in location_names]

@celery.task
def get_journeys(from_location, to_location, journey_date=None, journey_time=None):
    logger = logging.getLogger("%s.get_journeys" % APP_NAME)
    logger.debug("entry. from_location: %s, to_location: %s, journey_date: %s, journey_time: %s" %
                 (from_location, to_location, journey_date, journey_time))
    journeys = []
    if journey_date is None:
        now = datetime.datetime.now()
        journey_date = datetime.date(day = now.day,
                                     month = now.month,
                                     year = now.year)
        logger.debug("journey_date is None so set to %s" % journey_date)
    journey_date_string = journey_date.strftime("%d.%m.%y")
    logger.debug("journey_date_string: %s" % journey_date_string)
    with BrowserManager() as browser:
        # -----------------------------------------------------------------
        #   -   Get ROOT_URI.
        #   -   Submit form with date, from, and to field filled in.
        # -----------------------------------------------------------------
        logger.debug("get: %s" % ROOT_URI)
        browser.get(ROOT_URI)
        iframe = browser.get_element_by_xpath("//iframe")
        browser.switch_to_frame(iframe)

        browser.select_option_by_xpath("html/body/font/form/table[1]/tbody/tr[4]/td[2]/font/select",
                                      journey_date_string)
        browser.fill_element_by_xpath(".//*[@id='NKOD1']",
                                      from_location)
        browser.fill_element_by_xpath(".//*[@id='NKDO1']",
                                      to_location)

        # Click the radio group button for "All trains (with possible connections)
        browser.click_element_by_xpath("html/body/font/form/table[2]/tbody/tr[1]/td[2]/font/input")

        # first submit on homepage
        logger.debug("submit on homepage.")
        browser.click_element_by_xpath("html/body/font/form/font/p/input[6]")

        # -------------------------------------------------------------
        #   The results page is complex. There are no usable results
        #   here. You must click on each train identifier to get
        #   journey details.
        #
        #   As this doesn't require Javascript just use requests.
        # -------------------------------------------------------------
        logger.debug("parse results page.")
        doc = browser.get_page_source_as_doc()
        links = [elem for elem in doc.cssselect("a")
                 if "service=vred3" in elem.attrib.get("href", "") and
                 " " not in elem.text.strip()]
        cookies = browser.get_cookies()
        parser = lxml.html.HTMLParser(encoding = 'utf-8')
        for link in links:
            # -----------------------------------------------------------------
            #   Each journey leg is a separate page, which we're about to
            #   retrieve. Each row contains a "vlak", i.e. train identifier
            #   for that line. Each block of rows with matching "vlak"'s are
            #   a journey leg.
            # -----------------------------------------------------------------
            request = requests.get(link.attrib["href"], cookies=cookies)
            if request.status_code != 200:
                logger.error("Failed for link: %s." % link)
                return journeys
            subdoc = lxml.html.document_fromstring(request.text,
                                                   parser = parser)
            subtable = subdoc.cssselect("table")[1]
            header = [row for row in subtable if "Station" in row.text_content()][0]
            rows = [row for row in header.itersiblings() if row.tag == "tr"]
            blocks = []
            current_block = []
            for row in rows:
                if len(current_block) == 0:
                    current_block.append(row)
                    continue
                if len(row.getchildren()) == 0:
                    continue
                last_row = current_block[-1]
                last_row_train_ident = last_row.getchildren()[4].text_content().strip().split("(")[0]
                row_train_ident = row.getchildren()[4].text_content().strip().split("(")[0]
                if row_train_ident == last_row_train_ident:
                    current_block.append(row)
                    continue
                blocks.append(current_block)
                current_block = [row]
            blocks.append(current_block)
            # -----------------------------------------------------------------

            # -------------------------------------------------------------
            #   Go through each block and determine what the journey leg
            #   is, then combine into a journey
            # -------------------------------------------------------------
            journey_legs = []
            for block in blocks:
                first_row_children = block[0].getchildren()
                last_row_children = block[-1].getchildren()

                transport_identifier = first_row_children[4].text_content().strip()

                # From location and arrival time.
                actual_from_location = first_row_children[0].text_content().strip().capitalize()
                departure_time_string = first_row_children[2].text_content().strip()
                departure_location = Location(name = actual_from_location)
                departure_time = datetime.time(hour = int(departure_time_string.split(":")[0]),
                                               minute = int(departure_time_string.split(":")[1]))

                # To location and arrival time.
                actual_to_location = last_row_children[0].text_content().strip().capitalize()
                arrival_time_string = last_row_children[1].text_content().strip()
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
    #write_locations_to_file()

    journeys = get_journeys(from_location="Zagreb gl. kol.",
                            to_location="Split")
    pprint.pprint(journeys)

if __name__ == "__main__":
    main()
