#!/usr/bin/env python
# -*- coding: utf-8 -*-

# -----------------------------------------------------------------------------
#   Avtobusna postaja Ljubljana bus times.
# -----------------------------------------------------------------------------
__description__ = "Avtobusna Postaja Ljubljana bus times."

import os
import sys
import datetime
import time
import codecs
import pprint

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
APP_NAME = "backend.workers.slovenia.bus_ap"
ROOT_URI = "http://www.ap-ljubljana.si/eng/"

BUS_LOCATIONS_FILEPATH = os.path.join(CURRENT_DIR, "bus_ap_locations.txt")
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

# This task is depreceated, do not use. We now prefer to get a list of canonical
# locations rather than journey-type-specific locations.
# @celery.task
def get_locations():
    logger = logging.getLogger("%s.get_locations" % APP_NAME)
    logger.debug("entry.")
    with BrowserManager() as browser:
        # -----------------------------------------------------------------
        #   -   Get ROOT_URI.
        #   -   Submit form with date filled in, to/from empty.
        #   -   Switch to the only iframe on the page; it contains
        #       the travel points.
        #   -   Scrape points from the SELECT list.
        # -----------------------------------------------------------------
        logger.debug("get: %s" % ROOT_URI)
        browser.get(ROOT_URI)

        logger.debug("submit blank form")
        browser.click_element_by_xpath("html/body/table/tbody/tr[4]/td[1]/form/fieldset/input[4]")

        logger.debug("parse SELECT in result.")
        iframe = browser.get_element_by_xpath("//iframe")
        iframe_name = iframe.get_attribute("name")
        browser.switch_to_frame(iframe_name)
        points_select = browser.get_element_by_xpath("html/body/form/table/tbody/tr[2]/td[1]/select")
        doc = browser.get_page_source_as_doc()
        select = doc.cssselect("select")[0]
        options = select.iterchildren()
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
        now = datetime.datetime.now()
        journey_date = datetime.date(day = now.day,
                                     month = now.month,
                                     year = now.year)
        logger.debug("journey_date is None so set to %s" % journey_date)
    journey_date_string = journey_date.strftime("%d.%m.%Y")
    with BrowserManager() as browser:
        # -----------------------------------------------------------------
        #   -   Get ROOT_URI.
        #   -   Submit form with date, from, and to field filled in.
        #   -   Select the first elements of the from select list and the
        #       to select list (this is default so do nothing).
        #   -   Submit again to perform the search.
        #   -   If there is a direct journey between the two locations
        #       results are here.
        #   -   If there is not a direct journey between the two locations
        #       click the "Connection" button.
        #       -   If the result has the following phrase in it then
        #           there are no bus times available:
        #
        #           "Med izbranima postajališčema ni avtobusne povezave."
        #
        #       -   Sanity check by seeing if there are any rows, if not
        #           then there are no journeys.
        #
        #   Notes:
        #   -   Sometimes journeys are highlighted in red and the page
        #       says "The ride on the chosen bus is not possible." Don't
        #       return these journeys.
        # -----------------------------------------------------------------
        logger.debug("get: %s" % ROOT_URI)
        browser.get(ROOT_URI)
        browser.fill_element_by_xpath("html/body/table/tbody/tr[4]/td[1]/form/fieldset/input[1]",
                                      journey_date_string)
        browser.fill_element_by_xpath("html/body/table/tbody/tr[4]/td[1]/form/fieldset/input[2]",
                                      from_location)
        browser.fill_element_by_xpath("html/body/table/tbody/tr[4]/td[1]/form/fieldset/input[3]",
                                      to_location)
        # first submit on homepage
        logger.debug("submit on homepage.")
        browser.click_element_by_xpath("html/body/table/tbody/tr[4]/td[1]/form/fieldset/input[4]")

        # second submit on location clarification page. Note we must switch to the iframe first.
        logger.debug("second submit on location clarification page.")
        iframe = browser.get_element_by_xpath("//iframe")
        iframe_name = iframe.get_attribute("name")
        browser.switch_to_frame(iframe_name)
        browser.click_element_by_xpath("html/body/form/table/tbody/tr[3]/td[3]/input")

        # Parse the results page. There may not be any results here if there is not direct
        # connection.
        logger.debug("parse results page.")
        doc = browser.get_page_source_as_doc()
        indirect_journey = "There is no direct connection between the chosen stops" in doc.text_content()
        if indirect_journey:
            logger.debug("This is an indirect journey")
            browser.click_element_by_xpath(".//*[@id='footer']/tbody/tr[2]/td[1]/form/input")
            browser.wait_for_tag_name('iframe')
            iframe = browser.get_element_by_xpath("//iframe")
            iframe_name = iframe.get_attribute("name")
            browser.switch_to_frame(iframe_name)
            doc = browser.get_page_source_as_doc()

            # -------------------------------------------------------------
            #   The results page for journeys involving a connection is
            #   convoluted. It's a flat table of rows, where a given
            #   journey is termated by a line indicating its duration.
            # -------------------------------------------------------------
            table = doc.cssselect("table")[0]
            rows = table.cssselect("tr")[1:]
            blocks = []
            current_block = []
            for row in rows:
                text = row.text_content()
                if "Duration" in text:
                    blocks.append(current_block)
                    current_block = []
                    continue
                if text.strip() == '':
                    continue
                current_block.append(row)
            # -------------------------------------------------------------

            # -------------------------------------------------------------
            #   current_blocks is a list of lists. Each sublist contains
            #   a journey. The first row of a sublist is the start, the last
            #   row of the sublist is the end, and there are one or more
            #   intermediary points.
            # -------------------------------------------------------------
            for block in blocks:
                journey_legs = []
                for (start_point, end_point) in zip(block, block[1:]):
                    # From location and departure time.
                    actual_from_location = start_point.cssselect("td")[0].text_content()
                    departure_location = Location(name = actual_from_location)
                    departure_time_string = start_point.cssselect("td")[2].text_content()
                    departure_time = datetime.time(hour = int(departure_time_string.split(":")[0]),
                                                   minute = int(departure_time_string.split(":")[1]))

                    # To location and arrival time.
                    actual_to_location = end_point.cssselect("td")[0].text_content()
                    arrival_location = Location(name = actual_to_location)
                    arrival_time_string = end_point.cssselect("td")[1].text_content().strip()
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
                                             mode_of_transport = "bus")
                    journey_legs.append(journey_leg)

                journey = Journey(legs = journey_legs)
                journeys.append(journey)
            # -------------------------------------------------------------

        else:
            logger.debug("This is a direct journey")

            # -------------------------------------------------------------
            #   Parse out journey information.
            # -------------------------------------------------------------
            actual_date_string = doc.cssselect("#glava>tbody>tr>td>b")[1].text_content()
            actual_date = datetime.date(day = int(actual_date_string.split(".")[0]),
                                        month = int(actual_date_string.split(".")[1]),
                                        year = int(actual_date_string.split(".")[2]))
            actual_from_location = doc.cssselect("#glava>tbody>tr>td>b")[3].text_content()
            actual_to_location = doc.cssselect("#glava>tbody>tr>td>b")[5].text_content()
            table = doc.cssselect("table")[1]
            rows = table.cssselect("tr")[1:]
            departure_times = []
            arrival_times = []
            for row in rows:
                departure_time_string = row.cssselect("td")[0].text_content()
                departure_time = datetime.time(hour = int(departure_time_string.split(":")[0]),
                                               minute = int(departure_time_string.split(":")[1]))
                departure_times.append(departure_time)
                arrival_time_string = row.cssselect("td")[1].text_content()
                arrival_time = datetime.time(hour = int(arrival_time_string.split(":")[0]),
                                             minute = int(arrival_time_string.split(":")[1]))
                arrival_times.append(arrival_time)
            # -------------------------------------------------------------

            # -------------------------------------------------------------
            #   Construct return value.
            # -------------------------------------------------------------
            departure_location = Location(name = actual_from_location)
            arrival_location = Location(name = actual_to_location)
            for (departure_time, arrival_time) in zip(departure_times, arrival_times):
                departure_journey_legpoint_datetime = datetime.datetime(year = actual_date.year,
                                                                        month = actual_date.month,
                                                                        day = actual_date.day,
                                                                        hour = departure_time.hour,
                                                                        minute = departure_time.minute)
                departure_journey_legpoint = JourneyLegPoint(location = departure_location,
                                                             datetime = departure_journey_legpoint_datetime)

                arrival_journey_legpoint_datetime = datetime.datetime(year = actual_date.year,
                                                                      month = actual_date.month,
                                                                      day = actual_date.day,
                                                                      hour = arrival_time.hour,
                                                                      minute = arrival_time.minute)
                arrival_journey_legpoint = JourneyLegPoint(location = arrival_location,
                                                           datetime = arrival_journey_legpoint_datetime)

                journey_leg = JourneyLeg(departure = departure_journey_legpoint,
                                         arrival = arrival_journey_legpoint,
                                         mode_of_transport = "bus")
                journey = Journey(legs = [journey_leg])
                journeys.append(journey)
            # -------------------------------------------------------------

    logger.debug("returning.")
    return journeys

def write_bus_locations_to_file():
    bus_locations = get_locations()
    f = codecs.open(BUS_LOCATIONS_FILEPATH, "w", "utf-8-sig")
    for bus_location in bus_locations:
        f.write(u"%s\n" % bus_location.name)
    f.close()

def main():
    #write_bus_locations_to_file()

    # example of a direct connection.
    #journeys = get_journeys(from_location="Ljubljana",
    #                        to_location="Bled")

    # example of an indirect connection.
    journeys = get_journeys(from_location="Piran",
                            to_location="Bled")
    logger.debug("journeys: \n%s" % pprint.pformat(journeys))
    import ipdb; ipdb.set_trace()

if __name__ == "__main__":
    main()
