#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import datetime
import time
import codecs
import pprint
import apsw
import boto
from boto.s3.key import Key
from string import Template
import tempfile
import ipdb
import bz2
import re
import unidecode

# -----------------------------------------------------------------------------
#   Relative imports.
# -----------------------------------------------------------------------------
CURRENT_DIR = os.path.abspath(os.path.join(__file__, os.pardir))
WORKERS_DIR = os.path.abspath(os.path.join(CURRENT_DIR, os.pardir))
assert(os.path.isdir(WORKERS_DIR))
sys.path.append(WORKERS_DIR)

BACKEND_DIR = os.path.abspath(os.path.join(WORKERS_DIR, os.pardir))
assert(os.path.isdir(BACKEND_DIR))
sys.path.append(BACKEND_DIR)
from workers.celery import celery
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#   Constants.
# -----------------------------------------------------------------------------
APP_NAME = "backend.workers.slovenia.locations"
AWS_S3_BUCKET_NAME = "ai-travelbot"
AWS_S3_KEY_NAME = "geonames/"
AWS_CLOUDFRONT_NAME = "d2aqv6yvjgf8nl"

# Geonames feature codes are described in the reference below. We currently
# want codes prefixed by 'PPL' but use a regular expression to support
# expansion.
# Reference: http://www.geonames.org/export/codes.html
RE_DESIRED_GEONAMES_FEATURE_CODE = re.compile("^PPL")

DATABASE_FILENAME = "locations.sqlite"
DATABASE_FILEPATH = os.path.join(CURRENT_DIR, DATABASE_FILENAME)
DATABASE_COMPRESSED_FILENAME = "locations.sqlite.bz2"
DATABASE_COMPRESSED_FILEPATH = os.path.join(CURRENT_DIR, DATABASE_COMPRESSED_FILENAME)

RE_LOCATIONS_TEXT = re.compile(".*locations.txt$")
DIRECTORY_PATH_TO_COUNTRY_CODE = {"slovenia": "SI",
                                  "croatia": "HR"}
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

@celery.task
def get():
    logger = logging.getLogger("%s.get" % APP_NAME)
    logger.debug("entry.")

@celery.task
def update_provider():
    """Update the database that is a list of names that each provider will accept
    as an input for a location."""
    logger = logging.getLogger("%s.update_provider" % APP_NAME)

    with ProviderDatabase() as database:
        for root, dirs, files in os.walk(WORKERS_DIR):
            matching_names = (name for name in files if RE_LOCATIONS_TEXT.match(name))
            for name in matching_names:
                fullpath = os.path.join(root, name)
                logger.debug("location text file found: %s" % fullpath)
                country_code = None
                for (directory_elem, associated_country_code) in DIRECTORY_PATH_TO_COUNTRY_CODE.items():
                    if directory_elem in fullpath:
                        country_code = associated_country_code
                        break
                logger.debug("country_code: %s" % country_code)
                assert(country_code is not None)
                f = codecs.open(fullpath, "r", "utf-8-sig")
                try:
                    for line in f:
                        name = line.strip()
                        asciiname = unidecode.unidecode(name)
                        elements = {"name": name,
                                    "asciiname": asciiname,
                                    "country_code": country_code}
                        database.insert_place(elements)
                finally:
                    f.close()

    # -------------------------------------------------------------------------
    #   Compress the location data.
    # -------------------------------------------------------------------------
    if os.path.isfile(DATABASE_COMPRESSED_FILEPATH):
        logger.info("deleting old compressed database at: %s" % DATABASE_COMPRESSED_FILEPATH)
        os.remove(DATABASE_COMPRESSED_FILEPATH)
    logger.debug("Compress locations database...")
    with bz2.BZ2File(DATABASE_COMPRESSED_FILEPATH, "w") as bz2_out:
        with open(DATABASE_FILEPATH, "rb") as f_in:
            while True:
                chunk = f_in.read(4096)
                if len(chunk) == 0:
                    break
                bz2_out.write(chunk)

@celery.task
def update_geonames():
    """Update the database that is the geonames list of canonical places in a given
    country."""

    logger = logging.getLogger("%s.update" % APP_NAME)
    logger.debug("entry.")

    # -------------------------------------------------------------------------
    #   Find all region data in AWS S3 and add them to a local SQLite
    #   dataabase.
    # -------------------------------------------------------------------------
    with BotoS3Connection() as conn, GeonamesDatabase() as database:
        bucket = conn.get_bucket(AWS_S3_BUCKET_NAME)
        logger.debug("bucket: %s" % bucket)
        geonames_keys = bucket.list(prefix = AWS_S3_KEY_NAME)
        region_data_keys = (key for key in geonames_keys if key.name.endswith(".bz2"))
        for key in region_data_keys:
            logger.debug("key: %s" % key)
            with tempfile.NamedTemporaryFile() as fp:
                logger.debug("writing key to temporary file: %s" % fp.name)
                key.get_contents_to_file(fp)
                fp.seek(0)
                with bz2.BZ2File(fp.name) as decompressed_fp:
                    logger.debug("decompressing BZ2-compressed file line-by-line.")
                    for line in decompressed_fp:
                        elements = convert_line_to_elements(line)
                        if RE_DESIRED_GEONAMES_FEATURE_CODE.match(elements["feature_code"]):
                            database.insert_place(elements)
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    #   Compress and upload the location data.
    # -------------------------------------------------------------------------
    if os.path.isfile(DATABASE_COMPRESSED_FILEPATH):
        logger.info("deleting old compressed database at: %s" % DATABASE_COMPRESSED_FILEPATH)
        os.remove(DATABASE_COMPRESSED_FILEPATH)
    logger.debug("Compress locations database...")
    with bz2.BZ2File(DATABASE_COMPRESSED_FILEPATH, "w") as bz2_out:
        with open(DATABASE_FILEPATH, "rb") as f_in:
            while True:
                chunk = f_in.read(4096)
                if len(chunk) == 0:
                    break
                bz2_out.write(chunk)
    logger.debug("Upload compressed locations database...")
    with BotoS3Connection() as conn:
        bucket = conn.get_bucket(AWS_S3_BUCKET_NAME)
        key_name = DATABASE_COMPRESSED_FILENAME
        key = bucket.get_key(key_name)
        if not key:
            logger.info("key doesn't exist yet.")
            key = bucket.new_key(key_name)
        logger.debug("uploading compressed database %s to key %s..." % (DATABASE_COMPRESSED_FILEPATH, key))
        key.set_contents_from_filename(DATABASE_COMPRESSED_FILEPATH)
    # -------------------------------------------------------------------------

class BotoS3Connection(object):
    """A context manager wrapper around a Boto S3 connection to close it when
    we're done. We don't want to do anything else so return the raw S3
    connection object."""

    def __enter__(self):
        logger = logging.getLogger("%s.BotoS3Connection.__enter__" % APP_NAME)
        logger.debug("entry.")
        self.conn = boto.connect_s3()
        return self.conn

    def __exit__(self, exc_type, exc_value, traceback):
        logger = logging.getLogger("%s.BotoS3Connection.__exit__" % APP_NAME)
        logger.debug("entry.")
        self.conn.close()

class ProviderDatabase(object):
    """See LocationsDatabase for more information."""
    def __enter__(self):
        logger = logging.getLogger("%s.ProviderDatabase.__enter__" % APP_NAME)
        logger.debug("entry.")
        self.connection = apsw.Connection(DATABASE_FILEPATH)
        self.cursor = self.connection.cursor()
        self.cursor.execute("DROP TABLE IF EXISTS provider;")
        self.cursor.execute("""CREATE TABLE provider(name TEXT,
                                                     asciiname TEXT,
                                                     country_code TEXT);""")
        self.cursor.execute("DROP TABLE IF EXISTS provider_by_asciiname;")
        self.cursor.execute("""CREATE VIRTUAL TABLE provider_by_asciiname USING fts4(content="",
                                                                                     matchinfo="fts3",
                                                                                     prefix="1,3",
                                                                                     asciiname);""")
        # ---------------------------------------------------------------------
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        logger = logging.getLogger("%s.ProviderDatabase.__exit__" % APP_NAME)
        logger.debug("entry.")

        logger.debug("optimizing...")
        self.cursor.execute("INSERT INTO provider_by_asciiname(provider_by_asciiname) VALUES('optimize');")
        self.cursor.execute("CREATE INDEX provider_idx ON provider(country_code, asciiname);")
        self.cursor.execute("VACUUM")
        self.cursor.execute("ANALYZE")
        # ---------------------------------------------------------------------

        self.cursor.close()
        self.connection.close()

    def insert_place(self, elements):
        self.cursor.execute("""INSERT INTO provider VALUES(:name,
                                                           :asciiname,
                                                           :country_code);""",
                            elements)
        elements["rowid"] = self.connection.last_insert_rowid()
        self.cursor.execute("""INSERT INTO provider_by_asciiname (docid, asciiname) VALUES(:rowid, :asciiname)""",
                            elements)

class GeonamesDatabase(object):
    def __enter__(self):
        logger = logging.getLogger("%s.GeonamesDatabase.__enter__" % APP_NAME)
        logger.debug("entry.")
        self.connection = apsw.Connection(DATABASE_FILEPATH)
        self.cursor = self.connection.cursor()
        self.cursor.execute("DROP TABLE IF EXISTS geonames;")
        self.cursor.execute("""CREATE TABLE geonames(geonameid INTEGER PRIMARY KEY,
                                                     name TEXT,
                                                     asciiname TEXT,
                                                     alternatenames TEXT,
                                                     latitude REAL,
                                                     longitude REAL,
                                                     country_code TEXT);""")

        # ---------------------------------------------------------------------
        #   Reference: http://www.sqlite.org/fts3.html, particularly section 6
        #   ('FTS4 Options')
        #   -    Do not store the content of the indexed names in this table.
        #        Instead use 'content' as an empty string to not store any
        #        content in the FTS table. This means one is only allowed to
        #        SELECT on rowid.
        #   -    Disable FTS4 matchinfo() features using 'matchinfo', because
        #        we don't need them.
        #   -    Our use case is prefix searching, so use the 'prefix'
        #        parameter. Note that any prefix index is better than none
        #        so don't go overboard.
        # ---------------------------------------------------------------------
        self.cursor.execute("DROP TABLE IF EXISTS geonames_by_asciiname;")
        self.cursor.execute("""CREATE VIRTUAL TABLE geonames_by_asciiname USING fts4(content="",
                                                                                     matchinfo="fts3",
                                                                                     prefix="1,3",
                                                                                     asciiname);""")
        # ---------------------------------------------------------------------
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        logger = logging.getLogger("%s.GeonamesDatabase.__exit__" % APP_NAME)
        logger.debug("entry.")

        logger.debug("optimizing...")

        # 'rebuild' is used to rebuild external content FTS tables. However, we are using
        # contentless FTS tables so this command is invalid.
        #self.cursor.execute("INSERT INTO geonames_by_asciiname(locations_by_asciiname) VALUES('rebuild');")

        self.cursor.execute("INSERT INTO geonames_by_asciiname(geonames_by_asciiname) VALUES('optimize');")
        self.cursor.execute("CREATE INDEX geonames_idx ON geonames(country_code, asciiname);")
        self.cursor.execute("VACUUM")
        self.cursor.execute("ANALYZE")
        # ---------------------------------------------------------------------

        self.cursor.close()
        self.connection.close()

    def insert_place(self, elements):
        self.cursor.execute("""INSERT INTO geonames VALUES(:geonameid,
                                                           :name,
                                                           :asciiname,
                                                           :alternatenames,
                                                           :latitude,
                                                           :longitude,
                                                           :country_code);""",
                            elements)
        self.cursor.execute("""INSERT INTO geonames_by_asciiname (docid, asciiname) VALUES(:geonameid, :asciiname)""",
                            elements)

def convert_line_to_elements(line):
    """Parse out a line from the Geonames database into a dictionary.

    The contents of a given region's line depends on the country. The biggest variation is for
    the US. You must read the respective 'readme.txt' file for a region for more details. To
    date this function does not support the US and has been tested with SI."""

    logger = logging.getLogger("%s.convert_line_to_elements" % APP_NAME)

    line = unicode(line, "UTF-8")
    elems = line.strip().split("\t")
    country_code = elems[8]
    return_value = {"geonameid": int(elems[0]),
                    "name": elems[1],
                    "asciiname": elems[2],
                    "alternatenames": elems[3],
                    "latitude": elems[4],
                    "longitude": elems[5],
                    "feature_class": elems[6],
                    "feature_code": elems[7],
                    "country_code": country_code,
                    "cc2": elems[9],
                    "admin1_code": elems[10],
                    "admin2_code": elems[11],
                    "admin3_code": elems[12],
                    "admin4_code": elems[13],
                    "population": elems[14],
                    "elevation": elems[15],
                    "dem": elems[16],
                    "timezone": elems[17],
                    "modification_date": elems[18]}
    return return_value

if __name__ == "__main__":
    #update_geonames()
    update_provider()


