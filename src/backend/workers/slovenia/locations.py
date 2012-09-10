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
from string import Template

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
APP_NAME = "backend.workers.slovenia.locations"
AWS_S3_BUCKET_NAME = "ai-travelbot"
AWS_S3_BUCKET_REGION = "eu"
AWS_CLOUDFRONT_NAME = "d2aqv6yvjgf8nl"
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
def update():
    logger = logging.getLogger("%s.update" % APP_NAME)
    logger.debug("entry.")

