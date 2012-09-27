#!/usr/bin/env python
import os
import sys
import time

import gevent
import gevent.monkey; gevent.monkey.patch_all()

import pika
import json
import re
import types
import bz2
import base64

# -----------------------------------------------------------------------------
#   Relative imports.
# -----------------------------------------------------------------------------
CURRENT_DIR = os.path.abspath(os.path.join(__file__, os.pardir))
BACKEND_DIR = os.path.abspath(os.path.join(CURRENT_DIR, os.pardir))
assert(os.path.isdir(BACKEND_DIR))
sys.path.append(BACKEND_DIR)

ROOT_DIR = os.path.abspath(os.path.join(BACKEND_DIR, os.pardir))
assert(os.path.isdir(ROOT_DIR))
sys.path.append(ROOT_DIR)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#   Logging.
# -----------------------------------------------------------------------------
APP_NAME = "backend.transducers.celery"
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

def handle_incoming_task(channel, method, properties, body):
    # -------------------------------------------------------------------------
    #   Parse and validate input task.
    # -------------------------------------------------------------------------
    logger.debug("got: %s" % body)
    body_decoded = json.loads(body)
    task_id = body_decoded["_id"]
    task_method = body_decoded["method"]
    task_version = body_decoded["version"]
    task_args = body_decoded.get("args", [])
    task_kwargs = body_decoded.get("kwargs", {})
    if len(task_kwargs) == 0:
        task_kwargs = {}
    task_tag = body_decoded.get("tag", "")
    task_timeout = int(body_decoded.get("timeout", "60000"))

    is_valid = True
    if not re.match("^[A-Za-z\._]+$", task_method):
        logger.error("task_method '%s' is not valid." % task_method)
        is_valid = False
        error_reason = "task_method is not valid."
    else:
        logger.debug("task_method is valid so trying to import now.")
        # task_method is vaild, so try importing now.
        try:
            elems = task_method.split(".")
            module_path = "backend.workers.%s" % ".".join(elems[:-1])
            method_string = elems[-1]
            exec("import %s" % module_path)
            module_object = eval(module_path)
            method_callable = getattr(module_object, method_string)
            if not callable(method_callable):
                logger.error("method_callable %s is not callable" % method_callable)
                is_valid = False
                error_reason = "Requested method %s in module_path %s is not callable." % (method_string, module_path)
        except ImportError:
            logger.exception("ImportError when attempting import of %s" % task_method)
            is_valid = False
            error_reason = "ImportError when attempting import of %s" % task_method
    if not is_valid:
        logger.error("validation failed.")
        result = {"status": 400,
                  "error_reason": error_reason}
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    #   Call Celery task synchronously, get result.
    #
    #   !!AI need to use native Python JSON serialization.
    # -------------------------------------------------------------------------
    if is_valid:
        logger.debug("request is valid so call into celery.")
        task_result = method_callable.apply_async(args = task_args,
                                                  kwargs = task_kwargs,
                                                  task_id = task_id)
        try:
            result_value = task_result.get()
        except:
            logger.exception("request with task_id %s resulted in unhandled exception." % task_id)
            result = {"status": 500}
        else:
            if type(result_value) == types.ListType:
                logger.debug("result_value is a list")
                result_value = [elem.serialize_to_dict() for elem in result_value if hasattr(elem, "serialize_to_dict")]
            elif hasattr(result_value, "serialize_to_dict"):
                logger.debug("result_value has 'serialize_to_dict' method")
                result_value = result_value.serialize_to_dict()
            #result_value = base64.b64encode(bz2.compress(json.dumps(result_value)))
            result = {"status": 200,
                      "value": result_value}
    reply = {"version": task_version,
             "_id": task_id,
             "tag": task_tag,
             "result": result}
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    #   Publish result and mark the message as ACK'd.
    #
    #   !!AI might need streaming JSON and streaming compression for large
    #   responses. But Celery brings it into memory anyway.
    # -------------------------------------------------------------------------
    connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
    channel = connection.channel()
    channel.exchange_declare(exchange = 'tcpproxy', type='direct')
    routing_key = str('task_result_%s' % task_id)
    logger.debug("routing key: %s" % routing_key)
    channel.queue_declare(queue = routing_key)
    channel.queue_bind(exchange = 'tcpproxy',
                       queue = routing_key,
                       routing_key = routing_key)
    publish_payload = bz2.compress(json.dumps(reply))
    channel.basic_publish(exchange = 'tcpproxy',
                          routing_key = routing_key,
                          body = publish_payload)
    channel.basic_ack(delivery_tag = method.delivery_tag)
    connection.close()
    # -------------------------------------------------------------------------

def consume_callback(channel, method, properties, body):
    g = gevent.spawn(handle_incoming_task, channel, method, properties, body)

def main():
    connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
    channel = connection.channel()
    channel.exchange_declare(exchange = 'tcpproxy', type='direct')
    channel.queue_declare(queue = 'celery_transducer')
    channel.queue_bind(exchange = 'tcpproxy',
                       queue = 'celery_transducer',
                       routing_key = 'task')
    channel.basic_consume(consume_callback,
                          queue = 'celery_transducer')
    channel.start_consuming()

if __name__ == "__main__":
    main()


