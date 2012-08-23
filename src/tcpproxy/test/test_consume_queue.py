#!/usr/bin/env python
import os
import sys
import time

import gevent
import gevent.monkey; gevent.monkey.patch_all()

import pika
import test_celery.tasks
import json

# -----------------------------------------------------------------------------
#   Logging.
# -----------------------------------------------------------------------------
APP_NAME = "test_consume_queue"
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
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    #   Call Celery task synchronously, get result.
    # -------------------------------------------------------------------------
    if not hasattr(test_celery.tasks, task_method):
        result = {"status": 400,
                  "error_reason": "method does not have associated task."}
    else:
        method_callable = getattr(test_celery.tasks, task_method)
        task_result = method_callable.apply_async(args = task_args,
                                                  kwargs = task_kwargs,
                                                  task_id = task_id)
        result = {"status": 200,
                  "value": task_result.get()}
    reply = {"version": task_version,
             "_id": task_id,
             "tag": task_tag,
             "result": result}
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    #   Publish result and mark the message as ACK'd.
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

    logger.debug("publishing: %s" % json.dumps(reply))
    channel.basic_publish(exchange = 'tcpproxy',
                          routing_key = routing_key,
                          body = json.dumps(reply))
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

