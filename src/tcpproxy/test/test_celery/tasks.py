from __future__ import absolute_import

from test_celery.celery import celery
from celery.utils.log import get_task_logger
import time

logger = get_task_logger(__name__)

@celery.task
def add(a, b, **kwargs):
    logger.info('tasks.add: executing task ID %s, args %s, kwargs %s' %
        (add.request.id, add.request.args, add.request.kwargs))
    time.sleep(3)
    return a + b

@celery.task
def mul(a, b, **kwargs):
    logger.info('tasks.mul: executing task ID %s, args %s, kwargs %s' %
        (mul.request.id, mul.request.args, mul.request.kwargs))
    time.sleep(2)
    return a * b

@celery.task
def xsum(numbers, **kwargs):
    logger.info('tasks.xsum: executing task ID %s, args %s, kwargs %s' %
        (xsum.request.id, xsum.request.args, xsum.request.kwargs))
    time.sleep(1)
    return sum(numbers)
