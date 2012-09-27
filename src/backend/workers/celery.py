from __future__ import absolute_import

from celery import Celery

include = [
            # Croatia
            'backend.workers.croatia.bus_croatiabus',
            'backend.workers.croatia.train_hznet',

            # Slovenia
           'backend.workers.slovenia.bus_ap',
           'backend.workers.slovenia.train_zelenice',
           ]
celery = Celery('backend.workers.celery',
                broker = r"amqp://guest:guest@localhost:5672/",
                backend = 'amqp',
                include = include)
celery.conf.update(
    CELERY_RESULT_EXCHANGE = "celeryresults",
    CELERY_RESULT_EXCHANGE_TYPE = "direct",
    CELERY_RESULT_PERSISTENT = True,
    CELERY_TASK_RESULT_EXPIRES = 18000, # 5 hours
)

if __name__ == '__main__':
    celery.start()

