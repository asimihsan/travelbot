from __future__ import absolute_import

from celery import Celery

celery = Celery('test_celery.celery',
                broker = r"amqp://guest:guest@localhost:5672/",
                backend = 'amqp',
                include = ['test_celery.tasks'])
celery.conf.update(
    CELERY_RESULT_EXCHANGE = "celeryresults",
    CELERY_RESULT_EXCHANGE_TYPE = "direct",
    CELERY_RESULT_PERSISTENT = True,
    CELERY_TASK_RESULT_EXPIRES = 18000, # 5 hours
)

if __name__ == '__main__':
    celery.start()

