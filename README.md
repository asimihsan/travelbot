# travelbot

Get bus and train timetables from around the world. For iPhone.

## How to run

```
# Checkout code
git clone ...

# rabbitmq server in background
rabbitmq-server -detached

# window 1, celery workers
cd travelbot/src
celery worker --app=backend.workers -l info

# window 2, celery transducer
cd travelbot/src
./backend/transducers/celery_transducer.py

# window 3, erlang tcp proxy
cd travelbot/src/tcpproxy
make start
```

