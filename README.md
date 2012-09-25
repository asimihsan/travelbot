# travelbot

Get bus and train timetables from around the world. For iPhone.

## How to run

```
# Checkout code
git clone ...

# Use virtualenv and install the required dependencies.
mkvirtualenv travelbot
cd travelbot/src
pip install -r requirements.tt

# apsw, uses to install cutting-edge SQLite with extensions, must be installed separately.
wget apsw.zip
unzip apsw
cd apsw
python setup.py fetch --all --version=3.7.14
# modify setup.py to allow stat3 and stat2 as config options
python setup.py build --enable-all-extensions --enable=stat3 install test

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

