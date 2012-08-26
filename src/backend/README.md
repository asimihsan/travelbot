# backend

## Introduction

The following covers the part of travelbot that fetches travel information.

## TODO

x   Celery worker for Slovenia bus times. CUT.
-   create simple test app in xcode simulator, test it works.
    -   When app starts:
        -   start socket and 'ping' server,
        -   get list of locations in background.
    -   Tab for search:
        -   Country dropdown, Slovenia for now.
        -   To/from search. Clicking goes to indexed list of locations. Get locations form server.
        -   On search new window, show progress, display results. Also get list of holidays.
-   Celery worker for Slovenia train times. CUT.
-   Celery worker for Slovenia bus and train times, with synonyms for common places. CUT.
-   Celery worker for Slovenia holidays (http://www.timeanddate.com/holidays/slovenia/)
-   enable SSL.
-   enable Tor.

## request API:

Although the primary use of the app is to search for travel times the application will also need some way of displaying valid points of travel.

### travel points request API.

-   worker_id: identification of the worker you want to process this task. must be prefixed by "travelbot_", and suffixed by the name of the country, e.g. "slovenia", "turkey".

### travel points response API:

-   responses are always BZIP2 compressed.
-   need to stream-encode the JSON, and stream that into the BZIP2 compressor; verify by checking memory usage.
-   travel_points: list of travel points.

### travel times request API.

-   worker_id: identification of the worker you want to process this task. must be prefixed by "travelbot_", and suffixed by the name of the country, e.g. "slovenia", "turkey".
-   from: start location
-   to: end location
-   date: date of trip, default is today's date
-   time: time of trip, default is now + 10 minutes.
-   arrival: if "true" then time specified is wanted arrival time, else time is departure time.
-   search\_type: possible values:
    -   "F": search using all modes of transport.
    -   "T": search using regional trains.
    -   "B": search using regional buses.
-   modes: optional region-specific modes of transportation that may be used, array of values.

### travel times response API:

-   responses are always BZIP2 compressed.
-   list of dictionaries of length 0 or more. each dictionary is a trip.
-   will try to return trips before and after the requested time.
-   the following describes the dictionaries.
-   mot: means of transportation.
    -   type:
        -   "T": regional train
        -   "B": regional bus
    -   name: name of the particular instance of this means of transportation.
    -   id: identifier associated with this instance, e.g. bus number, plane number.
-   departure: point of departure of the trip segment
    -   name: name of departure point.
    -   datetime: ISO 8601 datetime in UTC of departure.
-   arrival: point of arrival of the trip segment
    -   name: name of the arrival point.
    -   datetime: ISO 8601 datetime in UTC of arrival.

