# backend

## Introduction

The following covers the part of travelbot that fetches travel information.

## TODO

-   from places view controller should be able to see nearby locations via a button. data is already there, but not in RTree index.
-   use tabs. new tab for previous searches and favourite searches.
    -   previous searches stores everything up to e.g. 100.
    -   favourite searches are only favourited searches, no limit.
-   start using preferences, even if read-only, to store major configuration.
    -   server hostname(s).
    -   method names for country codes.
-   improve table views of places and searches of places.
    -   do not show results for first letter. it's too slow, could cache but why bother. first letter will never show useful results.
    -   show indexed sections depending on depth of search. zero-letter search is A-Z, then second-letter search for e.g. "L" is "LA-LZ", but only for matching records, etc. Be careful of spaces (e.g. "A ") and accented letters. (this might be a bad idea).
-   Create task to return URI of location data. App has a baked-in URI but if a HTTP HEAD doesn't return 200 it asks the server for a new URI.
-   App comes with a copy of the location database. if server says it's outdated it'll try to update on Wi-Fi.
-   enable SSL.
    -   verify server certificate.
-   Celery worker for Slovenia train times. CUT.
-   Celery worker for Slovenia bus and train times, with synonyms for common places. CUT.
-   Celery worker for Slovenia holidays (http://www.timeanddate.com/holidays/slovenia/)
-   Celery worker for Spain.
-	Load holidays using local data.
-   Show warning on search results if today is a holiday.

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

