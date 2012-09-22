import json

# -----------------------------------------------------------------------------
#   -   Location: object for a city or place.
#   -   JourneyLegPoint: the start or the end of a JourneyLeg. It is a
#       Location and a datetime.
#   -   JourneyLeg: Two JourneyLegPoint objects. One is a departure and the
#       other is an arrival.
#   -   Journey: Zero or more JourneyLegs. legs attribute is a list of the
#       JourneyLegs; first is the start, last is the end.
# -----------------------------------------------------------------------------

class Location(object):
    def __init__(self, name):
        self._name = name

    @property
    def name(self):
        return self._name

    def __unicode__(self):
        return "{Location: name=%s}" % self.name

    def __repr__(self):
        return unicode(self)

    def serialize_to_dict(self):
        return {"Location": {"name": self.name}}

class JourneyLegPoint(object):
    def __init__(self, location, datetime):
        self._location = location
        self._datetime = datetime

    @property
    def location(self):
        return self._location

    @property
    def datetime(self):
        return self._datetime

    def __unicode__(self):
        return "{JourneyLegPoint: location=%s, datetime=%s}" % (self.location, self.datetime)

    def __repr__(self):
        return unicode(self)

    def serialize_to_dict(self):
        return {"JourneyLegPoint": {"location": self.location.serialize_to_dict(),
                                    "datetime": self.datetime.isoformat("T")}}

class JourneyLeg(object):
    def __init__(self, departure, arrival, mode_of_transport):
        self._departure = departure
        self._arrival = arrival
        self._mode_of_transport = mode_of_transport

    @property
    def departure(self):
        return self._departure

    @property
    def arrival(self):
        return self._arrival

    @property
    def mode_of_transport(self):
        return self._mode_of_transport

    def __unicode__(self):
        return "{JourneyLeg: departure=%s, arrival=%s, mode_of_transport=%2}" % \
                (self.departure, self.arrival, self.mode_of_transport)

    def __repr__(self):
        return unicode(self)

    def serialize_to_dict(self):
        return {"JourneyLeg": {"departure": self.departure.serialize_to_dict(),
                               "arrival": self.arrival.serialize_to_dict(),
                               "mode_of_transport": self.mode_of_transport}}

class Journey(object):
    def __init__(self, legs):
        self._legs = legs

    @property
    def legs(self):
        return self._legs

    def __unicode__(self):
        return "{Journey: legs=%s}" % self.legs

    def __repr__(self):
        return unicode(self)

    def serialize_to_dict(self):
        serialized_legs = [leg.serialize_to_dict() for leg in self.legs]
        return {"Journey": {"legs": serialized_legs}}

