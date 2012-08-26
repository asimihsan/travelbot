import requests
import lxml.html
import json

def request(method, uri, data=None, headers=None, cookies=None, proxy=None):
    # -------------------------------------------------------------------------
    #   Validate inputs.
    # -------------------------------------------------------------------------
    assert(method.lower() in ["get", "post"])
    # -------------------------------------------------------------------------

    lxml_output = None
    r = None
    if data is None:
        data = {}
    if headers is None:
        headers = {}
    if method.lower() == "get":
        r = requests.get(uri, data=data, headers=headers)
    else:
        r = requests.post(uri, data=data, headers=headers)
    if r and r.status_code == 200:
        lxml_output = lxml.html.document_fromstring(r.text)
    return (r, lxml_output)

