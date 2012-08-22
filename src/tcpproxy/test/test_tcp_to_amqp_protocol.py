#!/usr/bin/env python

import os
import sys
import struct
import gevent
import gevent.socket
import unittest
import json

# -----------------------------------------------------------------------------
#   Constants.
# -----------------------------------------------------------------------------
SERVER_HOST = "127.0.0.1"
SERVER_PORT = 8080
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#   Logging.
# -----------------------------------------------------------------------------
APP_NAME = "test_tcp_to_amqp_protocol"
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

class TestTCPToAMQPProtocol(unittest.TestCase):

    def setUp(self):
        self.sock = gevent.socket.create_connection((SERVER_HOST, SERVER_PORT))

    def tearDown(self):
        self._send_string("close")
        gevent.sleep(0.01)
        self.sock.close()

    def _get_packed_string(self, string_to_pack):
        format_string = ">I%ss" % len(string_to_pack)
        data = struct.pack(format_string, len(string_to_pack), string_to_pack)
        return data

    def _send_string(self, tosend, timeout=-1):
        data = self._get_packed_string(tosend)
        gevent.socket.wait_write(self.sock.fileno(), timeout)
        self.sock.sendall(data)

    def _recv_string(self, timeout=-1):
        return_value = []
        first_read = True
        while True:
            try:
                gevent.socket.wait_read(self.sock.fileno(), timeout)
            except:
                if not first_read:
                    break
                raise
            else:
                first_read = False
            data = self.sock.recv(4096)
            if data == '':
                break
            return_value.append(data)
        if len(return_value) == 0:
            return None
        return ''.join(return_value)

    def test_ping(self):
        self._send_string("ping")
        response = self._recv_string(timeout=0.01)
        self.assertEqual(response, self._get_packed_string("pong"))

    def test_two_pings(self):
        self.test_ping()
        self.test_ping()

    def test_addition_worker(self):
        payload = {"version": "1.0",
                   "tag": "unique_identifier",
                   "method": "addition",
                   "args": [1, 2],
                   "kwargs": {}}
        self._send_string(json.dumps(payload))
        response = self._recv_string(timeout=1)
        response_decoded = json.loads(response)
        self.assertEqual(response_decoded.get("return_value", None), 3)

