#!/usr/bin/env python

import os
import sys
import struct
import gevent
import gevent.socket
import unittest
import json
import bz2
import base64
import time

# -----------------------------------------------------------------------------
#   Constants.
# -----------------------------------------------------------------------------
SERVER_HOST = "127.0.0.1"
SERVER_PORT = 8080
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#   Logging.
# -----------------------------------------------------------------------------
APP_NAME = "test_integration"
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

    def _get_string_from_packing(self, string_to_unpack):
        """ Recall that this is just an unsigned integer of 4 bytes
        followed by a string of variable bytes. Strip the 4 bytes
        from the front."""
        return string_to_unpack[4:]

    def _send_string(self, tosend, timeout=-1):
        data = self._get_packed_string(tosend)
        gevent.socket.wait_write(self.sock.fileno(), timeout)
        self.sock.sendall(data)

    def _recv_nbytes_from_socket(self, socket, n):
        """Read n bytes from a socket. This function does not use timeouts.
        You must wrap calls to this function with gevent.Timeout() context
        managers."""

        output = []
        bytes_read = 0
        while True:
            chunk = socket.recv(n)
            if chunk != '':
                output.append(chunk)
                bytes_read += len(chunk)
            if bytes_read >= n:
                break
        return ''.join(output)

    def _recv_string(self, timeout=10):
        header = None
        payload = None
        with gevent.Timeout(timeout, False):
            start_time = time.time()
            header_size = 4
            header = self._recv_nbytes_from_socket(self.sock, header_size)
            if (not header) or (header and len(header) < header_size):
                return None
            payload_size = struct.unpack(">I", header)[0]
            payload = self._recv_nbytes_from_socket(self.sock, payload_size)
            if (not payload) or (payload and len(payload) < payload_size):
                return None

        return payload

    def test_slovenia_bus_ap_get_journeys(self):
        self._send_string("ping")
        response = self._recv_string(timeout=0.5)
        self.assertEqual(response, "pong")

        payload = {"version": "1.0",
                   "tag": "unique_identifier_slovenia_bus_ap_get_journeys",
                   "type": "task",
                   "method": "slovenia.bus_ap.get_journeys",
                   "kwargs": {"from_location": "Ljubljana", "to_location": "Bled"}}
        self._send_string(json.dumps(payload))
        response = self._recv_string(timeout=30)
        response_decoded = json.loads(response)
        self.assertIn("version", response_decoded)
        self.assertIn("tag", response_decoded)
        self.assertEqual("unique_identifier_slovenia_bus_ap_get_journeys", response_decoded["tag"])
        self.assertIn("result", response_decoded)
        result = response_decoded["result"]
        self.assertIn("status", result)
        self.assertEqual(200, result["status"])
        self.assertIn("value", result)
        value_decompressed = bz2.decompress(base64.b64decode(result["value"]))

