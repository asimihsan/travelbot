#!/usr/bin/env bash

DIRECTORY=/Users/ai/Programming/travelbot/src/tcpproxy/priv/ssl
cd ${DIRECTORY}

rm -f serial*
rm -f index*
rm -f *.crt
rm -f *.pem
rm -f private/*
rm -f signedcerts/*

# Initialization
echo Initialization...
echo '10000002' > serial
touch index.txt

# Root CA key.
echo Root CA key...
export OPENSSL_CONF=${DIRECTORY}/caconfig.cnf; openssl req -x509 -newkey rsa:4096 -out cacert.pem -outform PEM -days 1825

# Self-signed root CA certificate
echo Self-signed root CA certificate...
openssl x509 -in cacert.pem -out cacert.crt

# Generate server key
echo Generate server key...
export OPENSSL_CONF=${DIRECTORY}/server.cnf; openssl req -newkey rsa:4096 -keyout tempkey.pem -keyform PEM -out tempreq.pem -outform PEM
openssl rsa < tempkey.pem > server_key.pem

# Sign server key with our CA.
echo Sign server key with our CA...
export OPENSSL_CONF=${DIRECTORY}/caconfig.cnf; openssl ca -in tempreq.pem -out server_crt.pem
rm -f tempkey.pem && rm -f tempreq.pem

# Output X.509 versions of certs and keys.
echo Output X.509 versions of our certs and keys.
openssl x509 -in cacert.pem -out cacert.crt
openssl x509 -in server_crt.pem -out server_crt.crt

