#!/usr/bin/env bash

wget http://apsw.googlecode.com/files/apsw-3.7.14-r1.zip
unzip apsw-3.7.14-r1.zip
cd apsw-3.7.14-r1
python setup.py fetch --version=3.7.14 --all build --enable-all-extensions install test
cd ..
rm -rf apsw-3.7.14-r1

