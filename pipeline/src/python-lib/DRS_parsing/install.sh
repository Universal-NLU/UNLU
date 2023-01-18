#!/bin/sh
git clone https://github.com/RikVN/DRS_parsing.git
mv DRS_parsing/* .
rm -rf DRS_parsing
pip3 install numpy psutil PyYAML
