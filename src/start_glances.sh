#!/bin/bash
DIR="/opt/ngs2/glances"

# STD version (seems unreliable on NFS drives)
#glances -t 30 --stdout-csv cpu,mem,load,fs > $DIR/glances.$(hostname).csv
glances -t 30 --quiet --export csv --disable-plugin all --enable-plugin cpu,mem,load,fs --export-csv-file $DIR/glances.$(hostname).csv --export-csv-overwrite
