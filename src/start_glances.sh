#!/bin/bash
DIR="/opt/ngs2/glances"
glances -t 30 --stdout-csv cpu,mem,load > $DIR/glances.$(hostname).csv

