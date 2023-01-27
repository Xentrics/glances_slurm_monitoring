#!/bin/bash
DIR="/sbidata/glances"
glances -t 30 --stdout-csv cpu,mem,load > $DIR/glances.$(hostname).csv

