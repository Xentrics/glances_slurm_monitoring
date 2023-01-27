#!/bin/bash
ssh -N -f -L 61210:localhost:61209 seelbind@amantia  # amantia
ssh -N -f -L 61211:localhost:61209 seelbind@agaricus # agaricus
ssh -N -f -L 61212:localhost:61209 seelbind@russula  # russula
ssh -N -f -L 61213:localhost:61209 seelbind@kottos   # kottos
ps aux | grep ssh
