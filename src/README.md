# Server Monitoring using Glances

- [Glances](https://glances.readthedocs.io/en/latest/) is a simple tool that enables full server monitoring
- An instance of glances must be running on each server, ideally started using scripts/start_glances.sh
- This script makes glances write out the main resources of interest to /sbidata/glances


# Usage

- for coding, navigate to src & run 'make env' to start a rstudio server
  - you can reach the server on <server>:16930
- for running the server, navigate to src & run 'make shiny' to spawn the shiny server into the background
  - you can reach the shiny server on <server>:4444
  - glances can be reached on <server>:4444/glances

