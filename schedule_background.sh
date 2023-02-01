#!/bin/bash
# NOTE: do not use the -it option with CRON!
docker exec --user shiny -w /srv/shiny-server/glances Shiny_Glances Rscript --vanilla background_job.R
