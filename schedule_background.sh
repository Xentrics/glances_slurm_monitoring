#!/bin/bash
docker exec --user shiny -w /srv/shiny-server/glances -it Shiny_Glances Rscript --vanilla background_job.R
