#!/bin/bash
# Server-Side
*/30 * * * *  /home/seelbind/scripts/start_glances_cleaner.sh
# NOTE: do not use the -it option with CRON!
*/1  * * * *  docker exec --user shiny -w /srv/shiny-server/glances Shiny_Glances Rscript --vanilla background_job.R
*/1  * * * *  docker exec --user shiny -w /srv/shiny-server/glances_slurm Shiny_Glances Rscript --vanilla background_job.R

# Workers
@reboot       sleep 300 && /home/seelbind/scripts/start_glances.sh
*/2  * * * *  /home/seelbind/scripts/showCPUAllPerUser.sh | sort -k2 -nr | sed 's/ /\t/g' > /opt/ngs2/glances/top_users.$(hostname).tsv

