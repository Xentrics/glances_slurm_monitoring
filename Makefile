#!/usr/bin/make -f

# This is the main entry point for the analysis
# All results must be created by making the rule 'analysis'

.PHONY: build run env status analyse

# This should be set to the true root of the project
# Currently, it is assumed that this RServer files are put to /project_root/src/RServer/
PROJECT_ROOT := $(shell realpath $(shell pwd))
SOURCE_DIR := $(shell realpath $(shell pwd))/src
# Update to current user ID and GROUP
USER_ID := $(shell id -u)
GROUP_ID := $(shell id -g)

# external server port used to access rstudio-server
DOCKER_ENV_PORT := 16930
# set a custom name for the running container
DOCKER_CONTAINER_NAME := Shiny_Glances
DOCKER_ENV_NAME := Shiny_Glances_Env

# only required if you are not yet logged into docker
DOCKER_REGISTRY := sbi-registry.hki-jena.de
DOCKER_REGISTRY_USER := seelbind
DOCKER_NAMESPACE := seelbind


define DOCKER_RUN_ARGS
	--volume /sbidata:/sbidata \
	--volume /opt/ngs2/:/opt/ngs2 \
	--volume $(SOURCE_DIR)/Rprofile.R:/usr/local/lib/R/etc/Rprofile.site \
	--volume $(PROJECT_ROOT):/analysis \
	--volume $(SOURCE_DIR)/welcome.html:/srv/shiny-server/index.html \
	--hostname $(shell hostname)
endef

define DOCKER_RUN_ARGS_ENV
	-e USERID=$(USER_ID) \
	-e GROUPID=$(GROUP_ID) \
	-e DISABLE_AUTH=true
endef

# Start shiny server
# All shiny apps need to be linked under /srv/shiny-server
shiny:
	docker run -d --rm \
		-p 4444:3838 \
		--volume $(PROJECT_ROOT):/srv/shiny-server/glances \
		--name $(DOCKER_CONTAINER_NAME) \
		$(DOCKER_RUN_ARGS) \
		shiny-verse:4.1_SBI


# Start docker container for interactive analysis
# - detached RStudio Server
# - user rstudio has the same UID than the host user
# - user rstudio as the analysis root directory as home
env:
	docker run -d --rm \
		-p $(DOCKER_ENV_PORT):8787 \
		--name $(DOCKER_ENV_NAME) \
		$(DOCKER_RUN_ARGS) \
		$(DOCKER_RUN_ARGS_ENV) \
		rocker/verse:4.1 \
		/init && \
		echo SUCCESS: Interactive analysis container $(DOCKER_CONTAINER_NAME) started at http://$(shell hostname):$(DOCKER_ENV_PORT)


