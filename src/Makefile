#!/usr/bin/make -f

# This is the main entry point for the analysis
# All results must be created by making the rule 'analysis'

.PHONY: build run env status analyse

# This should be set to the true root of the project
# Currently, it is assumed that this RServer files are put to /project_root/src/RServer/
PROJECT_ROOT := $(shell realpath $(shell pwd)/../)
# Update to current user ID and GROUP
USER_ID := $(shell id -u)
GROUP_ID := $(shell id -g)
SOURCE_DIR := $(shell pwd)

# external server port used to access rstudio-server
DOCKER_ENV_PORT := 16930
# set a custom name for the running container
DOCKER_CONTAINER_NAME := Shiny_Glances

# only required if you are not yet logged into docker
DOCKER_REGISTRY := sbi-registry.hki-jena.de
DOCKER_REGISTRY_USER := seelbind
DOCKER_NAMESPACE := seelbind
# DOCKER IMAGE PATH IN REGISTRY
DOCKER_REPOSITORY := $(shell echo sbi-registry.hki-jena.de/seelbind/dockerfiles | sed 's/\/\//\//g')
DOCKER_TAG := RServer_R4.2.1_2022_07
DOCKER_IMAGE := $(DOCKER_REPOSITORY):$(DOCKER_TAG)


define DOCKER_RUN_ARGS
	--volume $(PROJECT_ROOT):/analysis \
	--volume /sbidata:/sbidata \
	--volume $(SOURCE_DIR)/profile:/etc/profile \
	--volume $(SOURCE_DIR)/Rprofile.R:/usr/local/lib/R/etc/Rprofile.site \
	-e USERID=$(USER_ID) \
	-e GROUPID=$(GROUP_ID) \
	--hostname $(shell hostname)
endef

# Run docker container. This will also run the analysis by default
run:
	docker run $(DOCKER_RUN_ARGS) $(CUSTOM_NAME) make analyse

# Main entry point for the analysis
analyse: 
	@echo "Start anlysis at $(shell hostname)"
	Rscript src/run.R 2>&1 | tee log/analyze.log

# Start docker container for interactive analysis
# - detached RStudio Server
# - user rstudio has the same UID than the host user
# - user rstudio as the analysis root directory as home
env:
	docker run -d --rm \
		-e PASSWORD=tesseract \
		-p $(DOCKER_ENV_PORT):8787 \
		--name $(DOCKER_CONTAINER_NAME) \
		$(DOCKER_RUN_ARGS) \
		$(DOCKER_IMAGE) \
		/init && \
		echo SUCCESS: Interactive analysis container $(DOCKER_CONTAINER_NAME) started at http://$(shell hostname):$(DOCKER_ENV_PORT)

login:
	docker login \
		-u $(DOCKER_REGISTRY_USER) \
		$(DOCKER_REGISTRY)

pull:
	docker pull $(DOCKER_IMAGE)

