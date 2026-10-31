# Shared Makefile boilerplate, included by the root and subdir Makefiles.
PROJECT_ROOT := $(realpath $(dir $(lastword $(MAKEFILE_LIST)))/.)

DATE := $(shell date +"%d-%m-%Y")
LOG_FILE := $(PROJECT_ROOT)/.log/log-$(DATE).log
LOGGER := 2>&1 | tee -a $(LOG_FILE)

# Print vocabulary: -> creating, xx removing, -- skipping, !! warning.
define SAFE_RM
	if [ -e "$(1)" ]; then \
	  ABS_PATH=$$(realpath "$(1)"); \
	  case "$$ABS_PATH" in \
	    $(PROJECT_ROOT)*) \
	      echo " xx $$ABS_PATH"; \
	      rm -rf "$$ABS_PATH";; \
	    *) \
	      echo " !! Skipping (outside project root): $$ABS_PATH";; \
	  esac; \
	else \
	  echo " -- Skipping (not found): $(1)"; \
	fi
endef

define SAFE_RM_LIST
	for item in $(1); do $(call SAFE_RM,$$item); done
endef
