CONDA_ENV_ID := pkft
CONDA_ENV_YML := environment.yml
APP_MODULE := pkft.api:app
HOST := 127.0.0.1
SERVE_PORT := 8000

include project.mk

.DEFAULT_GOAL := help

APP := $(PROJECT_ROOT)/pkft
TESTS := $(PROJECT_ROOT)/tests
DOCS := $(PROJECT_ROOT)/docs
CONDA_PYTHON = $$(conda run -n $(CONDA_ENV_ID) which python)
CLEAN := $(PROJECT_ROOT)/.pytest_cache $(PROJECT_ROOT)/build $(PROJECT_ROOT)/dist $(PROJECT_ROOT)/*.egg-info $(PROJECT_ROOT)/.coverage $(PROJECT_ROOT)/coverage.xml
DEEP_CLEAN := $(PROJECT_ROOT)/.log $(DOCS)/_build

.PHONY: all build run docs test format environment clean deep-clean help

build: environment test docs
	@echo "    --------------------------------------------------"
	@echo "    pkft built successfully!"
	@echo "    --------------------------------------------------"

all: build run

run:
	@$(CONDA_PYTHON) -m uvicorn $(APP_MODULE) --host $(HOST) --port $(SERVE_PORT) --reload

docs: $(LOG_FILE)
	@echo " -> $(DOCS)/_build/html"
	@$(MAKE) --no-print-directory -C $(DOCS) html || exit 1

test:
	@echo " -> running pytest"
	@$(CONDA_PYTHON) -m pytest --cov=pkft --cov-branch --cov-report=xml

format:
	@$(CONDA_PYTHON) -m ruff format .
	@npx --yes prettier --write "pkft/static/*.js" "pkft/static/*.css"

environment: $(CONDA_ENV_YML) $(LOG_FILE)
	@if conda info --envs | awk '{print $$1}' | grep -qx "$(CONDA_ENV_ID)"; then \
		echo " -- Environment '$(CONDA_ENV_ID)' already exists. Skipping..."; \
	else \
		echo " -> Creating environment: $(CONDA_ENV_ID) from $(CONDA_ENV_YML)"; \
		conda env create -n "$(CONDA_ENV_ID)" -f "$(CONDA_ENV_YML)" $(LOGGER) || exit 1; \
	fi

clean:
	@echo "    --------------------------------------------------"
	@echo "    Cleaning ..."
	@echo "    --------------------------------------------------"
	@$(call SAFE_RM_LIST,$(CLEAN))
	@find $(APP) $(TESTS) -name "__pycache__" -exec echo " xx {}" \; -exec rm -rf {} +
	@find $(APP) $(TESTS) -name "*.pyc" -exec echo " xx {}" \; -delete
	@$(MAKE) --no-print-directory -C $(DOCS) clean || true
	@find . -name ".DS_Store" -type f -delete

deep-clean: clean
	@echo "    --------------------------------------------------"
	@echo "    Deep cleaning ..."
	@echo "    --------------------------------------------------"
	@$(call SAFE_RM_LIST,$(DEEP_CLEAN))

$(LOG_FILE):
	@mkdir -p $(dir $(LOG_FILE))
	@touch $(LOG_FILE)

help:
	@echo "    --------------------------------------------------"
	@echo "    Available targets:"
	@echo "    --------------------------------------------------"
	@echo "    build        Build pkft from scratch (environment + test + docs)"
	@echo "    all          Build then run the app locally"
	@echo "    run          Run pkft app locally"
	@echo "    docs         Build documentation"
	@echo "    test         Run unit tests"
	@echo "    format       Format code to 150 columns (ruff + prettier)"
	@echo "    environment  Create Conda environment"
	@echo "    clean        Cleanup unnecessary files and directories (safe)"
	@echo "    deep-clean   Deep clean generated files and logs (use with caution!)"
	@echo "    help         Show this help message"
