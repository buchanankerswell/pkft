#!/bin/zsh

# Exit if any command fails
set -e
# Check for R dependencies
R/packages.R
# Run Application
cd app
./app.R