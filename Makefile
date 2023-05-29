R = R/packages.R app/app.R app/functions.R app/dashboard.R
FIGSPURGE = **/*.png **/*.pdf

all: $(R)
	@./run.sh

purge:
	@rm -rf $(FIGSPURGE)

clean: purge

.PHONY: all purge clean purge