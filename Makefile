.DEFAULT_GOAL := all

%:
	cd src && $(MAKE) $@
