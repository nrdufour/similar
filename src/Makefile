include ../support/include.mk

all: $(EBIN_FILES)
	(cd loggers;$(MAKE))
	(cd process;$(MAKE))
	(cd engine;$(MAKE))

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
	(cd loggers;$(MAKE) clean)
	(cd process;$(MAKE) clean)
	(cd engine;$(MAKE) clean)
