SHELL:=bash

all:
	$(MAKE) -C src
	$(MAKE) -C bin

clean:
	for f in src_*; do $(MAKE) -C $$f clean; done
