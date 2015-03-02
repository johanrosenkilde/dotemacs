ELs = $(wildcard *.el)
all: $(ELs:%.el=%.elc)

%.elc: %.el
	byte-compile $*.el

.PHONY: clean
clean:
	rm -rf *.elc
