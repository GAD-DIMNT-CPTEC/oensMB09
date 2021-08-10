CD = cd
RM = rm
MAKE = make

MAKEFILE = Makefile

PRJDIRS = ensmed spread probability probagr cluster plumes 

# not real file targets
.PHONY: $(MAKEFILE) all clean

# targets
all:
	@$(CD) libs/w3lib-1.4 && \
	$(MAKE)  
	@for dir in $(PRJDIRS); do \
		($(CD) $$dir/source && $(MAKE) $@); \
	done

# cleanup
clean:
	@$(CD) libs/w3lib-1.4 && \
	$(MAKE) $@ 
	@for dir in $(PRJDIRS); do \
		($(CD) $$dir/source && $(RM) *.o *.mod); \
		($(CD) $$dir/bin && $(RM) *.x); \
	done
