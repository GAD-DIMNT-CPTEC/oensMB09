CD = cd
RM = rm
MAKE = make

MAKEFILE = Makefile

PRJDIRS = ensmed spread probability probagr cluster plumes 

# not real file targets
.PHONY: $(MAKEFILE) all clean

# targets
all:
	@$(CD) libs/bacio && \
	./makebacio_cptec.sh $@
	@$(CD) libs/w3emc && \
	./makew3emc_cptec.sh $@
	@for dir in $(PRJDIRS); do \
		($(CD) $$dir/source && $(MAKE) $@); \
	done

# cleanup
clean:
	$(CD) libs/bacio/NCEPLIBS-bacio-2.5.0/ && \
  rm -rf build rls
	$(CD) libs/w3emc/NCEPLIBS-w3emc-2.9.3/ && \
  rm -rf build rls
	@for dir in $(PRJDIRS); do \
		($(CD) $$dir/source && $(RM) *.o *.mod); \
		($(CD) $$dir/bin && $(RM) *.x); \
	done
