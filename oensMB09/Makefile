# include makefile configure
include ./config/Makefile.conf.$(comp)

PRJDIRS = $(FFTPLN) $(EOFTEMP) $(EOFHUMI) $(EOFWIND) $(DECEOF) $(RECANL) $(RDPERT) $(DECANL) $(RECFCT) $(EOFPRES)

# not real file targets
.PHONY: $(MAKEFILE) all clean help 

# targets
all:	
	@$(CD) $(FFTPLN) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(EOFTEMP) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(EOFHUMI) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(EOFWIND) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(DECEOF) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(RECANL) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(RDPERT) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(DECANL) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(RECFCT) && \
	$(MAKE) comp=$(comp)

	@$(CD) $(EOFPRES) && \
	$(MAKE) comp=$(comp)

# cleanup
clean:
	@for dir in $(PRJDIRS); do \
		($(CD) $$dir && $(MAKE) comp=$(comp) $@); \
	done
