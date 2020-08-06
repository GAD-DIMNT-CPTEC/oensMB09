# include makefile configure
include ./config/Makefile.conf.$(comp)

PRJDIRS = $(FFTPLN) $(EOFTEMP) $(EOFHUMI) $(EOFWIND) $(DECEOF) $(RECANL) $(RDPERT) $(DECANL) $(RECFCT) $(EOFPRES)

# not real file targets
#.PHONY: $(MAKEFILE) all clean install help 
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
#	$(RM) $(CPTECBIN)/* $(CPTECLIB)/*   

# installation
#install:
#	@for dir in $(PRJDIRS); do \
#		($(CD) $$dir && $(MAKE) -f $(MAKEFILE) $@); \
#	done

# help page
help:
	@$(ECHO) "Defined targets:"
	@$(ECHO) "  all    : build targets (default)"
	@$(ECHO) "  clean  : cleanup"
	@$(ECHO) "  install: install executable"
	@$(ECHO) "Defined modes:"
	@$(ECHO) "  opt: enable flags for optimization (default)"
	@$(ECHO) "  dbg: enable flags for debugging"
	@$(ECHO) "  pro: enable flags for profiling"
	@$(ECHO) "Example:"
	@$(ECHO) "  type \`make mode=dbg+pro' to enable dbg and pro flags"
	@$(ECHO) "Defined Compilers:"
	@$(ECHO) "  pgi_cray: pgi compiler on cray machine"
	@$(ECHO) "Example:"
	@$(ECHO) "  type \`make comp=pgi_cray' to compile which pgi compiler"
