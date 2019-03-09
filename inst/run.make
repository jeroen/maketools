include $(R_MAKECONF)
ifeq ($(PROG),)
$(error empty command (variable undefined))
endif
all:
	@$(PROG) $(ARGS)
