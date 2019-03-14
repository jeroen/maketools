include $(R_MAKECONF)
ifeq ($(PROG),)
$(error empty variable or command: $(value PROG))
endif
all:
	@$(PROG) $(ARGS)

print-value:
	@echo $(PROG)
