include ../include.mk

ERLC_FLAGS += -I ../include -I ../src

all:

clean:
	-rm -f ${EBIN_DIR}/et.beam


$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<
