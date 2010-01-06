include ../include.mk

ERLC_FLAGS += -I ../include

all: $(ERL_OBJECTS)

clean:
	-rm -f $(ERL_OBJECTS)

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<
