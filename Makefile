#REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR=./rebar
REBAR_FLAGS ?=

all: compile

compile:
	$(REBAR) compile $(REBAR_FLAGS)

test:
	$(REBAR) eunit $(REBAR_FLAGS)

clean:
	$(REBAR) clean $(REBAR_FLAGS)
