#REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR=./rebar
REBAR_FLAGS ?=

VSN := "0.8.5"
NAME := eradius

ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/$(NAME)-$(VSN)

ERL_SOURCES  := $(wildcard src/*.erl)
ERL_INCLUDES := $(wildcard include/*.hrl)
ERL_DICTS    := $(wildcard priv/dictionary*.map)
ERL_OBJECTS  := $(ERL_SOURCES:src/%.erl=ebin/%.beam)
APP_FILE := ebin/$(NAME).app

all: compile

compile:
	VSN=$(VSN) $(REBAR) compile $(REBAR_FLAGS)

check: test
test: all
	@rm -rf .eunit
	$(REBAR) eunit $(REBAR_FLAGS)

install: all
	test -d $(DESTDIR)$(ERLDIR) || mkdir -p $(DESTDIR)$(ERLDIR)
	install -d $(DESTDIR)$(ERLDIR)/ebin
	install -d $(DESTDIR)$(ERLDIR)/include
	install -d $(DESTDIR)$(ERLDIR)/priv
	install -p -m 0644 $(APP_FILE) $(DESTDIR)$(ERLDIR)/ebin
	install -p -m 0644 $(ERL_OBJECTS) $(DESTDIR)$(ERLDIR)/ebin
	install -p -m 0644 $(ERL_INCLUDES) $(DESTDIR)$(ERLDIR)/include
	install -p -m 0644 $(ERL_DICTS) $(DESTDIR)$(ERLDIR)/priv

clean:
	$(REBAR) clean $(REBAR_FLAGS)
