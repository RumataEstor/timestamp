REBAR=$(shell which rebar)

all: compile

compile:
	$(REBAR) compile

test: compile
	$(REBAR) eunit
