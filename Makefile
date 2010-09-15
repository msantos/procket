
REBAR=./rebar
TEMPLATE=rebar.config.tmpl
CONFIG=rebar.config

all: compile

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

