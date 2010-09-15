
REBAR=./rebar

all: compile

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

