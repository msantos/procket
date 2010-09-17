
REBAR=./rebar

all: dirs compile

# sub_dirs is called after port_pre_script
dirs:
	-@mkdir priv

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

