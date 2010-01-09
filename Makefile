
ERL=erl
APP=procket

CC=gcc

#ERL_LIB=/usr/local/lib/erlang
ERL_ROOT=/media/opt/local/lib/erlang
ARCH=-m32
CFLAGS=-g -Wall
FLAGS=-fPIC -shared


all: dir erl ancillary nif cmd

dir:
	-@mkdir -p priv/tmp ebin

erl:
	@$(ERL) -noinput +B \
		-eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

ancillary:
	(cd c_src && $(MAKE) -f Makefile.ancillary)

nif:
	$(CC) -g -Wall $(FLAGS) -o priv/procket.so -L c_src \
		c_src/procket.c -lancillary -I $(ERL_ROOT)/usr/include/

cmd:
	$(CC) -g -Wall -o priv/procket -L c_src c_src/procket_cmd.c -lancillary

clean:  
	@rm -fv ebin/*.beam priv/$(APP) priv/$(APP).so c_src/*.a c_src/*.o


