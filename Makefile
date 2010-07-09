
ERL=erl
APP=procket

CC=gcc

# Use "-m64" for 64-bit Erlang installs
ARCH=-m32

# Mac OS X
#FLAGS=$(ARCH) -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common

# Linux
FLAGS=-fPIC -shared

ERL_ROOT=/usr/local/lib/erlang
CFLAGS=-g -Wall


all: dir erl ancillary nif cmd

dir:
	-@mkdir -p priv/tmp ebin

erl:
	@$(ERL) -noinput +B \
		-eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

ancillary:
	(cd c_src && $(MAKE) ARCH=$(ARCH) -f Makefile.ancillary)

nif:
	(cd c_src && \
	$(CC) $(ARCH) -g -Wall $(FLAGS) -o ../priv/procket.so -L.  \
		procket.c -l ancillary -I $(ERL_ROOT)/usr/include/ )

cmd:
	(cd c_src && \
	$(CC) $(ARCH) -g -Wall -o ../priv/procket -L. procket_cmd.c -l ancillary )

clean:  
	@rm -fv ebin/*.beam priv/$(APP) priv/$(APP).so c_src/*.a c_src/*.o


