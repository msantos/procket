
CC=gcc

all: compile

compile:
	$(CC) $(ARCH) -g -Wall -o ../priv/procket -L. procket_cmd.c -lancillary

