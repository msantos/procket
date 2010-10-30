
CC=gcc
CMD_PATH= $(dir$(lastword $(MAKEFILE_LIST)))../priv/procket

all: compile

compile:
	$(CC) $(ARCH) -g -Wall -o $(CMD_PATH) -L. procket_cmd.c -lancillary

