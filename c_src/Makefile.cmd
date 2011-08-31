
CC=gcc
CMD_DIR= $(dir$(lastword $(MAKEFILE_LIST)))../priv
CMD_PATH= $(CMD_DIR)/procket

all: dirs compile

dirs:
	-@mkdir -p $(CMD_DIR)

compile:
	$(CC) $(PROCKET_CFLAGS) -g -Wall -o $(CMD_PATH) -L. procket_cmd.c -lancillary
