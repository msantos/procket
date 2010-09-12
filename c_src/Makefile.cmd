
CC=gcc

all: compile

compile:
	$(CC) -g -Wall -o ../priv/procket -L. procket_cmd.c -lancillary

