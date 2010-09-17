
CC=gcc

all: dirs compile

dirs:
	-@mkdir ../priv

compile:
	$(CC) $(ARCH) -g -Wall -o ../priv/procket -L. procket_cmd.c -lancillary

