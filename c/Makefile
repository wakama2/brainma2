CC = cc
TARGET = brainfuck
OBJS = brainfuck.o
CFLAGS ?= -O2
#CFLAGS ?= -g3 -O0 -pg

$(TARGET) : $(OBJS)
	$(CC) -o $(TARGET) $(OBJS) $(CFLAGS)

.SUFFIXES: .c.o

.c.o:
	$(CC) -c $*.c $(CFLAGS)

clean:
	rm -f $(TARGET) $(OBJS) *~

