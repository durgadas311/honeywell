CC = cc200
AS = as200
LD = ld200

all: hello.ohw

%.ohw: %.c
	$(CC) -o $@ $<

%.o: %.s
	$(AS) -u -o $@ $<

%.o: %.c
	$(CC) -c $<

clean:
	rm -f *.ohw *.o
