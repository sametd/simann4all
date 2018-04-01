CC=ifort
CFLAGS= -march=native -O3

main.x: mersenne_twister.o sa_prints.o test_fcn.o simann.o
	$(CC) $(CFLAGS) -o main.x main.f90 mersenne_twister.o sa_prints.o \
	test_fcn.o simann.o

mersenne_twister.o : mersenne_twister.f90
	$(CC) -c $(CFLAGS) mersenne_twister.f90

sa_prints.o : sa_prints.f90
	$(CC) -c $(CFLAGS) sa_prints.f90

test_fcn.o : test_fcn.f90
	$(CC) -c $(CFLAGS) test_fcn.f90

simann.o : simann.f90
	$(CC) -c $(CFLAGS) simann.f90

clean:
	rm -f main.x *.o *.mod