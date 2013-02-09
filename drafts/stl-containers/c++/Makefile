all: libstl.a test

libstl.a: hashmap.o
	ar -cvq libstl.a hashmap.o

hashmap.o: hashmap.cpp hashmap.h
	g++ -std=c++11 -O2 -c hashmap.cpp -o hashmap.o

test: libstl.a test.cpp
	g++ -std=c++11 test.cpp -L. -lstl -o test

clean:
	rm -f *.a *.o
