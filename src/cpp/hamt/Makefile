

libgc_object = /usr/local/Cellar/bdw-gc/7.6.0/lib/libgc.a
libgc_include = /usr/local/Cellar/bdw-gc/7.6.0/include/

test_hamt:
	clang++ --std=c++11 -pthread -o3 -Wall -I $(libgc_include) -o test_hamt test_hamt.cpp $(libgc_object)

debug:
	clang++ --std=c++11 -pthread -g -Wall -I $(libgc_include) -o test_hamt test_hamt.cpp $(libgc_object)

clean:
	rm *.o test_hamt *#* *~*


