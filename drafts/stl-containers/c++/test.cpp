#include "hashmap.h"

#include <iostream>
#include <fstream>

using namespace std;

int main(int argc, char** argv){
    if(argc < 2){
	return EXIT_FAILURE;
    }
	
    fstream f(argv[1], ios::binary | ios::in);
    uint8_t block[512];
    HashMap* h = hashmap_create();
    while(f.good()){
	size_t n = f.readsome((char*)block, sizeof(block));
	if(n < 1)
	    break;
	hashmap_insert(h, block, n, block, n / 2);
    }
    cout << hashmap_size(h) << endl;
    hashmap_destroy(h);
    return EXIT_SUCCESS;
}
