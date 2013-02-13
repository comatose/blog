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

    HashMap::iterator* it = iter_create(h);
    while(iter_hasNext(h, it)){
	uint8_t *pKey, *pVal;
	size_t nKey, nVal;
	iter_next(h, it, &pKey, &nKey, &pVal, &nVal);
	cout << "[" << nKey << ":" << nVal << "]" << endl;
    }
    iter_destroy(it);

    hashmap_destroy(h);
    return EXIT_SUCCESS;
}
