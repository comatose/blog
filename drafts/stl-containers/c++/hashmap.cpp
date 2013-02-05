#include "hashmap.h"

#include <iostream>
#include <algorithm>

using namespace std;

HashMap* hashmap_create(){
    return new HashMap();
}

void hashmap_destroy(HashMap* h){
    delete h;
    h = NULL;
    cout << "hashmap_delete" << endl;
}

void hashmap_insert(HashMap* h, const uint8_t* key, size_t nK, const uint8_t* val, size_t nV){
    cout << "[I]" << nK << ":" << nV << endl;
    h->emplace(make_pair(Key(key, key + nK), Value(val, val + nV)));
}

void hashmap_lookup(HashMap* h, const uint8_t* key, size_t nK, uint8_t** pVal, size_t* pNV){
    cout << "[L]" << nK << ":";
    auto it = h->find(Key(key, key + nK));
    if(it == h->end()){
	*pVal = NULL;
	*pNV = 0;
    }
    else{
	*pVal = it->second.data();
	*pNV = it->second.size();
    }
    cout << *pNV << endl;
}

void hashmap_delete(HashMap* h, const uint8_t* key, size_t nK){
    cout << "[D]" << nK << endl;
    h->erase(Key(key, key + nK));
}

size_t hashmap_size(const HashMap* h){
    return h->size();
}
