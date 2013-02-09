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
    cout << "hashmap_destory" << endl;
}

void hashmap_insert(HashMap* h, const uint8_t* key, size_t nK, const uint8_t* val, size_t nV){
    // cout << "[I]" << nK << ":" << nV << endl;
    (*h)[Key(key, key + nK)] = Value(val, val + nV);
    // h->emplace(make_pair(Key(key, key + nK), Value(val, val + nV)));
}

void hashmap_lookup(HashMap* h, const uint8_t* key, size_t nK, uint8_t** pVal, size_t* pNV){
    // cout << "[L]" << nK << ":";
    auto it = h->find(Key(key, key + nK));
    if(it == h->end()){
	*pVal = NULL;
	*pNV = 0;
    }
    else{
	*pVal = it->second.data();
	*pNV = it->second.size();
    }
    // cout << *pNV << endl;
}

void hashmap_delete(HashMap* h, const uint8_t* key, size_t nK){
    // cout << "[D]" << nK << endl;
    h->erase(Key(key, key + nK));
}

size_t hashmap_size(const HashMap* h){
    return h->size();
}

HashMap::iterator* iter_create(HashMap* h){
    return new HashMap::iterator(h->begin());
}

void iter_destroy(HashMap::iterator* it){
    delete it;
    cout << "iter_destory" << endl;
}

bool iter_hasNext(HashMap* h, HashMap::iterator* it){
    return (*it != h->end());
}

void iter_next(HashMap* h, HashMap::iterator* it, uint8_t** pKey, std::size_t* pNK
	       , uint8_t** pVal, std::size_t* pNV){
    *pKey = const_cast<uint8_t*>((*it)->first.data());
    *pNK = (*it)->first.size();
    *pVal = (*it)->second.data();
    *pNV = (*it)->second.size();
    ++(*it);
}
