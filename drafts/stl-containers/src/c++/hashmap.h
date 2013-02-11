#ifndef __hashmap_h__
#define __hashmap_h__

#include <cstdint>
#include <boost/functional/hash.hpp>
#include <vector>
#include <unordered_map>

typedef std::vector<uint8_t> Key;
typedef std::vector<uint8_t> Value;
typedef std::unordered_map<Key, Value, boost::hash<Key>> HashMap;
typedef HashMap::iterator HashMapIter;

#ifdef __cplusplus
extern "C"
{
#endif

    HashMap* hashmap_create();
    HashMap* hashmap_create_sized(std::size_t size);
    void hashmap_destroy(HashMap* h);

    void hashmap_insert(HashMap* h, const uint8_t* key, std::size_t nK, const uint8_t* val, std::size_t nV);
    void hashmap_lookup(HashMap* h, const uint8_t* key, std::size_t nK, uint8_t** pVal, std::size_t* pNV);
    void hashmap_delete(HashMap* h, const uint8_t* key, std::size_t nK);
    std::size_t hashmap_size(const HashMap* h);

    HashMap::iterator* iter_create(HashMap* h);
    void iter_destroy(HashMap::iterator* it);

    bool iter_hasNext(HashMap* h, HashMap::iterator* it);
    void iter_next(HashMap* h, HashMap::iterator* it, uint8_t** pKey, std::size_t* pNK
		   , uint8_t** pVal, std::size_t* pNV);
    
#ifdef __cplusplus
}
#endif

#endif
