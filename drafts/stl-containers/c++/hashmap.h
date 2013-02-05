#ifndef __hashmap_h__
#define __hashmap_h__

#include <vector>
#include <unordered_map>
#include <cstdint>
#include <boost/functional/hash.hpp>

typedef std::vector<uint8_t> Key;
typedef std::vector<uint8_t> Value;
typedef std::unordered_map<Key, Value, boost::hash<Key>> HashMap;

#ifdef __cplusplus
extern "C"
{
#endif

    HashMap* hashmap_create();
    void hashmap_destroy(HashMap* h);

    void hashmap_insert(HashMap* h, const uint8_t* key, std::size_t nK, const uint8_t* val, std::size_t nV);
    void hashmap_lookup(HashMap* h, const uint8_t* key, std::size_t nK, uint8_t** pVal, std::size_t* pNV);
    void hashmap_delete(HashMap* h, const uint8_t* key, std::size_t nK);
    std::size_t hashmap_size(const HashMap* h);

    HashMap::iterator* iter(HashMap* h);
    bool hasNext(HashMap* h, HashMap::iterator* it);
    HashMap::iterator* next(HashMap* h, HashMap::iterator* it, uint8_t** pVal, std::size_t* pNV);
    
#ifdef __cplusplus
}
#endif

#endif
