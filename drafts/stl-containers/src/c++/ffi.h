#ifndef __ffi_h__
#define __ffi_h__

#ifdef __cplusplus
extern "C"
{
#endif

    void* hashmap_create();
    void hashmap_destroy(void* h);

    void hashmap_insert(void* h, const unsigned char* key, unsigned int nK, const unsigned char* val, unsigned int nV);
    void hashmap_lookup(void* h, const unsigned char* key, unsigned int nK, unsigned char** pVal, unsigned int* pNV);
    void hashmap_delete(void* h, const unsigned char* key, unsigned int nK);
    unsigned int hashmap_size(const void* h);

    void* iter_create(void* h);
    void iter_destroy(void* it);

    int iter_hasNext(void* h, void* it);
    void iter_next(void* h, void* it, unsigned char** pKey, unsigned int* pNK, unsigned char** pVal, unsigned int* pNV);
    
#ifdef __cplusplus
}
#endif

#endif
