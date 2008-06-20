#include <stdint.h>
#include <sys/types.h>

uint32_t _jenkins_hashword(const uint32_t *k, size_t length, uint32_t initval);

uint32_t _jenkins_hashlittle(const void *key, size_t length, uint32_t initval);

void _jenkins_hashword2(const uint32_t *key, size_t length,
			uint32_t *pc, uint32_t *pb);

void _jenkins_hashlittle2(const void *key, size_t length,
			  uint32_t *pc, uint32_t *pb);
