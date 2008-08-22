/* Test the raw performance of the burtleburtle.net hash functions. */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include "lookup3.h"

int main(int argc, char **argv)
{
  uint64_t a = 0xdeadfeed;
  int i;
  
  for (i = 1; i < argc; i++) {
    FILE *fp = fopen(argv[i], "r");
    char *line = NULL;
    size_t n;

    if (fp == NULL) {
      perror(argv[i]);
      continue;
    }

    while (getline(&line, &n, fp) != -1) {
      uint32_t *b = (uint32_t *) &a;
      uint32_t *c = b + 1;
      
      if ((n & 3) == 0)
	_jenkins_hashword2((uint32_t *) line, n >> 2, b, c);
      else
	_jenkins_hashlittle2(line, n, b, c);
    }
  }

  return 0;
}
