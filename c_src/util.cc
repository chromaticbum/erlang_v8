#include "erlang_v8_drv.h"

char *Util::BinaryToBuffer(ErlNifBinary binary) {
  char *buffer = (char *)malloc((binary.size + 1) * sizeof(char));
  memcpy(buffer, binary.data, binary.size);
  buffer[binary.size] = NULL;

  return buffer;
}
