#include "bigloo.h"
#include <unistd.h>

BGL_RUNTIME_DEF
long bgl_get_number_of_processors(void) {
  return (long) sysconf(_SC_NPROCESSORS_ONLN);
}
