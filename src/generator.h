#ifndef SMOKEGEN_GENERATOR
#define SMOKEGEN_GENERATOR

#include "options.h"

class SmokeGenerator {
public:
    SmokeGenerator(Options *options) : options(options) {};

private:
    Options *options;
};

#endif
