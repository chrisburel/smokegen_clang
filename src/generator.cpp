#include "generator.h"

void SmokeGenerator::addClass(clang::CXXRecordDecl *D) {
    classes[D->getQualifiedNameAsString()] = D;
}
