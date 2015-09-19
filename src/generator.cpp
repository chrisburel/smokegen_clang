#include "generator.h"

void SmokeGenerator::addClass(clang::CXXRecordDecl *D) {
    classes[D->getQualifiedNameAsString()] = D;
}

void SmokeGenerator::processDataStructures() {
}

void SmokeGenerator::writeDataFile(llvm::raw_ostream &out) {
}
