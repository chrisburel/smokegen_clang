#ifndef SMOKEGEN_ASTVISITOR
#define SMOKEGEN_ASTVISITOR

#include <clang/AST/RecursiveASTVisitor.h>
#include "generator.h"
#include "options.h"

class SmokegenASTVisitor : public clang::RecursiveASTVisitor<SmokegenASTVisitor> {
public:
    SmokegenASTVisitor(SmokeGenerator &generator, Options *options) :
        generator(generator), options(options) {}

    bool VisitCXXRecordDecl(clang::CXXRecordDecl *D);

private:
    SmokeGenerator &generator;
    Options *options;
};

#endif
