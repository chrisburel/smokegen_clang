#ifndef SMOKEGEN_ASTVISITOR
#define SMOKEGEN_ASTVISITOR

#include <clang/AST/RecursiveASTVisitor.h>
#include "generator.h"

class SmokegenASTVisitor : public clang::RecursiveASTVisitor<SmokegenASTVisitor> {
public:
    SmokegenASTVisitor(SmokeGenerator &generator) : generator(generator) {}

    bool VisitCXXRecordDecl(clang::CXXRecordDecl *D);

private:
    SmokeGenerator &generator;
};

#endif
