#ifndef SMOKEGEN_ASTVISITOR
#define SMOKEGEN_ASTVISITOR

#include <clang/AST/RecursiveASTVisitor.h>
#include "options.h"

class SmokegenASTVisitor : public clang::RecursiveASTVisitor<SmokegenASTVisitor> {
public:
    SmokegenASTVisitor(Options *options) :
        options(options) {}

    bool VisitCXXRecordDecl(clang::CXXRecordDecl *D);

private:
    Options *options;
};

#endif
