#ifndef SMOKEGEN_ASTCONSUMER
#define SMOKEGEN_ASTCONSUMER

#include <clang/AST/ASTConsumer.h>
#include <clang/Frontend/CompilerInstance.h>

#include "astvisitor.h"

class SmokegenASTConsumer : public clang::ASTConsumer {
public:
    SmokegenASTConsumer(clang::CompilerInstance &ci) : ci(ci) {}

    // Override the method that gets called for each parsed top-level
    // declaration.
    bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

private:
    SmokegenASTVisitor Visitor;
    clang::CompilerInstance &ci;
};

#endif
