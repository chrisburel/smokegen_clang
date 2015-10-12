#ifndef SMOKEGEN_ASTCONSUMER
#define SMOKEGEN_ASTCONSUMER

#include <clang/AST/ASTConsumer.h>
#include <clang/Frontend/CompilerInstance.h>

#include "astvisitor.h"
#include "options.h"

class SmokegenPPCallbacks;

namespace clang {
    class ASTContext;
};

class SmokegenASTConsumer : public clang::ASTConsumer {
public:
    SmokegenASTConsumer(clang::CompilerInstance &ci, Options *options) :
        ci(ci), options(options), generator(ci, options), Visitor(ci, generator) {}

    virtual void Initialize(clang::ASTContext &ctx) override;

    // Override the method that gets called for each parsed top-level
    // declaration.
    bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

    // Override the method that gets called once the translation unit is parsed
    void HandleTranslationUnit(clang::ASTContext& Ctx) override;

private:
    Options* options;
    SmokeGenerator generator;
    SmokegenASTVisitor Visitor;
    clang::CompilerInstance &ci;
    SmokegenPPCallbacks *ppCallbacks;
};

#endif
