#ifndef SMOKEGEN_FRONTENDACTION
#define SMOKEGEN_FRONTENDACTION

#include <clang/Frontend/FrontendActions.h>
#include <clang/Frontend/CompilerInstance.h>

// For each source file provided to the tool, a new FrontendAction is created.
class SmokegenFrontendAction : public clang::ASTFrontendAction {
public:
    SmokegenFrontendAction() {}

    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance &CI, clang::StringRef file) override;
};

#endif
