#include "frontendaction.h"
#include "astconsumer.h"

std::unique_ptr<clang::ASTConsumer>
SmokegenFrontendAction::CreateASTConsumer(clang::CompilerInstance &CI, clang::StringRef file) {
    return llvm::make_unique<SmokegenASTConsumer>(CI, options);
}
