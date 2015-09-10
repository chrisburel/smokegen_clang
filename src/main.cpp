#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>

#include "astvisitor.h"
#include "astconsumer.h"
#include "frontendaction.h"

static llvm::cl::OptionCategory SmokegenCategory("SMOKE binding generator");

int main(int argc, const char **argv) {
    clang::tooling::CommonOptionsParser op(argc, argv, SmokegenCategory);
    clang::tooling::ClangTool Tool(op.getCompilations(), op.getSourcePathList());

    // ClangTool::run accepts a FrontendActionFactory, which is then used to
    // create new objects implementing the FrontendAction interface. Here we use
    // the helper newFrontendActionFactory to create a default factory that will
    // return a new SmokegenFrontendAction object every time.
    // To further customize this, we could create our own factory class.
    return Tool.run(clang::tooling::newFrontendActionFactory<SmokegenFrontendAction>().get());
}
