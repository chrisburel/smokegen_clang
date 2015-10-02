#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>

#include "astvisitor.h"
#include "astconsumer.h"
#include "frontendaction.h"
#include "options_yaml.h"
#include "options.h"

static llvm::cl::OptionCategory SmokegenCategory("SMOKE binding generator");

llvm::cl::opt<std::string> SmokeConfigFile(
    "smokeconfig",
    llvm::cl::desc("Configuration options for the generated classes"),
    llvm::cl::value_desc("filename"),
    llvm::cl::cat(SmokegenCategory)
);

constexpr char SizeTDecider<unsigned int>::smokeName[];
constexpr char SizeTDecider<unsigned long>::smokeName[];

class SmokegenFrontendActionFactory : public clang::tooling::FrontendActionFactory {
public:
    SmokegenFrontendActionFactory(Options &options) : options(&options) {}

    virtual clang::FrontendAction *create() {
        return new SmokegenFrontendAction(options);
    }

private:
    Options *options;
};

int main(int argc, const char **argv) {
    clang::tooling::CommonOptionsParser op(argc, argv, SmokegenCategory);
    clang::tooling::ClangTool Tool(op.getCompilations(), op.getSourcePathList());

    // Load data from options file
    auto & FM = Tool.getFiles();
    auto optionsFile = FM.getBufferForFile(SmokeConfigFile);

    if (not optionsFile) {
        llvm::outs() << "Unable to load options file.\n";
        return 1;
    }

    Options options;
    llvm::yaml::Input yin((*optionsFile)->getBuffer());
    yin >> options;
    if (yin.error()) {
        llvm::outs() << "Error reading options file.\n";
        return 1;
    }

    // ClangTool::run accepts a FrontendActionFactory, which is then used to
    // create new objects implementing the FrontendAction interface. Here we use
    // the helper newFrontendActionFactory to create a default factory that will
    // return a new SmokegenFrontendAction object every time.
    // To further customize this, we could create our own factory class.
    SmokegenFrontendActionFactory factory(options);
    return Tool.run(&factory);
}
