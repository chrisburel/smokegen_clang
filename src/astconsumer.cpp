#include "astconsumer.h"
#include "ppcallbacks.h"

void SmokegenASTConsumer::Initialize(clang::ASTContext &ctx) {
    generator.setASTContext(&ctx);
    ppCallbacks = new SmokegenPPCallbacks(ci.getPreprocessor());
    ci.getPreprocessor().addPPCallbacks(std::unique_ptr<SmokegenPPCallbacks>(ppCallbacks));
}

bool SmokegenASTConsumer::HandleTopLevelDecl(clang::DeclGroupRef DR) {

    for (clang::DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
        // Traverse the declaration using our AST visitor.
        Visitor.TraverseDecl(*b);
    }
    return true;
}

void SmokegenASTConsumer::HandleTranslationUnit(clang::ASTContext& Ctx) {
    auto dataFileOut = ci.createOutputFile(
        options->outputDir + "smokedata.cpp",
        false,
        true,
        "",
        "",
        false,
        true
    );
    if (not dataFileOut) {
        return;
    }
    generator.processDataStructures();
    generator.writeDataFile(*dataFileOut);
    generator.writeClassFiles();
}
