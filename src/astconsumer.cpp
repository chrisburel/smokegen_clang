#include "astconsumer.h"

bool SmokegenASTConsumer::HandleTopLevelDecl(clang::DeclGroupRef DR) {

    for (clang::DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b) {
        // Only traverse declarations made in the main file
        auto SL = (*b)->getLocation();
        SL = ci.getSourceManager().getExpansionLoc(SL);
        if (ci.getSourceManager().getFileID(SL) != ci.getSourceManager().getMainFileID())
            continue;

        // Traverse the declaration using our AST visitor.
        Visitor.TraverseDecl(*b);
    }
    return true;
}
