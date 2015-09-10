#include "astvisitor.h"

bool SmokegenASTVisitor::VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    if (!D->hasDefinition())
        return false;

    llvm::outs() << "Visiting " << D->getQualifiedNameAsString() << "\n";
    return true;
}
