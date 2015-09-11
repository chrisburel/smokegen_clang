#include "astvisitor.h"

bool SmokegenASTVisitor::VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    if (!D->hasDefinition())
        return false;

    generator.addClass(D);

    return true;
}
