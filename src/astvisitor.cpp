#include "astvisitor.h"

bool SmokegenASTVisitor::VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    if (!D->hasDefinition())
        return false;

    // We can't make bindings for things that don't have names.
    if (!D->getDeclName())
        return false;

    generator.addClass(D);

    return true;
}
