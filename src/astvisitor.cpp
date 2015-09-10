#include "astvisitor.h"

bool SmokegenASTVisitor::VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    if (!D->hasDefinition())
        return false;

    llvm::outs() << "Visiting " << D->getQualifiedNameAsString() << "\n";

    for (auto method : D->methods()) {
        for (auto attr_it = method->specific_attr_begin<clang::AnnotateAttr>();
          attr_it != method->specific_attr_end<clang::AnnotateAttr>();
          ++attr_it) {

            const clang::AnnotateAttr *A = *attr_it;

            if (A->getAnnotation() == "qt_signal") {
                llvm::outs() << method->getQualifiedNameAsString() << "(signal)\n";
            }
            else if (A->getAnnotation() == "qt_slot") {
                llvm::outs() << method->getQualifiedNameAsString() << "(slot)\n";
            }
        }
    }

    return true;
}
