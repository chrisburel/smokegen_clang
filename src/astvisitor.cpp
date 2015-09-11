#include "astvisitor.h"

bool SmokegenASTVisitor::VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    if (!D->hasDefinition())
        return false;

    llvm::outs() << D->getQualifiedNameAsString() << "\n";

    for (auto method : D->methods()) {
        std::string signature("    ");

        signature += method->getNameAsString() + "(";

        auto end = method->param_end();
        --end;
        for (auto param : method->params()) {
            auto type = param->getType();
            signature += type.getAsString();
            if (param != *end) {
                signature += ", ";
            }
        }

        signature += ")";

        for (auto attr_it = method->specific_attr_begin<clang::AnnotateAttr>();
          attr_it != method->specific_attr_end<clang::AnnotateAttr>();
          ++attr_it) {

            const clang::AnnotateAttr *A = *attr_it;

            if (A->getAnnotation() == "qt_signal") {
                signature += "(signal)";
            }
            else if (A->getAnnotation() == "qt_slot") {
                signature += "(slot)";
            }
        }

        llvm::outs() << signature + "\n";
    }

    return true;
}
