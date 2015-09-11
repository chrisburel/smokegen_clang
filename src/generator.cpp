#include "generator.h"

void SmokeGenerator::addClass(clang::CXXRecordDecl* D) {
    llvm::outs() << D->getQualifiedNameAsString() << "\n";

    for (auto method : D->methods()) {
        if (method->getAccess() == clang::AS_private)
            continue;

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

        auto munged = mungedName(method);
        methodNames.insert(method->getNameAsString());
        methodNames.insert(munged);

        signature += mungedName(method);

        llvm::outs() << signature + "\n";
    }
}

std::string SmokeGenerator::mungedName(clang::FunctionDecl *D) const {
    std::string name = D->getNameAsString();
    for (auto param : D->params()) {
        auto type = param->getType();
        name += munge(type);
    }
    return name;
}

char SmokeGenerator::munge(clang::QualType type) const {
    if (type->isBuiltinType()) {
        // plain scalar
        return '$';
    }
    else if (type->isObjectType()) {
        // object
        return '#';
    }
    else {
        // unknown
        return '?';
    }
}

std::string SmokeGenerator::getMethodNamesCode() const {
    std::string output("// Raw list of all methods, using munged names\n"
            "static const char *methodNames[] = {\n");

    int i = 0;
    for (auto name : methodNames) {
        output += "    \"" + name + "\",\t//" + std::to_string(i++) + "\n";
    }
    output += "};\n";
    return output;
}
