#include <algorithm>
#include <list>

#include "generator.h"

void SmokeGenerator::addClass(clang::CXXRecordDecl* D) {
    llvm::outs() << D->getQualifiedNameAsString() << "\n";

    classes[D->getQualifiedNameAsString()] = D;

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

    // Our x_* subclasses will all have destructors.  Add the destructor method
    // to the methodNames set.
    methodNames.insert('~' + D->getNameAsString());
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

std::string SmokeGenerator::getClassesCode() const {
    // Initialize the inheritance list to one with no inheritance.
    std::list<std::list<clang::QualType> > inheritanceList = {{}};

    std::list<const clang::Type *> allClassTypes;

    std::string inheritanceOutput(
            "// Group of Indexes (0 separated) used as super class lists.\n"
            "// Classes with super classes have an index into this array.\n"
            "static Smoke::Index inheritanceList[] = {\n");


    std::string output("// List of all classes\n"
            "// Name, external, index into inheritanceList, method dispatcher, enum dispatcher, class flags, size\n"
            "static Smoke::Class classes[] = {\n"
            "    { 0L, false, 0, 0, 0, 0, 0 },	// 0 (no class)\n");


    int i = 1;
    for (auto const & kv : classes) {
        // Find inheritance index
        int inheritanceIndex = 0;
        auto klass = kv.second;
        std::list<clang::QualType> bases;
        for (auto base : klass->bases()) {
            bases.push_back(base.getType());
        }
        auto found = std::find(inheritanceList.begin(), inheritanceList.end(), bases);
        if (found == inheritanceList.end()) {
            inheritanceList.push_back(bases);
            found = --inheritanceList.end();
        }
        for (auto it = inheritanceList.begin(); it != found; ++it) {
            inheritanceIndex += (*it).size() + 1;
        }

        output += "    { "
            "\"" + kv.first + "\", " // name
            "false, " + // external
            std::to_string(inheritanceIndex) + ", " + // index into inheritance list
            getXCallName(kv.second) + ", " // method dispacher
            "0, " // enum dispacher
            "0, " // class flags
            "sizeof(" + kv.first + ") },\t" // size
            "//" + std::to_string(i++) + "\n";

        allClassTypes.push_back(klass->getTypeForDecl());
    }

    i = 0;
    for (auto const & inheritanceGroup : inheritanceList) {
        std::string thisLine("    ");
        std::string typeNameList;
        for (auto const & entry : inheritanceGroup) {
            auto found = std::find(allClassTypes.begin(), allClassTypes.end(), &(*entry));
            int distance = std::distance(allClassTypes.begin(), found) + 1; // account for leading empty entry
            thisLine += std::to_string(distance) + ", ";
            typeNameList += entry.getAsString();
            typeNameList += ", ";
        }
        if (i == 0) {
            typeNameList += "(no super class)";
        }

        thisLine += "0,\t// " + std::to_string(i) + ": " + typeNameList + "\n";
        i += inheritanceGroup.size() + 1;

        inheritanceOutput += thisLine;
    }

    output += "};\n";
    inheritanceOutput += "};\n";

    return inheritanceOutput + output;
}

std::string SmokeGenerator::getDataFileCode() const {
    std::string output;
    output += getClassesCode() + "\n";
    output += getMethodNamesCode() + "\n";
    return output;
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

std::string SmokeGenerator::getXCallName(clang::CXXRecordDecl *D) const {
    std::string xcallName = "xcall_" + D->getQualifiedNameAsString();
    std::replace(xcallName.begin(), xcallName.end(), ':', '_');
    return xcallName;
}
