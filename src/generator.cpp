#include "generator.h"
#include "util.h"

void SmokeGenerator::addClass(clang::CXXRecordDecl *D) {
    classes[D->getQualifiedNameAsString()] = D;
}

void SmokeGenerator::processDataStructures() {
    for (auto const &klass : classes) {
        if (contains(options->classList, klass.first) && klass.second->hasDefinition()) {
            classIndex[klass.first] = 1;
        }
    }

    for (auto const &klass : classIndex) {
        includedClasses.push_back(klass.first);
    }

    int i = 1;
    for (auto &idx : classIndex) {
        idx.second = i++;
    }
}

void SmokeGenerator::writeDataFile(llvm::raw_ostream &out) {

    // Write include lines
    out << "#include <smoke.h>\n";
    out << "#include <" << options->module << "_smoke.h>\n\n";

    // All this stuff goes inside a namespace
    std::string smokeNamespaceName("__smoke" + options->module);

    out << "namespace " << smokeNamespaceName << " {\n\n";

    // Write out cast function
    out << "static void *cast(void *xptr, Smoke::Index from, Smoke::Index to) {\n";
    out << "  switch(from) {\n";
    for (auto const &iter : classIndex) {
        const clang::CXXRecordDecl *klass = classes[iter.first];

        std::set<int> indices; // avoid duplicate case values (diamond-shape inheritance)
        out << "    case " << iter.second << ":   //" << iter.first << "\n";
        out << "      switch(to) {\n";
        // Add our parent classes to the cast
        for (auto const &base : superClassList(klass)) {
            auto className = base->getQualifiedNameAsString();

            if (contains(includedClasses, className)) {
                int index = classIndex[className];
                if (indices.count(index))
                    continue;

                indices.insert(index);

                out << "        case " << index << ": return (void*)(" << className << "*)(" << klass->getQualifiedNameAsString() << "*)xptr;\n";
            }
        }
        // Add ourself to the cast
        out << "        case " << iter.second << ": return (void*)(" << klass->getQualifiedNameAsString() << "*)xptr;\n";

        // Add our subclasses to the cast
        for (auto const &desc : descendantsList(klass)) {
            auto className = desc->getQualifiedNameAsString();

            if (contains(includedClasses, className)) {
                int index = classIndex[className];
                if (indices.count(index))
                    continue;

                indices.insert(index);

                //if (Util::isVirtualInheritancePath(desc, &klass)) {
                //    out << QString("        case %1: return (void*)dynamic_cast<%2*>((%3*)xptr);\n")
                //        .arg(index).arg(className).arg(klass.toString());
                //} else {
                    out << "        case " << index << ": return (void*)(" << className << "*)(" << klass->getQualifiedNameAsString() << "*)xptr;\n";
                //}
            }
        }

        out << "        default: return xptr;\n";
        out << "      }\n";
    }
    out << "    default: return xptr;\n";
    out << "  }\n";
    out << "}\n\n";

    out << "}\n\n"; // end namespace definition

    out << "extern \"C\" {\n\n";

    out << "static bool initialized = false;\n";
    out << "Smoke *" << options->module << "_Smoke = 0;\n\n";
    out << "// Create the Smoke instance encapsulating all the above.\n";
    out << "void init_" << options->module << "_Smoke() {\n";
    for (auto const &parentName : options->parentModules) {
        out << "    init_" << parentName << "_Smoke();\n";
    }
    out << "    if (initialized) return;\n";
    out << "    " << options->module << "_Smoke = new Smoke(\n";
    out << "        \"" << options->module << "\",\n";
    out << "        " << smokeNamespaceName << "::classes, " << /*classCount <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::methods, " << /*methodCount <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::methodMaps, " << /*methodMapCount <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::methodNames, " << /*methodNames.count() <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::types, " << /*typeIndex.count() <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::inheritanceList,\n";
    out << "        " << smokeNamespaceName << "::argumentList,\n";
    out << "        " << smokeNamespaceName << "::ambiguousMethodList,\n";
    out << "        " << smokeNamespaceName << "::cast );\n";
    out << "    initialized = true;\n";
    out << "}\n\n";
    out << "void delete_" << options->module << "_Smoke() { delete " << options->module << "_Smoke; }\n\n";
    out << "}\n";
}

std::set<const clang::CXXRecordDecl *> SmokeGenerator::superClassList(const clang::CXXRecordDecl *klass) const {
    std::set<const clang::CXXRecordDecl *> ret;

    // We can't get the base classes of a class that has no definition
    if (!klass->hasDefinition())
        return ret;

    for (auto const &base : klass->bases()) {
        clang::CXXRecordDecl *baseRecord = base.getType()->getAsCXXRecordDecl();
        if (baseRecord) {
            ret.insert(baseRecord);
            std::set<const clang::CXXRecordDecl *> baseBases = superClassList(baseRecord);
            ret.insert(baseBases.begin(), baseBases.end());
        }
    }
    return ret;
}

std::set<const clang::CXXRecordDecl *> SmokeGenerator::descendantsList(const clang::CXXRecordDecl *klass) const {
    std::set<const clang::CXXRecordDecl *> ret;
    for (auto const &iter : classes) {
        std::set<const clang::CXXRecordDecl *> superClasses = superClassList(iter.second);
        if (superClasses.count(klass)) {
            ret.insert(iter.second);
        }
    }
    return ret;
}
