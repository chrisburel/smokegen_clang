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

    classIndex["QGlobalSpace"] = 1;

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
        if (!klass)
            continue;

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

    // write out the inheritance list
    std::map<std::vector<int>, int> inheritanceList;
    std::map<const clang::CXXRecordDecl*, int> inheritanceIndex;
    out << "// Group of Indexes (0 separated) used as super class lists.\n";
    out << "// Classes with super classes have an index into this array.\n";
    out << "static Smoke::Index inheritanceList[] = {\n";
    out << "    0,\t// 0: (no super class)\n";

    int currentIdx = 1;
    for (auto const &iter : classIndex) {
        auto const &klass = classes[iter.first];
        if (!klass)
            continue;

        if (externalClasses.count(klass))
            continue;
        std::vector<int> indices;
        std::string comment;
        for (auto const & base : klass->bases()) {
            if (base.getType()->getAsCXXRecordDecl()->getAccess() == clang::AS_private)
                continue;
            auto className = base.getType()->getAsCXXRecordDecl()->getQualifiedNameAsString();
            indices.push_back(classIndex[className]);
            comment += className + ", ";
        }
        if (indices.size() == 0)
            continue;
        int idx = 0;

        if (!inheritanceList.count(indices)) {
            idx = currentIdx;
            inheritanceList[indices] = idx;
            out << "    ";
            for (int i = 0; i < indices.size(); i++) {
                if (i > 0) out << ", ";
                out << indices[i];
                currentIdx++;
            }
            currentIdx++;
            comment = comment.substr(0, comment.size() - 2); // Remove trailing ", "
            out << ", 0,\t// " << idx << ": " << comment << "\n";
        } else {
            idx = inheritanceList[indices];
        }

        // store the index into inheritanceList for the class
        inheritanceIndex[klass] = idx;
    }
    out << "};\n\n"; // end of inheritance list

    // xenum functions
    out << "// These are the xenum functions for manipulating enum pointers\n";
    out << "\n";

    // xcall functions
    out << "// Those are the xcall functions defined in each x_*.cpp file, for dispatching method calls\n";
    for (auto const &iter : classIndex) {
        auto const &klass = classes[iter.first];
        if (klass && (externalClasses.count(klass) || isTemplate(klass)))
            continue;

        std::string smokeClassName = iter.first;
        std::replace(smokeClassName.begin(), smokeClassName.end(), ':', '_');
        out << "void xcall_" << smokeClassName << "(Smoke::Index, void*, Smoke::Stack);\n";
    }

    // classes table
    out << "\n// List of all classes\n";
    out << "// Name, external, index into inheritanceList, method dispatcher, enum dispatcher, class flags, size\n";
    out << "static Smoke::Class classes[] = {\n";
    out << "    { 0L, false, 0, 0, 0, 0, 0 },\t// 0 (no class)\n";
    int classCount = 0;
    for (auto const &iter : classIndex) {
        if (!iter.second)
            continue;

        auto const &klass = classes[iter.first];

        if (externalClasses.count(klass)) {
            out << "    { \""  << iter.first << "\", true, 0, 0, 0, 0, 0 },\t//" << iter.second << "\n";
        } else {
            std::string smokeClassName = iter.first;
            std::replace(smokeClassName.begin(), smokeClassName.end(), ':', '_');
            out << "    { \"" << iter.first << "\", false" << ", "
                << inheritanceIndex[klass] << ", xcall_" << smokeClassName << ", "
                << "0" << ", ";
            std::string flags;
            if (klass) { // !klass->isNamespace()
                if (canClassBeInstantiated(klass)) flags += "Smoke::cf_constructor|";
                if (canClassBeCopied(klass)) flags += "Smoke::cf_deepcopy|";
                if (hasClassVirtualDestructor(klass)) flags += "Smoke::cf_virtual|";
                if (flags[flags.size()-1] == '|') {
                    flags.pop_back();
                }
                else {
                    flags = "0";
                }
            } else {
                flags = "Smoke::cf_namespace";
            }
            out << flags << ", ";
            if (klass) // !klass->isNamespace()
                out << "sizeof(" << iter.first << ")";
            else
                out << '0';
            out << " },\t//" << iter.second << "\n";
        }
        classCount = iter.second;
    }
    out << "};\n\n";

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
        if (!iter.second)
            continue;
        std::set<const clang::CXXRecordDecl *> superClasses = superClassList(iter.second);
        if (superClasses.count(klass)) {
            ret.insert(iter.second);
        }
    }
    return ret;
}

bool SmokeGenerator::canClassBeInstantiated(const clang::CXXRecordDecl *klass) const {
    bool ctorFound = false, publicCtorFound = false, privatePureVirtualsFound = false;
    for (auto const & ctor : klass->ctors()) {
        ctorFound = true;
        if (ctor->getAccess() != clang::AS_private) {
            // this class can be instanstiated
            publicCtorFound = true;
            break;
        }
    }
    for (auto const * method : klass->methods()) {
        if ((method->isPure() && method->isVirtual()) && method->getAccess() == clang::AS_private) {
            privatePureVirtualsFound = true;
        }
    }

    // The class can be instanstiated if it has a public constructor or no
    // constructor at all because then it has a default one generated by the
    // compiler.  If it has private pure virtuals, then it can't be
    // instanstiated either.
    return ((publicCtorFound || !ctorFound) && !privatePureVirtualsFound);
}

bool SmokeGenerator::canClassBeCopied(const clang::CXXRecordDecl *klass) const {
    if (not klass) return false;

    bool privateCopyCtorFound = false;
    for (auto const & ctor : klass->ctors()) {
        if (ctor->getAccess() == clang::AS_private && ctor->isCopyConstructor()) {
            privateCopyCtorFound = true;
            break;
        }
    }

    bool parentCanBeCopied = true;
    for (auto const & base : klass->bases()) {
        if (!canClassBeCopied(base.getType()->getAsCXXRecordDecl())) {
            parentCanBeCopied = false;
            break;
        }
    }

    // if the parent can be copied and we didn't find a private copy c'tor, the
    // class is copiable
    return (parentCanBeCopied && !privateCopyCtorFound);
}

bool SmokeGenerator::hasClassVirtualDestructor(const clang::CXXRecordDecl *klass) const {
    if (not klass) return false;

    bool virtualDtorFound = false;
    auto d = klass->getDestructor();
    if (d && d->isVirtual()) {
        virtualDtorFound = true;
    }

    bool superClassHasVirtualDtor = false;
    for (auto const & base : klass->bases()) {
        if (hasClassVirtualDestructor(base.getType()->getAsCXXRecordDecl())) {
            superClassHasVirtualDtor = true;
            break;
        }
    }

    // if the superclass has a virtual d'tor, then the descendants have one
    // automatically, too
    return (virtualDtorFound || superClassHasVirtualDtor);
}
