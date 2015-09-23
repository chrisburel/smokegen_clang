#include "generator.h"
#include "util.h"
#include <clang/Sema/DeclSpec.h>
#include <clang/Sema/AttributeList.h>

void SmokeGenerator::addClass(clang::CXXRecordDecl *D) {
    auto const className = D->getQualifiedNameAsString();

    // Classes can be forward declared even after their primary declaration is
    // seen.  We always want the one with a definition, if it exists.
    D = D->hasDefinition() ? D->getDefinition() : D;
    classes[className] = D;
}

void SmokeGenerator::addEnum(clang::EnumDecl *D) {
    enums[D->getQualifiedNameAsString()] = D;
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

    // Get used types in class methods
    for (auto const &klassName : includedClasses) {
        clang::CXXRecordDecl *klass = classes[klassName];
        if (!klass) {
            continue;
        }

        auto ptrToThisClassType = ctx->getPointerType(clang::QualType(klass->getTypeForDecl(), 0));

        for (auto const &field : klass->fields()) {
            // Set name
            clang::DeclarationName Name = ctx->DeclarationNames.getIdentifier(&ctx->Idents.get(field->getName()));
            clang::SourceLocation FieldLoc = field->getLocation();
            clang::DeclarationNameInfo NameInfo(Name, FieldLoc);

            // Set return type
            clang::QualType functionType = ctx->getFunctionType(field->getType(), clang::ArrayRef<clang::QualType>(), clang::FunctionProtoType::ExtProtoInfo());

            clang::CXXMethodDecl *method = clang::CXXMethodDecl::Create(*ctx, klass, FieldLoc,
                    NameInfo, functionType,
                    /*TInfo=*/nullptr, /*StorageClass=*/clang::SC_None,
                    /*isInline=*/true, /*isConst=*/true, FieldLoc);
            klass->addDecl(method);
            fieldAccessors[method] = field;

            // const non-pointer types can't be set
            if (field->getType().isConstQualified() && !field->getType()->isPointerType())
                continue;

            // setter
            auto setterName = field->getNameAsString();
            setterName[0] = std::toupper(setterName[0]);
            setterName = "set" + setterName;
            Name = ctx->DeclarationNames.getIdentifier(&ctx->Idents.get(setterName));
            NameInfo = clang::DeclarationNameInfo(Name, FieldLoc);

            functionType = ctx->getFunctionType(ctx->VoidTy, llvm::makeArrayRef(field->getType()), clang::FunctionProtoType::ExtProtoInfo());

            method = clang::CXXMethodDecl::Create(*ctx, klass, FieldLoc,
                    NameInfo, functionType,
                    /*TInfo=*/nullptr, /*StorageClass=*/clang::SC_None,
                    /*isInline=*/true, /*isConst=*/false, FieldLoc);

            clang::ParmVarDecl *newValueArg = clang::ParmVarDecl::Create(*ctx, method, FieldLoc, FieldLoc,
                    /*IdentifierInfo=*/nullptr, field->getType(), /*TInfo=*/nullptr, /*StorageClass=*/clang::SC_None, /*DefArg=*/nullptr);
            method->setParams(llvm::makeArrayRef(newValueArg));

            klass->addDecl(method);
            fieldAccessors[method] = field;
        }

        // Add types from methods
        for (auto const &method : klass->methods()) {
            if (method->getAccess() == clang::AS_private) {
                continue;
            }
            if (hasTypeNonPublicParts(method->getReturnType())) {
                continue;
            }

            if (method->getKind() == clang::Decl::CXXConstructor) {
                // clang reports constructors as returning void.  According to
                // smoke, they return a pointer to the class.
                usedTypes.insert(ptrToThisClassType);
            }
            else {
                usedTypes.insert(method->getReturnType());
            }

            // Add the types from the parameters of this method
            for (auto const &param : method->params()) {
                usedTypes.insert(param->getType());
            }
        }
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

        // avoid duplicate case values (diamond-shape inheritance).  Use std::map to sort output.
        std::map<int, const clang::CXXRecordDecl *> indices;
        out << "    case " << iter.second << ":   //" << iter.first << "\n";
        out << "      switch(to) {\n";
        // Add our parent classes to the cast
        for (auto const &base : superClassList(klass)) {
            auto className = base->getQualifiedNameAsString();

            if (contains(includedClasses, className)) {
                int index = classIndex[className];
                if (indices.count(index))
                    continue;

                indices[index] = base;
            }
        }
        // Add ourself to the cast
        indices[iter.second] = klass;

        // Add our subclasses to the cast
        for (auto const &desc : descendantsList(klass)) {
            auto className = desc->getQualifiedNameAsString();

            if (contains(includedClasses, className)) {
                int index = classIndex[className];
                if (indices.count(index))
                    continue;

                indices[index] = desc;
            }
        }

        // Do the output
        for (auto const &it : indices) {
            if (it.second == klass) {
                out << "        case " << iter.second << ": return (void*)(" << klass->getQualifiedNameAsString() << "*)xptr;\n";
            }
            else {
            //if (Util::isVirtualInheritancePath(desc, &klass)) {
            //    out << QString("        case %1: return (void*)dynamic_cast<%2*>((%3*)xptr);\n")
            //        .arg(index).arg(className).arg(klass.toString());
            //} else {
                out << "        case " << it.first << ": return (void*)(" << it.second->getQualifiedNameAsString() << "*)(" << klass->getQualifiedNameAsString() << "*)xptr;\n";
            // }
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
    std::set<std::string> enumClassesHandled;
    for (auto const & it : enums) {
        std::string smokeClassName;
        auto parent = it.second->getParent();

        if (clang::isa<clang::TranslationUnitDecl>(parent)) {
            smokeClassName = "QGlobalSpace";
        }
        else {
            smokeClassName = clang::cast<clang::NamedDecl>(parent)->getQualifiedNameAsString();
        }

        if (!smokeClassName.empty() && contains(includedClasses, smokeClassName) && it.second->getAccess() != clang::AS_private) {
            if (enumClassesHandled.count(smokeClassName) || contains(options->voidpTypes, smokeClassName))
                continue;
            enumClassesHandled.insert(smokeClassName);
            std::replace(smokeClassName.begin(), smokeClassName.end(), ':', '_');
            out << "void xenum_" << smokeClassName << "(Smoke::EnumOperation, Smoke::Index, void*&, long&);\n";
        } else if (smokeClassName.empty() && it.second->getAccess() != clang::AS_private) {
            if (enumClassesHandled.count("QGlobalSpace")) {
                continue;
            }
            out << "void xenum_QGlobalSpace(Smoke::EnumOperation, Smoke::Index, void*&, long&);\n";
            enumClassesHandled.insert("QGlobalSpace");
        }
    }
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
                << (enumClassesHandled.count(iter.first) ? "xenum_" + smokeClassName : "0") << ", ";
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

    // types list
    out << "// List of all types needed by the methods (arguments and return values)\n"
        << "// Name, class ID if arg is a class, and TypeId\n";
    out << "static Smoke::Type types[] = {\n";
    out << "    { 0, 0, 0 },\t//0 (no type)\n";
    std::map<std::string, clang::QualType> sortedTypes;
    for (auto const &type : usedTypes) {

        // We don't want the "class" keyword in front of all the class types
        clang::LangOptions options;
        clang::PrintingPolicy pp(options);
        pp.SuppressTagKeyword = true;
        pp.Bool = true;

        std::string typeString = type.getAsString(pp);
        if (!typeString.empty()) {
            sortedTypes[typeString] = type;
        }
    }

    int i = 1;
    for (auto const &it : sortedTypes) {
        clang::QualType t = it.second;
        // don't include void as a type
        if (t.getAsString() == "void")
            continue;
        int classIdx = 0;
        std::string flags = getTypeFlags(t, &classIdx);
        typeIndex[t] = i;
        out << "    { \"" << it.first << "\", " << classIdx << ", " << flags << " },\t//" << i++ << "\n";
    }
    out << "};\n\n";

    out << "static Smoke::Index argumentList[] = {\n";
    out << "    0,\t//0  (void)\n";

    std::map<std::vector<int>, int> parameterList;
    std::map<const clang::CXXMethodDecl*, int> parameterIndices;

    // munged name => index
    std::map<std::string, int> methodNames;
    // class => list of munged names with possible methods or enum members
    std::map<const clang::CXXRecordDecl*, std::map<std::string, std::vector<const clang::CXXMethodDecl*> > > classMungedNames;

    currentIdx = 1;
    for (auto const & iter : classIndex) {
        auto klass = classes[iter.first];
        if (!klass)
            continue;
        bool isExternal = externalClasses.count(klass);
        //bool isDeclaredVirtual = declaredVirtualMethods.contains(klass);
        bool isDeclaredVirtual = false;
        if (isExternal && !isDeclaredVirtual)
            continue;
        std::map<std::string, std::vector<const clang::CXXMethodDecl*> >& map = classMungedNames[klass];
        for (auto const & meth : klass->methods()) {
            if (meth->getAccess() == clang::AS_private)
                continue;
            if (isExternal && !isDeclaredVirtual)
                continue;

            methodNames[meth->getNameAsString()] = 1;
            if (!isExternal) {
                auto munged = mungedName(meth);
                methodNames[munged] = 1;
                map[munged].push_back(meth);
            }

            if (!meth->getNumParams()) {
                parameterIndices[meth] = 0;
                continue;
            }
            std::vector<int> indices(meth->getNumParams());
            std::string comment;
            for (int i = 0; i < meth->getNumParams(); ++i) {
                auto param = meth->getParamDecl(i);
                auto t = param->getType();
                if (!typeIndex.count(t)) {
                    llvm::outs() << "missing type: " << t.getAsString() << " in method " << meth->getNameAsString() << " (while building munged names map)\n";
                }
                indices[i] = typeIndex[t];
                comment += t.getAsString() + ", ";
            }
            int idx = 0;
            auto const & it = parameterList.find(indices);
            if (it == parameterList.end()) {
                idx = currentIdx;
                parameterList[indices] = idx;
                out << "    ";
                for (int i = 0; i < indices.size(); i++) {
                    if (i > 0) out << ", ";
                    out << indices[i];
                }
                if (comment.substr(comment.size()-2, comment.size()) == ", ")
                    comment = comment.substr(0, comment.size()-2);
                out << ", 0,\t//" << idx << "  " << comment << "\n";
                currentIdx += indices.size() + 1;
            }
            else {
                idx = it->second;
            }
            parameterIndices[meth] = idx;
        }
        //foreach (BasicTypeDeclaration* decl, klass->children()) {
        //    const Enum* e = 0;
        //    if ((e = dynamic_cast<Enum*>(decl))) {
        //        if (e->access() == Access_private)
        //            continue;
        //        foreach (const EnumMember& member, e->members()) {
        //            methodNames[member.name()] = 1;
        //            map[member.name()].append(&member);
        //        }
        //    }
        //}
    }
    out << "};\n\n";

    out << "// Raw list of all methods, using munged names\n";
    out << "static const char *methodNames[] = {\n";
    out << "    \"\",\t//0\n";
    i = 1;
    for (auto &it : methodNames) {
        it.second = i++;
        out << "    \"" << it.first << "\",\t//" << it.second << "\n";
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
    out << "        " << smokeNamespaceName << "::classes, " << includedClasses.size() <<  ",\n";
    out << "        " << smokeNamespaceName << "::methods, " << /*methodCount <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::methodMaps, " << /*methodMapCount <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::methodNames, " << /*methodNames.count() <<*/ ",\n";
    out << "        " << smokeNamespaceName << "::types, " << typeIndex.size() << ",\n";
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

bool SmokeGenerator::hasTypeNonPublicParts(const clang::QualType &type) const {
    if (type->getAsCXXRecordDecl() && type->getAsCXXRecordDecl()->getAccess() != clang::AS_public)
        return true;
    return false;
}

std::string SmokeGenerator::getTypeFlags(const clang::QualType &t, int *classIdx) const {
    auto tname = t.getAsString();
    if (!t.isCanonical()) {
        return getTypeFlags(t.getCanonicalType(), classIdx);
    }

    clang::QualType noPointerType = t;
    while(noPointerType->isPointerType()) {
        noPointerType = noPointerType->getPointeeType();
    }
    if (auto refType = noPointerType->getAs<clang::ReferenceType>()) {
        noPointerType = refType->getPointeeType();
    }
    auto D = noPointerType->getAsCXXRecordDecl();

    std::string flags;
    if (contains(options->voidpTypes, t.getAsString())) {
        // support some of the weird quirks the kalyptus code has
        flags += "Smoke::t_voidp|";
    } else if (D) {
        if (D->getDescribedClassTemplate()) {
            if (options->qtMode && D->getQualifiedNameAsString() == "QFlags" && !t->isReferenceType() && t->isPointerType()) {
                flags += "Smoke::t_uint|";
            } else {
                flags += "Smoke::t_voidp|";
            }
        } else {
            flags += "Smoke::t_class|";
            *classIdx = classIndex.at(D->getQualifiedNameAsString());
        }
    } else if (t->isBuiltinType() && t.getAsString() != "void" && !t->isPointerType() && !t->isReferenceType()) {
        flags += "Smoke::t_";
        clang::LangOptions options;
        clang::PrintingPolicy pp(options);
        pp.SuppressTagKeyword = true;
        pp.Bool = true;
        std::string typeName = t.getUnqualifiedType().getAsString(pp);

        // replace the unsigned stuff, look the type up in Util::typeMap and if
        // necessary, add a 'u' for unsigned types at the beginning again
        bool _unsigned = false;
        if (typeName.substr(0, 9) == "unsigned ") {
            typeName = typeName.substr(9, typeName.size());
            _unsigned = true;
        }
        else if (typeName.substr(0, 7) == "signed ") {
            typeName = typeName.substr(7, typeName.size());
        }
        //typeName = Util::typeMap.value(typeName, typeName);
        if (_unsigned)
            typeName = "u" + typeName;

        flags += typeName + '|';
    } else if (t->isEnumeralType()) {
        flags += "Smoke::t_enum|";
        auto tag = noPointerType->getAsTagDecl();
        if (!tag) {
            *classIdx = classIndex.at("QGlobalSpace");
        }
        auto parent = tag->getParent();
        if (clang::isa<clang::TagDecl>(parent)) {
            auto parentTagDecl = clang::cast<clang::TagDecl>(parent);
            *classIdx = classIndex.at(parentTagDecl->getQualifiedNameAsString());
        }
        else if (clang::isa<clang::NamespaceDecl>(parent)) {
            auto parentNamespaceDecl = clang::cast<clang::NamespaceDecl>(parent);
            if (classIndex.count(parentNamespaceDecl->getQualifiedNameAsString())) {
                *classIdx = classIndex.at(parentNamespaceDecl->getQualifiedNameAsString());
            }
        }
    } else {
        flags += "Smoke::t_voidp|";
    }

    if (t->isReferenceType())
        flags += "Smoke::tf_ref|";
    if (t->isPointerType())
        flags += "Smoke::tf_ptr|";
    if (!t->isReferenceType() && !t->isPointerType())
        flags += "Smoke::tf_stack|";
    if (noPointerType.isConstQualified())
        flags += "Smoke::tf_const|";
    if (flags[flags.size()-1] == '|') {
        flags.pop_back();
    }

    return flags;
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
    if (!type.isCanonical()) {
        return munge(type.getCanonicalType());
    }

    while(type->isPointerType()) {
        type = type->getPointeeType();
    }
    if (auto refType = type->getAs<clang::ReferenceType>()) {
        type = refType->getPointeeType();
    }

    if ((type->isPointerType() && type->getPointeeType()->isPointerType()) ||
        // (type->getClass() && type->getClass()->isTemplate() && (!Options::qtMode || (Options::qtMode && type->getClass()->name() != "QFlags"))) ||
        (contains(options->voidpTypes, type.getAsString()) && !contains(options->scalarTypes, type.getAsString()))) {

        // reference to array or hash or unknown
        return '?';
    }
    if (type->isBuiltinType() || type->isEnumeralType() || contains(options->scalarTypes, type.getAsString())) {
        // plain scalar
        return '$';
    }
    else if (type->isObjectType() || (type->isPointerType() && type->getPointeeType()->isObjectType())) {
        // object
        return '#';
    }
    else {
        // unknown
        return '?';
    }
}
