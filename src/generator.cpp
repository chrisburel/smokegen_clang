#include <regex>

#include "generator.h"
#include "util.h"
#include <clang/Sema/DeclSpec.h>
#include <clang/Sema/AttributeList.h>
#include <clang/AST/Attr.h>

void SmokeGenerator::addClass(clang::CXXRecordDecl *D) {
    // Classes can be forward declared even after their primary declaration is
    // seen.  We always want the one with a definition, if it exists.
    D = D->hasDefinition() ? D->getDefinition() : D->getCanonicalDecl();

    if (classes.count(D->getQualifiedNameAsString()) && classes[D->getQualifiedNameAsString()] == D) {
        return;
    }
    classes[D->getQualifiedNameAsString()] = D;

    addQPropertyAnnotations(D);
}

void SmokeGenerator::addEnum(clang::EnumDecl *D) {
    auto parent = D->getParent();
    if (!parent->isTranslationUnit() && !clang::isa<clang::NamedDecl>(parent)) {
        return;
    }

    enums[D->getQualifiedNameAsString()] = D;
}

void SmokeGenerator::addNamespace(clang::NamespaceDecl *D) {
    namespaces[D->getQualifiedNameAsString()] = D;
}

void SmokeGenerator::addFunction(clang::FunctionDecl *D) {
    const auto proto = getFullFunctionPrototype(D, pp());
    D = D->getCanonicalDecl();
    if (!contains(functions[proto], D))
        functions[proto].push_back(D);
}

void SmokeGenerator::processDataStructures() {
    // QGlobalSpace holds global-level enums and functions.
    clang::NamespaceDecl *globalSpace = clang::NamespaceDecl::Create(
            *ctx, ctx->getTranslationUnitDecl(), false, clang::SourceLocation(), clang::SourceLocation(),
            &ctx->Idents.get("QGlobalSpace"), nullptr);
    namespaces[globalSpace->getQualifiedNameAsString()] = globalSpace;

    for (auto const &klass : classes) {
        if (contains(options->classList, klass.first) && klass.second->hasDefinition()) {
            classIndex[klass.first] = 1;
        }
    }
    for (auto const &nspace : namespaces) {
        if (contains(options->classList, nspace.first)) {
            classIndex[nspace.first] = 1;
        }
    }

    // superclasses might be in different modules, still they need to be indexed for inheritanceList to work properly
    std::set<const clang::CXXRecordDecl*> superClasses;
    for (auto const &klass : classIndex) {
        includedClasses.push_back(klass.first);
    }

    // add all functions as methods to a class called 'QGlobalSpace' or a class that represents a namespace
    for (auto const & it : functions) {
        const std::vector<clang::FunctionDecl *> fns = it.second;

        std::string fnString = it.first;

        for (auto fn : fns) {

            bool isGlobalFunction = fn->getParent()->isTranslationUnit() || clang::isa<clang::LinkageSpecDecl>(fn->getParent());

            // functions in named namespaces are covered by the class list - only check for top-level functions here
            if ((isGlobalFunction && (!options->functionNameIncluded(fn->getQualifiedNameAsString()) && !options->functionSignatureIncluded(fnString)))
                || options->typeExcluded(fnString))
            {
                // we don't want that function...
                continue;
            }

            if (auto parent = clang::dyn_cast<clang::NamedDecl>(fn->getParent())) {
                if (!contains(options->classList, parent->getQualifiedNameAsString())) {
                    continue;
                }
            }
            if (fn->isVariadic()) {
                continue;
            }

            // Ignore functions where any of the arguments or return type
            // depends on a template parameter.
            if (fn->isDependentContext()) {
                continue;
            }

            clang::FunctionDecl *newFn = fn;
            if (isGlobalFunction) {
                // QGlobalSpace is used, add it to the class index
                classIndex[globalSpace->getQualifiedNameAsString()] = 1;

                // Make a copy of this function decl inside the QGlobalSpace
                // namespace
                newFn = clang::FunctionDecl::Create(*ctx, globalSpace,
                    fn->getSourceRange().getBegin(),
                    fn->getNameInfo().getLoc(), fn->getNameInfo().getName(),
                    fn->getType(), fn->getTypeSourceInfo(),
                    fn->getStorageClass(), fn->isInlineSpecified(),
                    fn->hasWrittenPrototype(), fn->isConstexpr());
                globalSpace->addDecl(newFn);
                newFn->setParams(fn->parameters());
            }

            //if (isRepeating(parentModules, parent->name().toLatin1(), meth)) {
            //    continue;
            //}

            addOverloads(newFn);

            usedTypes.insert(getCanonicalType(newFn->getReturnType()));
            for (const auto & param : newFn->params()) {
                usedTypes.insert(getCanonicalType(param->getType()));
            }
        }
    }

    // all enums that don't have a parent are put under QGlobalSpace, too
    for (const auto & it : enums) {
        clang::EnumDecl *e = it.second;
        if (options->typeExcluded(it.first)) {
            continue;
        }
        if (e->getParent()->isTranslationUnit()) {
            // see if it is already defined in a parent module
            //if (isRepeating(parentModules, parent->name().toLatin1(), e)) {
            //    continue;
            //}

            // QGlobalSpace is used, add it to the class index
            classIndex[globalSpace->getQualifiedNameAsString()] = 1;

            // Make a copy of this enum decl inside the QGlobalSpace namespace
            auto newEnum = clang::EnumDecl::Create(*ctx,
                globalSpace,
                e->getSourceRange().getBegin(),
                clang::SourceLocation(),
                e->getIdentifier(),
                nullptr,
                e->isScoped(),
                e->isScopedUsingClassTag(),
                e->isFixed()
            );

            for (const auto & enumerator : e->enumerators()) {
                if (options->typeExcluded(enumerator->getQualifiedNameAsString())) {
                    continue;
                }
                // copy these too.
                newEnum->addDecl(clang::EnumConstantDecl::Create(
                    *ctx,
                    newEnum,
                    enumerator->getSourceRange().getBegin(),
                    enumerator->getIdentifier(),
                    enumerator->getType(),
                    enumerator->getInitExpr(),
                    enumerator->getInitVal()
                ));
            }

            globalSpace->addDecl(newEnum);

            movedEnums[clang::QualType(e->getTypeForDecl(), 0)] = clang::QualType(newEnum->getTypeForDecl(), 0);
            usedTypes.insert(getCanonicalType(clang::QualType(newEnum->getTypeForDecl(), 0)));
        }
        else {
            if (auto parent = clang::dyn_cast<clang::NamedDecl>(e->getParent())) {
                if (contains(options->classList, parent->getQualifiedNameAsString())) {
                    usedTypes.insert(getCanonicalType(clang::QualType(e->getTypeForDecl(), 0)));
                }
            }
        }
    }

    // Get used types in class methods
    for (auto const &klassName : includedClasses) {
        if (!classes.count(klassName)) {
            // This names a namespace, not a class
            continue;
        }
        clang::CXXRecordDecl *klass = classes.at(klassName);

        for (auto const & base : klass->bases()) {
            superClasses.insert(base.getType()->getAsCXXRecordDecl());
        }

        checkForAbstractClass(klass);

        auto ptrToThisClassType = ctx->getPointerType(clang::QualType(klass->getTypeForDecl(), 0));

        for (const auto& d : klass->decls()) {
            if (const auto usingShadowDecl = clang::dyn_cast<clang::UsingShadowDecl>(d)) {
                if (const auto method = clang::dyn_cast<clang::CXXMethodDecl>(usingShadowDecl->getTargetDecl())) {
                    auto newMethod = clang::CXXMethodDecl::Create(
                        *ctx,
                        klass,
                        method->getSourceRange().getBegin(),
                        method->getNameInfo(),
                        method->getType(),
                        method->getTypeSourceInfo(),
                        method->getStorageClass(),
                        method->isInlineSpecified(),
                        method->isConstexpr(),
                        method->getSourceRange().getEnd()
                    );
                    klass->addDecl(newMethod);
                    newMethod->setParams(method->parameters());
                }
            }
        }

        // Add types from methods
        std::vector<clang::CXXMethodDecl*> methods;
        methods.insert(methods.begin(), klass->method_begin(), klass->method_end());
        for (auto const &method : methods) {
            if (method->getAccess() == clang::AS_private) {
                continue;
            }
            if (hasTypeNonPublicParts(method->getReturnType()) || options->typeExcluded(getFullFunctionPrototype(method, pp()))) {
                klass->removeDecl(method);
                continue;
            }
            if (method->isCopyAssignmentOperator() && method->isImplicit())
                continue;
            if (method->getFriendObjectKind() != clang::Decl::FOK_None) {
                klass->removeDecl(method);
                continue;
            }

            addOverloads(method);

            if (method->getKind() == clang::Decl::CXXConstructor) {
                // clang reports constructors as returning void.  According to
                // smoke, they return a pointer to the class.
                usedTypes.insert(getCanonicalType(ptrToThisClassType));
            }
            else {
                usedTypes.insert(getCanonicalType(method->getReturnType()));
            }

            // Add the types from the parameters of this method
            for (auto const &param : method->params()) {
                usedTypes.insert(getCanonicalType(param->getType()));
            }
        }

        std::vector<clang::DeclaratorDecl *> fields;
        fields.insert(fields.end(), klass->field_begin(), klass->field_end());
        auto vars = var_range(var_iterator(klass->decls_begin()), var_iterator(klass->decls_end()));
        fields.insert(fields.end(), vars.begin(), vars.end());

        for (auto const &field : fields) {
            if (field->getAccess() == clang::AS_private) {
                continue;
            }

            if (options->typeExcluded(field->getQualifiedNameAsString())) {
                continue;
            }
            clang::QualType getterReturnType = field->getType();
            if (getterReturnType->getAsCXXRecordDecl()) {
                // Skip anonymous structs
                if (!getterReturnType->getAsCXXRecordDecl()->getDeclName()) {
                    continue;
                }
                if (!(options->qtMode && getterReturnType->getAsCXXRecordDecl()->getNameAsString() == "QFlags")) {
                    getterReturnType = ctx->getLValueReferenceType(getterReturnType);
                }
            }
            usedTypes.insert(getCanonicalType(getterReturnType));

            // Set name
            clang::DeclarationName Name = ctx->DeclarationNames.getIdentifier(&ctx->Idents.get(field->getName()));
            clang::SourceLocation FieldLoc = field->getLocation();
            clang::DeclarationNameInfo NameInfo(Name, FieldLoc);

            // Set return type
            auto protoInfo = clang::FunctionProtoType::ExtProtoInfo();
            protoInfo.TypeQuals |= clang::Qualifiers::Const; // set method->setIsConst(true)
            clang::QualType functionType = ctx->getFunctionType(getterReturnType, clang::ArrayRef<clang::QualType>(), protoInfo);

            const auto storage = clang::isa<clang::VarDecl>(field) ? clang::SC_Static : clang::SC_None;

            clang::CXXMethodDecl *method = clang::CXXMethodDecl::Create(*ctx, klass, FieldLoc,
                    NameInfo, functionType,
                    /*TInfo=*/nullptr, /*StorageClass=*/storage,
                    /*isInline=*/true, /*isConst=*/true, FieldLoc);
            method->setAccess(field->getAccess());
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
            auto setterArgType = field->getType();
            if (setterArgType->getAsCXXRecordDecl() && !(options->qtMode && setterArgType->getAsCXXRecordDecl()->getNameAsString() == "QFlags")) {
               setterArgType = ctx->getLValueReferenceType(ctx->getConstType(setterArgType));
            }
            usedTypes.insert(getCanonicalType(setterArgType));

            functionType = ctx->getFunctionType(ctx->VoidTy, llvm::makeArrayRef(setterArgType), clang::FunctionProtoType::ExtProtoInfo());

            method = clang::CXXMethodDecl::Create(*ctx, klass, FieldLoc,
                    NameInfo, functionType,
                    /*TInfo=*/nullptr, /*StorageClass=*/storage,
                    /*isInline=*/true, /*isConst=*/false, FieldLoc);
            method->setAccess(field->getAccess());

            clang::ParmVarDecl *newValueArg = clang::ParmVarDecl::Create(*ctx, method, FieldLoc, FieldLoc,
                    /*IdentifierInfo=*/nullptr, setterArgType, /*TInfo=*/nullptr, /*StorageClass=*/clang::SC_None, /*DefArg=*/nullptr);
            method->setParams(llvm::makeArrayRef(newValueArg));

            klass->addDecl(method);
            fieldAccessors[method] = field;
        }

    }

    for (const auto & type : usedTypes) {
        insertTemplateParameters(type);
    }

    // if a class is used somewhere but not listed in the class list, mark it external
    for (auto const & iter : classes) {
        if (isTemplate(iter.second) || contains(options->voidpTypes, iter.first))
            continue;

        if (   (isClassUsed(iter.second) && iter.second->getAccess() != clang::AS_private)
            || superClasses.count(iter.second))
            //|| declaredVirtualMethods.contains(&iter.value()))
        {
            classIndex[iter.first] = 1;

            if (!contains(options->classList, iter.first) || iter.second->getDefinition() != iter.second /*isForwardDecl*/)
                externalClasses.insert(iter.second);
            else if (!contains(includedClasses, iter.first))
                includedClasses.push_back(iter.first);
        }
        //else if (iter.second->isNameSpace() && (contains(options->classList, iter.first) || iter.first == "QGlobalSpace")) {
        //    // wanted namespace or QGlobalSpace
        //    classIndex[iter.first] = 1;
        //    includedClasses.push_back(iter.first);
        //}
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
        const clang::CXXRecordDecl* klass = nullptr;
        if (classes.count(iter.first)) {
            klass = classes.at(iter.first);
        }
        else {
            continue;
        }

        // avoid duplicate case values (diamond-shape inheritance).  Use std::map to sort output.
        std::map<int, const clang::CXXRecordDecl *> indices;
        out << "    case " << iter.second << ":   //" << iter.first << "\n";
        out << "      switch(to) {\n";
        // Add our parent classes to the cast
        for (auto const &base : superClassList(klass)) {
            auto className = base->getQualifiedNameAsString();

            if (contains(includedClasses, className)) {
                int index = classIndex.at(className);
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
                int index = classIndex.at(className);
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
            //if (isVirtualInheritancePath(desc, &klass)) {
            //    out << std::string("        case %1: return (void*)dynamic_cast<%2*>((%3*)xptr);\n")
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
        const clang::CXXRecordDecl* klass = nullptr;
        if (classes.count(iter.first)) {
            klass = classes.at(iter.first);
        }
        else {
            continue;
        }

        if (externalClasses.count(klass))
            continue;
        std::vector<int> indices;
        std::string comment;
        for (auto const & base : klass->bases()) {
            if (base.getType()->getAsCXXRecordDecl()->getAccess() == clang::AS_private)
                continue;
            auto className = base.getType()->getAsCXXRecordDecl()->getQualifiedNameAsString();
            if (classIndex.count(className)) {
                indices.push_back(classIndex.at(className));
            }
            else {
                indices.push_back(0);
            }
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
        const clang::CXXRecordDecl* klass = nullptr;
        const clang::NamespaceDecl* nspace = nullptr;
        if (classes.count(iter.first)) {
            klass = classes.at(iter.first);
        }
        else {
            nspace = namespaces[iter.first];
        }

        if ((klass && (externalClasses.count(klass) || isTemplate(klass))) ||
            (nspace && externalClasses.count(nspace)))
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

        const clang::CXXRecordDecl* klass = nullptr;
        const clang::NamespaceDecl* nspace = nullptr;
        if (classes.count(iter.first)) {
            klass = classes.at(iter.first);
        }
        else {
            nspace = namespaces[iter.first];
        }

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
    for (auto type : usedTypes) {

        if (movedEnums.count(type)) {
            type = movedEnums.at(type);
        }

        std::string typeString = type.getAsString(pp());
        if (typeString == "__va_list_tag *") {
            typeString = "va_list";
        }

        if (typeString.substr(0, 14) == "QGlobalSpace::") {
            typeString = typeString.substr(14, typeString.size());
        }
        if (!typeString.empty()) {
            sortedTypes[typeString] = type;
        }
    }

    int i = 1;
    for (auto const &it : sortedTypes) {
        clang::QualType t = it.second;
        // don't include void as a type
        if (t->isVoidType())
            continue;
        int classIdx = 0;
        std::string flags = getTypeFlags(t, &classIdx);
        typeIndex[t] = i;
        out << "    { \"" << it.first << "\", " << classIdx << ", " << flags << " },\t//" << i++ << "\n";
    }
    out << "};\n\n";

    out << "static Smoke::Index argumentList[] = {\n";
    out << "    0,\t//0  (void)\n";

    std::map<std::vector<int>, std::vector<const clang::FunctionDecl*> > parameterList;
    std::map<const clang::FunctionDecl*, int> parameterIndices;

    // munged name => index
    std::map<std::string, int> methodNames;
    // class => list of munged names with possible methods or enum members
    std::map<const clang::NamedDecl*, std::map<std::string, std::vector<const clang::ValueDecl*> > > classMungedNames;

    for (auto const & iter : classIndex) {
        const clang::NamedDecl* klass;
        if (classes.count(iter.first)) {
            klass = classes.at(iter.first);
        }
        else {
            klass = namespaces.at(iter.first);
        }
        const auto context = clang::cast<clang::DeclContext>(klass);

        bool isExternal = externalClasses.count(klass);

        //bool isDeclaredVirtual = declaredVirtualMethods.contains(klass);
        bool isDeclaredVirtual = false;
        if (isExternal && !isDeclaredVirtual)
            continue;

        auto map = &classMungedNames[klass];

        auto methods = function_range(function_iterator(context->decls_begin()), function_iterator(context->decls_end()));

        for (const auto & meth : methods) {
            if (meth->getAccess() == clang::AS_private)
                continue;
            if (isExternal && !isDeclaredVirtual)
                continue;
            if (clang::isa<clang::CXXMethodDecl>(meth) && clang::cast<clang::CXXMethodDecl>(meth)->isCopyAssignmentOperator() && meth->isImplicit())
                continue;

            methodNames[meth->getNameAsString()] = 1;
            if (!isExternal) {
                auto munged = mungedName(meth);
                methodNames[munged] = 1;
                (*map)[munged].push_back(meth);
            }

            if (!meth->getNumParams()) {
                continue;
            }
            std::vector<int> indices(meth->getNumParams());
            for (int i = 0; i < meth->getNumParams(); ++i) {
                auto param = meth->getParamDecl(i);
                auto t = getCanonicalType(param->getType());
                if (movedEnums.count(t)) {
                    t = movedEnums.at(t);
                }
                if (typeIndex.count(t)) {
                    indices[i] = typeIndex.at(t);
                }
                else {
                    llvm::outs() << "missing type: " << t.getAsString() << " in method " << getFullFunctionPrototype(meth, pp()) << " (while building munged names map)\n";
                }
            }
            parameterList[indices].push_back(meth);
        }
        for (auto const & decl : context->decls()) {
            if (const auto e = clang::dyn_cast<clang::EnumDecl>(decl)) {
                if (e->getAccess() == clang::AS_private)
                    continue;
                if (options->typeExcluded(e->getQualifiedNameAsString())) {
                    continue;
                }
                for (auto const & member : e->enumerators()) {
                    methodNames[member->getName()] = 1;
                    (*map)[member->getName()].push_back(member);
                }
            }
        }
    }

    int currentIndex = 1;
    for (const auto it : parameterList) {
        const auto& indices = it.first;
        const auto& methods = it.second;

        out << "    ";
        for (int i = 0; i < indices.size(); i++) {
            if (i > 0) out << ", ";
            out << indices[i];
        }

        std::string comment;
        for (const auto param : methods[0]->params()) {
            comment += getCanonicalType(param->getType()).getAsString(pp()) + ", ";
        }
        if (comment.size()) {
            comment.erase(comment.size()-2, comment.size());
        }
        comment += '\t';
        for (const auto method : methods) {
            comment += getFullFunctionPrototype(method, pp()) + " <" + method->getSourceRange().getBegin().printToString(ctx->getSourceManager()) + "> " + method->getParent()->getDeclKindName() + "         ";
        }

        out << ", 0,\t//" << currentIndex << "  " << comment << "\n";

        for (const auto method : methods) {
            parameterIndices[method] = currentIndex;
        }

        currentIndex += indices.size() + 1;
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

    out << "// (classId, name (index in methodNames), argumentList index, number of args, method flags, "
        << "return type (index in types), xcall() index)\n";
    out << "static Smoke::Method methods[] = {\n";
    out << "    { 0, 0, 0, 0, 0, 0, 0 },\t// (no method)\n";

    i = 1;
    int methodCount = 1;
    for (auto const & iter : classIndex) {
        const clang::CXXRecordDecl* klass = nullptr;
        const clang::NamespaceDecl* nspace = nullptr;
        if (classes.count(iter.first)) {
            klass = classes.at(iter.first);
        }
        else {
            nspace = namespaces[iter.first];
        }

        const clang::CXXDestructorDecl *destructor = nullptr;
        std::vector<const clang::CXXMethodDecl*> virtualMethods;
        if (klass) {
            destructor = klass->getDestructor();
            const auto vmeths = virtualMethodsForClass(klass);
            virtualMethods.insert(virtualMethods.begin(), vmeths.begin(), vmeths.end());
        }

        bool isExternal = false;
        if (externalClasses.count(klass) || externalClasses.count(nspace))
            isExternal = true;
        if (isExternal /*&& !declaredVirtualMethods.contains(klass)*/)
            continue;

        int xcall_index = 1;
        std::vector<const clang::FunctionDecl *> methods;
        if (klass) {
            methods.insert(methods.end(), klass->method_begin(), klass->method_end());
        }
        else if (nspace) {
            methods.insert(methods.end(), function_iterator(nspace->decls_begin()), function_iterator(nspace->decls_end()));
        }
        for (auto const &meth : methods) {
            if (isExternal /*&& !declaredVirtualMethods[klass].contains(&meth)*/)
                continue;
            if (meth->getAccess() == clang::AS_private)
                continue;
            if (const auto asCXXMethod = clang::dyn_cast<clang::CXXMethodDecl>(meth)) {
                if (asCXXMethod->isCopyAssignmentOperator() && meth->isImplicit()) {
                    continue;
                }
            }
            if (clang::isa<clang::CXXDestructorDecl>(meth)) {
                // Destructors are added at the end of the method list
                continue;
            }

            out << "    {" << iter.second << ", " << methodNames[meth->getNameAsString()] << ", ";
            int numArgs = meth->getNumParams();
            if (numArgs) {
                out << parameterIndices[meth] << ", " << numArgs << ", ";
            } else {
                out << "0, 0, ";
            }

            const clang::CXXConstructorDecl * asCtor = clang::dyn_cast<clang::CXXConstructorDecl>(meth);
            const clang::CXXMethodDecl * asMethod = clang::dyn_cast<clang::CXXMethodDecl>(meth);

            std::string flags;
            if (asMethod) {
                if (asMethod->isConst())
                    flags += "Smoke::mf_const|";
                if (asMethod->isStatic())
                    flags += "Smoke::mf_static|";
            }
            else {
                // All functions are static
                flags += "Smoke::mf_static|";
            }
            if (asCtor) {
                flags += "Smoke::mf_ctor|";
                if (asCtor->isExplicit())
                    flags += "Smoke::mf_explicit|";
            }
            if (meth->getAccess() == clang::AS_protected)
                flags += "Smoke::mf_protected|";
            if (asCtor && asCtor->isCopyConstructor())
                flags += "Smoke::mf_copyctor|";
            if (fieldAccessors.count(asMethod))
                flags += "Smoke::mf_attribute|";
            for (auto attr_it = meth->specific_attr_begin<clang::AnnotateAttr>();
              attr_it != meth->specific_attr_end<clang::AnnotateAttr>();
              ++attr_it) {
                const clang::AnnotateAttr *A = *attr_it;
                if (A->getAnnotation() == "qt_property")
                    flags += "Smoke::mf_property|";
            }

            // Simply checking for flags() & Method::Virtual won't be enough, because methods can override virtuals without being
            // declared 'virtual' themselves (and they're still virtual, then).
            if (asMethod && contains(virtualMethods, const_cast<const clang::CXXMethodDecl *>(asMethod)))
                flags += "Smoke::mf_virtual|";
            if (asMethod && asMethod->isVirtual() && asMethod && asMethod->isPure())
                flags += "Smoke::mf_purevirtual|";

            for (auto attr_it = meth->specific_attr_begin<clang::AnnotateAttr>();
              attr_it != meth->specific_attr_end<clang::AnnotateAttr>();
              ++attr_it) {
                const clang::AnnotateAttr *A = *attr_it;
                if (A->getAnnotation() == "qt_signal")
                    flags += "Smoke::mf_signal|";
                else if (A->getAnnotation() == "qt_slot")
                    flags += "Smoke::mf_slot|";
            }

            if (flags.size())
                flags.pop_back();
            else
                flags = '0';

            out << flags;

            clang::QualType retType = getCanonicalType(meth->getReturnType());
            if (asCtor)
                retType = ctx->getPointerType(clang::QualType(klass->getTypeForDecl(), 0));

            if (retType->isVoidType()) {
                out << ", 0";
            } else if (!typeIndex.count(retType)) {
                llvm::outs() << "missing type: " << retType.getAsString() << " in method " << getFullFunctionPrototype(meth, pp()) << " (while writing out methods table)\n";
            } else {
                out << ", " << typeIndex[retType];
            }
            out << ", " << (isExternal ? 0 : xcall_index) << "},";

            // comment
            out << "\t//" << i << " " << getCanonicalType(retType).getAsString(pp()) << " " << meth->getQualifiedNameAsString() << '(';
            for (int j = 0; j < meth->getNumParams(); j++) {
                if (j > 0) out << ", ";
                out << getCanonicalType(meth->getParamDecl(j)->getType()).getAsString(pp());
            }
            out << ')';
            if (asMethod) {
                if ( asMethod->isConst())
                    out << " const";
                if (asMethod->isPure() && asMethod->isVirtual())
                    out << " [pure virtual]";
            }
            out << "\n";
            methodIdx[meth] = i;
            xcall_index++;
            i++;
            methodCount++;
        }
        // enums
        const clang::DeclContext *context;
        if (klass)
            context = klass;
        else if (nspace)
            context = nspace;

        for (clang::Decl *decl : context->decls()) {
            if (auto e = clang::dyn_cast<clang::EnumDecl>(decl)) {
                if (e->getAccess() == clang::AS_private)
                    continue;
                if (options->typeExcluded(e->getQualifiedNameAsString())) {
                    continue;
                }

                clang::QualType enumType = clang::QualType(e->getTypeForDecl(), 0);

                int index = 0;
                auto const & typeIt = typeIndex.find(enumType);
                if (typeIt == typeIndex.end()) {
                    // this enum doesn't have an index, so we don't want it here
                    continue;
                } else {
                    index = typeIt->second;
                }

                for (auto const & member : e->enumerators()) {
                    out << "    {" << iter.second << ", " << methodNames[member->getName()]
                        << ", 0, 0, Smoke::mf_static|Smoke::mf_enum, " << index
                        << ", " << xcall_index << "},";

                    // comment
                    out << "\t//" << i << " " << member->getQualifiedNameAsString() << " (enum)";
                    out << "\n";
                    enumIdx[member] = i;
                    xcall_index++;
                    i++;
                    methodCount++;
                }
            }
        }
        if (destructor) {
            out << "    {" << iter.second << ", " << methodNames[destructor->getNameAsString()] << ", 0, 0, Smoke::mf_dtor";
            if (destructor->getAccess() == clang::AS_private)
                out << "|Smoke::mf_protected";
            out << ", 0, " << xcall_index << " },\t//" << i << " " << destructor->getQualifiedNameAsString() << "()\n";
            methodIdx[destructor] = i;
            xcall_index++;
            i++;
            methodCount++;
        }
    }

    out << "};\n\n";

    out << "static Smoke::Index ambiguousMethodList[] = {\n";
    out << "    0,\n";

    std::map<const clang::NamedDecl*, std::map<std::string, int> > ambigiousIds;
    i = 1;
    // ambigious method list
    for (auto const & iter : classMungedNames) {
        const clang::NamedDecl* klass = iter.first;
        const std::map<std::string, std::vector<const clang::ValueDecl*> >& map = iter.second;

        for (auto const & munged_it : map) {
            if (munged_it.second.size() < 2)
                continue;
            for (const clang::ValueDecl* value : munged_it.second) {
                if (auto meth = clang::dyn_cast<clang::FunctionDecl>(value)) {
                    out << "    " << methodIdx[meth] << ',';

                    // comment
                    out << "  // " << meth->getQualifiedNameAsString();
                    out << '(';
                    for (int j = 0; j < meth->getNumParams(); j++) {
                        if (j > 0) out << ", ";
                        out << meth->getParamDecl(j)->getType().getAsString();
                    }
                    out << ')';
                    if (auto method = clang::dyn_cast<clang::CXXMethodDecl>(meth))
                       if (method->isConst()) out << " const";
                }
                out << "\n";
            }
            out << "    0,\n";
            ambigiousIds[klass][munged_it.first] = i;
            i += munged_it.second.size() + 1;
        }
    }
    out << "};\n\n";

    int methodMapCount = 1;
    out << "// Class ID, munged name ID (index into methodNames), method def (see methods) if >0 or number of overloads if <0\n";
    out << "static Smoke::MethodMap methodMaps[] = {\n";
    out << "    {0, 0, 0},\t//0 (no method)\n";

    for (auto const & iter : classIndex) {
        const clang::CXXRecordDecl* klass = nullptr;
        const clang::NamespaceDecl* nspace = nullptr;
        if (classes.count(iter.first)) {
            klass = classes.at(iter.first);
        }
        else {
            nspace = namespaces[iter.first];
        }
        clang::NamedDecl *base = klass ? (clang::NamedDecl*)klass : (clang::NamedDecl*)nspace;

        if (externalClasses.count(base))
            continue;

        std::map<std::string, std::vector<const clang::ValueDecl*> >* map = 0;
        map = &classMungedNames[base];
        for (auto const & munged_it : *map) {
            // class index, munged name index
            out << "    {" << classIndex.at(iter.first) << ", " << methodNames[munged_it.first] << ", ";

            // if there's only one matching method for this class and the munged name, insert the index into methodss
            if (munged_it.second.size() == 1) {
                if (auto meth = clang::dyn_cast<clang::FunctionDecl>(munged_it.second[0]))
                    out << methodIdx[meth];
                else if (auto enumDecl = clang::dyn_cast<clang::EnumConstantDecl>(munged_it.second[0]))
                    out << enumIdx[enumDecl];
            } else {
                // negative index into ambigious methods list
                out << '-' << ambigiousIds[base][munged_it.first];
            }
            out << "},";
            // comment
            out << "\t// " << base->getQualifiedNameAsString() << "::" << munged_it.first;
            out << "\n";
            methodMapCount++;
        }
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
    out << "        " << smokeNamespaceName << "::classes, " << classIndex.size() <<  ",\n";
    out << "        " << smokeNamespaceName << "::methods, " << methodCount << ",\n";
    out << "        " << smokeNamespaceName << "::methodMaps, " << methodMapCount << ",\n";
    out << "        " << smokeNamespaceName << "::methodNames, " << methodNames.size() << ",\n";
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
    if (!klass->hasDefinition()) {
        return false;
    }

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
    if (type->getAsCXXRecordDecl()) {
        switch(type->getAsCXXRecordDecl()->getAccess()) {
            case clang::AS_protected:
            case clang::AS_private:
                return true;
            default:
                return false;
        }
    }
    return false;
}

std::string SmokeGenerator::stackItemField(const clang::QualType type) const {
    auto deref = dereferenced(type);
    if (options->qtMode && !type->isReferenceType() && !type->isPointerType() &&
        deref->getAsCXXRecordDecl() && isTemplate(deref->getAsCXXRecordDecl()) && deref->getAsCXXRecordDecl()->getNameAsString() == "QFlags")
    {
        return "s_uint";
    }

    if (type->isPointerType() || type->isReferenceType() || deref->isFunctionPointerType() || deref->isArrayType() || contains(options->voidpTypes, deref.getAsString(pp()))
        || (!deref->isBuiltinType() && !deref->isEnumeralType()))
    {
        return "s_class";
    }

    if (deref->isEnumeralType()) {
        return "s_enum";
    }

    std::string typeName = type.getAsString(pp());
    // replace the unsigned stuff, look the type up in typeMap and if
    // necessary, add a 'u' for unsigned types at the beginning again
    bool _unsigned = false;
    if (typeName.substr(0, 9) == "unsigned ") {
        typeName = typeName.substr(9, typeName.size());
        _unsigned = true;
    }
    else if (typeName.substr(0, 7) == "signed ") {
        typeName = typeName.substr(7, typeName.size());
    }
    if (options->typeMap.count(typeName)) {
        typeName = options->typeMap[typeName];
    }
    if (_unsigned)
        typeName = "u" + typeName;
    return "s_" + typeName;
}

std::string SmokeGenerator::assignmentString(const clang::QualType type, const std::string& var) const {
    auto deref = dereferenced(type);
    if (type->isPointerType() || deref->isFunctionPointerType()) {
        return "(void*)" + var;
    } else if (type->isReferenceType()) {
        return "(void*)&" + var;
    } else if (deref->isBuiltinType() && !contains(options->voidpTypes, deref.getAsString(pp()))) {
        return var;
    } else if (deref->isEnumeralType()) {
        return var;
    } else if (options->qtMode && type->getAsCXXRecordDecl() && isTemplate(type->getAsCXXRecordDecl()) && type->getAsCXXRecordDecl()->getNameAsString() == "QFlags")
    {
        return "(uint)" + var;
    } else {
        std::string ret = "(void*)new " + type.getAsString(pp());
        ret += '(' + var + ')';
        return ret;
    }
    return std::string();
}

std::string SmokeGenerator::getTypeFlags(clang::QualType t, int *classIdx) const {
    if (t->isFunctionPointerType()) {
        const auto& fn = t->getPointeeType()->getAs<clang::FunctionType>();
        t = fn->getReturnType();
    }
    clang::QualType noPointerType = dereferenced(t);
    auto D = noPointerType->getAsCXXRecordDecl();

    clang::PrintingPolicy noTagKeyword = pp();
    noTagKeyword.SuppressTagKeyword = true;
    std::string typeName = t.getUnqualifiedType().getAsString(noTagKeyword);

    std::string flags;
    if (contains(options->voidpTypes, noPointerType.getUnqualifiedType().getAsString(pp()))) {
        // support some of the weird quirks the kalyptus code has
        flags += "Smoke::t_voidp|";
    } else if (D) {
        if (isTemplate(D)) {
            if (options->qtMode && D->getNameAsString() == "QFlags" && !t->isReferenceType() && !t->isPointerType()) {
                flags += "Smoke::t_uint|";
            } else {
                flags += "Smoke::t_voidp|";
            }
        } else {
            flags += "Smoke::t_class|";
            if (classIndex.count(D->getQualifiedNameAsString())) {
                *classIdx = classIndex.at(D->getQualifiedNameAsString());
            }
        }
    } else if (t->isBuiltinType() && t.getAsString() != "void" && !t->isPointerType() && !t->isReferenceType()) {
        flags += "Smoke::t_";

        // replace the unsigned stuff, look the type up in typeMap and if
        // necessary, add a 'u' for unsigned types at the beginning again
        bool _unsigned = false;
        if (typeName.substr(0, 9) == "unsigned ") {
            typeName = typeName.substr(9, typeName.size());
            _unsigned = true;
        }
        else if (typeName.substr(0, 7) == "signed ") {
            typeName = typeName.substr(7, typeName.size());
        }
        if (options->typeMap.count(typeName)) {
            typeName = options->typeMap[typeName];
        }
        if (_unsigned)
            typeName = "u" + typeName;

        flags += typeName + '|';
    } else if (noPointerType->isEnumeralType()) {
        flags += "Smoke::t_enum|";
        auto tag = noPointerType->getAsTagDecl();
        if (!tag) {
            *classIdx = classIndex.at("QGlobalSpace");
        }
        auto parent = tag->getParent();
        if (parent->isTranslationUnit()) {
            *classIdx = classIndex.at("QGlobalSpace");
        }
        else {
            auto parentDecl = clang::cast<clang::NamedDecl>(parent);
            if (classIndex.count(parentDecl->getQualifiedNameAsString())) {
                *classIdx = classIndex.at(parentDecl->getQualifiedNameAsString());
            }
        }
    } else {
        flags += "Smoke::t_voidp|";
    }

    if (typeName == "__va_list_tag *") {
        flags += "Smoke::tf_stack|";
    }
    else {
        if (t->isReferenceType()) {
            flags += "Smoke::tf_ref|";
            if (t->getAs<clang::ReferenceType>()->getPointeeType()->isPointerType())
                flags += "Smoke::tf_ptr|";
        }
        if (t->isPointerType())
            flags += "Smoke::tf_ptr|";
        if (!t->isReferenceType() && !t->isPointerType())
            flags += "Smoke::tf_stack|";
        if (noPointerType.isConstQualified())
            flags += "Smoke::tf_const|";
    }
    if (flags[flags.size()-1] == '|') {
        flags.pop_back();
    }

    return flags;
}

std::string SmokeGenerator::mungedName(const clang::FunctionDecl *D) const {
    std::string name = D->getNameAsString();
    for (auto param : D->params()) {
        auto type = param->getType();
        name += munge(getCanonicalType(type));
    }
    return name;
}

char SmokeGenerator::munge(clang::QualType type) const {
    if (type->isFunctionPointerType()) {
        const auto& fn = type->getPointeeType()->getAs<clang::FunctionType>();
        type = fn->getReturnType();
    }
    const auto deref = dereferenced(type).getLocalUnqualifiedType();
    const auto klass = deref->getAsCXXRecordDecl();

    if ((type->isPointerType() && type->getPointeeType()->isPointerType()) ||
        (klass && isTemplate(klass) && (!options->qtMode || (options->qtMode && klass->getNameAsString() != "QFlags"))) ||
        (contains(options->voidpTypes, deref.getAsString(pp())) && !contains(options->scalarTypes, deref.getAsString(pp())))) {

        // reference to array or hash or unknown
        return '?';
    }
    if (deref->isBuiltinType() || deref->isEnumeralType() || contains(options->scalarTypes, deref.getAsString(pp())) ||
         (options->qtMode && !type->isReferenceType() && !type->isPointerType() &&
         (klass && isTemplate(klass) && klass->getNameAsString() == "QFlags"))) {
        // plain scalar
        return '$';
    }
    else if (deref->isObjectType()) {
        // object
        return '#';
    }
    else {
        // unknown
        return '?';
    }
}

std::vector<const clang::CXXMethodDecl *> SmokeGenerator::virtualMethodsForClass(const clang::CXXRecordDecl *klass) const {
    std::vector<const clang::CXXMethodDecl *> ret;
    // virtual method callbacks for classes that can't be instantiated aren't useful
    if (!canClassBeInstantiated(klass))
        return ret;

    for (auto const &meth : collectVirtualMethods(klass)) {
        // TODO this is a synthesized overload, skip it.
        // if (!meth->remainingDefaultValues().isEmpty())
        //     continue;
        if (meth->getParent() == klass) {
            // this method can't be overriden, because it's defined in the class for which this method was called
            ret.push_back(meth);
            continue;
        }

        // Check if the method is overriden, so the callback will always point to the latest definition of the virtual method.
        const clang::CXXMethodDecl *override = 0;
        if ((override = isVirtualOverriden(meth, klass))) {
            // If the method was overriden and put under private access, skip it. If we already have the method, skip it as well.
            if (override->getAccess() == clang::AS_private || contains(ret, override))
                continue;
            ret.push_back(override);
        } else if (!contains(ret, meth)) {
            ret.push_back(meth);
        }
    }
    return ret;
}

std::vector<const clang::CXXMethodDecl *> SmokeGenerator::collectVirtualMethods(const clang::CXXRecordDecl *klass) const {
    std::vector<const clang::CXXMethodDecl *> methods;
    clang::CXXDestructorDecl *destructor = klass->getDestructor();
    for (auto const & meth : klass->methods()) {
        if((meth->isVirtual() || meth->isPure()) && meth->getAccess() != clang::AS_private && meth != destructor) {
            methods.push_back(meth);
        }
    }
    for (auto const & baseClass : klass->bases()) {
        auto baseMethods = collectVirtualMethods(baseClass.getType()->getAsCXXRecordDecl());
        methods.insert(methods.end(), baseMethods.begin(), baseMethods.end());
    }
    return methods;
}

// checks if method meth is overriden in class klass or any of its superclasses
const clang::CXXMethodDecl* SmokeGenerator::isVirtualOverriden(const clang::CXXMethodDecl *meth, const clang::CXXRecordDecl *klass) const {
    // is the method virtual at all?
    if (!(meth->isVirtual()) && !(meth->isPure()))
        return 0;

    // if the method is defined in klass, it can't be overriden there or in any parent class
    if (meth->getParent() == klass)
        return 0;

    for (auto const m : klass->methods()) {
        if (!(m->isStatic()) && m == meth)
            // the method m overrides meth
            return m;
    }

    for (auto const & base : klass->bases()) {
        // we reached the class in which meth was defined and we still didn't find any overrides => return
        if (base.getType()->getAsCXXRecordDecl() == meth->getParent())
            return 0;

        // recurse into the base classes
        const clang::CXXMethodDecl* m = 0;
        if ((m = isVirtualOverriden(meth, base.getType()->getAsCXXRecordDecl())))
            return m;
    }

    return 0;
}

bool SmokeGenerator::isClassUsed(const clang::CXXRecordDecl* klass) const {
    for (auto const & type : usedTypes) {
        if (dereferenced(type)->getAsCXXRecordDecl() == klass)
            return true;
    }
    return false;
}

void SmokeGenerator::checkForAbstractClass(clang::CXXRecordDecl* klass) const {
    bool hasPrivatePureVirtuals = false;
    for (const auto& meth : klass->methods()) {
        if ((meth->isPure() && meth->isVirtual()) && meth->getAccess() == clang::AS_private) {
            hasPrivatePureVirtuals = true;
            break;
        }
    }

    // abstract classes can't be instanstiated - remove the constructors
    if (hasPrivatePureVirtuals) {
        std::vector<clang::CXXConstructorDecl*> ctors(klass->ctor_begin(), klass->ctor_end());
        for (const auto& ctor : ctors) {
            klass->removeDecl(ctor);
        }
    }
}

void SmokeGenerator::insertTemplateParameters(const clang::QualType type) {
    const auto recordDecl = type->getAsCXXRecordDecl();
    if (!recordDecl) {
        return;
    }
    const auto templateSpecializationDecl = clang::dyn_cast<clang::ClassTemplateSpecializationDecl>(recordDecl);
    if (!templateSpecializationDecl) {
        return;
    }

    const auto & args = templateSpecializationDecl->getTemplateArgs();
    for (int i=0; i < args.size(); ++i) {
        const auto & arg = args[i];
        usedTypes.insert(getCanonicalType(arg.getAsType()));
        insertTemplateParameters(getCanonicalType(arg.getAsType()));
    }
}

std::vector<clang::FunctionDecl*> SmokeGenerator::addOverloads(clang::FunctionDecl* function) const {
    clang::DeclContext* parent = function->getParent();
    std::vector<clang::FunctionDecl*> createdFunctions;

    // Get a list of all parameters to this function
    std::vector<clang::ParmVarDecl*> params;
    params.insert(params.begin(), function->param_begin(), function->param_end());

    std::vector<clang::QualType> paramTypes;
    for (const auto& p : params) {
        paramTypes.push_back(p->getType());
    }

    while(params.size()) {
        // Check to see if the last parameter has a default argument
        auto lastParam = params.back();
        paramTypes.pop_back();
        clang::Expr* defaultArgument = lastParam->getDefaultArg();

        // done with lastParam
        lastParam = nullptr;
        params.pop_back();

        // We've reached the last parameter with a default value.  We're done
        // adding overloads.
        if (!defaultArgument) {
            break;
        }

        // Make a new function with the latest parameter with a default argument
        // removed.
        clang::DeclarationName Name = function->getDeclName();
        clang::SourceLocation Loc = function->getLocation();
        clang::DeclarationNameInfo NameInfo(Name, Loc);

        // Make the underlying function type
        clang::QualType functionType = ctx->getFunctionType(
            function->getReturnType(),
            llvm::makeArrayRef<clang::QualType>(paramTypes),
            function->getType()->getAs<clang::FunctionProtoType>()->getExtProtoInfo()
        );

        // Make the function instance
        clang::FunctionDecl* newFunction = nullptr;
        if (const auto ctor = clang::dyn_cast<clang::CXXConstructorDecl>(function)) {
            newFunction = clang::CXXConstructorDecl::Create(
                *ctx,
                clang::cast<clang::CXXRecordDecl>(parent),
                Loc,
                NameInfo,
                functionType,
                /*TInfo=*/nullptr,
                /*isExplicit=*/ctor->isExplicitSpecified(),
                /*isInline=*/ctor->isInlined(),
                /*isInline=*/ctor->isImplicit(),
                /*isConst=*/ctor->isConstexpr()
            );
        }
        else if (const auto method = clang::dyn_cast<clang::CXXMethodDecl>(function)) {
            newFunction = clang::CXXMethodDecl::Create(
                *ctx,
                clang::cast<clang::CXXRecordDecl>(parent),
                Loc,
                NameInfo,
                functionType,
                /*TInfo=*/nullptr,
                /*StorageClass=*/method->getStorageClass(),
                /*isInline=*/function->isInlined(),
                /*isConst=*/function->isConstexpr(),
                Loc
            );
        }
        else {
            newFunction = clang::FunctionDecl::Create(
                *ctx,
                parent,
                Loc,
                NameInfo,
                functionType,
                /*TInfo=*/nullptr,
                /*StorageClass=*/clang::SC_None,
                /*isInline=*/function->isInlined(),
                function->hasWrittenPrototype(),
                /*isConst=*/function->isConstexpr()
            );
        }
        newFunction->setAccess(function->getAccess());
        for (auto attr_it = function->specific_attr_begin<clang::AnnotateAttr>();
          attr_it != function->specific_attr_end<clang::AnnotateAttr>();
          ++attr_it) {
            newFunction->addAttr(*attr_it);
        }

        newFunction->setParams(llvm::makeArrayRef(params));

        createdFunctions.push_back(newFunction);
    }
    for (auto fn = createdFunctions.rbegin(); fn < createdFunctions.rend(); ++fn) {
        // Add the function to the class
        parent->addDecl(*fn);
    }
    return createdFunctions;
}

void SmokeGenerator::addQPropertyAnnotations(const clang::CXXRecordDecl* D) const {
    for (const auto& d : D->decls()) {
        if (clang::StaticAssertDecl *S = llvm::dyn_cast<clang::StaticAssertDecl>(d) ) {
            if (auto *E = llvm::dyn_cast<clang::UnaryExprOrTypeTraitExpr>(S->getAssertExpr())) {
                if (clang::ParenExpr *PE = llvm::dyn_cast<clang::ParenExpr>(E->getArgumentExpr())) {
                    llvm::StringRef key = S->getMessage()->getString();
                    if (key == "qt_property") {
                        clang::StringLiteral *Val = llvm::dyn_cast<clang::StringLiteral>(PE->getSubExpr());

                        std::string propertyStr = Val->getString().str();
                        std::smatch match;
                        std::regex readRe("READ +([^ ]*)");

                        if (std::regex_search(propertyStr, match, readRe)) {
                            auto Name = ctx->DeclarationNames.getIdentifier(&ctx->Idents.get(llvm::StringRef(match[1])));
                            auto lookup = D->lookup(Name);
                            auto data = lookup.data();
                            if (const auto& method = clang::dyn_cast<clang::CXXMethodDecl>(*data)) {
                                auto annotate = clang::AnnotateAttr(clang::SourceRange(), *ctx, llvm::StringRef("qt_property"), 0).clone(*ctx);
                                method->addAttr(annotate);
                            }
                        }

                        std::regex writeRe("WRITE +([^ ]*)");
                        if (std::regex_search(propertyStr, match, writeRe)) {
                            auto Name = ctx->DeclarationNames.getIdentifier(&ctx->Idents.get(llvm::StringRef(match[1])));
                            auto lookup = D->lookup(Name);
                            auto data = lookup.data();
                            if (const auto& method = clang::dyn_cast<clang::CXXMethodDecl>(*data)) {
                                auto annotate = clang::AnnotateAttr(clang::SourceRange(), *ctx, llvm::StringRef("qt_property"), 0).clone(*ctx);
                                method->addAttr(annotate);
                            }
                        }
                    }
                }
            }
        }
    }
}

std::string SmokeGenerator::generateMethodBody(const std::string& indent, const std::string& className, const std::string& smokeClassName, const clang::FunctionDecl* function,
                                            int index, bool dynamicDispatch, std::set<std::string>& includes)
{
    std::stringstream out;

    out << indent;

    const auto meth = clang::dyn_cast<clang::CXXMethodDecl>(function);

    auto retType = function->getReturnType().getCanonicalType();

    if (clang::isa<clang::CXXConstructorDecl>(meth)) {
        retType = ctx->getPointerType(clang::QualType(meth->getParent()->getTypeForDecl(), 0));
        out << smokeClassName << "* xret = new " << smokeClassName << "(";
    } else {
        auto ploc = ctx->getSourceManager().getPresumedLoc(function->getSourceRange().getBegin());
        if (!ploc.isInvalid()) {
            includes.insert(ploc.getFilename());
        }

        if (const auto typeClass = retType->getAsCXXRecordDecl()) {
            ploc = ctx->getSourceManager().getPresumedLoc(typeClass->getSourceRange().getBegin());
            if (!ploc.isInvalid()) {
                includes.insert(ploc.getFilename());
            }
        }

        if (retType->isFunctionPointerType() || retType->isArrayType()) {
            std::string fnPtrName = "xret";
            retType.getAsStringInternal(fnPtrName, pp());
            out << fnPtrName << " = ";
        }
        else if (!retType->isVoidType()) {
            out << retType.getAsString(pp()) << " xret = ";
        }

        if (!(meth && meth->isStatic())) {
            if (meth->isConst()) {
                out << "((const " << smokeClassName << "*)this)->";
            }
            else {
                out << "this->";
            }
        }
        if (!dynamicDispatch) {
            // dynamic dispatch not wanted, call with 'this->Foo::method()'
            out << className << "::";
        }
        else if (!meth) {
            if (clang::cast<clang::NamedDecl>(function->getParent())->getNameAsString() != "QGlobalSpace") {
                out << clang::cast<clang::NamedDecl>(function->getParent())->getQualifiedNameAsString() << "::";
            }
        }
        out << function->getNameAsString() << "(";
    }

    for (int j = 0; j < function->parameters().size(); j++) {
        const auto& param = function->getParamDecl(j);

        if (const auto typeClass = param->getType()->getAsCXXRecordDecl()) {
            auto ploc = ctx->getSourceManager().getPresumedLoc(typeClass->getSourceRange().getBegin());
            if (!ploc.isInvalid()) {
                includes.insert(ploc.getFilename());
            }
        }

        if (j > 0) out << ",";

        std::string field = stackItemField(param->getType());
        const auto type = param->getType().getCanonicalType();
        std::string typeName = type.getAsString(pp());
        if (type->isArrayType()) {
            //Type t = *param.type();
            //t.setPointerDepth(t.pointerDepth() + 1);
            //t.setIsRef(false);
            //typeName = t.toString();
            out << '*';
        }
        //else if (field == "s_class" && (param->getType()->isPointerType() || param->getType()->isReferenceType()) && !param->getType()->isFunctionPointerType()) {
        //    // references and classes are passed in s_class
        //    typeName += '*';
        //    out << '*';
        //}
        // casting to a reference doesn't make sense in this case
        if (type->isReferenceType() && !type->isFunctionPointerType()) {
            std::remove(typeName.begin(), typeName.end(), '&');
        }
        out << "(" << typeName << ")" << "x[" << j + 1 << "]." << field;
    }

    //// if the method has any other default parameters, append them here as values
    //if (!function->remainingDefaultValues().isEmpty()) {
    //    const std::stringList& defaultParams = function->remainingDefaultValues();
    //    if (function->parameters().count() > 0)
    //        out << "," ;
    //    out << defaultParams.join(",");
    //}

    out << ");\n";
    if (!retType->isVoidType()) {
        out << indent << "x[0]." << stackItemField(retType) << " = " << assignmentString(retType, "xret") << ";\n";
    } else {
        out << indent << "(void)x; // noop (for compiler warning)\n";
    }

    return out.str();
}

void SmokeGenerator::generateMethod(std::stringstream& out, const std::string& className, const std::string& smokeClassName,
                                     const clang::FunctionDecl* function, int index, std::set<std::string>& includes) {

    const auto meth = clang::dyn_cast<clang::CXXMethodDecl>(function);

    out << "    ";
    if (meth && (meth->isStatic() || clang::isa<clang::CXXConstructorDecl>(meth))) {
        out << "static ";
    }
    out << "void x_" << std::to_string(index) << "(Smoke::Stack x) {\n";
    out << "        // " << getFullFunctionPrototype(function, pp()) << "\n";

    bool dynamicDispatch = function->isPure() && (!meth || meth->isVirtual());

    if (dynamicDispatch || (meth && !contains(virtualMethodsForClass(meth->getParent()), meth))) {
        // This is either already flagged as dynamic dispatch or just a normal method. We can generate a normal method call for it.

        out << generateMethodBody("        ",   // indent
                                  className, smokeClassName, meth, index, dynamicDispatch, includes);
    } else {
        // This is a virtual method. To know whether we should call with dynamic dispatch, we need a bit of RTTI magic.
        includes.insert("typeinfo");
        out << "        if (dynamic_cast<__internal_SmokeClass*>(static_cast<" << className << "*>(this))) {\n";   //
        out << generateMethodBody("            ",   // indent
                                  className, smokeClassName, meth, index, false, includes);
        out << "        } else {\n";
        out << generateMethodBody("            ",   // indent
                                  className, smokeClassName, meth, index, true, includes);
        out << "        }\n";
    }

    out << "    }\n";

    // If the constructor was generated from another one with default parameteres, we don't need to explicitly create
    // it here again. The x_* call will append the default parameters at the end and thus choose the right constructor.
    //if (meth.isConstructor() && meth.remainingDefaultValues().isEmpty()) {
    //    out << "    explicit " << smokeClassName << '(';
    //    std::stringList x_list;
    //    for (int i = 0; i < meth.parameters().count(); i++) {
    //        if (i > 0) out << ", ";
    //        out << meth.parameters()[i].type()->toString() << " x" << std::string::number(i + 1);
    //        x_list << "x" + std::string::number(i + 1);
    //    }
    //    out << ") : " << meth.getClass()->name() << '(' << x_list.join(", ") << ") {}\n";
    //}
}

void SmokeGenerator::writeClassFiles() {
    // how many classes go in one file
    int count = includedClasses.size() / options->parts;

    for (int i = 0; i < options->parts; i++) {
        std::set<std::string> includes;
        std::string classCode;
        std::stringstream classOut;

        // write the class code to a string so we can later prepend the #includes
        auto begin = includedClasses.begin() + (count * i);
        auto end = begin;
        if (i == options->parts - 1) {
            end = includedClasses.end();
        }
        else {
            end = begin + count;
        }
        for (const auto& str : llvm::iterator_range<decltype(includedClasses)::iterator>(begin, end)) {
            const clang::NamedDecl* klass = nullptr;
            if (classes.count(str)) {
                klass = classes.at(str);
            }
            else if (namespaces.count(str)) {
                klass = namespaces.at(str);
            }
            const auto ploc = ctx->getSourceManager().getPresumedLoc(klass->getSourceRange().getBegin());
            if (!ploc.isInvalid()) {
                includes.insert(ploc.getFilename());
            }
            writeClass(classOut, klass, str, includes);
            classCode = classOut.str();
        }

        // create the file
        auto& fileOut = *ci.createOutputFile(
            options->outputDir + "x_" + std::to_string(i + 1) + ".cpp",
            false,
            true,
            "",
            "",
            false,
            true
        );

        // write out the header
        fileOut << "//Auto-generated by smokegen. DO NOT EDIT.\n";

        // ... and the #includes
        std::vector<std::string> sortedIncludes(includes.begin(), includes.end());
        std::sort(sortedIncludes.begin(), sortedIncludes.end());
        for (const auto& str : sortedIncludes) {
            fileOut << "#include <" << str << ">\n";
        }

        fileOut << "\n#include <smoke.h>\n#include <" << options->module << "_smoke.h>\n";

        fileOut << "\nclass __internal_SmokeClass {};\n";

        fileOut << "\nnamespace __smoke" << options->module << " {\n\n";

        // now the class code
        fileOut << classCode;

        fileOut << "\n}\n";
    }
}

void SmokeGenerator::generateGetAccessor(std::stringstream& out, const std::string& className, const clang::CXXMethodDecl* method,
                                          const clang::QualType type, int index) const {
    out << "    ";
    std::string fieldName;
    const auto field = fieldAccessors.at(method);
    if (method->isStatic()) {
        out << "static ";
    }
    else {
        fieldName = "this->";
    }
    fieldName += className + "::" + field->getNameAsString();
    out << "void x_" << index << "(Smoke::Stack x) {\n"
        << "        // " << getFullFunctionPrototype(method, pp()) << "\n"
        << "        x[0]." << stackItemField(type) << " = "
            << assignmentString(type, fieldName) << ";\n"
        << "    }\n";
}

void SmokeGenerator::generateSetAccessor(std::stringstream& out, const std::string& className, const clang::CXXMethodDecl* method,
                                          const clang::QualType type, int index) const {
    out << "    ";
    std::string fieldName;
    const auto field = fieldAccessors.at(method);
    if (method->isStatic()) {
        out << "static ";
    }
    else {
        fieldName = "this->";
    }
    fieldName += className + "::" + field->getNameAsString();
    out << "void x_" << index << "(Smoke::Stack x) {\n"
        << "        // " << getFullFunctionPrototype(method, pp()) << "=\n"
        << "        " << fieldName << " = ";
    std::string unionField = stackItemField(type);
    std::string cast = type.getAsString(pp());
    std::remove(cast.begin(), cast.end(), '&');
    if (unionField == "s_class" && !type->isPointerType()) {
        out << '*';
        cast += '*';
    }
    out << '(' << cast << ')' << "x[1]." << unionField << ";\n";
    out << "    }\n";
}

void SmokeGenerator::generateEnumMemberCall(std::stringstream& out, const std::string& className, const std::string& member, int index) const {
    out << "    static void x_" << index << "(Smoke::Stack x) {\n"
        << "        x[0].s_enum = (long)";

    if (!className.empty()) {
        out  << className << "::";
    }

    out << member << ";\n"
        << "    }\n";
}

void SmokeGenerator::generateVirtualMethod(std::stringstream& out, const clang::CXXMethodDecl* meth, std::set<std::string>& includes) const {
    std::string x_params;
    std::string x_list;
    std::string type = meth->getReturnType().getAsString(pp());
    if (meth->getReturnType()->getAsCXXRecordDecl()) {
        auto ploc = ctx->getSourceManager().getPresumedLoc(meth->getReturnType()->getAsCXXRecordDecl()->getSourceRange().getBegin());
        if (!ploc.isInvalid()) {
            includes.insert(ploc.getFilename());
        }
    }

    out << "    virtual " << type << " " << meth->getNameAsString() << "(";
    for (int i = 0; i < meth->parameters().size(); i++) {
        if (i > 0) {
            out << ", "; x_list.append(", ");
        }
        const auto& param = meth->parameters()[i];

        if (param->getType()->getAsCXXRecordDecl()) {
            auto ploc = ctx->getSourceManager().getPresumedLoc(param->getType()->getAsCXXRecordDecl()->getSourceRange().getBegin());
            if (!ploc.isInvalid()) {
                includes.insert(ploc.getFilename());
            }
        }

        out << param->getType().getAsString(pp()) << " x" << i + 1;
        x_params += "        x[" + std::to_string(i + 1) + "]." + stackItemField(param->getType()) + " = " +
            assignmentString(param->getType(), "x" + std::to_string(i + 1)) + ";\n";
        x_list += "x" + std::to_string(i + 1);
    }
    out << ") ";
    if (meth->isConst()) {
        out << "const ";
    }
    //if (meth.hasExceptionSpec()) {
    //    out << "throw(";
    //    for (int i = 0; i < meth.exceptionTypes().count(); i++) {
    //        if (i > 0) out << ", ";
    //        out << meth.exceptionTypes()[i].toString();
    //    }
    //    out << ") ";
    //}
    out << "{\n";
    out << "        Smoke::StackItem x[" + std::to_string(meth->parameters().size() + 1) + "];\n";
    out << x_params;

    if (meth->isPure() && meth->isVirtual()) {
        out << "        this->_binding->callMethod(" + std::to_string(methodIdx.at(meth)) + ", (void*)this, x, true /*pure virtual*/);\n";
        if (!meth->getReturnType()->isVoidType()) {
            std::string field = stackItemField(meth->getReturnType());
            if (!meth->getReturnType()->isPointerType() && field == "s_class") {
                std::string tmpType = type;
                if (meth->getReturnType()->isReferenceType()) {
                    std::remove(tmpType.begin(), tmpType.end(), '&');
                }
                tmpType += '*';
                out << "        " << tmpType << " xptr = (" << tmpType << ")x[0].s_class;\n";
                out << "        " << type << " xret(*xptr);\n";
                out << "        delete xptr;\n";
                out << "        return xret;\n";
            } else {
                out << "        return (" + type + ")x[0]." + stackItemField(meth->getReturnType()) + ";\n";
            }
        }
    } else {
        out << std::string("        if (this->_binding->callMethod(" + std::to_string(methodIdx.at(meth)) + ", (void*)this, x)) ");
        if (meth->getReturnType()->isVoidType()) {
            out << "return;\n";
        } else {
            std::string field = stackItemField(meth->getReturnType());
            if (!meth->getReturnType()->isPointerType() && field == "s_class") {
                std::string tmpType = type;
                if (meth->getReturnType()->isReferenceType()) {
                    std::remove(tmpType.begin(), tmpType.end(), '&');
                }
                tmpType += '*';
                out << "{\n";
                out << "            " << tmpType << " xptr = (" << tmpType << ")x[0].s_class;\n";
                out << "            " << type << " xret(*xptr);\n";
                out << "            delete xptr;\n";
                out << "            return xret;\n";
                out << "        }\n";
            } else {
                out << "return (" + type + ")x[0]." + stackItemField(meth->getReturnType()) + ";\n";
            }
        }
        out << "        ";
        if (!meth->getReturnType()->isVoidType()) {
            out << "return ";
        }
        out << "this->" + meth->getQualifiedNameAsString() + "(" + x_list + ");\n";
    }
    out << "    }\n";
}

void SmokeGenerator::writeClass(std::stringstream& out, const clang::NamedDecl* decl, const std::string& className, std::set<std::string>& includes) {

    const auto klass = clang::dyn_cast<clang::CXXRecordDecl>(decl);
    const auto nspace = clang::dyn_cast<clang::NamespaceDecl>(decl);
    const auto context = clang::dyn_cast<clang::DeclContext>(decl);

    std::string underscoreName = std::string(className);
    std::replace(underscoreName.begin(), underscoreName.end(), ':', '_');
    const std::string smokeClassName = "x_" + underscoreName;

    std::stringstream switchOut;

    const clang::CXXDestructorDecl *destructor = nullptr;
    out << "class " << smokeClassName;
    if (klass) {
        destructor = klass->getDestructor();

        out << " : public " << className;
        if (hasClassVirtualDestructor(klass) && destructor->getAccess() == clang::AS_public) {
            out << ", public __internal_SmokeClass";
        }
    }
    out << " {\n";
    if (klass && canClassBeInstantiated(klass)) {
        out << "    SmokeBinding* _binding;\n";
        out << "public:\n";
        out << "    void x_0(Smoke::Stack x) {\n";
        out << "        // set the smoke binding\n";
        out << "        _binding = (SmokeBinding*)x[1].s_class;\n";
        out << "    }\n";

        switchOut << "        case 0: xself->x_0(args);\tbreak;\n";
    } else {
        out << "public:\n";
    }

    std::vector<const clang::FunctionDecl *> functions;
    if (klass) {
        functions.insert(functions.end(), klass->method_begin(), klass->method_end());
    }
    else if (nspace) {
        functions.insert(functions.end(), function_iterator(nspace->decls_begin()), function_iterator(nspace->decls_end()));
    }
    int xcall_index = 1;
    for (const auto& function : functions) {
        if (function->getAccess() == clang::AS_private) {
            continue;
        }
        if (clang::isa<clang::CXXDestructorDecl>(function)) {
            continue;
        }
        const auto& meth = clang::dyn_cast<clang::CXXMethodDecl>(function);
        if (meth && meth->isCopyAssignmentOperator() && meth->isImplicit()) {
            continue;
        }
        switchOut << "        case " << xcall_index << ": "
                  << ((meth && (meth->isStatic() || clang::isa<clang::CXXConstructorDecl>(meth))) ? smokeClassName + "::" : "xself->")
                  << "x_" << xcall_index << "(args);\tbreak;\n";
        if (fieldAccessors.count(meth)) {
            // accessor method?
            if (meth->getNameAsString().compare(0, 3, "set") == 0) {
                generateSetAccessor(out, className, meth, meth->parameters()[0]->getType(), xcall_index);
            } else {
                generateGetAccessor(out, className, meth, meth->getReturnType(), xcall_index);
            }
        } else {
            generateMethod(out, className, smokeClassName, meth, xcall_index, includes);
        }
        xcall_index++;
    }

    std::stringstream enumOut;
    bool enumFound = false;
    for (const auto* e : enum_range(enum_iterator(context->decls_begin()), enum_iterator(context->decls_end()))) {
        if (e->getAccess() == clang::AS_private) {
            continue;
        }

        enumFound = true;

        for (const auto& member : e->enumerators()) {
            switchOut << "        case " << xcall_index << ": " << smokeClassName <<  "::x_" << xcall_index << "(args);\tbreak;\n";
            //if (e->getParent()->getAsCXXRecordDecl()) {
            generateEnumMemberCall(out, className, member->getQualifiedNameAsString(), xcall_index++);
            //}
            //else {
            //    generateEnumMemberCall(out, e->nameSpace(), member.name(), xcall_index++);
            //}
        }

        // only generate the xenum_call if the enum has a valid name
        if (e->getNameAsString().empty()) {
            continue;
        }

        // xenum_operation method code
        std::string enumString = e->getQualifiedNameAsString();
        auto enumType = getCanonicalType(clang::QualType(e->getTypeForDecl(), 0));
        if (movedEnums.count(enumType)) {
            enumType = movedEnums.at(enumType);
        }
        if (!typeIndex.count(enumType)) {
            llvm::outs() << "missing enum: " << enumString << "\n";
            continue;
        }
        enumOut << "        case " << typeIndex.at(enumType) << ": //" << enumString << '\n';
        enumOut << "            switch(xop) {\n";
        enumOut << "                case Smoke::EnumNew:\n";
        enumOut << "                    xdata = (void*)new " << enumString << ";\n";
        enumOut << "                    break;\n";
        enumOut << "                case Smoke::EnumDelete:\n";
        enumOut << "                    delete (" << enumString << "*)xdata;\n";
        enumOut << "                    break;\n";
        enumOut << "                case Smoke::EnumFromLong:\n";
        enumOut << "                    *(" << enumString << "*)xdata = (" << enumString << ")xvalue;\n";
        enumOut << "                    break;\n";
        enumOut << "                case Smoke::EnumToLong:\n";
        enumOut << "                    xvalue = (long)*(" << enumString << "*)xdata;\n";
        enumOut << "                    break;\n";
        enumOut << "            }\n";
        enumOut << "            break;\n";
    }

    if (klass) {
        for (const auto meth : virtualMethodsForClass(klass)) {
            generateVirtualMethod(out, meth, includes);
        }
    }

    // this class contains enums, write out an xenum_operation method
    if (enumFound) {
        out << "    static void xenum_operation(Smoke::EnumOperation xop, Smoke::Index xtype, void *&xdata, long &xvalue) {\n";
        out << "        switch(xtype) {\n";
        out << enumOut.str();
        out << "        }\n";
        out << "    }\n";
    }

    // destructor
    // if the class can't be instanstiated, a callback when it's deleted is unnecessary
    if (klass && canClassBeInstantiated(klass)) {
        out << "    ~" << smokeClassName << "() ";
        //if (destructor && destructor->hasExceptionSpec()) {
        //    out << "throw(";
        //    for (int i = 0; i < destructor->exceptionTypes().count(); i++) {
        //        if (i > 0) out << ", ";
        //        out << destructor->exceptionTypes()[i].toString();
        //    }
        //    out << ") ";
        //}
        out << "{ this->_binding->deleted(" + std::to_string(classIndex[className]) + ", (void*)this); }\n";
    }
    out << "};\n";

    if (enumFound) {
        out << "void xenum_" << underscoreName << "(Smoke::EnumOperation xop, Smoke::Index xtype, void *&xdata, long &xvalue) {\n";
        out << "    " << smokeClassName << "::xenum_operation(xop, xtype, xdata, xvalue);\n";
        out << "}\n";
    }

    // xcall_class function
    out << "void xcall_" << underscoreName << "(Smoke::Index xi, void *obj, Smoke::Stack args) {\n";
    out << "    " << smokeClassName << " *xself = (" << smokeClassName << "*)obj;\n";
    out << "    switch(xi) {\n";
    out << switchOut.str();
    if (klass && destructor->getAccess() == clang::AS_public) {
        out << "        case " << xcall_index << ": delete (" << className << "*)xself;\tbreak;\n";
    }
    out << "    }\n";
    out << "}\n";
}
