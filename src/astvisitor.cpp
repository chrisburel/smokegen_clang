#include <clang/Frontend/CompilerInstance.h>
#include <clang/Sema/Sema.h>
#include <clang/Sema/SemaDiagnostic.h>

#include "astvisitor.h"

bool SmokegenASTVisitor::VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    // We can't make bindings for things that don't have names.
    if (!D->getDeclName())
        return true;

    InstantiateImplicitMethods(D);

    generator.addClass(D);

    return true;
}

bool SmokegenASTVisitor::VisitEnumDecl(clang::EnumDecl *D) {
    // We can't make bindings for things that don't have names.
    if (!D->getDeclName())
        return true;

    generator.addEnum(D);

    return true;
}

bool SmokegenASTVisitor::VisitNamespaceDecl(clang::NamespaceDecl *D) {
    if (!D->getDeclName())
        return true;

    generator.addNamespace(D);

    return true;
}

bool SmokegenASTVisitor::VisitFunctionDecl(clang::FunctionDecl *D) {
    if (clang::isa<clang::CXXMethodDecl>(D)){
        return true;
    }

    if (!D->getDeclName())
        return true;

    generator.addFunction(D);

    return true;
}

void SmokegenASTVisitor::InstantiateImplicitMethods(clang::CXXRecordDecl* decl) {
    if (!decl || !decl->isThisDeclarationADefinition())
        return;
    if (decl->isDependentType())   // only instantiate if class is instantiated
        return;

    clang::Sema& sema = ci.getSema();

    ci.getDiagnostics().setSeverity(clang::diag::err_uninitialized_member_in_ctor, clang::diag::Severity::Ignored, decl->getLocation());

    // Default destructor
    clang::CXXDestructorDecl* dtor = sema.LookupDestructor(decl);
    if (!dtor->isDeleted()) {
        if (!dtor->hasBody() && dtor->isImplicit())
            sema.DefineImplicitDestructor(decl->getLocation(), dtor);
        if (!dtor->isDefined() && dtor->getTemplateInstantiationPattern())
            sema.PendingInstantiations.push_back(std::make_pair(dtor, decl->getLocation()));
    }

    // Default constructor
    if (dtor->getAccess() != clang::AS_private) {
        clang::CXXConstructorDecl* defCtor = sema.LookupDefaultConstructor(decl);
        if (decl->needsImplicitDefaultConstructor()) {
            // Ignore templated constructors.
            if (!clang::isa<clang::FunctionTemplateDecl>(defCtor) && !defCtor->hasBody() && !defCtor->isDeleted() && defCtor->isImplicit()) {
                sema.DefineImplicitDefaultConstructor(decl->getLocation(), defCtor);
            }
            // Unreferenced template constructors stay uninstantiated on purpose.
        }
    }

    // Copy constructor
    bool addCopyCtor = true;

    if (dtor->getAccess() == clang::AS_private) {
        addCopyCtor = false;
    }
    for (const auto& base : decl->bases()) {
        if (!canClassBeCopied(base.getType()->getAsCXXRecordDecl())) {
            addCopyCtor = false;
            break;
        }
    }

    if (addCopyCtor) {
        clang::CXXConstructorDecl* copyCtor = sema.LookupCopyingConstructor(decl, 0);
        if (clang::isa<clang::FunctionTemplateDecl>(copyCtor) && !copyCtor->hasBody() && !copyCtor->isDeleted() && copyCtor->isImplicit()) {
            sema.DefineImplicitCopyConstructor(decl->getLocation(), copyCtor);
        }
    }

    // clang queues up method instantiations.  We need to process them now.
    sema.PerformPendingInstantiations();
}

bool SmokegenASTVisitor::canClassBeCopied(const clang::CXXRecordDecl *klass) const {
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
