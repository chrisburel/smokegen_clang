#include <clang/Frontend/CompilerInstance.h>
#include <clang/Sema/Sema.h>
#include <clang/Sema/SemaDiagnostic.h>

#include "astvisitor.h"

bool SmokegenASTVisitor::VisitCXXRecordDecl(clang::CXXRecordDecl *D) {
    // We can't make bindings for things that don't have names.
    if (!D->getDeclName())
        return false;

    InstantiateImplicitMethods(D);

    generator.addClass(D);

    return true;
}

bool SmokegenASTVisitor::VisitEnumDecl(clang::EnumDecl *D) {
    // We can't make bindings for things that don't have names.
    if (!D->getDeclName())
        return false;

    generator.addEnum(D);

    return true;
}

void SmokegenASTVisitor::InstantiateImplicitMethods(clang::CXXRecordDecl* decl) {
    if (!decl || !decl->isThisDeclarationADefinition())
        return;
    if (decl->isDependentType())   // only instantiate if class is instantiated
        return;

    clang::Sema& sema = ci.getSema();

    ci.getDiagnostics().setSeverity(clang::diag::err_uninitialized_member_in_ctor, clang::diag::Severity::Ignored, decl->getLocation());

    clang::DeclContext::lookup_result ctors = sema.LookupConstructors(decl);
    for (auto const & it : ctors) {
        // Ignore templated constructors.
        if (clang::isa<clang::FunctionTemplateDecl>(*it))
            continue;
        clang::CXXConstructorDecl* ctor = &clang::cast<clang::CXXConstructorDecl>(*it);
        if (!ctor->hasBody() && !ctor->isDeleted() && ctor->isImplicit()) {
            if (sema.getSpecialMember(ctor) == clang::Sema::CXXDefaultConstructor) {
                sema.DefineImplicitDefaultConstructor(decl->getLocation(), ctor);
            }
        }
        // Unreferenced template constructors stay uninstantiated on purpose.
    }

    if (clang::CXXConstructorDecl* copyCtor = sema.LookupCopyingConstructor(decl, 0)) {
        if (clang::isa<clang::FunctionTemplateDecl>(copyCtor) &&
            !copyCtor->hasBody() && !copyCtor->isDeleted() && copyCtor->isImplicit()) {
            sema.DefineImplicitCopyConstructor(decl->getLocation(), copyCtor);
        }
    }

    if (clang::CXXDestructorDecl* dtor = sema.LookupDestructor(decl)) {
        if (!dtor->isDeleted()) {
            if (!dtor->hasBody() && dtor->isImplicit())
                sema.DefineImplicitDestructor(decl->getLocation(), dtor);
            if (!dtor->isDefined() && dtor->getTemplateInstantiationPattern())
                sema.PendingInstantiations.push_back(std::make_pair(dtor, decl->getLocation()));
        }
    }

    // clang queues up method instantiations.  We need to process them now.
    sema.PerformPendingInstantiations();
}
