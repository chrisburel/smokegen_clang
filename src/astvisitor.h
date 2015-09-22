#ifndef SMOKEGEN_ASTVISITOR
#define SMOKEGEN_ASTVISITOR

#include <clang/AST/RecursiveASTVisitor.h>
#include "generator.h"

class SmokegenASTVisitor : public clang::RecursiveASTVisitor<SmokegenASTVisitor> {
public:
    SmokegenASTVisitor(clang::CompilerInstance &ci, SmokeGenerator &generator) : ci(ci), generator(generator) {}

    bool VisitCXXRecordDecl(clang::CXXRecordDecl *D);

    bool VisitEnumDecl(clang::EnumDecl *D);

    // clang lazily constructs the implicit methods of a C++ class (the
    // default constructor and destructor, etc) -- it only bothers to
    // create a CXXMethodDecl if someone actually calls these classes.
    // But we need to be non-lazy: iwyu depends on analyzing what future
    // code *may* call in a class, not what current code *does*.  So we
    // force all the lazy evaluation to happen here.  This will
    // (possibly) add a bunch of MethodDecls to the AST, as children of
    // decl.  We're hoping it will always be safe to modify the AST
    // while it's being traversed!
    void InstantiateImplicitMethods(clang::CXXRecordDecl* decl);

private:
    clang::CompilerInstance &ci;
    SmokeGenerator &generator;
};

#endif
