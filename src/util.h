#ifndef GENERATOR_UTIL_H
#define GENERATOR_UTIL_H

#include <clang/AST/DeclTemplate.h>

typedef clang::CapturedDecl::specific_decl_iterator<clang::FunctionDecl> function_iterator;
typedef llvm::iterator_range<function_iterator> function_range;

typedef clang::CapturedDecl::specific_decl_iterator<clang::VarDecl> var_iterator;
typedef llvm::iterator_range<var_iterator> var_range;

typedef clang::CapturedDecl::specific_decl_iterator<clang::EnumDecl> enum_iterator;
typedef llvm::iterator_range<enum_iterator> enum_range;

template <class T>
bool contains(const std::vector<T> &vec, const T &value)
{
    return std::find(vec.begin(), vec.end(), value) != vec.end();
}

bool isTemplate(const clang::CXXRecordDecl *D) {
    return (D->getDescribedClassTemplate() || clang::isa<clang::ClassTemplateSpecializationDecl>(D));
}

clang::QualType dereferenced(const clang::QualType &type) {
    clang::QualType dereferenced = type;
    if (auto refType = dereferenced->getAs<clang::ReferenceType>()) {
        dereferenced = refType->getPointeeType();
    }
    while(dereferenced->isPointerType()) {
        dereferenced = dereferenced->getPointeeType();
    }
    return dereferenced;
}

std::string getFullFunctionPrototype(const clang::FunctionDecl *d, const clang::PrintingPolicy &policy) {
    // Use getAsStringInternal to inject the function name into the string that
    // FunctionProtoType.getAsString() returns.
    auto name = d->getQualifiedNameAsString();
    d->getType().getAsStringInternal(name, policy);

    // The resulting string will have the function name wrapped in parens.
    // Remove them.
    auto firstOpenParen = name.find('(');
    name.erase(firstOpenParen, 1);
    auto firstCloseParen = name.find(')');
    name.erase(firstCloseParen, 1);

    if (auto ctor = clang::dyn_cast<clang::CXXConstructorDecl>(d)) {
        name.erase(0, 5);
        name = ctor->getNameAsString() + " *" + name;
    }

    return name;
}

clang::QualType getCanonicalType(const clang::QualType &type) {
    if (dereferenced(type).getAsString() == "FILE") {
        return type;
    }
    auto canonicalType = type.getCanonicalType();
    canonicalType.removeLocalRestrict();
    return canonicalType;
}

#endif
