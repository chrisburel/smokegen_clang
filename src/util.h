#ifndef GENERATOR_UTIL_H
#define GENERATOR_UTIL_H

#include <clang/AST/DeclTemplate.h>

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
    while(dereferenced->isPointerType()) {
        dereferenced = dereferenced->getPointeeType();
    }
    if (auto refType = dereferenced->getAs<clang::ReferenceType>()) {
        dereferenced = refType->getPointeeType();
    }
    return dereferenced;
}

std::string getFullFunctionPrototype(clang::FunctionDecl *d, const clang::PrintingPolicy &policy) {
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

    return name;
}

#endif
