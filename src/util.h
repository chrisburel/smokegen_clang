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
#endif
