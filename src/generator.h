#ifndef SMOKEGEN_GENERATOR
#define SMOKEGEN_GENERATOR

#include <map>
#include <string>
#include <set>

#include <clang/AST/AST.h>

class SmokeGenerator {
public:
    SmokeGenerator() {};

    void addClass(clang::CXXRecordDecl *D);

    std::string mungedName(clang::FunctionDecl *D) const;
    char munge(clang::QualType T) const;

    // Data file methods
    std::string getClassesCode() const;
    std::string getDataFileCode() const;
    std::string getMethodNamesCode() const;
    std::string getXCallName(clang::CXXRecordDecl *D) const;

private:
    std::map<std::string, clang::CXXRecordDecl *> classes;
    std::set<std::string> methodNames = {""};

};

#endif
