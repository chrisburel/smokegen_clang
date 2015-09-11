#ifndef SMOKEGEN_GENERATOR
#define SMOKEGEN_GENERATOR

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
    std::string getDataFileCode() const;
    std::string getMethodNamesCode() const;

private:
    std::set<std::string> methodNames = {""};

};

#endif
