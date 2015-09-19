#ifndef SMOKEGEN_GENERATOR
#define SMOKEGEN_GENERATOR

#include <map>
#include <set>
#include <string>

#include <clang/AST/DeclCXX.h>
#include <llvm/Support/raw_ostream.h>

#include "options.h"

class SmokeGenerator {
public:
    SmokeGenerator(Options *options) : options(options) {};

    // Registers a record decl as a class known to the generator.  All
    // declarations are passed in, even forward declarations.  This allows the
    // generator to handle external classes.  When writing the class list,
    // items are checked to see if they exist in the smokeconfig file, as
    // specified in options.classList.
    void addClass(clang::CXXRecordDecl *D);

    void processDataStructures();

    void writeDataFile(llvm::raw_ostream &out);

private:
    std::set<const clang::CXXRecordDecl *> superClassList(const clang::CXXRecordDecl *klass) const;
    std::set<const clang::CXXRecordDecl *> descendantsList(const clang::CXXRecordDecl *klass) const;

    Options *options;

    // All classes found while reading the header files.
    std::map<std::string, clang::CXXRecordDecl *> classes;
};

#endif
