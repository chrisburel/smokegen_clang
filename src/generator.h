#ifndef SMOKEGEN_GENERATOR
#define SMOKEGEN_GENERATOR

#include <map>
#include <set>
#include <string>

#include <clang/AST/DeclCXX.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/TypeOrdering.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Sema/Sema.h>
#include <llvm/Support/raw_ostream.h>

#include "options.h"

class SmokeGenerator {
public:
    SmokeGenerator(clang::CompilerInstance &ci, Options *options) : ci(ci), options(options) {}

    void setASTContext(clang::ASTContext *ctx) { this->ctx = ctx; }

    // Registers a record decl as a class known to the generator.  All
    // declarations are passed in, even forward declarations.  This allows the
    // generator to handle external classes.  When writing the class list,
    // items are checked to see if they exist in the smokeconfig file, as
    // specified in options.classList.
    void addClass(clang::CXXRecordDecl *D);
    void addEnum(clang::EnumDecl *D);
    void addNamespace(clang::NamespaceDecl *D);
    void addFunction(clang::FunctionDecl *D);

    void processDataStructures();

    void writeDataFile(llvm::raw_ostream &out);

    clang::PrintingPolicy pp() const { return ci.getSema().getPrintingPolicy(); }

private:
    std::set<const clang::CXXRecordDecl *> superClassList(const clang::CXXRecordDecl *klass) const;
    std::set<const clang::CXXRecordDecl *> descendantsList(const clang::CXXRecordDecl *klass) const;

    bool canClassBeInstantiated(const clang::CXXRecordDecl *klass) const;
    bool canClassBeCopied(const clang::CXXRecordDecl *klass) const;
    bool hasClassVirtualDestructor(const clang::CXXRecordDecl *klass) const;

    bool hasTypeNonPublicParts(const clang::QualType &type) const;

    std::string getTypeFlags(clang::QualType t, int *classIdx) const;

    std::string mungedName(const clang::FunctionDecl *D) const;
    char munge(clang::QualType T) const;

    std::vector<const clang::CXXMethodDecl *> virtualMethodsForClass(const clang::CXXRecordDecl *klass) const;
    std::vector<const clang::CXXMethodDecl *> collectVirtualMethods(const clang::CXXRecordDecl *klass) const;
    const clang::CXXMethodDecl* isVirtualOverriden(const clang::CXXMethodDecl *meth, const clang::CXXRecordDecl *klass) const;

    bool isClassUsed(const clang::CXXRecordDecl* klass) const;

    void checkForAbstractClass(clang::CXXRecordDecl* klass) const;

    void insertTemplateParameters(const clang::QualType type);

    std::vector<clang::FunctionDecl*> addOverloads(clang::CXXMethodDecl* method) const;

    Options *options;

    // All classes, enums, and namespaces found while reading the header files.
    std::map<std::string, clang::CXXRecordDecl *> classes;
    std::map<std::string, clang::EnumDecl *> enums;
    std::map<std::string, clang::NamespaceDecl *> namespaces;
    // A list of functions that are not methods.
    std::map<std::string, std::vector<clang::FunctionDecl *> > functions;

    // Stores the class index of each class, as it appears in the classes list
    // in the data file.  Classes are added to it, and as a final step we
    // iterate over all the keys and set the appropriate index.  We get the
    // sorting for free because std::map.
    std::map<std::string, int> classIndex;

    // The index of a specific method in the methods array
    std::map<const clang::FunctionDecl *, int> methodIdx;
    std::map<const clang::EnumConstantDecl *, int> enumIdx;

    // The index of a specific type in the types array
    std::map<clang::QualType, int, clang::QualTypeOrdering> typeIndex;

    // Set of all types found.
    std::set<clang::QualType, clang::QualTypeOrdering> usedTypes;

    // A list of classes that do not appear in the classList, but are seen when
    // reading the header files
    std::set<const clang::NamedDecl *> externalClasses;

    // A list of classes that appear in the classList from the options, and
    // have actually been found when reading the header files.
    std::vector<std::string> includedClasses;

    // Necessary for the getPointerType method
    clang::ASTContext *ctx;

    // Stores the relationship between a manually created method for a field
    // accessor, and the field it accesses.
    std::map<const clang::CXXMethodDecl *, const clang::DeclaratorDecl *> fieldAccessors;

    std::map<clang::QualType, clang::QualType, clang::QualTypeOrdering> movedEnums;

    clang::CompilerInstance &ci;
};

#endif
