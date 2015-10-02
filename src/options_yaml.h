#ifndef SMOKEGEN_OPTIONS_YAML_H
#define SMOKEGEN_OPTIONS_YAML_H

#include <llvm/Support/YAMLTraits.h>
#include "options.h"

LLVM_YAML_IS_SEQUENCE_VECTOR(std::regex);

namespace llvm {
namespace yaml {
template <>
struct SequenceTraits<std::vector<std::string> > {
    static size_t size(IO &io, std::vector<std::string> &vec) {
        return vec.size();
    }

    static std::string &element(IO &io, std::vector<std::string> &vec, size_t index) {
        // We never write out this yaml file, and we want the types specified
        // in the yaml file to append to our vector, not overwrite elements.
        vec.resize(vec.size() + 1);
        return vec[vec.size() - 1];
    }
};

template <>
struct ScalarTraits<std::regex> {
    static void output(const std::regex &value, void*, llvm::raw_ostream &out) {
    }

    static llvm::StringRef input(llvm::StringRef scalar, void*, std::regex &value) {
        try {
            value = std::regex(scalar.str());
        }
        catch (std::regex_error &e) {
            return llvm::StringRef("Error compiling regex");
        }
        return llvm::StringRef();
    }

    static bool mustQuote(llvm::StringRef) { return true; }
};

template <>
struct MappingTraits<Options> {
    static void mapping(IO &io, Options &options) {
        io.mapOptional("parts", options.parts);
        io.mapRequired("moduleName", options.module);
        io.mapOptional("parentModules", options.parentModules);
        io.mapOptional("scalarTypes", options.scalarTypes);
        io.mapOptional("voidpTypes", options.voidpTypes);
        io.mapOptional("headerList", options.headerList);
        io.mapRequired("classList", options.classList);
        io.mapOptional("qtMode", options.qtMode);

        io.mapOptional("exclude", options.excludeExpressions);
        io.mapOptional("functions", options.includeFunctions);
    }
};

template <>
struct MappingTraits<FunctionMatchers> {
    static void mapping(IO &io, FunctionMatchers &fms) {
        io.mapOptional("names", fms.names);
        io.mapOptional("signatures", fms.signatures);
    }
};

};
};

#endif
