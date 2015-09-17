#ifndef SMOKEGEN_OPTIONS_YAML_H
#define SMOKEGEN_OPTIONS_YAML_H

#include <llvm/Support/YAMLTraits.h>
#include "options.h"

LLVM_YAML_IS_SEQUENCE_VECTOR(std::string);
LLVM_YAML_IS_SEQUENCE_VECTOR(std::regex);

namespace llvm {
namespace yaml {

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
        io.mapOptional("functions", options.includeFunctionNames);
    }
};
};
};

#endif
