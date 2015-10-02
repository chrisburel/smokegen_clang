#ifndef SMOKEGEN_OPTIONS_H
#define SMOKEGEN_OPTIONS_H

#include <vector>
#include <string>
#include <regex>

struct FunctionMatchers {
    std::vector<std::regex> names;
    std::vector<std::regex> signatures;
};

struct Options {
    std::string outputDir;
    int parts;
    std::string module;
    std::vector<std::string> parentModules;
    std::string libDir;
    std::vector<std::string> scalarTypes = {"long long", "long long int", "unsigned long long", "unsigned long long int"};
    std::vector<std::string> voidpTypes = {"long long", "long long int", "unsigned long long", "unsigned long long int"};
    std::vector<std::string> headerList;
    std::vector<std::string> classList;
    bool qtMode;

    std::vector<std::regex> excludeExpressions;
    FunctionMatchers includeFunctions;

    bool typeExcluded(const std::string &name) const {
        for (const auto & regex : excludeExpressions) {
            if (std::regex_match(name, regex))
                return true;
        }
        return false;
    }

    bool functionNameIncluded(const std::string &name) const {
        for (const auto & regex : includeFunctions.names) {
            if (std::regex_match(name, regex))
                return true;
        }
        return false;
    }

    bool functionSignatureIncluded(const std::string &name) const {
        for (const auto & regex : includeFunctions.signatures) {
            if (std::regex_match(name, regex))
                return true;
        }
        return false;
    }
};

#endif
