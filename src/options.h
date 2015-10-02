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
};

#endif
