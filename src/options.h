#ifndef SMOKEGEN_OPTIONS_H
#define SMOKEGEN_OPTIONS_H

#include <vector>
#include <string>
#include <regex>

struct Options {
    std::string outputDir;
    int parts;
    std::string module;
    std::vector<std::string> parentModules;
    std::string libDir;
    std::vector<std::string> scalarTypes;
    std::vector<std::string> voidpTypes;
    std::vector<std::string> headerList;
    std::vector<std::string> classList;
    bool qtMode;

    std::vector<std::regex> excludeExpressions;
    std::vector<std::regex> includeFunctionNames;
};

#endif
