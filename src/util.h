#ifndef GENERATOR_UTIL_H
#define GENERATOR_UTIL_H

template <class T>
bool contains(const std::vector<T> &vec, const T &value)
{
    return std::find(vec.begin(), vec.end(), value) != vec.end();
}

#endif
