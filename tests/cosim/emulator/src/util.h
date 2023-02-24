#pragma once

#include <cstdint>

/// @return: binary[a, b]
inline uint64_t clip(uint64_t binary, int a, int b) { return (binary >> a) & ((1 << (b - a + 1)) - 1); }

/// back-port of `std::erase_if` in C++ 20.
/// refer to https://en.cppreference.com/w/cpp/container/map/erase_if
template<class Key, class T, class Compare, class Alloc, class Pred>
typename std::map<Key, T, Compare, Alloc>::size_type
erase_if(std::map<Key, T, Compare, Alloc> &c, Pred pred) {
  auto old_size = c.size();
  for (auto i = c.begin(), last = c.end(); i != last;) {
    if (pred(*i)) {
      i = c.erase(i);
    } else {
      ++i;
    }
  }
  return old_size - c.size();
};