#pragma once

#include <stdexcept>

class CosimException : public std::runtime_error {
public:
  explicit CosimException(const char *what) : runtime_error(what) {}
};

class TimeoutException : CosimException {
public:
  TimeoutException() : CosimException("timeout") {}
};

#define CHECK_S(condition)                                       \
  LOG_IF(FATAL_S, GOOGLE_PREDICT_BRANCH_NOT_TAKEN(!(condition))) \
      << "Check failed: " #condition " "
