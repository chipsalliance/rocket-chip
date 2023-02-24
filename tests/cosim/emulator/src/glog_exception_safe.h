#pragma once

#include <glog/logging.h>

namespace google {

  class CheckFailedException : public std::runtime_error {
  public:
    explicit CheckFailedException() : std::runtime_error("check failed") {}
  };

  class LogMessageFatal_S : public LogMessage {
  public:
    LogMessageFatal_S(const char *file, int line) : LogMessage(file, line, GLOG_ERROR){};
    LogMessageFatal_S(const char *file, int line, const CheckOpString &result) : LogMessage(file, line, GLOG_ERROR) {
      stream() << "Check failed: " << (*result.str_) << " ";
    };
    ~LogMessageFatal_S() noexcept(false) {
      Flush();
      throw CheckFailedException();
    };
  };
}// namespace google

#define CHECK_OP_S(name, op, val1, val2) \
  CHECK_OP_LOG(name, op, val1, val2, google::LogMessageFatal_S)

#define COMPACT_GOOGLE_LOG_FATAL_S google::LogMessageFatal_S(__FILE__, __LINE__)

#define CHECK_EQ_S(val1, val2) CHECK_OP_S(_EQ, ==, val1, val2)
#define CHECK_NE_S(val1, val2) CHECK_OP_S(_NE, !=, val1, val2)
#define CHECK_LE_S(val1, val2) CHECK_OP_S(_LE, <=, val1, val2)
#define CHECK_LT_S(val1, val2) CHECK_OP_S(_LT, <, val1, val2)
#define CHECK_GE_S(val1, val2) CHECK_OP_S(_GE, >=, val1, val2)
#define CHECK_GT_S(val1, val2) CHECK_OP_S(_GT, >, val1, val2)
