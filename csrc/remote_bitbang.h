#ifndef REMOTE_BITBANG_H
#define REMOTE_BITBANG_H

#include <stdint.h>
#include <sys/types.h>

class remote_bitbang_t
{
public:
  // Create a new server, listening for connections from localhost on the given
  // port.
  remote_bitbang_t(uint16_t port);

  // Do a bit of work.
  void tick(bool * jtag_tck,
            bool * jtag_tms,
            bool * jtag_tdi,
            bool * jtag_trstn,
            bool jtag_tdo);

  bool done() {return quit;}
  
  int exit_code() {return err;}
  
 private:

  int err;
  
  bool tck;
  bool tms;
  bool tdi;
  bool trstn;
  bool tdo;
  bool quit;
    
  int socket_fd;
  int client_fd;

  static const ssize_t buf_size = 64 * 1024;
  char recv_buf[buf_size];
  ssize_t recv_start, recv_end;

  // Check for a client connecting, and accept if there is one.
  void accept();
  // Execute any commands the client has for us.
  // But we only execute 1 because we need time for the
  // simulation to run.
  void execute_command();

  // Reset. Currently does nothing.
  void reset();

  void set_pins(char _tck, char _tms, char _tdi);

};

#endif
