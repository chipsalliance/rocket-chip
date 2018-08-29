// See LICENSE.Berkeley for license details.

//*****************************************************
// Christopher Celio
// 2015 Feb 2
// 
// Utility for taking a raw commit log from a processor and post-processing it
// into a diff-able format against the spike ISA simulator's commit log.
//
// INPUT : a raw commit log via std::cin
// OUTPUT: a cleaned up commit log via std::cout
//
// PROBLEM: some writebacks can occur after the commit point in a processor.
// These partial entries will be marked as appropriate, and the writebacks will
// be written to the log as soon as they occur (possibly out-of-order). This
// program will put the writebacks in their proper place.
//
// FUNFACT: with processors that use renaming to break WAW hazards, it is
// possible for writes to the same ISA register to also occur OoO. Thus, the
// raw commit log must tell give us the physical destination (pdst)
// indentifiers for matching write-backs to the appropriate partial commit
// entry.

/*
  // Typical (raw) commit log...
  

  -----------------------------------------------------------

  0 0x0000000000002ccc (0x00973423)
  0 0x0000000000002cd0 (0x00a73023)
  0 0x0000000000002cd4 (0x05070113) x2 0x0000000000025180
  0 0x0000000000002cd8 (0xd8070713) x14 0x0000000000024eb0
  0 0x0000000000002cdc (0xea5ff0ef) x1 0x0000000000002ce0
  0 0x0000000000002cdc (0xea5ff0ef) x1 0x0000000000002ce0
  0 0x000000000000208c (0x00b6b72f) x14 p 1 0xXXXXXXXXXXXXXXXX
  0 0x0000000000002090 (0x80000eb7) x29 0xffffffff80000000
  x14 p 1 0xffffffff80000000
  0 0x0000000000002cdc (0xea5ff0ef) x1 0x0000000000002ce0

  // Note: the first number is the current privileged mode of the committed
  // instruction.
  //
  -----------------------------------------------------------

   The "x14 p 1 0xXXXXXXXXXXXXXXXX" segment needs to be replaced with the later
   "x14 0xffffffff80000000" segment.

   The physical tags must be used for matching writebacks to partial commits,
   as it is possible with renaming to break the write-after-write hazard and
   have out-of-order writebacks to the same ISA/logical register!  However, the
   physical tag needs to be deleted in the final output, as the ISA simulator
   does not know or care about renamed registers.

  // Final (cleaned up) commit log

  -----------------------------------------------------------

  0 0x0000000000002ccc (0x00973423)
  0 0x0000000000002cd0 (0x00a73023)
  0 0x0000000000002cd4 (0x05070113) x2 0x0000000000025180
  0 0x0000000000002cd8 (0xd8070713) x14 0x0000000000024eb0
  0 0x0000000000002cdc (0xea5ff0ef) x1 0x0000000000002ce0
  0 0x0000000000002cdc (0xea5ff0ef) x1 0x0000000000002ce0
  0 0x000000000000208c (0x00b6b72f) x14 0xffffffff80000000
  0 0x0000000000002090 (0x80000eb7) x29 0xffffffff80000000
  0 0x0000000000002cdc (0xea5ff0ef) x1 0x0000000000002ce0

  -----------------------------------------------------------
*/

#include <stdio.h>
#include <iostream>
#include <array>
#include <queue>
#include <assert.h>
#include <vector>


// Maximum number of physical destination registers, 64 for Rocket
const int kMaxPdst = 64;

// data-structures

typedef struct RobEntry
{
   bool        ready;               // is entry ready to be committed?
   int         pdst;                // the wb physical dest. register
   std::string str;                 // the commit string to print out
} RobEntry;

std::deque <RobEntry> rob;


// maps from physical destination register to rob entry waiting on it
//   a value of nullptr implies there is no rob entry waiting on pdst
std::vector<RobEntry*> pdst_to_rob(kMaxPdst, nullptr);


// functions

void push               (std::string& line);
void commit             ();
void writeback          (std::string& line);
bool is_instruction     (std::string& line);
bool is_partial_commit  (std::string& line);
int  get_ldst           (std::string& line);
int  get_pdst           (std::string& line);

// add instruction to the ROB
// mark as "not ready" if writeback data not ready
void push (std::string& line)
{
   bool is_partial = is_partial_commit(line);

   RobEntry rob_entry;
   rob_entry.str   = line;
   rob_entry.ready = !(is_partial);
   rob_entry.pdst  = is_partial ? get_pdst(line) : 0;
   rob.push_back(rob_entry);

   if (is_partial)
   {
      assert (rob_entry.pdst < kMaxPdst);
      assert (pdst_to_rob[rob_entry.pdst] == nullptr);
      pdst_to_rob[rob_entry.pdst] = &(rob.back());
   }
}

void commit ()
{
   while (!rob.empty() && rob.front().ready)
   {
      std::cout << rob.front().str << std::endl;
      rob.pop_front();
   }
}

int get_ldst (std::string& line)
{
   assert (line.length() > 46);

   int idx = line.find_first_of('x');
   int dst = atoi(line.substr(idx+1,2).c_str());
   return dst;
}

int get_pdst (std::string& line)
{
   assert (line.length() > 46);

   int idx = line.find_first_of('p');
   int dst = atoi(line.substr(idx+1,2).c_str());
   return dst;
}

bool is_partial_commit (std::string& line)
{
   if (line.length() > 46 && (line.at(34) == 'x' || line.at(34) == 'f') && line.at(46) == 'X')
      return true;
   else
      return false;
}

bool is_instruction (std::string& line)
{
   return !(line.at(0) == 'x' || line.at(0) == 'f');
}

// find instruction in ROB and substitute in the writeback data
// and mark it as ready for commit
void writeback (std::string& line)
{
   assert (line.at(0) == 'x' || line.at(0) == 'f');

   size_t idx = line.find_first_of('p');
   assert (idx != std::string::npos);
   int pdst = atoi(line.substr(idx+1,2).c_str());

   // search the partial queue for writeback
   assert (pdst < kMaxPdst);
   RobEntry* rob_entry = pdst_to_rob[pdst];
   assert (rob_entry != nullptr);
   pdst_to_rob[pdst] = nullptr;

   // update ROB
   assert (rob_entry->str.length() > 32);
   std::string* rob_str = &(rob_entry->str);

   // mark as ready
   rob_entry->ready = true;
   idx = line.find("0x");
   std::string wbdata = line.substr(idx+2,16);
   idx = rob_str->find("0x",32); // actually want to find the 3rd occurrence
   int p_idx = rob_str->find_first_of('p');
   rob_str->replace(idx+2,16,wbdata);
   rob_str->erase(p_idx, (idx-p_idx));
}

int main (int argc, char** argv)
{
   std::string line;

   // check for errno
   while (getline(std::cin, line))
   {
      if (is_instruction(line))
      {
         push(line);
      }
      else
      {
         writeback(line);
      }

      // check if head of the rob is ready, commit
      // instructions until either empty or not ready
      commit();
   }

   if (std::cin.bad())
   {
      // IO error
      std::cout << "\nIO ERROR: cin.bad()\n\n";
      return 1;
   }

   return 0;
}

