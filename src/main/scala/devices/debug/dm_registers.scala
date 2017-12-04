package freechips.rocketchip.devices.debug

import Chisel._

// This file was auto-generated from the repository at https://github.com/riscv/riscv-debug-spec.git,
// 'make chisel'

object DMI_RegAddrs {
  /* The address of this register will not change in the future, because it
        contains \Fversion.  It has changed from version 0.11 of this spec.

        This register reports status for the overall debug module
        as well as the currently selected harts, as defined in \Fhasel.

        Harts are nonexistent if they will never be part of this system, no
        matter how long a user waits. Eg. in a simple single-hart system only
        one hart exists, and all others are nonexistent.

        Harts are unavailable if they might exist/become available at a later
        time.  Eg. in a multi-hart system some might temporarily be powered
        down, or a system might support hot-swapping harts.
  */
  def DMI_DMSTATUS =  0x11

  /* This register controls the overall debug module
        as well as the currently selected harts, as defined in \Fhasel.
  */
  def DMI_DMCONTROL =  0x10

  /* This register gives information about the hart currently
      selected by \Fhartsel.

      This register is optional. If it is not present it should
      read all-zero.

      If this register is included, the debugger can do more with
      the Program Buffer by writing programs which
      explicitly access the {\tt data} and/or {\tt dscratch}
      registers.
  */
  def DMI_HARTINFO =  0x12

  /* This register contains a summary of which harts are halted.

        Each bit contains the logical OR of 32 halt bits. When there are a
        large number of harts in the system, the debugger can first read this
        register, and then read from the halt region (0x40--0x5f) to determine
        which hart is the one that is halted.
  */
  def DMI_HALTSUM =  0x13

  /* This register selects which of the 32-bit portion of the hart array mask register
      is accessible in \Rhawindow.

      The hart array mask register provides a mask of all harts controlled by
      the debug module. A hart is part of the currently selected harts if
      the corresponding bit is set in the hart array mask register and
      \Fhasel in \Rdmcontrol is 1, or if the hart is selected by \Fhartsel.
  */
  def DMI_HAWINDOWSEL =  0x14

  /* This register provides R/W access to a 32-bit portion of the
      hart array mask register.
      The position of the window is determined by \Rhawindowsel. I.e. bit 0
      refers to hart $\Rhawindowsel * 32$, while bit 31 refers to hart
      $\Rhawindowsel * 32 + 31$.
  */
  def DMI_HAWINDOW =  0x15

  def DMI_ABSTRACTCS =  0x16

  /* Writes to this register cause the corresponding abstract command to be
        executed.

        Writing while an abstract command is executing causes \Fcmderr to be set.

        If \Fcmderr is non-zero, writes to this register are ignored.

        \begin{commentary}
            \Fcmderr inhibits starting a new command to accommodate debuggers
            that, for performance reasons, send several commands to be executed
            in a row without checking \Fcmderr in between. They can safely do
            so and check \Fcmderr at the end without worrying that one command
            failed but then a later command (which might have depended on the
            previous one succeeding) passed.
        \end{commentary}
  */
  def DMI_COMMAND =  0x17

  /* This register is optional. Including it allows more efficient burst accesses.
      Debugger can attempt to set bits and read them back to determine if the functionality is supported.
  */
  def DMI_ABSTRACTAUTO =  0x18

  /* When {\tt devtreevalid} is set, reading this register returns bits 31:0
      of the Device Tree address. Reading the other {\tt devtreeaddr}
      registers returns the upper bits of the address.

      When system bus mastering is implemented, this must be an
      address that can be used with the System Bus Access module. Otherwise,
      this must be an address that can be used to access the
      Device Tree from the hart with ID 0.

      If {\tt devtreevalid} is 0, then the {\tt devtreeaddr} registers
      hold identifier information which is not
      further specified in this document.

      The Device Tree itself is described in the RISC-V Privileged
      Specification.
  */
  def DMI_DEVTREEADDR0 =  0x19

  def DMI_DEVTREEADDR1 =  0x1a

  def DMI_DEVTREEADDR2 =  0x1b

  def DMI_DEVTREEADDR3 =  0x1c

  /* Basic read/write registers that may be read or changed by abstract
        commands.

        Accessing them while an abstract command is executing causes \Fcmderr
        to be set.

        Attempts to write them while \Fbusy is set does not change their value.

        The values in these registers may not be preserved after an abstract
        command is executed. The only guarantees on their contents are the ones
        offered by the command in question. If the command fails, no
        assumptions can be made about the contents of these registers.
  */
  def DMI_DATA0 =  0x04

  def DMI_DATA11 =  0x0f

  /* The {\tt progbuf} registers provide read/write access to the optional
        program buffer.

        Accessing them while an abstract command is executing causes \Fcmderr
        to be set.

        Attempts to write them while \Fbusy is set does not change their value.
  */
  def DMI_PROGBUF0 =  0x20

  def DMI_PROGBUF15 =  0x2f

  /* This register serves as a 32-bit serial port to the authentication
        module.

        When \Fauthbusy is clear, the debugger can communicate with the
        authentication module by reading or writing this register. There is no
        separate mechanism to signal overflow/underflow.
  */
  def DMI_AUTHDATA =  0x30

  def DMI_SBCS =  0x38

  /* If \Fsbasize is 0, then this register is not present.

        When the system bus master is busy,
        writes to this register will set \Fsberror.

        If \Fsberror is 0 and \Fsbautoread is set then the system bus
        master will start
        to read after updating the address from \Faddress. The access size is
        controlled by \Fsbaccess in \Rsbcs.

        If \Fsbsingleread is set, the bit is cleared.
  */
  def DMI_SBADDRESS0 =  0x39

  def DMI_SBADDRESS1 =  0x3a

  /* If \Fsbasize is less than 65, then this register is not present.
  */
  def DMI_SBADDRESS2 =  0x3b

  /* If all of the {\tt sbaccess} bits in \Rsbcs are 0, then this register
        is not present.

        Any successful system bus read updates the data in this register, and
        marks it no longer stale.

        If \Fsberror isn't 0 then accesses do nothing.
        
        \begin{steps}{Writes to this register:}
            \item If the bus master is busy then accesses set \Fsberror, and
            don't do anything else.
            \item Start a bus write of {\tt sbdata} to {\tt sbaddress}.
            \item If \Fsbautoincrement is set, increment {\tt sbaddress}.
        \end{steps}

        \begin{steps}{Reads from this register:}
            \item If the register is marked stale, then set \Fsberror and don't
            do anything else.
            \item ``Return'' the data.
            \item Mark the register stale.
            \item If \Fsbautoincrement is set, increment {\tt sbaddress}.
            \item If \Fsbautoread is set, start another system bus read.
        \end{steps}

        Only \Rsbdatazero has this behavior. The other {\tt sbdata} registers
        have no side effects. On systems that have buses wider than 32 bits, a
        debugger should access \Rsbdatazero after accessing the other {\tt
        sbdata} registers.
  */
  def DMI_SBDATA0 =  0x3c

  /* If \Fsbaccesssixtyfour and \Fsbaccessonetwentyeight are 0, then this
        register is not present.
  */
  def DMI_SBDATA1 =  0x3d

  /* This register only exists if \Fsbaccessonetwentyeight is 1.
  */
  def DMI_SBDATA2 =  0x3e

  /* This register only exists if \Fsbaccessonetwentyeight is 1.
  */
  def DMI_SBDATA3 =  0x3f

}

class DMSTATUSFields extends Bundle {

  val reserved0 = UInt(5.W)

  /* Gets set if the Debug Module was accessed incorrectly.

            0 (none): No error.

            1 (badaddr): There was an access to an unimplemented Debug Module
            address.

            7 (other): An access failed for another reason.
  */
  val dmerr = UInt(3.W)

  val reserved1 = UInt(1.W)

  /* If 1, then there is an implicit {\tt ebreak} instruction at the
            non-existent word immediately after the Program Buffer. This saves
            the debugger from having to write the {\tt ebreak} itself, and
            allows the Program Buffer to be one word smaller.

            This must be 1 when \Fprogbufsize is 1.
  */
  val impebreak = Bool()

  val reserved2 = UInt(2.W)

  /* This field is 1 when all currently selected harts have been reset but the reset has not been acknowledged.
  */
  val allhavereset = Bool()

  /* This field is 1 when any currently selected hart has been reset but the reset has not been acknowledged.
  */
  val anyhavereset = Bool()

  /* This field is 1 when all currently selected harts have acknowledged
            the previous resume request.
  */
  val allresumeack = Bool()

  /* This field is 1 when any currently selected hart has acknowledged
            the previous resume request.
  */
  val anyresumeack = Bool()

  /* This field is 1 when all currently selected harts do not exist in this system.
  */
  val allnonexistent = Bool()

  /* This field is 1 when any currently selected hart does not exist in this system.
  */
  val anynonexistent = Bool()

  /* This field is 1 when all currently selected harts are unavailable.
  */
  val allunavail = Bool()

  /* This field is 1 when any currently selected hart is unavailable.
  */
  val anyunavail = Bool()

  /* This field is 1 when all currently selected harts are running.
  */
  val allrunning = Bool()

  /* This field is 1 when any currently selected hart is running.
  */
  val anyrunning = Bool()

  /* This field is 1 when all currently selected harts are halted.
  */
  val allhalted = Bool()

  /* This field is 1 when any currently selected hart is halted.
  */
  val anyhalted = Bool()

  /* 0 when authentication is required before using the DM.  1 when the
            authentication check has passed. On components that don't implement
            authentication, this bit must be preset as 1.
  */
  val authenticated = Bool()

  /* 0: The authentication module is ready to process the next
            read/write to \Rauthdata.

            1: The authentication module is busy. Accessing \Rauthdata results
            in unspecified behavior.

            \Fauthbusy only becomes set in immediate response to an access to
            \Rauthdata.
  */
  val authbusy = Bool()

  val reserved3 = UInt(1.W)

  /* 0: \Rdevtreeaddrzero--\Rdevtreeaddrthree hold information which
            is not relevant to the Device Tree.

            1: \Rdevtreeaddrzero--\Rdevtreeaddrthree registers hold the address of the
            Device Tree.
  */
  val devtreevalid = Bool()

  /* 0: There is no Debug Module present.

            1: There is a Debug Module and it conforms to version 0.11 of this
            specification.

            2: There is a Debug Module and it conforms to version 0.13 of this
            specification.

            15: There is a Debug Module but it does not conform to any
            available version of this spec.
  */
  val version = UInt(4.W)

}

class DMCONTROLFields extends Bundle {

  /* Writes the halt request bit for all currently selected harts.
            When set to 1, each selected hart will halt if it is not currently
            halted.

            Writing 1 or 0 has no effect on a hart which is already halted, but
            the bit must be cleared to 0 before the hart is resumed.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val haltreq = Bool()

  /* Writes the resume request bit for all currently selected harts.
            When set to 1, each selected hart will resume if it is currently
            halted.

            The resume request bit is ignored while the halt request bit is
            set.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val resumereq = Bool()

  /* This optional field writes the reset bit for all the currently
            selected harts.  To perform a reset the debugger writes 1, and then
            writes 0 to deassert the reset signal.

            If this feature is not implemented, the bit always stays 0, so
            after writing 1 the debugger can read the register back to see if
            the feature is supported.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val hartreset = Bool()

  /* Writing 1 to this bit clears the {\tt havereset} bits for
            any selected harts.

            Writes apply to the new value of \Fhartsel and \Fhasel.
  */
  val ackhavereset = Bool()

  val reserved0 = UInt(1.W)

  /* Selects the  definition of currently selected harts.

            0: There is a single currently selected hart, that selected by \Fhartsel.

            1: There may be multiple currently selected harts -- that selected by \Fhartsel,
               plus those selected by the hart array mask register.

            An implementation which does not implement the hart array mask register
            should tie this field to 0. A debugger which wishes to use the hart array
            mask register feature should set this bit and read back to see if the functionality
            is supported.
  */
  val hasel = Bool()

  /* The DM-specific index of the hart to select. This hart is always part of the
            currently selected harts.
  */
  val hartsel = UInt(10.W)

  val reserved1 = UInt(14.W)

  /* This bit controls the reset signal from the DM to the rest of the
            system. The signal should reset every part of the system, including
            every hart, except for the DM and any logic required to access the
            DM.
            To perform a system reset the debugger writes 1,
            and then writes 0
            to deassert the reset.
  */
  val ndmreset = Bool()

  /* This bit serves as a reset signal for the Debug Module itself.

            0: The module's state, including authentication mechanism,
            takes its reset values (the \Fdmactive bit is the only bit which can
            be written to something other than its reset value).

            1: The module functions normally.

            No other mechanism should exist that may result in resetting the
            Debug Module after power up, including the platform's system reset
            or Debug Transport reset signals.

            A debugger may pulse this bit low to get the debug module into a
            known state.

            Implementations may use this bit to aid debugging, for example by
            preventing the Debug Module from being power gated while debugging
            is active.
  */
  val dmactive = Bool()

}

class HARTINFOFields extends Bundle {

  val reserved0 = UInt(8.W)

  /* Number of {\tt dscratch} registers available for the debugger
            to use during program buffer execution, starting from \Rdscratchzero.
            The debugger can make no assumptions about the contents of these
            registers between commands.
  */
  val nscratch = UInt(4.W)

  val reserved1 = UInt(3.W)

  /* 0: The {\tt data} registers are shadowed in the hart by CSR
            registers. Each CSR register is XLEN bits in size, and corresponds
            to a single argument, per Table~\ref{tab:datareg}.

            1: The {\tt data} registers are shadowed in the hart's memory map.
            Each register takes up 4 bytes in the memory map.
  */
  val dataaccess = Bool()

  /* If \Fdataaccess is 0: Number of CSR registers dedicated to
            shadowing the {\tt data} registers.

            If \Fdataaccess is 1: Number of 32-bit words in the memory map
            dedicated to shadowing the {\tt data} registers.
  */
  val datasize = UInt(4.W)

  /* If \Fdataaccess is 0: The number of the first CSR dedicated to
            shadowing the {\tt data} registers.

            If \Fdataaccess is 1: Signed address of RAM where the {\tt data}
            registers are shadowed, to be used to access relative to \Rzero.
  */
  val dataaddr = UInt(12.W)

}

class HALTSUMFields extends Bundle {

  val halt1023_992 = Bool()

  val halt991_960 = Bool()

  val halt959_928 = Bool()

  val halt927_896 = Bool()

  val halt895_864 = Bool()

  val halt863_832 = Bool()

  val halt831_800 = Bool()

  val halt799_768 = Bool()

  val halt767_736 = Bool()

  val halt735_704 = Bool()

  val halt703_672 = Bool()

  val halt671_640 = Bool()

  val halt639_608 = Bool()

  val halt607_576 = Bool()

  val halt575_544 = Bool()

  val halt543_512 = Bool()

  val halt511_480 = Bool()

  val halt479_448 = Bool()

  val halt447_416 = Bool()

  val halt415_384 = Bool()

  val halt383_352 = Bool()

  val halt351_320 = Bool()

  val halt319_288 = Bool()

  val halt287_256 = Bool()

  val halt255_224 = Bool()

  val halt223_192 = Bool()

  val halt191_160 = Bool()

  val halt159_128 = Bool()

  val halt127_96 = Bool()

  val halt95_64 = Bool()

  val halt63_32 = Bool()

  val halt31_0 = Bool()

}

class HAWINDOWSELFields extends Bundle {

  val reserved0 = UInt(27.W)

  val hawindowsel = UInt(5.W)

}

class HAWINDOWFields extends Bundle {

  val maskdata = UInt(32.W)

}

class ABSTRACTCSFields extends Bundle {

  val reserved0 = UInt(3.W)

  /* Size of the Program Buffer, in 32-bit words. Valid sizes are 0 - 16.
  */
  val progbufsize = UInt(5.W)

  val reserved1 = UInt(11.W)

  /* 1: An abstract command is currently being executed.

            This bit is set as soon as \Rcommand is written, and is
            not cleared until that command has completed.
  */
  val busy = Bool()

  val reserved2 = UInt(1.W)

  /* Gets set if an abstract command fails. The bits in this field remain set until
            they are cleared by writing 1 to them. No abstract command is
            started until the value is reset to 0.

            0 (none): No error.

            1 (busy): An abstract command was executing while \Rcommand,
            \Rabstractcs, \Rabstractauto was written, or when one
            of the {\tt data} or {\tt progbuf} registers was read or written.

            2 (not supported): The requested command is not supported. A
            command that is not supported while the hart is running may be
            supported when it is halted.

            3 (exception): An exception occurred while executing the command
            (eg. while executing the Program Buffer).

            4 (halt/resume): An abstract command couldn't execute because the
            hart wasn't in the expected state (running/halted).

            7 (other): The command failed for another reason.
  */
  val cmderr = UInt(3.W)

  val reserved3 = UInt(3.W)

  /* Number of {\tt data} registers that are implemented as part of the
            abstract command interface. Valid sizes are 0 - 12.
  */
  val datacount = UInt(5.W)

}

class COMMANDFields extends Bundle {

  /* The type determines the overall functionality of this
            abstract command.
  */
  val cmdtype = UInt(8.W)

  /* This field is interpreted in a command-specific manner,
            described for each abstract command.
  */
  val control = UInt(24.W)

}

class ABSTRACTAUTOFields extends Bundle {

  /* When a bit in this field is 1, read or write accesses the corresponding {\tt progbuf} word
          cause the command in \Rcommand to be executed again.
  */
  val autoexecprogbuf = UInt(16.W)

  val reserved0 = UInt(4.W)

  /* When a bit in this field is 1, read or write accesses the corresponding {\tt data} word
          cause the command in \Rcommand to be executed again.
  */
  val autoexecdata = UInt(12.W)

}

class DEVTREEADDR0Fields extends Bundle {

  val addr = UInt(32.W)

}

class DATA0Fields extends Bundle {

  val data = UInt(32.W)

}

class PROGBUF0Fields extends Bundle {

  val data = UInt(32.W)

}

class AUTHDATAFields extends Bundle {

  val data = UInt(32.W)

}

class SBCSFields extends Bundle {

  val reserved0 = UInt(11.W)

  /* When a 1 is written here, triggers a read at the address in {\tt
            sbaddress} using the access size set by \Fsbaccess.
  */
  val sbsingleread = Bool()

  /* Select the access size to use for system bus accesses triggered by
            writes to the {\tt sbaddress} registers or \Rsbdatazero.

            0: 8-bit

            1: 16-bit

            2: 32-bit

            3: 64-bit

            4: 128-bit

            If an unsupported system bus access size is written here, the DM
            does not perform the access and sberror is set to 3.
  */
  val sbaccess = UInt(3.W)

  /* When 1, {\tt sbaddress} is incremented by the access size (in
            bytes) selected in \Fsbaccess after every system bus access.
  */
  val sbautoincrement = Bool()

  /* When 1, every read from \Rsbdatazero automatically triggers a
            system bus read at the (possibly auto-incremented) address.
  */
  val sbautoread = Bool()

  /* When the debug module's system bus
            master causes a bus error, this field gets set. The bits in this
            field remain set until they are cleared by writing 1 to them.
            While this field is non-zero, no more system bus accesses can be
            initiated by the debug module.

            0: There was no bus error.

            1: There was a timeout.

            2: A bad address was accessed.

            3: There was some other error (eg. alignment).

            4: The system bus master was busy when one of the
            {\tt sbaddress} or {\tt sbdata} registers was written,
            or \Rsbdatazero was read when it had stale data.
  */
  val sberror = UInt(3.W)

  /* Width of system bus addresses in bits. (0 indicates there is no bus
            access support.)
  */
  val sbasize = UInt(7.W)

  /* 1 when 128-bit system bus accesses are supported.
  */
  val sbaccess128 = Bool()

  /* 1 when 64-bit system bus accesses are supported.
  */
  val sbaccess64 = Bool()

  /* 1 when 32-bit system bus accesses are supported.
  */
  val sbaccess32 = Bool()

  /* 1 when 16-bit system bus accesses are supported.
  */
  val sbaccess16 = Bool()

  /* 1 when 8-bit system bus accesses are supported.
  */
  val sbaccess8 = Bool()

}

class SBADDRESS0Fields extends Bundle {

  /* Accesses bits 31:0 of the physical address in {\tt sbaddress}.
  */
  val address = UInt(32.W)

}

class SBADDRESS1Fields extends Bundle {

  /* Accesses bits 63:32 of the physical address in {\tt sbaddress} (if
            the system address bus is that wide).
  */
  val address = UInt(32.W)

}

class SBADDRESS2Fields extends Bundle {

  /* Accesses bits 95:64 of the physical address in {\tt sbaddress} (if
            the system address bus is that wide).
  */
  val address = UInt(32.W)

}

class SBDATA0Fields extends Bundle {

  /* Accesses bits 31:0 of {\tt sbdata}.
  */
  val data = UInt(32.W)

}

class SBDATA1Fields extends Bundle {

  /* Accesses bits 63:32 of {\tt sbdata} (if the system bus is that
            wide).
  */
  val data = UInt(32.W)

}

class SBDATA2Fields extends Bundle {

  /* Accesses bits 95:64 of {\tt sbdata} (if the system bus is that
            wide).
  */
  val data = UInt(32.W)

}

class SBDATA3Fields extends Bundle {

  /* Accesses bits 127:96 of {\tt sbdata} (if the system bus is that
            wide).
  */
  val data = UInt(32.W)

}

