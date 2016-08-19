// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

class TLEdgeOut(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
  extends TLEdgeParameters(client, manager)
{
  // Transfers
  def Acquire(x: Int) = () // A
  def Release(x: Int) = () // C
  def ReleaseData(x: Int) = () // C
  def ProbeAck(x: Int) = () // C
  def ProbeDataAck(x: Int) = () // C
  def GrantAck(x: Int) = () // E

  // Accessors
  def Get(x: Int) = () // A
  def Put(x: Int) = () // A
  def Atomic(x: Int) = () // A
  def AccessAck(x: Int) = () // C
  def AccessDataAck(x: Int) = () // C
  
  def Hint(x: Int) = () // A
  def HintAck(x: Int) = () // C
}

class TLEdgeIn(
  client:  TLClientPortParameters,
  manager: TLManagerPortParameters)
  extends TLEdgeParameters(client, manager)
{
  // Transfers
  def Probe(x: Int) = () // B
  def Grant(x: Int) = () // D
  def GrantData(x: Int) = () // D
  def ReleaseAck(x: Int) = () // D

  // Accessors
  def Get(x: Int) = () // B
  def Put(x: Int) = () // B
  def Atomic(x: Int) = () // B
  def AccessAck(x: Int) = () // D
  def AccessDataAck(x: Int) = () // D

  def Hint(x: Int) = () // B
  def HintAck(x: Int) = () // D
}
