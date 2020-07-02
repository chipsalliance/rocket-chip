// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import firrtl.options.StageMain
import freechips.rocketchip.system.RocketChipStage

object Generator extends StageMain(new RocketChipStage)
