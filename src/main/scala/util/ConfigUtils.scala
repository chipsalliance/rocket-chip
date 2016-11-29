// See LICENSE.Berkeley for license details.

package util

import scala.math.max

object ConfigUtils {
  def max_int(values: Int*): Int = {
    values.reduce((a, b) => max(a, b))
  }
}
