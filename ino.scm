(module
 agidel-plugin.ino
 *
 (import (rename (prefix scheme -) (-quote quote))
         (prefix chicken.base -)
         (prefix chicken.string -)
         (prefix matchable -)
         (only chicken.syntax expand)
         (prefix srfi-13 -)
         format))
