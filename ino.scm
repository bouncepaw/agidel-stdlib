(module
 agidel-plugin.ino
 *
 (import (rename (prefix scheme -) (-quote quote))
         (prefix chicken.base -)
         (prefix chicken.string -)
         (prefix matchable -)
         (only chicken.syntax expand)
         (prefix srfi-13 -)
         format)
 (-define scln ;; SemiCoLoN maybe
          (-match-lambda*
           (() ";\n")
           ((str) (format "~A;\n" str))))

 (-define (digital-read pin)
          (scln (format "digitalRead(~A)" pin)))

 (-define digital-write
          (-match-lambda*
           ((pin 'high) (scln (format "digitalWrite(~A, HIGH)" pin)))
           ((pin 'low) (scln (format "digitalWrite(~A, LOW)" pin)))))

 (-define pin-mode
          (-match-lambda*
           ((pin 'output) (scln (format "pinMode(~A, OUTPUT)" pin)))
           ((pin 'input) (scln (format "pinMode(~A, INPUT)" pin)))
           ((pin 'pullup) (scln (format "pinMode(~A, INPUT_PULLUP)" pin)))))
 )
