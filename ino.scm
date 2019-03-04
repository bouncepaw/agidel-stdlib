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

 (-define (analog-read pin)
          (scln (format "analogRead(~A)" pin)))

 (-define (analog-reference type)
          (scln (format "analogReference(~A)"
                        (-match type
                                ('default         "DEFAULT")
                                ('internal        "INTERNAL")
                                ('internal1v1     "INTERNAL1V1")
                                ('internal2v56    "INTERVAL2V56")
                                ('external        "EXTERNAL")
                                ('ar-default      "AR_DEFAULT")
                                ('ar-internal     "AR_INTERNAL")
                                ('ar-internal1v0  "AR_INTERNAL1V0")
                                ('ar-internal1v65 "AR_INTERNAL1V65")
                                ('ar-interval2v23 "AR_INTERNAL2V23")
                                ('ar-external     "AR_EXTERNAL")
                                ('ar-default      "AR_DEFAULT")))))

 (-define (analog-write pin value)
          (scln (format "analogWrite(~A, ~A)" pin value)))

 )
