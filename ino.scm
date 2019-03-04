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

 (-define (analog-read-resolution bits)
          (scln (format "analogReadResolution(~A)" bits)))

 (-define (analog-write-resolution bits)
          (scln (format "analogWriteResolution(~A)" bits)))

 (-define (no-tone pin)
          (scln (format "noTone(~A)" pin)))

 (-define pulse-in
          (-match-lambda*
           ((pin 'high) (scln (format "pulseIn(~A, ~A)" pin "HIGH")))
           ((pin 'low) (scln (format "pulseIn(~A, ~A)" pin "LOW")))
           ((pin 'high timeout)
            (scln (format "pulseIn(~A, ~A, ~A)" pin "HIGH" timeout)))
           ((pin 'low timeout)
            (scln (format "pulseIn(~A, ~A, ~A)" pin "LOW" timeout)))))

 (-define pulse-in-long
          (-match-lambda*
           ((pin 'high) (scln (format "pulseInLong(~A, HIGH)" pin)))
           ((pin 'low) (scln (format "pulseInLong(~A, LOW)" pin)))
           ((pin 'high timeout)
            (scln (format "pulseInLong(~A, HIGH, ~A)" pin timeout)))
           ((pin 'low timeout)
            (scln (format "pulseInLong(~A, LOW, ~A)" pin timeout)))))

 (-define shift-in
          (-match-lambda*
           ((data clock 'msb-first)
            (scln (format "shiftIn(~A, ~A, MSBFIRST)" data clock)))
           ((data clock 'lsb-first)
            (scln (format "shiftIn(~A, ~A, LSBFIRST)" data clock)))))

 (-define shift-out
          (-match-lambda*
           ((data clock 'msb-first value)
            (scln (format "shiftOut(~A, ~A, MSBFIRST, ~A)" data clock value)))
           ((data clock 'lsb-first value)
            (scln (format "shiftOut(~A, ~A, LSBFIRST, ~A)" data clock value)))))

 (-define tone
          (-match-lambda*
           ((pin frequency) (scln (format "tone(~A, ~A)" pin frequency)))
           ((pin frequency duration)
            (scln (format "tone(~A, ~A, ~A)") pin frequency duration))))

 )
