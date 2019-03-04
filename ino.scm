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

 (-define (fun name . args)
          (-let ((~A-s (-substring
                        (-let loop ((cnt (-length args))
                                    (acc ""))
                              (-if (-zero? cnt)
                                   acc
                                   (loop (-- cnt 1)
                                         (-string-append acc "~A, "))))
                        0
                        (-- (-* 4 (-length args)) 2))))
                (scln (-apply format (-string-append name "(" ~A-s ")")
                              args))))

 (-define (digital-read pin) (fun "digitalRead" pin))

 (-define digital-write
          (-match-lambda*
           ((pin 'high) (fun "digitalWrite" pin "HIGH"))
           ((pin 'low) (fun "digitalWrite" pin "LOW"))))

 (-define pin-mode
          (-match-lambda*
           ((pin 'output) (fun "pinMode" pin "OUTPUT"))
           ((pin 'input) (fun "pinMode" pin "INPUT"))
           ((pin 'pullup) (fun "pinMode" pin "INPUT_PULLUP"))))

 (-define (analog-read pin) (fun "analogRead" pin))

 (-define (analog-reference type)
          (fun "analogReference"
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
                       ('ar-default      "AR_DEFAULT"))))

 (-define (analog-write pin value) (fun "analogWrite" pin value))
 (-define (analog-read-resolution bits) (fun "analogReadResolution" bits))
 (-define (analog-write-resolution bits) (fun "analogWriteResolution" bits))
 (-define (no-tone pin) (fun "noTone" pin))

 (-define pulse-in
          (-match-lambda*
           ((pin 'high) (fun "pulseIn" pin "HIGH"))
           ((pin 'low) (fun "pulseIn" pin "LOW"))
           ((pin 'high timeout) (fun "pulseIn" pin "HIGH" timeout))
           ((pin 'low timeout) (fun "pulseIn" pin "LOW" timeout))))

 (-define pulse-in-long
          (-match-lambda*
           ((pin 'high) (fun "pulseInLong" pin "HIGH"))
           ((pin 'low) (fun "pulseInLong" pin "LOW"))
           ((pin 'high timeout) (fun "pulseInLong" pin "HIGH" timeout))
           ((pin 'low timeout) (fun "pulseInLong" pin "LOW" timeout))))

 (-define shift-in
          (-match-lambda*
           ((data clock 'msb-first) (fun "shiftIn" data clock "MSBFIRST"))
           ((data clock 'lsb-first) (fun "shiftIn" data clock "LSBFIRST"))))

 (-define shift-out
          (-match-lambda*
           ((data clock 'msb-first value)
            (fun "shiftOut" data clock "MSBFIRST" value))
           ((data clock 'lsb-first value)
            (fun "shiftOut" data clock "LSBFIRST" value))))

 (-define tone
          (-match-lambda*
           ((pin frequency) (fun "tone" pin frequency))
           ((pin frequency duration) (fun "tone" pin frequency duration))))

 (-define (delay ms) (fun "delay" ms))
 (-define (delay-microseconds us) (fun "delayMicroseconds" us))
 (-define (micros) (fun "micros"))
 (-define (millis) (fun "millis"))

 (-define (abs x) (fun "abs" x))
 (-define (constrain x a b) (fun "constrain" x a b))
 (-define (map value from-low from-high to-low to-high)
          (fun "map" value from-low from-high to-low to-high))
 (-define (max x y) (fun "max" x y))
 (-define (min x y) (fun "min" x y))
 (-define (pow base exponent) (fun "pow" base exponent))
 (-define (sq x) (fun "sq" x))
 (-define (sqrt x) (fun "sqrt" x))

 (-define (cos rad) (fun "cos" rad))
 (-define (sin rad) (fun "sin" rad))
 (-define (tan rad) (fun "tan" rad))

 (-define (is-alpha ch) (fun "isAlpha" ch))
 (-define (is-alpha-numeric ch) (fun "isAlphaNumeric" ch))
 (-define (is-ascii ch) (fun "isAscii" ch))
 (-define (is-control ch) (fun "isControl" ch))
 (-define (is-digit ch) (fun "isDigit" ch))
 (-define (is-graph ch) (fun "isGraph" ch))
 (-define (is-hexadecimal-digit ch) (fun "isHexadecimalDigit" ch))
 (-define (is-lower-case ch) (fun "isLowerCase" ch))
 (-define (is-printable ch) (fun "isPrintable" ch))
 (-define (is-punct ch) (fun "isPunct" ch))
 (-define (is-space ch) (fun "isSpace" ch))
 (-define (is-upper-case ch) (fun "isUpperCase" ch))
 (-define (is-whitespace ch) (fun "isWhitespace" ch))

 )
