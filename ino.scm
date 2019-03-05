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
           ((pin 'high) (digital-write pin "HIGH"))
           ((pin 'low) (digital-write pin "LOW"))
           ((pin val) (fun "digitalWrite" pin val))))

 (-define pin-mode
          (-match-lambda*
           ((pin 'output) (pin-mode "OUTPUT"))
           ((pin 'input) (pin-mode "INPUT"))
           ((pin 'pullup) (pin-mode "INPUT_PULLUP"))
           ((pin mode) (fun "pinMode" pin mode))))

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
           ((pin 'high) (pulse-in pin "HIGH"))
           ((pin 'low) (pulse-in pin "LOW"))
           ((pin val) (fun "pulseIn" pin val))
           ((pin 'high timeout) (pulse-in pin "HIGH" timeout))
           ((pin 'low timeout) (pulse-in pin "LOW" timeout))
           ((pin val timeout) (fun "pulseIn" pin val timeout))))

 (-define pulse-in-long
          (-match-lambda*
           ((pin 'high) (fun "pulseInLong" pin "HIGH"))
           ((pin 'low) (fun "pulseInLong" pin "LOW"))
           ((pin 'high timeout) (fun "pulseInLong" pin "HIGH" timeout))
           ((pin 'low timeout) (fun "pulseInLong" pin "LOW" timeout))))

 (-define shift-in
          (-match-lambda*
           ((data clock 'msb-first) (shift-in data clock "MSBFIRST"))
           ((data clock 'lsb-first) (shift-in data clock "LSBFIRST"))
           ((data clock order) (fun "shiftIn" data clock order))))

 (-define shift-out
          (-match-lambda*
           ((data clock 'msb-first val) (shift-out data clock "MSBFIRST" val))
           ((data clock 'lsb-first val) (shift-out data clock "LSBFIRST" val))
           ((data clock order val) (fun "shiftOut" data clock order val))))

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

 (-define random
          (-match-lambda*
           ((max) (fun "random" max))
           ((min max) (fun "random" min max))))
 (-define (random-seed seed) (fun "randomSeed" seed))

 (-define (bit n) (fun "bit" n))
 (-define (bit-clear x n) (fun "bitClear" x n))
 (-define (bit-read x n) (fun "bitRead" x n))
 (-define (bit-set x n) (fun "bitSet" x n))
 (-define (bit-write x n b) (fun "bitWrite" x n b))
 (-define (high-byte x) (fun "highByte" x))
 (-define (low-byte x) (fun "lowByte" x))

 (-define (attach-interrupt pin isr mode)
          (fun "attachInterrupt"
               (format "digitalPinToInterrupt(~A)" pin)
               isr
               (-match mode
                       ('low     "LOW")
                       ('change  "CHANGE")
                       ('rising  "RISING")
                       ('falling "FALLING")
                       ('high    "HIGH"))))
 (-define (detach-interrupt pin)
          (fun "attachInterrupt" (format "digitalPinToInterrupt(~A)" pin)))
 (-define (interrupts) (fun "interrupts"))
 (-define (no-interrupts) (fun "noInterrupts"))
 )
