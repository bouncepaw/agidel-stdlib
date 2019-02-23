(module
 agidel-plugin.c
 *
 (import (prefix scheme -)
         (prefix chicken.base -)
         (prefix chicken.string -)
         (prefix srfi-13 -)
         format)

 (-define (eval-maybe o)
          (-if (-list? o) (-eval o) o))
 (-define-syntax
  str
  (syntax-rules ()
    ((_ elt) (-cond
              ((-string? 'elt) (-string-append "\"" 'elt "\""))
              ((-list? 'elt) (-map -->string 'elt))
              (else (-->string 'elt))))
    ((_ elt elt* ...) (str (-eval (-list elt elt* ...))))))

 (-define-syntax
  prefix->infix
  (syntax-rules ()
    ((_ operator operand operand* ...)
     (format
      "(~A)"
      (-let* ((operands (-list 'operand 'operand* ...))
              (evalλ    (-lambda (o)
                                 (-cond
                                  ((-list? o) (-eval o))
                                  ((-string? o) (-string-append "\"" o "\""))
                                  (else (-->string o)))))
              (evaled   (-map evalλ operands)))
             (-string-join evaled 'operator 'infix))))))
 (-define-syntax
  +
  (syntax-rules ()
    ((_ o) (format "+~A" 'o))
    ((_ o o* ...) (prefix->infix " + " o o* ...))))

 (-define-syntax
  -
  (syntax-rules ()
    ((_ o) (format "-(~A)" o))
    ((_ o o* ...) (prefix->infix " - " o o* ...))))

 (-define-syntax
  /
  (syntax-rules ()
    ((_ o) (format "(1/~A)" o))
    ((_ o o* ...) (prefix->infix " / " o o* ...))))

 (-define-syntax
  *
  (syntax-rules ()
    ((_ o) (format "*~A" o))
    ((_ o o* ...) (prefix->infix " * " o o* ...))))

 (-define-syntax
  %
  (syntax-rules ()
    ((_ o) (-number->string (-exact->inexact (-/ o 100))))
    ((_ o o* ...) (prefix->infix " % " o o* ...))))

 (-define-syntax
  bitor
  (syntax-rules ()
    ((_ o) (str o))
    ((_ o o* ...) (prefix->infix " | " o o* ...))))

 (-define-syntax
  bitand
  (syntax-rules ()
    ((_ o) (str o))
    ((_ o o* ...) (prefix->infix " & " o o* ...))))

 (-define-syntax
  xor
  (syntax-rules ()
    ((_ o) (str o))
    ((_ o o* ...) (prefix->infix " ^ " o o* ...))))

 (-define-syntax
  compl
  (syntax-rules ()
    ((_ o) (format "~~(~A)" (eval-maybe o)))))

 (-define-syntax
  left-shift
  (syntax-rules ()
    ((_ val sft) (format "~A << ~A"
                         (eval-maybe val)
                         (eval-maybe sft)))))

 (-define-syntax
  right-shift
  (syntax-rules ()
    ((_ val sft) (format "~A >> ~A"
                         (eval-maybe 'val)
                         (eval-maybe 'sft)))))

 (-define-syntax
  not
  (syntax-rules ()
    ((_ o) (format "~(~A)" (eval-maybe 'o)))))

 (-define-syntax
  and
  (syntax-rules ()
    ((_ o o* ...) (prefix->infix " && " o o* ...))))

 (-define-syntax
  or
  (syntax-rules ()
    ((_ o o* ...) (prefix->infix " || " o o* ...))))

 (-define-syntax
  import
  (syntax-rules ()
    ((_ o o* ...) (-apply
                   -string-append
                   (-map
                    (-lambda (f)
                             (-if (-string? f)
                                  (format "#include \"~A\"\n" f)
                                  (format "#include <~A>\n" f)))
                    (-list 'o 'o* ...))))))
 )
