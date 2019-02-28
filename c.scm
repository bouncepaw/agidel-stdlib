(module
 agidel-plugin.c
 *
 (import (prefix scheme -)
         (prefix chicken.base -)
         (prefix chicken.string -)
         (only chicken.syntax expand)
         (prefix srfi-13 -)
         format)

 (-define-syntax
  newline
  (syntax-rules ()
    ((_) "\n")
    ((_ str) (format "~A\n" str))))

 (-define-syntax
  expand-maybe
  (syntax-rules ()
    ((_ o) (-if (-symbol? 'o) 'o (expand o)))))
 (-define (eval-maybe o)
          (-if (-list? o) (-eval o) o))

 (-define-syntax
  str
  (syntax-rules (!!)
    ((_ !! lst) (-map -->string lst))
    ((_ elt) (-cond
              ((-string? elt) (-string-append "\"" elt "\""))
              ((-list? elt) (-map -->string 'elt))
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
  join-with-space
  (syntax-rules ()
    ((_) "")
    ((_ o* ...) (-string-join (str !! (-map eval-maybe (-list 'o* ...)))
                              " " 'infix))))
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
    ((_ o) (format "!(~A)" (eval-maybe 'o)))))

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

 (-define-syntax
  binding-deconstruct
  (syntax-rules (=)
    ((_ binding) (-eval (-cons 'binding-deconstruct 'binding)))
    ((_ name type type* ... = val)
     (format "~A~A = ~A;\n"
             (-string-join (-map -symbol->string
                                 (-list 'type 'type* ...))
                           " " 'suffix)
             (-symbol->string 'name)
             val))
    ((_ name type type* ...)
     (format "~A~A;\n"
             (-string-join (-map -symbol->string
                                 (-list 'type 'type* ...))
                           " " 'suffix)
             (-symbol->string 'name)))))

 (-define-syntax
  defvar
  (syntax-rules (binding-deconstruct)
    ((_ binding) (binding-deconstruct binding))
    ((_ binding binding* ...)
     (-string-append (binding-deconstruct binding)
                     (defvar binding* ...)))))

 (-define-syntax
  disname+types
  (syntax-rules ()
    ((_ name+types)
     (format "~A~A"
             (-string-join (-map -symbol->string (-cdr name+types))
                           " "
                           'suffix)
             (-car name+types)))))

 (-define-syntax
  disarg
  (syntax-rules ()
    ((_ args) (-string-join
               (-map (-lambda (arg) (disname+types arg)) args)
               ", "
               'infix))))

 (-define-syntax
  defun
  (syntax-rules ()
    ((_ name+types args)
     (-let ((signature (disname+types 'name+types))
            (arguments (disarg 'args)))
           (format "~A (~A);\n" signature arguments)))
    ((_ name+types args expr ...)
     (-let ((signature (disname+types 'name+types))
            (arguments (disarg 'args)))
           (format "~A (~A) {\n~A}\n"
                   signature
                   arguments
                   (-string-append expr ...))))))

 (-define (semicolon-maybe)
          ";\n")
 (-define-syntax
  pragma
  (syntax-rules ()
    ((_ dir dir* ...)
     (-apply -string-append
             "#pragma "
             (-string-join (-map ->string (-list dir dir* ...))
                           " " 'infix)
             "\n"))))

 (-define-syntax
  return
  (syntax-rules ()
    ((_ o) (format "return ~A~A" (eval-maybe 'o) (semicolon-maybe)))))

 (-define-syntax
  _bracket
  (syntax-rules ()
    ((_ fun arg ...)
     (format "~A(~A)~A"
             'fun
             (-string-join (-map (-lambda (s)
                                          (str s))
                                 (-map eval-maybe
                                       (-list 'arg ...)))
                           ", "
                           'infix)
             (semicolon-maybe)))))

 (-define-syntax
  deftype
  (syntax-rules ()
    ((_ name type type* ...)
     (format "typedef ~A ~A;"
             (join-with-space type type* ...)
             'name))))

 (-define-syntax
  _brace
  (syntax-rules ()
    ((_ str) str)))

 (-define-syntax
  if
  (syntax-rules ()
    ((_ test thenc elsec)
     (format "if (~A) ~A else ~A"
             (expand-maybe 'test)
             thenc
             elsec))))

 (-define-syntax
  unless
  (syntax-rules ()
    ((_ test thenc elsec)
     (if test elsec thenc))))

 (-define-syntax
  if*
  (syntax-rules ()
    ((_ test thenc elsec)
     (format "~A ? ~A : ~A~A"
             (expand-maybe 'test)
             thenc
             elsec
             (semicolon-maybe)))))

 (-define-syntax
  unless*
  (syntax-rules ()
    ((_ test thenc elsec)
     (if* test elsec thenc))))

 (-define-syntax
  begin
  (syntax-rules ()
    ((_ expr ...)
     (format "{\n~A}\n" (string-append expr ...)))))

 (-define-syntax
  while
  (syntax-rules ()
    ((_ test expr ...)
     (format "while (~A) ~A"
             (-if (-symbol? 'test) 'test test)
             (begin expr ...)))))

 (-define-syntax
  until
  (syntax-rules (not)
    ((_ test expr ...)
     (while (not test) expr ...))))

 (-define-syntax
  enum
  (syntax-rules ()
    ((_ name enumerator* ...)
     (format "enum ~A {\n~A\n}~A"
             'name
             (-string-join (-map (-lambda (e)
                                          (-if (-list? e)
                                               (format "~A = ~A"
                                                       (-car e)
                                                       (eval-maybe (-cadr e)))
                                               (symbol->string e)))
                                 (-list 'enumerator* ...))
                           ",\n")
             (semicolon-maybe)))))

 (-define-syntax
  struct
  (syntax-rules (defvar)
    ((_ name decl* ...)
     (format "struct ~A {\n~A}~A"
             'name
             (defvar decl* ...)
             (semicolon-maybe)))))

 (-define-syntax
  union
  (syntax-rules (|| defvar)
    ((_ name decl* ...)
     (-apply format "union ~A {\n~A}~A"
             (-if (-list? 'name)
                  (-list '|| (defvar name decl* ...))
                  (-list name (defvar decl* ...)))
             (semicolon-maybe)))))

 (-define-syntax
  label
  (syntax-rules ()
    ((_ name stmt)
     (format "~A: ~A" 'name stmt))))
 )
