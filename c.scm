(module
 agidel-plugin.c
 *
 (import (rename (prefix scheme -) (-quote quote))
         (prefix chicken.base -)
         (prefix chicken.string -)
         (prefix matchable -)
         (only chicken.syntax expand)
         (prefix srfi-13 -)
         format)

 (-define (semicolon-maybe) ";\n")
 (-define newline
          (-match-lambda*
           ((_) "\n")
           ((_ str) (format "~A\n" str))))

 (-define-syntax
  str
  (syntax-rules (!!)
    ((_ !! lst) (-map -->string lst))
    ((_ elt) (-cond
              ((-string? elt) (-string-append "\"" elt "\""))
              ((-list? elt) (-map -->string 'elt))
              (else (-->string 'elt))))
    ((_ elt elt* ...) (str (-eval (-list elt elt* ...))))))

 (-define (prefix->infix operator os)
          (format "(~A)"
                  (-string-join
                   (-map -->string os)
                   operator
                   'infix)))
 (-define-syntax
  multioperator
  (syntax-rules ()
    ((_ operator expr1)
     (-match-lambda*
      ((o) 'expr1)
      (os (prefix->infix operator os))))))

 (-define + (multioperator " + " (format "+~A" o)))
 (-define - (multioperator " - " (format "-(~A)" o)))
 (-define / (multioperator " / " (format "(1/~A)" o)))
 (-define * (multioperator " * " (format "*~A" o)))
 (-define % (multioperator " % " (-number->string (-exact->inexact (-/ o 100)))))
 (-define bitor (multioperator " | " (str o)))
 (-define bitand (multioperator " & " (str o)))
 (-define xor (multioperator " ^ " (str o)))
 (-define and (multioperator " && " (quote false)))
 (-define or (multioperator " || " (quote true)))

 (-define (not o) (format "!(~A)" o))
 (-define (compl o) (format "~~(~A)" o))
 (-define (left-shift val sft) (format "~A << ~A" val sft))
 (-define (right-shift val sft) (format "~A >> ~A" val sft))

 (-define (import . fs)
          (-apply -string-append
                  (-map (-lambda (f)
                                 (-if (-string? f)
                                      (format "#include \"~A\"\n" f)
                                      (format "#include <~A>\n" f)))
                        fs)))

 (-define (deconstruct-binding* types name . rhand)
          (-string-append
           (-string-join (-map -symbol->string types)
                         " " 'suffix)
           (-->string name)
           (-if (-null? (-car rhand))
                ""
                (format " = ~A" (-car rhand)))
           (semicolon-maybe)))
 (-define-syntax
  deconstruct-binding
  (syntax-rules (=)
    ((_ name type type* ... = rhand)
     (deconstruct-binding* (-list 'type 'type* ...) 'name rhand))
    ((_ name type type* ...)
     (deconstruct-binding* (-list 'type 'type* ...) 'name '()))))

 (-define-syntax
  defvar
  (syntax-rules ()
    ((_ binding) (eval (cons 'deconstruct-binding 'binding)))
    ((_ binding binding* ...)
     (-string-append (eval (cons 'deconstruct-binding 'binding))
                     (defvar binding* ...)))))

 (-define (disname+types name+types)
          (format "~A~A"
                  (-string-join (-map -symbol->string (-cdr name+types))
                                " "
                                'suffix)
                  (-car name+types)))
 (-define (disarg args)
          (-string-join
           (-map (-lambda (arg) (disname+types arg)) args)
           ", "
           'infix))

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
 #|
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
 ((_ test thenc)
 (format "if (~A) ~A"
 (expand-maybe 'test)
 thenc))
 ((_ test thenc elsec)
 (format "if (~A) ~A else ~A"
 (expand-maybe 'test)
 thenc
 elsec))))

 (-define-syntax
 unless
 (syntax-rules ()
 ((_ test thenc)
 (format "if (!(~A)) ~A"
 (expand-maybe 'test)
 thenc))
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
 when
 (syntax-rules ()
 ((_ test expr* ...)
 (format "if (~A) ~A"
 (expand-maybe 'test)
 (expand (begin expr* ...))))))

 (-define-syntax
 when-not
 (syntax-rules ()
 ((_ test expr* ...)
 (format "if (!(~A)) ~A"
 (expand-maybe 'test)
 (expand (begin expr* ...))))))

 (-define-syntax
 do-while
 (syntax-rules ()
 ((_ test expr* ...)
 (format "do ~A while (~A)"
 (expand (begin expr* ...))
 (expand-maybe 'test)))))

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

 (-define-syntax
 goto
 (syntax-rules ()
 ((_ lbl)
 (format "goto ~A~A" 'lbl (semicolon-maybe)))))
 |#
 )
