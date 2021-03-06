(module
 agidel-plugin.c
 *
 (import (rename (prefix scheme -) (-quote quote)
                 (-quasiquote quasiquote) (-define define)
                 (-define-syntax define-syntax) (-lambda lambda))
         (prefix chicken.base -)
         (prefix chicken.string -)
         (prefix matchable -)
         (only chicken.syntax expand)
         (prefix srfi-13 -)
         format)
 (define should-use-semicolon? #t)
 (define-syntax no-scln
   (syntax-rules (should-use-semicolon?)
     ((_ expr)
      (-let ((sclnbuf should-use-semicolon?)
             (resbuf ""))
            (set! should-use-semicolon? #f)
            (set! resbuf expr)
            (set! should-use-semicolon? sclnbuf)
            resbuf))))
 (define scln
   (-match-lambda*
    (() (scln ""))
    ((o) (format "~A~A" o (-if should-use-semicolon? ";\n" "")))))
 (define (eval-if-list expr)
   (-->string (-if (-symbol? expr) expr (-eval expr))))

 (define (as-is o) o)

 (define (prefix->infix operator os)
   (format "(~A)"
           (-string-join
            (-map -->string os)
            operator
            'infix)))
 (define-syntax multioperator
   (syntax-rules ()
     ((_ operator expr1)
      (-match-lambda*
       ((o) 'expr1)
       (os (prefix->infix operator os))))))

 (define +      (multioperator " + " (format "+~A" o)))
 (define -      (multioperator " - " (format "-(~A)" o)))
 (define /      (multioperator " / " (format "(1/~A)" o)))
 (define *      (multioperator " * " (format "*~A" o)))
 (define %      (multioperator " % "
                               (-number->string (-exact->inexact (-/ o 100)))))
 (define bitor  (multioperator " | " (as-is o)))
 (define bitand (multioperator " & " (as-is o)))
 (define xor    (multioperator " ^ " (as-is o)))
 (define and    (multioperator " && " (quote false)))
 (define or     (multioperator " || " (quote true)))

 (define (not o)   (format "!(~A)" o))
 (define (compl o) (format "~~(~A)" o))
 (define (inc o)   (scln (format "++(~A)" o)))
 (define (inc* o)  (scln (format "(~A)++" o)))
 (define (dec o)   (scln (format "--(~A)" o)))
 (define (dec* o)  (scln (format "(~A)--" o)))

 (define (left-shift val sft)  (format "~A << ~A" val sft))
 (define (right-shift val sft) (format "~A >> ~A" val sft))

 (define (import . fs)
   (-apply -string-append
           (-map (lambda (f)
                   (-if (-string? f)
                        (format "#include \"~A\"\n" f)
                        (format "#include <~A>\n" f)))
                 fs)))

 (define (deconstruct-binding* types name rhand)
   (scln
    (-string-append
     "  "
     (-string-join (-map (-compose -->string -cadr) types)
                   " " 'suffix)
     (-->string name)
     (-if (-null? rhand)
          ""
          (format " = ~A" (-if (-symbol? rhand) rhand (-eval rhand)))))))

 (define deconstruct-binding
   (-match-lambda*
    ((name type ... ''= rhand)
     (deconstruct-binding* type name rhand))
    ((name type ...)
     (deconstruct-binding* type name '()))))

 (define-syntax defvar
   (syntax-rules ()
     ((_ binding) (-apply deconstruct-binding 'binding))
     ((_ binding binding* ...)
      (-string-append (-apply deconstruct-binding 'binding)
                      (defvar binding* ...)))))

 (define (disname+types name+types)
   (format "~A ~A"
           (-string-join (-map (-compose -->string -cadr)
                               (-cdr name+types)))
           (-car name+types)))
 (define (disarg args)
   (-string-join
    (-map (lambda (arg) (disname+types arg)) args)
    ", "
    'infix))

 (define-syntax defun
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

 (define (pragma . dirs)
   (format "#pragma ~A\n"
           (-string-join (-map -->string dirs) " " 'infix)))

 (define (continue) (scln (format "continue")))
 (define (break)    (scln (format "break")))
 (define return
   (-match-lambda*
    (() (scln (format "return")))
    ((o) (scln (format "return ~A" (no-scln o))))))

 (define (_bracket fun . args)
   (scln (no-scln (format "~A(~A)"
                          fun
                          (-string-join (-map -->string args) ", " 'infix)))))

 (define (deftype name . types)
   (scln (format "typedef ~A ~A"
                 (-string-join (-map -->string types) " ")
                 name)))

 (define (_brace o) o)

 (define (begin . body)
   (format "{\n~A\n}\n" (-apply -string-append body)))

 (define if
   (-match-lambda*
    ((test thenc) (format "if (~A) ~A" (no-scln test) thenc))
    ((test thenc elsec)
     (format "if (~A) ~A else ~A" (no-scln test) thenc elsec))))

 (define (if* test thenc elsec)
   (scln (format "~A ? ~A : ~A"
                 (no-scln test)
                 thenc
                 elsec)))

 (define (when test . body)
   (format "if (~A) ~A" (no-scln test) (-apply begin body)))
 (define (unless test . body)
   (format "if (!(~A)) ~A" (no-scln test) (-apply begin body)))

 (define (do-while test . body)
   (format "do ~A while (~A)\n" (-apply begin body) (no-scln test)))

 (define (while test . body)
   (format "while (~A) ~A" (no-scln test) (-apply begin body)))
 (define (until test . body)
   (-apply while (not (no-scln test)) body))

 (define (label name stmt) (format "~A: ~A" name stmt))
 (define (goto lbl) (scln (format "goto ~A~A" lbl)))

 (define-syntax enum
   (syntax-rules ()
     ((_ name enumerator* ...)
      (scln (format "enum ~A {\n  ~A\n}"
                    name
                    (-string-join
                     (-map (lambda (e)
                             (-if (-eq? 'quote (-car e))
                                  (symbol->string (-cadr e))
                                  (format "~A = ~A"
                                          (-car e)
                                          (eval (no-scln (-cadr e))))))
                           (-list 'enumerator* ...))
                     ",\n  "))))))

 (define-syntax struct
   (syntax-rules (defvar)
     ((_ name decl* ...)
      (scln (format "struct ~A {\n~A}~A"
                    name
                    (defvar decl* ...))))))

 (define-syntax union
   (syntax-rules (|| defvar)
     ((_ name decl* ...)
      (scln (-apply format
                    "union ~A {\n~A}~A"
                    (-flatten
                     (-list
                      (-if (-eq? 'quote (-car 'name))
                           (-list name (defvar decl* ...))
                           (-list '|| (defvar name decl* ...))))))))))

 (define (comparison-operator operator)
   (define (form-one-pair pair)
     (format "(~A~A~A)" (-car pair) operator (-cdr pair)))
   (define (groups-of-two operands)
     (-let loop ((acc '())
                 (src operands))
           (-if (-eq? 2 (-length src))
                (-append acc (-list (-cons (-car src) (-cadr src))))
                (loop
                 (-append acc (-list (-cons (-car src) (-cadr src))))
                 (-cdr src)))))
   (lambda operands
     (-if (-eq? 2 (-length operands))
          (form-one-pair (-cons (-car operands) (-cadr operands)))
          (-apply and (-map form-one-pair (groups-of-two operands))))))

 (define eq?  (comparison-operator " == "))
 (define neq? (comparison-operator " != "))
 (define >    (comparison-operator " > "))
 (define <    (comparison-operator " < "))
 (define >=   (comparison-operator " >= "))
 (define <=   (comparison-operator " <= "))

 (define (zero? o)     (eq? o 0))
 (define (positive? o) (> o 0))
 (define (negative? o) (< o 0))

 (define (set l r) (format "~A = ~A~A" l r (scln)))

 (define (size-of-expr expr)         (format "sizeof ~A" (no-scln expr)))
 (define (size-of-type . type-words) (-apply format "sizeof(~A)" type-words))
 (define (cast expr type)            (format "(~A)~A" type (no-scln expr)))

 ;;;; <The C Preprocessor macros>
 (define expand-prep-if
   (-match-lambda*
    ((test thenc) (format "#if ~A\n~A\n" test thenc))
    (((clause-test . thencs))
     (expand-prep-if (eval-if-list clause-test)
                     (-apply -string-append
                             (-map (-compose -->string eval-if-list) thencs))))))

 (define expand-prep-elif
   (-match-lambda*
    (('else . elsecs)
     (format "#else\n~A\n"
             (-apply -string-append
                     (-map (-compose -->string eval-if-list) elsecs))))
    ((test thenc) (format "#elif ~A\n~A\n" test thenc))
    (((clause-test . thencs))
     (expand-prep-elif clause-test
                       (-apply -string-append (-map
                                               (-compose -->string eval-if-list)
                                               thencs))))))
 (define prep-if
   (-match-lambda*
    ((test thenc) (format "~A#endif" (expand-prep-if test thenc)))
    ((test thenc elsec)
     (format "~A#else\n~A\n#endif" (expand-prep-if test thenc) elsec))))

 (define (defined id) (format "defined(~A)" id))

 (define-syntax prep-cond
   (syntax-rules (else)
     ((_ clause clause* ...)
      (format "~A~A#endif"
              (expand-prep-if 'clause)
              (-string-join (-map expand-prep-elif (-list 'clause* ...)) "")))))

 (define (prep-error error-msg) (format "#error ~A\n" error-msg))
 ;; todo: add support for parameterized macros
 (define (defmacro id . replacement)
   (format "#define ~A ~A\n" id (-string-join (-map -->string replacement))))
 (define (undef id) (format "#undef ~A\n" id))
 (define prep-line
   (-match-lambda*
    ((line) (format "#line ~A\n" line))
    ((line file) (format "#line ~A ~A\n" line file))))
 ;;;; </The C Preprocessor macros>


 (define (case-expand clause)
   (-cond
    ((-eq? 'else (-car clause))
     (format "default:\n~Abreak;\n"
             (-string-join (-map eval-if-list (-cdr clause)) "")))
    (else
     (format "~A~Abreak;\n"
             (-string-join (-map eval-if-list (-car clause)) ":\n" 'suffix)
             (-string-join (-map eval-if-list (-cdr clause)) "")))))
 (define-syntax case
   (syntax-rules ()
     ((_ key clause ...)
      (format "switch (~A) {\n~A\n}\n"
              key
              (-string-join (-map case-expand (-list 'clause ...)) "")))))

 (define-syntax for
   (syntax-rules ()
     ((_ init-vars test iter-expr body ...)
      (format "~A\n~A"
              (-if (-list? (-car 'init-vars))
                   (expand (cons 'defvar 'init-vars))
                   (defvar 'init-vars))
              (while test
                     (-apply -string-append iter-expr (-list body ...)))))))

 (define (get array element) (format "~A[~A]" array (no-scln element)))
 (define (deref pointer)     (format "*~A" pointer))
 (define (ref pointer)       (format "&~A" pointer))
 (define (sub . os)          (-string-join (-map -->string os) "." 'infix))
 (define (-> . os)           (-string-join (-map -->string os) "->" 'infix))
 )
