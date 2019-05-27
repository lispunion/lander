(cond-expand
  (chibi
   (import (scheme base)
           (scheme char)
           (scheme file)
           (scheme cxr)
           (scheme read)
           (scheme write)
           (srfi 1)
           (srfi 69)
           (chibi filesystem)))
  (gauche
   (use srfi-1)
   (use srfi-69)
   (use file.util)))

;;;;

(define (show x)
  (write x (current-error-port))
  (newline (current-error-port))
  x)

(define (alist->plist xs)
  (let loop ((xs xs) (acc '()))
    (if (null? xs)
        acc
        (loop (cdr xs) (append acc (list (caar xs) (cdar xs)))))))

(define (tab . xs)
  (let ((ht (make-hash-table)))
    (let loop ((xs xs))
      (cond ((null? xs) ht)
            (else (hash-table-set! ht (car xs) (cadr xs))
                  (loop (cddr xs)))))))

(define (maptab fun xs)
  (let ((ht (make-hash-table)))
    (let loop ((xs xs))
      (if (null? xs)
          ht
          (let ((pair (fun (car xs))))
            (hash-table-set! ht (car pair) (cdr pair))
            (loop (cdr xs)))))))

(define (path-join . strings)
  (if (null? strings)
      ""
      (fold (lambda (s so-far) (string-append so-far "/" s))
            (car strings)
            (cdr strings))))

(define (map-string-chars transform-char s)
  (let loop ((chars '()) (cs (string->list s)))
    (if (null? cs)
        (list->string chars)
        (loop (append chars (list (transform-char (car cs))))
              (cdr cs)))))

;;;;

(define (line . xs)
  (for-each display xs)
  (newline))

(define (indented-line nest . xs)
  (display (make-string (* 2 nest) #\space))
  (apply line xs))

(define (yaml-simple x)
  (cond ((or (and (hash-table? x) (not (= 0 (hash-table-size x))))
             (and (list? x) (not (null? x))))
         #f)
        ((hash-table? x)
         "{}")
        ((null? x)
         "[]")
        (else
         x)))

(define (yaml-object x nest)
  (cond ((null? x)
         (indented-line nest "- []"))
        ((list? x)
         (for-each (lambda (val)
                     (let ((simple (yaml-simple val)))
                       (if simple
                           (indented-line nest "- " simple)
                           (begin (indented-line nest "-")
                                  (yaml-object val (+ nest 1))))))
                   x))
        ((hash-table? x)
         (hash-table-walk
          x (lambda (key val)
              (let ((key key))
                (let ((simple (yaml-simple val)))
                  (if simple
                      (indented-line nest key ": " simple)
                      (begin (indented-line nest key ":")
                             (yaml-object val (+ nest 1)))))))))))

(define (yaml-document x)
  (line "---")
  (yaml-object x 0))

;;;;

(define (object? x)
  (and (list? x)
       (symbol? (car x))))

(define (the-object head x)
  (unless (and (object? x) (equal? head (car x)))
    (error (string-append "Not " (symbol->string head) " object")))
  (cdr x))

(define (complex-property x name)
  (let ((pair (assoc name x)))
    (and pair
         (begin (unless (list? pair) (error "Bad property"))
                (cdr pair)))))

(define (simple-property x name predicate)
  (let ((pair (assoc name x)))
    (and pair
         (begin (unless (and (list? pair)
                             (null? (cddr pair))
                             (predicate (cadr pair)))
                  (error "Bad property"))
                (cadr pair)))))

(define (char->identifier-char c)
  (if (or (char-alphabetic? c) (char-numeric? c)) c #\_))

(define (identifier x)
  (map-string-chars char->identifier-char
                    (if (string? x) x (symbol->string x))))

;;;;

(define (gen-ansible-cfg options)
  (with-output-to-file "ansible.cfg"
    (lambda ()
      (line "[defaults]")
      (line "inventory = hosts.yml")
      (for-each (lambda (var)
                  (let* ((var (the-object 'var var))
                         (name (car var))
                         (value (cadr var)))
                    (line (identifier name) " = " value)))
                options))))

(define (gen-hosts-yml-vars vars)
  (maptab (lambda (var)
            (let ((var (the-object 'var var)))
              (cons (identifier (first var))
                    (second var))))
          vars))

(define (gen-hosts-yml-group-hosts hosts)
  (maptab (lambda (host)
            (let ((host (the-object 'host host)))
              (cons (simple-property host 'name symbol?)
                    (gen-hosts-yml-vars
                     (complex-property host 'vars)))))
          hosts))

(define (gen-hosts-yml groups)
  (with-output-to-file "hosts.yml"
    (lambda ()
      (yaml-document
       (maptab (lambda (group)
                 (let ((group (the-object 'group group))
                       (hosts (gen-hosts-yml-group-hosts
                               (or (complex-property group 'hosts) '())))
                       (vars  (gen-hosts-yml-vars
                               (or (complex-property group 'vars) '()))))
                   (cons (simple-property group 'name symbol?)
                         (tab 'hosts hosts 'vars vars))))
               groups)))))

(define (gen-module-params module-params)
  (maptab (lambda (module-param)
            (cons (identifier (car module-param))
                  (second module-param)))
          module-params))

(define (gen-module-invocation invocation)
  (let* ((title (simple-property invocation 'title string?))
         (module-and-params (second invocation))
         (module-name (car module-and-params))
         (module-params (cdr module-and-params))
         (other-things (cddr invocation)))
    (apply tab
           'name title
           module-name (gen-module-params module-params)
           (alist->plist other-things))))

(define (gen-role-tasks-directory tasks-dir tasks-property)
  (create-directory* tasks-dir)
  (with-output-to-file (path-join tasks-dir "main.yml")
    (lambda ()
      (yaml-document
       (map (lambda (task) (gen-module-invocation (the-object 'task task)))
            tasks-property)))))

(define (gen-role-handlers-directory handlers-dir handlers-property)
  (create-directory* handlers-dir)
  (with-output-to-file (path-join handlers-dir "main.yml")
    (lambda ()
      (yaml-document
       (map (lambda (handler)
              (gen-module-invocation (the-object 'handler handler)))
            handlers-property)))))

(define (gen-role-directory role)
  (let* ((role (the-object 'role role))
         (role-name (simple-property role 'name symbol?))
         (tasks-dir (path-join "roles" (identifier role-name) "tasks"))
         (handlers-dir (path-join "roles" (identifier role-name) "handlers")))
    (gen-role-tasks-directory
     tasks-dir (or (complex-property role 'tasks)
                   (error "Role has no (tasks ...)")))
    (let ((handlers (complex-property role 'handlers)))
      (when handlers (gen-role-handlers-directory handlers-dir handlers)))))

(define (gen-playbook-yml playbook)
  (let ((playbook (the-object 'playbook playbook)))
    (with-output-to-file
        (string-append (identifier (simple-property playbook 'name symbol?))
                       ".yml")
      (lambda ()
        (yaml-document
         (list
          (tab 'hosts (complex-property playbook 'hosts)
               'become (simple-property playbook 'become symbol?)
               'roles (map identifier (complex-property playbook
                                                        'roles)))))))))

(define (gen-ansible form)
  (let ((ansible (the-object 'ansible form)))
    (gen-ansible-cfg (complex-property ansible 'options))
    (gen-hosts-yml (complex-property ansible 'groups))
    (for-each gen-role-directory (complex-property ansible 'roles))
    (for-each gen-playbook-yml (complex-property ansible 'playbooks))))

(gen-ansible (read))
