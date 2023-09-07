#lang racket

(require net/url)
(require web-server/servlet)
(require web-server/servlet-env)
(require json)

(define data (make-hash))

(define (update-data)
  (thread (lambda ()
            (with-handlers ((exn? (lambda (e)
                                    (displayln "[ERROR] Update Failed"))))
              (let-values (((h s r) (http-sendrecv/url (string->url "https://f-droid.org/repo/index-v2.json"))))
                (let* ((key-string (port->string r))
                       (j (string->jsexpr key-string)))

                  ;; (displayln r)
                  ;; (displayln j)

                  (set! data j)))))))

(define (generate-package package-info)
  (let* ((file-info (cdar (sort (hash->list (hash-ref package-info 'versions)) (lambda (a b)
                                                                                 (> (hash-ref (cdr a) 'added)
                                                                                    (hash-ref (cdr b) 'added))))))
         (download-url (string-append "https://mirrors.tuna.tsinghua.edu.cn/fdroid/repo" (hash-ref (hash-ref file-info 'file) 'name)))
         (categories (apply string-append (map (lambda (a) (string-append a " "))
                                               (hash-ref (hash-ref package-info 'metadata) 'categories)))))
    `(div ((class "card"))
          (div ((class "card-body"))
               (h5 ((class "card-title")) ,(hash-ref (hash-ref (hash-ref package-info 'metadata) 'name) 'en-US))
               (p ((class "card-text")) ,categories)
               (p ((class "card-text")) (a ((href ,download-url)) ,(hash-ref (hash-ref file-info 'manifest) 'versionName)))))))

(define (search req)
  (displayln req)
  (let ((search-string ""))
    (for ((i (request-bindings req)))
      (when (symbol=? (car i) 'searchs)
        (set! search-string (cdr i))))
    (cond [(string=? search-string "")
           (response/xexpr `(html (head (title "Fdroid Searcher")
                                        ;; (mate ((charset "utf-8")))
                                        ;; (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
                                        (link ((rel "stylesheet") (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/css/bootstrap.min.css") (integrity "sha384-GLhlTQ8iRABdZLl6O3oVMWSktQOp6b7In1Zl3/Jr59b6EGGoI1aFkw7cmDA6j6gD") (crossorigin "anonymous")))
                                        (script ((src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/js/bootstrap.bundle.min.js") (integrity "sha384-w76AqPfDkMBDXo30jS1Sgez6pr3x5MlQ1ZAGC+nuZB+EYdgRZgiwxhTBTkF7CXvN") (crossorigin "anonymous")))
                                        (h1 "ERROR!"))))]

          [else (let ((body '(div ((class "container"))
                                  (br)
                                  (h1 "FDroid Searcher")
                                  (br))))
                  (hash-for-each (hash-ref data 'packages)
                                 (lambda (key value)
                                   ;;(displayln key)
                                   (let ((key-string (string-downcase (symbol->string key))))
                                     (if (string-contains? key-string search-string)
                                         ;;(displayln key)
                                         (set! body (append body (list (generate-package (hash-ref (hash-ref data 'packages) key)) '(br))))
                                         (void)))))

                  ;;(displayln (hash-ref (data) 'packages))

                  (response/xexpr `(html (head (title "Fdroid Searcher")
                                               ;; (mate ((charset "utf-8")))
                                               ;; (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
                                               (link ((rel "stylesheet") (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/css/bootstrap.min.css") (integrity "sha384-GLhlTQ8iRABdZLl6O3oVMWSktQOp6b7In1Zl3/Jr59b6EGGoI1aFkw7cmDA6j6gD") (crossorigin "anonymous")))
                                               (script ((src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/js/bootstrap.bundle.min.js") (integrity "sha384-w76AqPfDkMBDXo30jS1Sgez6pr3x5MlQ1ZAGC+nuZB+EYdgRZgiwxhTBTkF7CXvN") (crossorigin "anonymous")))
                                               ,body))))


                ])))

(define (main-server req)

  (send/suspend/dispatch
   (lambda (embed/url)
     (define body `(div ((class "container"))
                        (br)
                        (h1 "FDroid Searcher")
                        (br)
                        (form ((action
                                ,(embed/url search)))
                              (div ((class "input-group mb-3"))
                                   (div ((class "input-group-prepend"))
                                        (span ((class "input-group-text") (id "basic-addon1")) "Key Word"))
                                   (input ((type "text") (class "form-control") (placeholder "something") (name "searchs")))
                                   (input ((type "submit") (class "form-control")))))))

     ;;(displayln back)

     (response/xexpr `(html (head (title "Fdroid Searcher")
                                  ;; (mate ((charset "utf-8")))
                                  ;; (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
                                  (link ((rel "stylesheet") (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/css/bootstrap.min.css") (integrity "sha384-GLhlTQ8iRABdZLl6O3oVMWSktQOp6b7In1Zl3/Jr59b6EGGoI1aFkw7cmDA6j6gD") (crossorigin "anonymous")))
                                  (script ((src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0-alpha1/dist/js/bootstrap.bundle.min.js") (integrity "sha384-w76AqPfDkMBDXo30jS1Sgez6pr3x5MlQ1ZAGC+nuZB+EYdgRZgiwxhTBTkF7CXvN") (crossorigin "anonymous")))
                                  ,body))))))

(thread-wait (update-data))

(displayln "Data Fetched!")

(displayln (hash-keys data))

(thread (lambda ()
          (let loop ()
            (sleep 3600)
            (update-data))))

(serve/servlet main-server
               #:command-line? true
               #:port 8901
               #:listen-ip "0.0.0.0"
               #:servlet-path "/")
