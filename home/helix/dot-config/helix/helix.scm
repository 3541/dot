(require "helix/editor.scm")
(require "helix/misc.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(provide open-helix-scm open-init-scm alt)

;;@doc
;; Open the helix.scm file
(define (open-helix-scm)
  (helix.open (helix.static.get-helix-scm-path)))

;;@doc
;; Opens the init.scm file
(define (open-init-scm)
  (helix.open (helix.static.get-init-scm-path)))

; (define (editor-get-doc-if-exists doc-id)
;   (if (editor-doc-exists? doc-id) (editor->get-document doc-id) #f))

; (define (current-path)
;   (let* ([focus (editor-focus)]
;          [focus-doc-id (editor->doc-id focus)]
;          [document (editor-get-doc-if-exists focus-doc-id)])

;     (if document (Document-path document) #f)))

(define (current-path)
  (let* ([focus (editor-focus)]
         [focus-doc-id (editor->doc-id focus)])
    (editor-document->path focus-doc-id)))

;;@doc
;; Toggles between C/C++ source and header files
(define (alt)
  (send-lsp-command "clangd" "textDocument/switchSourceHeader"
                    (hash "uri" (string-append "file://" (current-path)))
                    (lambda (result)
                             (helix.open (trim-start-matches result "file://")))))
