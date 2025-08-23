; From https://github.com/mattwparas/helix-config/blob/master/cogs/keymaps.scm

(require-builtin helix/core/keymaps as helix.keymaps.)
(require "helix/configuration.scm")

(provide add-global-keybinding)

(define (get-doc name)
  ;; Do our best - if the identifier doesn't exist (for example, if we're checking)
  ;; something like 'no_op, we should just continue
  (with-handler (lambda (_) #f)
                (eval `(#%function-ptr-table-get #%function-ptr-table ,(string->symbol name)))))

(define (get-typed-command-doc name)
  (get-doc (trim-start-matches name ":")))

(define (walk-leaves keybindings)
  (if (hash? keybindings) (map walk-leaves (hash-values->list keybindings)) keybindings))

(define (keybindings->leaves keybindings)
  (flatten (walk-leaves keybindings)))

(define (keybindings->docs keybindings)
  (define leaves
    (map (lambda (key) (if (symbol? key) (symbol->string key) key))
         (keybindings->leaves keybindings)))

  ;; Filter out anything without values - so we only want strings
  (define doc-map
    (map (lambda (command) (cons (trim-start-matches command ":") (get-typed-command-doc command)))
         leaves))

  ;; Filter out anything without values - so we only want strings on the
  ;; right hand side
  (transduce doc-map (filtering (lambda (p) (string? (cdr p)))) (into-hashmap)))

;;@doc
;; Add keybinding to the global default
(define (add-global-keybinding map)
  ;; Copy the global ones
  (define global-bindings (get-keybindings))
  (helix.keymaps.helix-merge-keybindings
   global-bindings
   (~> map (value->jsexpr-string) (helix.keymaps.helix-string->keymap)))
  (helix.keymaps.keymap-update-documentation! global-bindings (keybindings->docs map))
  (keybindings global-bindings))
