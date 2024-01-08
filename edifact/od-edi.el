;;; edifact/od-edi.el -*- lexical-binding: t; -*-


;;; Code:

(require 'ob)
(require 'seq)
(require 's)
(require 'dash)
(require 'request)
(require 'edi-mode)

(defvar org-babel-default-header-args:edi
  '((:results . "code"))
  "Default arguments for evaluating an edi source block.

The evaluation returns the formmatted edi.")

(add-to-list 'org-src-lang-modes '("edi" . edi))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-babel-execute:edi (body params)
  "Execute edi BODY but PARAMS are ignored."
  (setq vra-debug-var
        (--reduce-from
         (if (eq :var (car it))
           (cons (cdr it) acc)
           acc)
         '()
         params))
  (setq vra-debug params)
  ;; TODO Add Proxy overide config
  (let* ((vars (--reduce-from
                (if (eq :var (car it))
                    (cons (cdr it) acc)
                  acc)
                '()
                params))
         (edi-host (cdr (assoc 'host vars)))
         (edi-route (cdr (assoc 'route vars)))
         (edi-url (format "https://%s%s" edi-host edi-route))
         (edifact (edi-edi-ensure-not-readable body))
         (request-curl-options '("-k")))
    (message (format "XXX %s -> %s" edi-url edifact))
    (with-temp-buffer
      (insert
       (request-response-data
        (request edi-url
          :type "POST"
          :sync t
          :headers '(("Content-Type" . "application/json")
                     ("accept" . "application/json"))
          :data edifact
          )))
      (edi-edi-to-readable)
      (buffer-string))))

(defun edi-edi-ensure-not-readable (edi)
  (s-join "" (s-split "\n" edi 'omit-nulls)))
