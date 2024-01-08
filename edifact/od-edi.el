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
  ;; TODO Add Proxy overide config
  (let* ((vars (--reduce-from
                (if (eq :var (car it))
                    (cons (cdr it) acc)
                  acc)
                '()
                params))
         (edi-host (cdr (assoc 'host vars)))
         (edi-route (cdr (assoc 'route vars)))
         (edifact (edi-edi-ensure-not-readable body)))
    (message (format "host: %s -- route: %s" edi-host edi-route))
    (edi--validate-and-send-request edi-host edi-route edifact)))

(defun edi--send-request (url body)
  (let ((request-curl-options '("-k")))
    (request-response-data
     (request url
       :type "POST"
       :sync t
       :headers '(("Content-Type" . "application/json")
                  ("accept" . "application/json"))
       :data body))))

(defun edi-edi-ensure-not-readable (edi)
  (s-join "" (s-split "\n" edi 'omit-nulls)))


(defun edi--validate-and-send-request (edi-host edi-route body)
  (prin1 body)
  (cond ((not (stringp edi-host)) (format "Host %s is invalid" edi-host))
        ((not (stringp edi-route)) (format "Route %s is invalid" edi-route))
        ((string-empty-p edi-host) (format "Host is empty"))
        ((string-empty-p edi-route) (format "Route is empty"))
        (t (let ((url (format "https://%s%s" edi-host edi-route)))
             (with-temp-buffer
               (insert
                (edi--send-request url body))
               (edi-edi-to-readable)
               (buffer-string))))))
