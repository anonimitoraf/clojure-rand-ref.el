;;; clojure-rand-ref.el --- Retrieves a random entry from https://clojuredocs.org/quickref -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimitoraf>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: May 22, 2021
;; Modified: May 22, 2021
;; Version: 0.0.0
;; Keywords: clojure, clojuredocs, quickref
;; Homepage: https://github.com/anonimitoraf/clojure-rand-ref
;; Package-Requires: ((emacs "27.1") (dash "2.18.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Retrieves a random entry from https://clojuredocs.org/quickref for discoverability purposes.
;;
;;; Code:

(require 'dom)
(require 'cl-lib)
(require 'dash)

(defvar clojure-rand-ref--base-url "https://clojuredocs.org")
(defvar clojure-rand-ref-cache-file "/tmp/clojure-quickref-dom.el")

;; Taken from https://stackoverflow.com/a/23078813
(defun clojure-rand-ref--async-shell-str (command callback)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (let* ((output-buffer (generate-new-buffer " *temp*"))
         (callback-fn callback))
    (set-process-sentinel
     (start-process "clojure-rand-ref-async" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fn output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun clojure-rand-ref--fetch-doc (callback)
  "Fetch the clojuredocs quickref page, parse it then invoke CALLBACK with the parsed result."
  (let ((callback-fn callback))
    (clojure-rand-ref--async-shell-str
     (concat "curl " clojure-rand-ref--base-url "/quickref")
     (lambda (html)
       (let* ((parsed (clojure-rand-ref--parse-doc html)))
         (write-region (prin1-to-string parsed) nil clojure-rand-ref-cache-file)
         (funcall callback-fn parsed))))))

(defun clojure-rand-ref--parse-doc (html)
  "Given a HTML string, parse it and return it as a list of items like:
`(:symbol \"map\"
  :link \"https://clojuredocs.org/clojure.core/map\"
  :description \"Returns a lazy sequence consisting of ...\")'"
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (cl-map 'list (lambda (entry)
                      (let ((value (-> entry
                                     (dom-child-by-tag 'dt)
                                     (dom-child-by-tag 'a)
                                     ((lambda (node)
                                        (list :symbol (dom-text node)
                                              :link (dom-attr node 'href))))))
                            (desc (-> entry
                                    (dom-child-by-tag 'dd)
                                    (dom-text))))
                        (list :symbol (plist-get value :symbol)
                              :link (concat clojure-rand-ref--base-url
                                            (plist-get value :link))
                              :description desc)))
              (dom-by-class dom "dl-row")))))

(defun clojure-rand-ref--rand-elem (l)
  "Return a random element in list L."
  (let ((idx (random (length l))))
    (nth idx l)))

(defun clojure-rand-ref (callback)
  "Invokes CALLBACK with a random item from https://clojuredocs.org/quickref."
  (if (file-exists-p clojure-rand-ref-cache-file)
      (with-temp-buffer
        (insert-file-contents clojure-rand-ref-cache-file)
        (->> (buffer-string)
          read-from-string
          car
          clojure-rand-ref--rand-elem
          (funcall callback)))
    (clojure-rand-ref--fetch-doc (lambda (parsed)
                                   (->> parsed
                                     (clojure-rand-ref--rand-elem)
                                     (funcall callback))))))

(provide 'clojure-rand-ref)
;;; clojure-rand-ref ends here
