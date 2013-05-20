#lang racket/base

(require "utils.rkt")

;; Requires the keyboard/keystrokes directory to be raco link'ed and raco
;; setup to be run to set up the keystrokes tool.

(call-with-frozen-collects
 "./keyboard/"
 ;; Redirecting to the frozen racket/draw didn't work for references in the
 ;; drracket sollect, so had to change them to refer to the frozen version
 ;; directly. (Maybe drracket changes collection paths too?)
 '("drracket" "framework" "mred" "mrlib" "keystrokes" "racket/draw")
 (lambda ()
   (dynamic-require '(lib "drracket/drracket") #f)))
