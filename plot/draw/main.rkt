#lang racket/base
(require racket/class
         racket/contract/base
         "private/contract.rkt"
	 "private/color.rkt"
         "private/point.rkt"
         "private/font.rkt"
         "private/font-dir.rkt"
         "private/font-syms.rkt"
         "private/pen.rkt"
         "private/brush.rkt"
         "private/gradient.rkt"
         "private/region.rkt"
         "private/bitmap.rkt"
         "private/dc-path.rkt"
         "private/dc-intf.rkt"
         "private/bitmap-dc.rkt"
         "private/record-dc.rkt"
         "private/post-script-dc.rkt"
         "private/ps-setup.rkt"
         "private/svg-dc.rkt"
         "private/gl-config.rkt"
         "private/gl-context.rkt")

(provide color-database<%>
	 the-color-database
         font-list% the-font-list make-font
         font-name-directory<%> the-font-name-directory
	 the-pen-list
         the-brush-list
         dc<%>
         recorded-datum->procedure
         ps-setup% current-ps-setup
         get-face-list
         get-family-builtin-face
         gl-context<%>
         make-bitmap
         make-platform-bitmap
         read-bitmap
         make-monochrome-bitmap

         ;; predicates/contracts
         brush-style/c
         pen-cap-style/c
         pen-join-style/c
         pen-style/c
         font-family/c
         font-weight/c
         font-style/c
         font-smoothing/c
         font-hinting/c)

(provide/contract [color%            color%/c]
                  [point%            point%/c]
                  [font%             font%/c]
                  [pen%              pen%/c]
                  [pen-list%         pen-list%/c]
                  [brush%            brush%/c]
                  [brush-list%       brush-list%/c]
                  [bitmap-dc%        bitmap-dc%/c]
                  [post-script-dc%   post-script-dc%/c]
                  [pdf-dc%           pdf-dc%/c]
                  [svg-dc%           svg-dc%/c]
                  [record-dc%        record-dc%/c]
                  [linear-gradient%  linear-gradient%/c]
                  [radial-gradient%  radial-gradient%/c]
                  [region%           region%/c]
                  [dc-path%          dc-path%/c]
                  [gl-config%        gl-config%/c]
                  [bitmap%           bitmap%/c]
                  [make-color        make-color/c]
                  [make-pen          make-pen/c]
                  [make-brush        make-brush/c])

