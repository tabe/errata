(library (errata font)
  (export face->style)
  (import (rnrs (6)))

  (define *face-style* (make-eq-hashtable))

  (define (face->style face)
    (hashtable-ref *face-style* (string->symbol face) #f))

  (for-each
   (lambda (face style)
     (hashtable-set! *face-style* face style))
   '(bold italic oblique constant-width-bold constant-width-italic constant-width-oblique)
   '("font-style: bold"
     "font-style: italic"
     "font-style: oblique"
     "font-family: monospace; font-style: bold"
     "font-family: monospace; font-style: italic"
     "font-family: monospace; font-style: oblique"))

)
