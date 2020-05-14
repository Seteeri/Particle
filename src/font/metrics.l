(class +Metrics)

(dm pos> (Ori)
  #{
    Pos is the ori
    Actual vert pos is rel to ori
    Adj vert by bnds since glyph has been cropped to bbox/bnds
    
    For example, 'g' hangs below baseline,
    bottom bnds is neg,
    y pos + y bnds (neg), shifts glyph downward into proper pos
  }#

  (list (+ (car  Ori) (*/ (car  (: bnds)) (: sca-f) 1.0))
        (+ (cadr Ori) (*/ (cadr (: bnds)) (: sca-f) 1.0))
        0.0))


(dm ori> (Pos)
  #{
    Derive ori from pos
  }#

  (list (+ (car  Pos) (- (*/ (car  (: bnds)) (: sca-f) 1.0)))
        (+ (cadr Pos) (- (*/ (cadr (: bnds)) (: sca-f) 1.0)))
        0.0))
      
        
(dm sca> ()
  # Scale based on uv dims (actual area being drawn on glyph), not tex dims
  (list (*/ (car  (: dims)) (: sca-f) 1.0)
        (*/ (cadr (: dims)) (: sca-f) 1.0)
        1.0))


(dm calc-sca-f> ()
  (=: sca-f (*/ 1.0
                (meta '(+Vertex) 'sca-vert)
                (: scale))))

(dm calc-cen> ()
  (=: cen (list (*/ (*/ (car (: dims))  (: sca-f) 1.0) 0.5 1.0)
                (*/ (*/ (cadr (: dims)) (: sca-f) 1.0) 0.5 1.0)
                0.0))
  (=: mat4-cen (mat4-tra
                 (list (- (car (: cen)))
                       (- (cadr (: cen)))
                       0.0)))
  (=: mat4-cen-i (mat4-inv
                  (: mat4-cen))))
