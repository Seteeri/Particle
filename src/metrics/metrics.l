#{
  Based on texture...poss move to render?

  Product of X and Y = (*/ X Y 1.0).

  : (*/ 32.1 1.056 1.0)  # 32.1 * 1.056 = 33.8976
  -> 33898

  Quotient of N and D = (*/ 1.0 N D)

  : (*/ 1.0 91.8 1.323)  # 91.8 / 1.323 = 69.38775510...
  -> 69388
}#

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


(dm calc> (Pro)

  # bnds: l b r t

  (=: scale  (format (get Pro 'scale) *Scl))
  (=: adv    (format (get Pro 'advance) *Scl))
  (=: range  (format (get Pro 'range) *Scl))
  (=: bounds (if (get Pro 'bounds)
                 (let L NIL
                    (for X (get Pro 'bounds) 
                      (push 'L (format X *Scl)))
                    (flip L))
                 (need 4 0.0)))
  (=: translate (if (get Pro 'translate)
                 (let L NIL
                    (for X (get Pro 'translate) 
                      (push 'L (format X *Scl)))
                    (flip L))
                 (need 2 0.0)))

  (=: sca-f (*/ 1.0
                *sca-vert
                (: scale)))

  # sca-glyph * shape units = pixel units
  # Bounds is rel to ori so neg nums possible
  (=: bnds 
      (list (*/ (get (: bounds) 1) (: scale) 1.0)
            (*/ (get (: bounds) 2) (: scale) 1.0)
            (*/ (get (: bounds) 3) (: scale) 1.0)
            (*/ (get (: bounds) 4) (: scale) 1.0)))          
            
  # bbox (for tex draw) rel to 
  # 1. Get origin which may have moved during gen to create enough space for df
  # 2. Add bnds
  # 3. Expand by 1.0 since ori=(0,0)
  (=: bnds-tex 
      (list (+ (*/ (car  (: translate)) (: scale) 1.0) (get (: bnds) 1) -1.0)
            (+ (*/ (cadr (: translate)) (: scale) 1.0) (get (: bnds) 2) -1.0)
            (+ (*/ (car  (: translate)) (: scale) 1.0) (get (: bnds) 3) 1.0)
            (+ (*/ (cadr (: translate)) (: scale) 1.0) (get (: bnds) 4) 1.0)))
  
  # dims of bbox
  (=: dims (list (- (get (: bnds-tex) 3) (get (: bnds-tex) 1))
                 (- (get (: bnds-tex) 4) (get (: bnds-tex) 2))))


  # l b r t = 1 2 3 4
  # ori=(0,0)=bottom left
  # so (1,1)=top right
  (=: uv 
      (list (*/ 1.0 (get (: bnds-tex) 3) (car  *glyph-dims))
            (*/ 1.0 (get (: bnds-tex) 4) (cadr *glyph-dims))
            
            0.0
            0.0
            
            (*/ 1.0 (get (: bnds-tex) 3) (car  *glyph-dims))
            (*/ 1.0 (get (: bnds-tex) 2) (cadr *glyph-dims))
            
            0.0
            0.0
            
            (*/ 1.0 (get (: bnds-tex) 1) (car  *glyph-dims))
            (*/ 1.0 (get (: bnds-tex) 2) (cadr *glyph-dims))
            
            0.0
            0.0
            
            (*/ 1.0 (get (: bnds-tex) 1) (car  *glyph-dims))
            (*/ 1.0 (get (: bnds-tex) 4) (cadr *glyph-dims))
            
            0.0
            0.0)))
