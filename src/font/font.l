(class +Font)

(dm T (Nm
       Mets
       Tex-Ixs
       Adv)

  (=: nm Nm)
  
  (if (str? Mets)
      (ld-mets> This Mets)
      (=: mets Mets))
  
  (if (str? Tex-Ixs)
      (ld-tex-ixs> This Tex-Ixs)
      (=: tex-ixs Tex-Ixs))
  
  # For monospace only
  (=: adv Adv))
  

(dm lup-tex-ixs-cdr> (Ch)
  (cdr (lup (: tex-ixs) Ch)))
  
  
(dm ld-tex-ixs> (File)
  (=: tex-ixs
      (in File (rd))))

      
(dm lup-mets-cdr> (Ch)
  (cdr (lup (: mets) Ch)))

  
(dm ld-mets> (File)

  # Must update scale
  # unless scale is done during gen
  # for now, keep dynamic

  # HEAP:
  # B-Tree
  # Objs
  # 
  # Should interleave by looping and creating simul.
  # This would require building b-tree here
  
  (=: mets 
      (let B (in File (rd))
        (for M (idx 'B)
          (let O (new '(+Metrics))
            (putl O (cdr M))
            (calc-sca-f> O)
            (con M O)))
        B)))


#{
(de ld-metrics (Path)
  (let (Metrics ()
        Path-M-Def (pack Path "32-m.v.l"))
    (for Code 255 
      (push 'Metrics
            (let (Met (new '(+Metrics))
                  Path-M (pack Path Code "-m.v.l"))
              (unless (info Path-M) (setq Path-M Path-M-Def))
              (let Pro (car (str (in Path-M
                                      (till NIL T))))
                (putl Met Pro)
                (calc-sca-f> Met))
              Met)))
    (flip Metrics)))
}#
