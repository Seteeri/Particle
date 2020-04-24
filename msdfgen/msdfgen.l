# pil msdfgen.l -main +

(de main ()
  
  (gc 32)
  
  (setq *Scl 16
        *sz-s "96"
        *sz-f '(96.0 96.0)
        *path-msdfgen "/home/user/Git/msdfgen/build/msdfgen")

  # FiraMono
          
  (for Font '(("/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf"            "DejaVuSansMono-Bold")
              ("/usr/share/fonts/TTF/DejaVuSansMono-BoldOblique.ttf"     "DejaVuSansMono-BoldOblique")
              ("/usr/share/fonts/TTF/DejaVuSansMono-Oblique.ttf"         "DejaVuSansMono-Oblique")
              ("/usr/share/fonts/TTF/DejaVuSansMono.ttf"                 "DejaVuSansMono")
              ("/usr/share/fonts/TTF/DejaVuSansMono.ttf"                 "DejaVuSansMono")
              ("/usr/share/fonts/OTF/FiraCode-Bold.otf"                  "FiraCode-Bold")
              ("/usr/share/fonts/OTF/FiraCode-Light.otf"                 "FiraCode-Light")
              ("/usr/share/fonts/OTF/FiraCode-Medium.otf"                "FiraCode-Medium")
              ("/usr/share/fonts/OTF/FiraCode-Regular.otf"               "FiraCode-Regular")
              ("/usr/share/fonts/OTF/FiraCode-Retina.otf"                "FiraCode-Retina")
              ("/usr/share/fonts/TTF/FantasqueSansMono-Regular.ttf"      "FantasqueSansMono-Regular")
              ("/usr/share/fonts/TTF/FantasqueSansMono-Italic.ttf"       "FantasqueSansMono-Italic")
              ("/usr/share/fonts/TTF/FantasqueSansMono-BoldItalic.ttf"   "FantasqueSansMono-BoldItalic")
              ("/usr/share/fonts/TTF/FantasqueSansMono-Bold.ttf"         "FantasqueSansMono-Bold"))

    (setq *path-font  (car Font)
          *nm-font    (cadr Font)
          *uni-rx     (gen-uni-rx *nm-font)
          *dir-out    (pack (pwd) "/" *nm-font)
          *dir-ppm    (pack *dir-out "/ppm")
          *dir-png-af (pack *dir-out "/png-af")
          *dir-m-af   (pack *dir-out "/m-af")
          *dir-png    (pack *dir-out "/png")
          *dir-m      (pack *dir-out "/m")
          *dir-l      (pack *dir-out "/l"))
    
    (when T
      (call 'rm "-rv" (pack *dir-out    "/"))
      
      (call 'mkdir "-pv" (pack *dir-ppm    "/"))
      (call 'mkdir "-pv" (pack *dir-png-af "/"))
      (call 'mkdir "-pv" (pack *dir-m-af   "/"))    
      (call 'mkdir "-pv" (pack *dir-png    "/"))
      (call 'mkdir "-pv" (pack *dir-m      "/"))
      (call 'mkdir "-pv" (pack *dir-l      "/")))
  
    (start))
    
  (println "DONE" Font)  
  
  (bye))

    
(de start ()

  #{
    1. Use fc-query/match to find supported glyphs
    2. Gen metrics and output png
    3. Using metrics, regenerate final png
    4. Convert png to ppm/lisp
    5. Calc metrics
  }#
  
  (gen-png-msdf)

  (prinl)
  
  (conv-png-ppm)

  (prinl)
       
  # Run
  
  (conv-ppm-lisp)
  
  (calc-m))


(de gen-png-msdf ()

  (for Uni-Pair *uni-rx

    (for (U (car Uni-Pair) (<= U (cadr Uni-Pair)) (inc U))

      # Gen png + metrics
            
      (let (Out-Nm (pack *dir-png-af "/" U ".png")
            M-Nm   (pack *dir-m-af   "/" U "-m.l"))
      
        (if (info Out-Nm)
      
            (prinl (pack "[1] Skip: " Out-Nm))
      
            (let Metrics (in (list *path-msdfgen
                                   "-font"
                                   *path-font (format U)
                                   "-autoframe"
                                   "-size" *sz-s *sz-s
                                   "-printmetrics"
                                   "-o" Out-Nm)
                            (till))
                                          
              (wr-met U Metrics M-Nm)
                      
              (prinl (pack "[1] MSDF: " Out-Nm ", " M-Nm)))))
            
      # Gen final png + metrics
      
      (let (Out-Nm   (pack *dir-png "/" U ".png")
            In-M-Nm  (pack *dir-m-af   "/" U "-m.l")
            Out-M-Nm  (pack *dir-m   "/" U "-m.l"))
      
        (if (info Out-Nm)
      
            (prinl (pack "[2] Skip: " Out-Nm))
      
            (let (Pr (car (str (pack (in In-M-Nm (till)))))
                  Sca (get Pr 'scale)
                  Trans (get Pr 'translate)
                  Metrics (in (list *path-msdfgen
                                    "-font"
                                    *path-font (format U)
                                    "-scale" (format Sca *Scl)
                                    "-translate" (format (car Trans) *Scl) (format (cadr Trans) *Scl)
                                    "-size" *sz-s *sz-s
                                    "-printmetrics"
                                    "-o" Out-Nm)
                            (till)))
              
              (wr-met U Metrics Out-M-Nm Sca Trans)
              
              (prinl (pack "[2] MSDF: " Out-Nm ", " Out-M-Nm))))))))
  
  
(de conv-png-ppm ()

  (if (dir *dir-ppm)
  
      (prinl (pack "[3] Skip: PNG -> PPM"))
  
      (chdir *dir-png
  
        (println (pwd))
      
        (let (Cmd (list "gm"
                        "mogrify"
                        "-format" "ppm"
                        "-quality" "0"
                        "-create-directories"
                        "-output-directory" *dir-ppm
                        "-verbose"
                        "*.png")
              Ppm (in Cmd
                    (till)))

          (println Cmd)
          (prinl)
          (println (pack Ppm))))))  


(de conv-ppm-lisp ()
  
  (for File (dir *dir-ppm)
  
    (let (In-Nm  (pack *dir-ppm "/" File)
          Out-Nm (pack *dir-l "/" File ".l"))

        (if (info Out-Nm)
        
            (prinl (pack "[4] Skip: " In-Nm))
    
            (prog
            
              (let V (in In-Nm (till))
                (out Out-Nm
                  (prin "(")
                  (prin (clip V))
                  (prin ")")))
              
              (prinl (pack "[4] PPM -> LISP: " Out-Nm)))))))
  

(de calc-m ()

  (for File (dir *dir-m)
  
    (let Out-M-Nm (pack *dir-m "/" (pack (head -1 (chop File))) "v.l")
    
      (if (info Out-M-Nm)
      
          (prinl (pack "[5] Skip: " Out-M-Nm))
          
          (let (In-M-Nm (pack *dir-m "/" File)
                Pr (car (str (pack (in In-M-Nm (till)))))
                Pr-Out (calc Pr))
    
            (pretty Pr-Out)
          
            (out Out-M-Nm
              (pretty (getl Pr-Out)))
              
            (println File))))))

      
(de calc (In)

  # bnds: l b r t
  
  (let (Out T
        Sca (get In 'scale)
        Bnds (if (get In 'bounds) @ (need 4 0.0)))

    (put 'Out 'scale Sca)
    (put 'Out 'range (get In 'range))
    (put 'Out 'code (get In 'code))
    
    # sca * shape units = px
    # Bounds is rel to ori so neg nums possible
    (put 'Out 'bnds
        (list (* (get Bnds 1) Sca)
              (* (get Bnds 2) Sca)
              (* (get Bnds 3) Sca)
              (* (get Bnds 4) Sca)))

    # bbox (for tex draw) rel to 
    # 1. Get origin which may have moved during gen to create enough space for df
    # 2. Add bnds
    # 3. Expand to df rx
    (put 'Out 'bnds-tex 
        (list (+ (get Bnds 1) -1.0)
              (+ (get Bnds 2) -1.0)
              (+ (get Bnds 3) 1.0)
              (+ (get Bnds 4) 1.0)))
    
    # dims of bbox
    (put 'Out 'dims (list (- (get 'Out 'bnds-tex 3) (get 'Out 'bnds-tex 1))
                         (- (get 'Out 'bnds-tex 4) (get 'Out 'bnds-tex 2))))

    # AR of bbox
    (put 'Out 'ar (/ (car  (get 'Out 'dims))
                    (cadr (get 'Out 'dims))))

    # l b r t = 1 2 3 4
    # ori=(0,0)=bottom left
    # so (1,1)=top right
    #
    # baseline.x - bounds.l
    # baseline.y - bounds.b
    # baseline.x + bounds.r
    # baseline.y + bounds.t
    (put 'Out 'uv (list (*/ 1.0 (get 'Out 'bnds-tex 3) (car  *sz-f))
                        (*/ 1.0 (get 'Out 'bnds-tex 4) (cadr *sz-f))
                        0.0
                        0.0   # r-t 1,1
                        (*/ 1.0 (get 'Out 'bnds-tex 3) (car  *sz-f))
                        (*/ 1.0 (get 'Out 'bnds-tex 2) (cadr *sz-f))
                        0.0
                        0.0   # r-b 1,0
                        (*/ 1.0 (get 'Out 'bnds-tex 1) (car  *sz-f))
                        (*/ 1.0 (get 'Out 'bnds-tex 2) (cadr *sz-f))
                        0.0
                        0.0   # l-b 0,0
                        (*/ 1.0 (get 'Out 'bnds-tex 1) (car  *sz-f))
                        (*/ 1.0 (get 'Out 'bnds-tex 4) (cadr *sz-f))
                        0.0
                        0.0)) # l-t 0,1
    
    'Out))

    
(de wr-met (U Metrics M-Nm Sca Trans)
                                            
  (setq Metrics (mapcar pack (split Metrics "^J")))
  
  #(println Metrics)
  
  # Build property list out of metrics
  
  (let M-Out (list (cons 'code U))
  
    (for Line Metrics
    
      (let ((Nm Val) (mapcar pack (split (chop Line) "="))
            Nm (pack (clip (chop Nm)))
            Vals (mapcar pack (split (chop Val) ","))
            Vals (mapcar '((V) (format V *Scl))
                         Vals))
        
        (when Nm
          (push 'M-Out
                (cons (intern Nm)
                      (if (= (length Vals) 1) (car Vals) Vals))))))

    (when Sca (push 'M-Out (cons (intern "scale") Sca)))
    (when Trans (push 'M-Out (cons (intern "translate") Trans)))
                      
    (println M-Out)
                      
    (out M-Nm
      (pretty M-Out))))    
      
      
(de gen-uni-rx (Font-Nm)
  
  (let Match (mapcar pack 
               (split
                 (clip 
                   (delete "'" 
                     (in (list "fc-match" "--format='%{charset}'" Font-Nm)
                       (till))
                     T))
                  " "))
                  
    (setq Match (let L NIL
                  (for X Match
                    (push 'L (mapcar pack (split (chop X) "-"))))
                  (flip L)))

    (setq Match (let L NIL
                  (for X Match
                    (push 'L (if (= (length X) 1) 
                                 (list (hex (car X)))
                                 (list (hex (car X))
                                       (hex (cadr X))))))
                  (flip L)))))      
