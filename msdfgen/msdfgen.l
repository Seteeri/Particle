# pil msdfgen.l -main +

(de main ()
  
  (gc 32)
  
  (setq *Scl 6
        *num3 (** 10 *Scl)
        *sz-s "96"
        *sz-f (list (format "96.0" *Scl)
                    (format "96.0" *Scl))
        *path-msdfgen "/home/user/Git/msdfgen/build/msdfgen")
        
  # FiraMono
          
  (for Font 
       
       '(("/usr/share/fonts/TTF/DejaVuSansMono.ttf"                 "DejaVuSansMono"))
       
#{
      '(("/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf"            "DejaVuSansMono-Bold")
        ("/usr/share/fonts/TTF/DejaVuSansMono-BoldOblique.ttf"     "DejaVuSansMono-BoldOblique")
        ("/usr/share/fonts/TTF/DejaVuSansMono-Oblique.ttf"         "DejaVuSansMono-Oblique")
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
}#

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
      (call 'rm "-r" (pack *dir-out    "/"))
      
      (call 'mkdir "-p" (pack *dir-ppm    "/"))
      (call 'mkdir "-p" (pack *dir-png-af "/"))
      (call 'mkdir "-p" (pack *dir-m-af   "/"))    
      (call 'mkdir "-p" (pack *dir-png    "/"))
      (call 'mkdir "-p" (pack *dir-m      "/"))
      (call 'mkdir "-p" (pack *dir-l      "/")))
  
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
  
  (calc-m)
  
  (conv-ppm-lisp)
    
  T)


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

              # Error
              (when (= (car Metrics) "I")
                (println (pack Metrics))
                (bye))                            
                            
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
                                    *path-font
                                    (format U)
                                    "-scale"
                                    (format Sca *Scl)
                                    "-translate"
                                    (format (car Trans) *Scl)
                                    (format (cadr Trans) *Scl)
                                    "-size" *sz-s *sz-s
                                    "-printmetrics"
                                    "-o" Out-Nm)
                            (till)))
              
              # Error
              (when (= (car Metrics) "I")
                (println (pack Metrics))
                (bye))
              
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
          (prinl)))))


(de conv-ppm-lisp ()
  
  (for File (dir *dir-ppm)
  
    (let (In-Nm  (pack *dir-ppm "/" File)
          Out-Nm (pack *dir-l "/" File ".l"))

        (if (info Out-Nm)
        
            (prinl (pack "[4] Skip: " In-Nm))
            
            (let Data (nth (str (in In-Nm
                                  (till NIL T)))
                          5)
                          
              # Add alpha 255 then write to file
              (out Out-Nm
                (pretty 
                  (make
                    (while Data
                      (chain (cut 3 'Data))
                      (link 255)))))
              
              (prinl (pack "[4] PPM -> LISP: " Out-Nm)))))))
  

(de calc-m ()

  (for File (dir *dir-m)
  
    (let Out-M-Nm (pack *dir-m "/" (pack (head -1 (chop File))) "v.l")
    
      (if (info Out-M-Nm)
      
          (prinl (pack "[5] Skip: " Out-M-Nm))
          
          (let (In-M-Nm (pack *dir-m "/" File)
                Pr (car (str (pack (in In-M-Nm (till)))))
                Pr-Out (calc Pr))
    
            (out Out-M-Nm
              (pretty (getl Pr-Out)))
              
            (println File))))))

      
(de calc (In)

  # bnds: l b r t
  
  # http://pico-lisp.blogspot.com/p/arithmetic-operations.html
  
  (let (Out NIL
        Sca (get In 'scale)
        Tra (get In 'translate)
        Bnds (if (get In 'bounds) @ (need 4 0.0)))

    (put 'Out 'code (get In 'code))
    (put 'Out 'advance (get In 'advance))
    (put 'Out 'range (get In 'range))
    (put 'Out 'scale Sca)
    (put 'Out 'translate Tra)    
    
    # RENAME bnds -> bbox OR combine with bottom
    # sca * shape units = px
    # Bounds is rel to ori so neg nums possible
    (put 'Out 'bnds
          (list (*/ (get Bnds 1) Sca *num3)
                (*/ (get Bnds 2) Sca *num3)
                (*/ (get Bnds 3) Sca *num3)
                (*/ (get Bnds 4) Sca *num3)))

                
    # RENAME bnds-tex -> bbox-gly
    # bbox (for tex draw) rel to 
    # 1. Get origin 
    # 2. Add bnds
    # 3. Expand to df rx or shift?
    (put 'Out 'bnds-tex
          (list (+ (*/ (car  Tra) Sca *num3) (get 'Out 'bnds 1) (- *num3))
                (+ (*/ (cadr Tra) Sca *num3) (get 'Out 'bnds 2) (- *num3))
                (+ (*/ (car  Tra) Sca *num3) (get 'Out 'bnds 3) *num3)
                (+ (*/ (cadr Tra) Sca *num3) (get 'Out 'bnds 4) *num3)))
    
    # dims of bbox
    (put 'Out 'dims 
        (list (- (get 'Out 'bnds-tex 3) (get 'Out 'bnds-tex 1))
              (- (get 'Out 'bnds-tex 4) (get 'Out 'bnds-tex 2))))


    # l b r t = 1 2 3 4
    # ori=(0,0)=bottom left
    # so (1,1)=top right
    #
    # baseline.x - bounds.l
    # baseline.y - bounds.b
    # baseline.x + bounds.r
    # baseline.y + bounds.t
    (put 'Out 'uv (list (*/ *num3 (get 'Out 'bnds-tex 3) (car  *sz-f))
                        (*/ *num3 (get 'Out 'bnds-tex 4) (cadr *sz-f))
                        (format "0.0" *Scl)
                        (format "0.0" *Scl)
                        (*/ *num3 (get 'Out 'bnds-tex 3) (car  *sz-f))                        
                        (*/ *num3 (get 'Out 'bnds-tex 2) (cadr *sz-f))
                        (format "0.0" *Scl)
                        (format "0.0" *Scl)
                        (*/ *num3 (get 'Out 'bnds-tex 1) (car  *sz-f))
                        (*/ *num3 (get 'Out 'bnds-tex 2) (cadr *sz-f))
                        (format "0.0" *Scl)
                        (format "0.0" *Scl)
                        (*/ *num3 (get 'Out 'bnds-tex 1) (car  *sz-f))
                        (*/ *num3 (get 'Out 'bnds-tex 4) (cadr *sz-f))
                        (format "0.0" *Scl)
                        (format "0.0" *Scl)))
    
    'Out))
    
    
(de wr-met (U Metrics M-Nm Sca Tra)

  # Build property list out of metrics
  # Convert strings to numbers

  (setq Metrics (mapcar pack (split Metrics "^J")))
  
  (let M-Out (list (cons 'code U))
  
    (for Line Metrics
    
      (let ((Nm Val) (mapcar pack (split (chop Line) "="))
            Nm (pack (clip (chop Nm)))
            Vals-2 (mapcar pack (split (chop Val) ","))
            Vals (mapcar '((V)
                            (if (sub? "." V)
                                (format (pack (clip (chop V))) *Scl)
                                (format (pack (clip (chop V)) ".0") *Scl)))
                          Vals-2))
        
        # Check for exponent if number is very small
        
        (when Nm
          (push 'M-Out
                (cons (intern Nm)
                      (if (= (length Vals) 1)
                             (car Vals)
                             Vals))))))

    (when Sca
      (push 'M-Out
            (cons (intern "scale") Sca)))
      
    (when Tra
      (push 'M-Out
            (cons (intern "translate") Tra)))
    
    #(println Metrics)
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
