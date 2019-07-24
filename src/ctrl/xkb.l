(class +Xkb)
(dm T ()
  
  (=: context (xkb~context-new xkb~CONTEXT-NO-FLAGS))

  (when NIL
    (let (Rule-Names (native "@" "malloc" 'N 'xkb~SZ-RULE-NAMES)
          Rules      (native "@" "malloc" 'N 8)
          Model      (native "@" "malloc" 'N 8) 
          Layout     (native "@" "malloc" 'N 8))
      (struct Rule-Names 'N 
                         (cons (struct Rules 'N (cons "evdev" 5)) 8)
                         (cons (struct Model 'N (cons "pc105" 5)) 8)
                         (cons (struct Layout 'N (cons "us" 2))   8)
                         (cons 0 8)
                         (cons 0 8))))

  (=: keymap  (xkb~keymap-new-from-names (: context) 0 xkb~KEYMAP-COMPILE-NO-FLAGS))
    
  (=: state   (xkb~state-new (: keymap))))


(dm unref> ()
  (xkb~state-unref   (: state))
  (xkb~keymap-unref  (: keymap))
  (xkb~context-unref (: ctx)))


(dm get-one-sym> (Keycode)

  (xkb~state-key-get-one-sym (: state) Keycode))
  
  
(dm get-utf8> (Keycode Buffer Size)
  
  (xkb~state-key-get-utf8 (: state) Keycode Buffer Size))
  
  
(dm update-key> (Keycode Direction)

  (xkb~state-update-key (: state) Keycode Direction))
  
  
(dm key-repeats> (Keycode)
  
  (xkb~keymap-key-repeats (: keymap) Keycode))
  
  
(dm serialize-mods> (Components)

  (xkb~state-serialize-mods (: state) Components))
  
  
(dm serialize-layout> (Components)

  (xkb~state-serialize-layout (: state) Components))
  
#(serialize-mods> *xkb 'xkb~STATE-MODS-DEPRESSED) # depressed-mods
#(serialize-mods> *xkb 'xkb~STATE-MODS-LATCHED) # latched-mods
#(serialize-mods> *xkb 'xkb~STATE-MODS-LOCKED) # locked-mods
#(serialize-layout> *xkb 'xkb~STATE-LAYOUT-EFFECTIVE) # mods-group  