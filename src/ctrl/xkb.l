# https://medium.com/@damko/a-simple-humble-but-comprehensive-guide-to-xkb-for-linux-6f1ad5e13450
# https://github.com/wayland-project/weston/blob/master/libweston/input.c

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

  (=: state   (xkb~state-new (: keymap)))

  (=: states  ()))


(dm unref> ()
  (xkb~state-unref   (: state))
  (xkb~keymap-unref  (: keymap))
  (xkb~context-unref (: ctx)))

(dm get-one-sym>      (Keycode)             (xkb~state-key-get-one-sym  (: state) Keycode))
(dm get-utf8>         (Keycode Buffer Size) (xkb~state-key-get-utf8     (: state) Keycode Buffer Size))
(dm update-key>       (Keycode Direction)   (xkb~state-update-key       (: state) Keycode Direction))
(dm key-repeats>      (Keycode)             (xkb~keymap-key-repeats     (: keymap) Keycode))
(dm serialize-mods>   (Components)          (xkb~state-serialize-mods   (: state) Components))
(dm serialize-layout> (Components)          (xkb~state-serialize-layout (: state) Components))
(dm state-mod-name-is-active> (Name Type) (xkb~state-mod-name-is-active (: state) Name Type))

(dm is-shift-active> () (state-mod-name-is-active> This xkb~MOD-NAME-SHIFT xkb~STATE-MODS-EFFECTIVE))
(dm is-caps-active>  () (state-mod-name-is-active> This xkb~MOD-NAME-CAPS  xkb~STATE-MODS-EFFECTIVE))
(dm is-ctrl-active>  () (state-mod-name-is-active> This xkb~MOD-NAME-CTRL  xkb~STATE-MODS-EFFECTIVE))
(dm is-alt-active>   () (state-mod-name-is-active> This xkb~MOD-NAME-ALT   xkb~STATE-MODS-EFFECTIVE))
(dm is-num-active>   () (state-mod-name-is-active> This xkb~MOD-NAME-NUM   xkb~STATE-MODS-EFFECTIVE))
(dm is-logo-active>  () (state-mod-name-is-active> This xkb~MOD-NAME-LOGO  xkb~STATE-MODS-EFFECTIVE))

#{
(println
(serialize-mods> *xkb xkb~STATE-MODS-DEPRESSED)
(serialize-mods> *xkb xkb~STATE-MODS-LATCHED)
(serialize-mods> *xkb xkb~STATE-MODS-LOCKED)
(serialize-layout> *xkb xkb~STATE-LAYOUT-DEPRESSED)
(serialize-layout> *xkb xkb~STATE-LAYOUT-LATCHED)
(serialize-layout> *xkb xkb~STATE-LAYOUT-LOCKED)
(serialize-layout> *xkb xkb~STATE-LAYOUT-EFFECTIVE))
}#