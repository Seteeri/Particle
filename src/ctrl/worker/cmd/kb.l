(de cmd-kb (Data)

  (let (Keycode (+ (get Data 4) 8)
        State   (get Data 5)
        Keysym  (get-one-sym> *xkb Keycode)  # or Data 7
        Name    (xkb~keysym-get-name Keysym)
        Utf-8   (get-utf8> *xkb Keycode))

    (update-key> *xkb Keycode State)

    # Dispatch bindings

    #(prinl-info "cmd-kb" (pack (str Data) " " Keysym))

    (when (or (= State xkb~KEY-DOWN)
              (= State KEY-REPEAT))

      (cond ((= Keysym ksd~End)       (cmd-pack))
            ((= Keysym ksd~Home)      (cmd-eval))
            ((= Keysym ksd~BackSpace) (cmd-del))
            ((= Keysym ksd~Delete)    (cmd-del))

            # HJKL

            ((= Keysym ksd~Right) (cmd-move-cam 1 '(0.25   0.0)))
            ((= Keysym ksd~Left)  (cmd-move-cam 1 '(-0.25  0.0)))
            ((= Keysym ksd~Up)    (cmd-move-cam 2 '(0.0    0.25)))
            ((= Keysym ksd~Down)  (cmd-move-cam 2 '(0.0   -0.25)))

            ((= Keysym ksd~Prior) (cmd-zoom-cam -0.3))
            ((= Keysym ksd~Next)  (cmd-zoom-cam 0.3))

            (T                    (cmd-char Keysym))))))


(de bind-keys (Keys Fn)
  #{

    For each key pressed, check bindings

    bind-keys((Ctrl X), Fn)

    Add to assoc list

  }#

  T)
