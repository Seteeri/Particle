(de cmd-kb (Data)

  (let (Keycode (+ (get Data 4) 8)
        State   (get Data 5)
        Keysym  (get-one-sym> *xkb Keycode)  # or Data 7
        Name    (xkb~keysym-get-name Keysym)
        Utf-8   (get-utf8> *xkb Keycode))

    (prinl-info "cmd-kb" (pack (str Data) " " Keysym))

    # Update states
    (update-key> *xkb Keycode State)
    
    # Note this returns the actual cell
    (with *xkb
      (let State-Xkb (assoc Keysym (: states))
        (when State-Xkb
          (del State-Xkb (:: states)))
        (push (:: states) (cons Keysym State))))

    # Check bindings and dispatch handlers

    # Find map
    (let Map (assoc (cons Keysym State) *binds)

      # CAR = key
      (when Map

        # Check each key
        (let (Dispatch T
              Chord (car (cdr Map))
              Fn    (cdr (cdr Map)))

          (with *xkb

            (for KeyState Chord

              (let (Ks    (car KeyState)
                    State (cdr KeyState)
                    State-Ks (cdr (assoc Ks (: states))))

                # No entry means key has not been pressed
                (when (not State-Ks) (setq State-Ks 0))

                (when (not (= State-Ks State))
                  (setq Dispatch NIL)))))

          (when Dispatch
            #(prinl-info "cmd-kb")
            #(println (car Map))
            (Fn Keysym)))))))


(de bind-keys (Map Fn)

  #{

      1. (X) . (()   Fn))
      2. (X) . (Ctrl Fn))

      Problem! -> 1 is always found first so 2 never executes

      Bitmask?

      1. (X) . ( (()   Fn)
                 (Ctrl Fn)
                 (Alt Fn)
                 (Shift Fn) )

      Hold Ctrl, Press X

      1. assoc ctrl -> no exist
      2. assoc x    -> list exist, assoc x list, check times
  }#

  # Get last key and put remainder
  (let Map-Flipped (flip Map)
    (push '*binds
          (cons (car Map-Flipped)
                (cons (cdr Map-Flipped)
                      Fn)))))


