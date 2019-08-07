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

    Bind Ctrl + X

    Press Ctrl
          -> Check Ctrl events
          X
          -> Check X events
          Y
          -> Check Y events

    Must add backwards

    X must come after Ctrl

  }#

  #{
    ((ksd~Control-L . xkb~KEY-DOWN)
     (ksd~x         . xkb~KEY-DOWN))

     (X 1) : ( ((Ctrl-L State) ...) Fn)
  }#

  # Get last key and put remainder
  (let Map-Flipped (flip Map)
    (push '*binds
          (cons (car Map-Flipped)
                (cons (cdr Map-Flipped)
                      Fn)))))


