(de point-b-prv ()
  (println 'point-b-prv)

  (setq *on-car)

  (let (Ref (get *0 'b)
        Prv (get Ref 'c))

    (when Prv

      # If symbol

      # Set list and line
      (let? Ln (get Ref 'nl 'p)
        (setq *line Ln
              *main Ln)
        (println 'point-b-prv "Mov to super-line/list" Ln (get Ln 'data)))

      (println 'point-b-prv Prv (get Prv 'data))
      (put *0 'b Prv)
      (mov-part-abv> *0 Prv (cons T)))))


(de point-b-nxt ()
  (println 'point-b-nxt)

  (setq *on-car)

  # If ref is NIL, aka end of list, move to superlist
  (let Ref (get *0 'b)

    (if (= (type Ref) '(+Nil))

        (let? Cb (get *line 'c 'b)
          (setq *line (get *line 'nl 'p)
                *main *line)
          (println 'point-b-nxt Cb (get Cb 'data))
          (put *0 'b Cb)
          (mov-part-abv> *0 Cb (cons T)))

        (let? Cdr (get Ref 'b)

          # Set line
          (when (get Cdr 'nl)
            (setq *line Cdr
                  *main Cdr)
            (println 'point-b-nxt "Mov to sub-line/list" *line (get *line 'data)))

          (println 'point-b-nxt Cdr (get Cdr 'data))
          (put *0 'b Cdr)
          (mov-part-abv> *0 Cdr (cons T))))))


(de point-a-out ()
  (println 'point-a-out)

  (point-b-prv))


(de point-a-in ()

  (println 'point-a-in)

  # Clean this up!

  (let Ref (get *0 'b)

    (if (isa '+Nil Ref)
        
        (prog
          (setq *on-car T)
        
          # Keep ptr but move it
          (if (= (get Ref 'lay) 'x)

              (prog
                # Move to lower bnd of Car
                (set-cur-x (get Ref 'ori 1))
                (set-cur-y (get (bounds> Ref) 2)) (nl-cur)
                (lay-part-start *0 *cur 'skip-cdr))

              (prog
                (mov-part-abv> *0 Ref (cons T)))))

        (let (Car (get Ref 'a)
              Dims (bounds> Car))

          (put *0 'b Car)

          # Set list
          (if (= (type Car) '(+Pair))

            (prog
              # Dont set Car flag
              (println 'point-a-in "Mov to sub-line/list" Car (get Car 'data))
              (setq *line Car
                    *main *line)
              # Check layout
              (mov-part-abv> *0 Car (cons T)))

            (prog
              (setq *on-car T)
              (if (= (get Ref 'lay) 'x)

                  (prog
                    # Move to lower bnd of Car
                    (set-cur-x (get Car 'ori 1))
                    (set-cur-y (get Dims 2)) (nl-cur)
                    (lay-part-start *0 *cur 'skip-cdr))

                  (prog
                    # Move above Car
                    (mov-part-abv> *0 Car (cons T))))))))))
