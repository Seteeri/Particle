(de init-comp ()
    (setq *prog-comp (gl~init-prog-comp *paths-comp-shader))
    (gl~use-program *prog-comp)
    (init-buff-compute params-model))


(de init-buffs-comp ()
    T)
