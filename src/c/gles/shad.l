(de cad-shader (Type
                Program
                Paths)

  (let (Shader (compile-shader-2 Type
                                  Paths)
        Info-Log (get-shader-info-log-2 Shader))

    # If reusing/caching shaders, need not delete
    (if Info-Log
        Info-Log
        (prog
            (attach-shader Program Shader)
           (delete-shader Shader)))))


(de compile-shader-2 (Type
                      Paths)
  (let Shader (create-shader Type)

    #Function will manage memory, need only pass file paths
    (shader-source-2 Shader
                     Paths)

    (compile-shader Shader)

    Shader))
