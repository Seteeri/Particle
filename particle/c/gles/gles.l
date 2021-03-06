#{

Naming Conventions:

* gl{fn-name-wo-gl-prefix} = original function call

* {fn-name-wo-gl-prefix} = direct wrapper

* {fn-name-wo-gl-prefix}-2 = extended wrapper

* {fn-name-wo-gl-prefix}-{first-arg} = name conflict

}#

(symbols 'gl 'pico)

(local) (clear)

(load (pack (pwd) "/particle/c/gles/buffer.l"))
(load (pack (pwd) "/particle/c/gles/shad.l"))
(load (pack (pwd) "/particle/c/gles/prog.l"))

(def '*glBufferStorage 0)


(de blend-equation-separate (Mode-RGB Mode-A)
  (native "libGLESv2.so.2" "glBlendEquationSeparate" NIL Mode-RGB Mode-A))

  
(de blend-equation (Mode)
  (native "libGLESv2.so.2" "glBlendEquation" NIL Mode))

  
(de blend-func-separate (Src-RGB Dst-RGB Src-A Dst-A)
  (native "libGLESv2.so.2" "glBlendFuncSeparate" NIL Src-RGB Dst-RGB Src-A Dst-A))
  
  
(de blend-func (Sfactor Dfactor)
  (native "libGLESv2.so.2" "glBlendFunc" NIL Sfactor Dfactor))  
  

(de viewport (X Y Width Height)
  (native "libGLESv2.so.2" "glViewport" NIL X Y Width Height))


(de enable (Cap)
  (native "libGLESv2.so.2" "glEnable" NIL Cap))


(de disable (Cap)
  (native "libGLESv2.so.2" "glDisable" NIL Cap))


(de clear (Mask)
  (native "libGLESv2.so.2" "glClear" NIL Mask))


(de clear-color (Red Green Blue Alpha)
  (native "libGLESv2.so.2" "glClearColor" NIL
	    (cons Red -1.0)
	    (cons Green -1.0)
	    (cons Blue -1.0)
	    (cons Alpha -1.0)))
    
#{
(de get-num (Args Data)
    #Args = list
    #Data = num or symbol
    (if (num? Data)
	(apply 'native Args)
	(prog
	    (println 'native Args (list 'Data-2 Data))
	    (apply 'native Args (list 'Data-2 Data))
	   Data-2)))


(de get-boolean-v (Pname Data)
  (native "libGLESv2.so.2" "glGetBooleanv" NIL Pname '(Ret (1) . 0))
    Ret)


(de get-boolean-v (Pname Data)
    (get-num (list "libGLESv2.so.2" "glGetBooleanv" NIL Pname)
	     (if Data @ (cons 1 'B))))
}#


(de get-boolean-v (Pname Data)
  (if Data
    (native "libGLESv2.so.2" "glGetBooleanv" NIL Pname Data)
    (let Data-2 (native "@" "malloc" 'N 2)
      (native "libGLESv2.so.2" "glGetBooleanv" NIL Pname Data-2)
      (let Ret (struct Data-2 'B)
        (native "@" "free" NIL Data-2)
        Ret))))


(de get-float-v (Pname Data)
  (native "libGLESv2.so.2" "glGetFloatv" NIL Pname Data))


(de get-integer-v (Pname Data)
  (native "libGLESv2.so.2" "glGetIntegerv" NIL Pname Data))


(de get-integer64-v (Pname Data)
  (native "libGLESv2.so.2" "glGetInteger64v" NIL Pname Data))


(de get-integer-i-v (Target Index Data)
  (native "libGLESv2.so.2" "glGetIntegeri_v" NIL Target Index Data))


(de get-integer64-i-v (Target Index Data)
  (native "libGLESv2.so.2" "glGetInteger64i_v" NIL Target Index Data))


(de get-string (Name)
  (native "libGLESv2.so.2" "glGetString" 'N Name))


(de get-string-i (Name Index)
  (native "libGLESv2.so.2" "glGetStringi" 'N Name Index))

#{
(de gen-buffers (N)
    (let Buffers (native "@" "malloc" 'N (* 4 N))
	 (native "libGLESv2.so.2" "glGenBuffers" NIL N Buffers)
	 (let Ret (struct Buffers '())
	    (native "@" "free" NIL Buffers)
	      Ret)))
}#

(de gen-buffers (N)
    #TODO: Use more idiomatic method
  (native "libGLESv2.so.2" "glGenBuffers" NIL N (list 'Buffers (cons (* 4 N) 'I N)))
    Buffers)


(de bind-buffer (Target Buffer)
  (native "libGLESv2.so.2" "glBindBuffer" NIL Target Buffer))


(de bind-texture (Target Texture)
  (native "libGLESv2.so.2" "glBindTexture" NIL Target Texture))


(de bind-buffer-base (Target Index Buffer)
  (native "libGLESv2.so.2" "glBindBufferBase" NIL Target Index Buffer))


(de buffer-storage (Target Size Data Flags)
  (native "libGLESv2.so.2" *glBufferStorage NIL Target Size Data Flags))


(de unmap-buffer (Target)
  #Returns GLboolean = 1 byte
  (native "libGLESv2.so.2" "glUnmapBuffer" 'B Target))


(de map-buffer-range (Target Offset Length Access)
  (native "libGLESv2.so.2" "glMapBufferRange" 'N Target Offset Length Access))


(de buffer-data (Target Size Data Usage)
  (native "libGLESv2.so.2" "glBufferData" Target Size Data Usage))


(de active-texture (Texture)
  (native "libGLESv2.so.2" "glActiveTexture" NIL Texture))


(de tex-buffer (Target Internal-format Buffer)
  (native "libGLESv2.so.2" "glTexBuffer" NIL Target Internal-format Buffer))


(de create-program ()
  (native "libGLESv2.so.2" "glCreateProgram" 'I))


(de delete-program (Program)
  (native "libGLESv2.so.2" "glDeleteProgram" NIL Program))


(de use-program (Program)
  (native "libGLESv2.so.2" "glUseProgram" NIL Program))


(de link-program (Program)
  (native "libGLESv2.so.2" "glLinkProgram" NIL Program))


#(de get-program-iv (Program Pname Params)
#  #TODO: Need not use list for struct
#  (native "libGLESv2.so.2" "glGetProgramiv" NIL Program Pname (list 'Params (cons 4 'I 1)))
#    (car Params))


(de get-info-log (Max-Length Fn Target)
  (when (> Max-Length 0)
    (let (Info-Log   (native "@" "malloc" 'N (+ Max-Length 1))
          Length   (native "@" "malloc" 'N 4))
      (native "libGLESv2.so.2" Fn NIL Target Max-Length Length Info-Log)
      # Length does not include null char
      (when (> (car (struct Length '(I))) 0)
        # String is already null-terminated
        (let Ret (struct Info-Log 'S)
          (native "@" "free" NIL Length)
          (native "@" "free" NIL Info-Log)
          Ret)))))


(de get-program-info-log-2 (Program)
  (get-info-log (get-program-iv Program INFO-LOG-LENGTH)
    "glGetProgramInfoLog"
    Program))


(de create-shader (Shader-type)
  (native "libGLESv2.so.2" "glCreateShader" 'I Shader-type))


(de compile-shader (Shader)
  (native "libGLESv2.so.2" "glCompileShader" NIL Shader))


(de get-shader-iv (Shader Pname Params)
  #TODO: Need not use list for struct
  (native "libGLESv2.so.2" "glGetShaderiv" NIL Shader Pname (list 'Params (cons 4 'I 1)))
    (car Params))


(de get-shader-info-log-2 (Shader)
  (get-info-log (get-shader-iv Shader INFO-LOG-LENGTH)
                "glGetShaderInfoLog"
                Shader))


(de attach-shader (Program Shader)
  (native "libGLESv2.so.2" "glAttachShader" NIL Program Shader))


(de shader-source-2 (Shader Paths)
  #{
    * Read paths into string arrays:
    count = number in array
    string = array of strs
    length = array of corresponding str len
  }#
  (let (Count  (length Paths)
        String (native "@" "malloc" 'N (* Count 8))
        Length (native "@" "malloc" 'N (* Count 4))
        Ptr-Str String
        Ptr-Len Length
        Ls-Free ())

    (for Path Paths
   
       # Get file size from stat then allocate
       # Poss to use mmap to minimize intermittent allocation

       (let (Sz-f  (stat-size Path)
             Buf   (native "@" "malloc" 'N (+ Sz-f 1)) # + null character
             File  (native "@" "fopen" 'I Path "rb"))
               
             (native "@" "fread"  NIL Buf Sz-f 1 File)
             (native "@" "fclose" NIL File)
             
             # Null terminate after reading
             (struct (+ Buf Sz-f) 'N (cons 0 1))
             
             # Set String ptr to point to Buffer
             (struct Ptr-Str 'N (cons Buf 8))

             # Set Buffer Length ptr (w/o null char)
             (struct Ptr-Len 'N (cons Sz-f 4))
             
             # Push ptrs to lst-free
             (push 'Ls-Free Buf)

             #(prinl "shader-source-2 | fread " Path " " Sz-f " bytes")
             (inc 'Ptr-Str 8)
             (inc 'Ptr-Len 4)))

     # TODO: Fix using Length
     (native "libGLESv2.so.2" "glShaderSource" NIL Shader Count String Length)
      
    (for Ptr Ls-Free (native "@" "free" NIL Ptr))
    (native "@" "free" NIL String)
    (native "@" "free" NIL Length)))


(de stat-size (Path)
  (pipe (call "stat" "--format" "\"%s\"" Path)
        (format (read))))


(de delete-shader (Shader)
  (native "libGLESv2.so.2" "glDeleteShader" NIL Shader))


(de gen-vertex-arrays (N)
  (native "libGLESv2.so.2" "glGenVertexArrays" NIL N (list 'Arrays (cons (* 4 N) 'I N)))
    Arrays)


(de bind-vertex-array (Array)
  (native "libGLESv2.so.2" "glBindVertexArray" NIL Array))


(de draw-elements-indirect (Mode Type Indirect)
  (native "libGLESv2.so.2" "glDrawElementsIndirect" NIL Mode Type Indirect))


(de fence-sync (Condition Flags)
  (native "libGLESv2.so.2" "glFenceSync" 'N Condition Flags))


(de delete-sync (Sync)
  (native "libGLESv2.so.2" "glDeleteSync" NIL Sync))


(de client-wait-sync (Sync Flags Timeout)
  (native "libGLESv2.so.2" "glClientWaitSync" 'I Sync Flags Timeout))


(de client-wait (Sync)
  # Check first, then flush only if necessary
  (let (Flags 0
        Timeout 0
        Return NIL)
          (loop
             (setq Return (client-wait-sync Sync Flags Timeout))

             # Return when this is satisfied
             # WAIT-FAILED also?
             (T (or (= Return ALREADY-SIGNALED)
              (= Return CONDITION-SATISFIED)))
             
             (setq Flags   SYNC-FLUSH-COMMANDS-BIT)
             (setq Timeout TIMEOUT-IGNORED))))


(de get-program-iv (Program Pname Params)
  (native "libGLESv2.so.2" "glGetProgramiv" NIL Program Pname Params))


(de get-program-binary (Program Bufsize Length Binary-Format Binary)
  (native "libGLESv2.so.2" "glGetProgramBinary" NIL Program Bufsize Length Binary-Format Binary))


(de program-binary (Program Binary-Format Binary Length)
  (native "libGLESv2.so.2" "glProgramBinary" NIL Program Binary-Format Binary Length))


(de up-prg (Paths-Vert Paths-Frag Path-Bin)

  (let (Program (create-program)
        Sz-f  (stat-size Path-Bin)
        Buf   (native "@" "malloc" 'N (+ Sz-f 1))
        File  (native "@" "fopen" 'I Path-Bin "rb"))
        
    (native "@" "fread"  NIL Buf 1 Sz-f File)
    (native "@" "fclose" NIL File)

    (program-binary Program
                    (car (struct Buf '(I . 1)))
                    (+ Buf 4)
                    (- Sz-f 4))

    (native "@" "free" NIL Buf)

    # (prinl-info "up-prg" (pack Sz-f " " (car (struct Buf '(I . 1)))))

    # If it fails, use dl-prg
    (if (query-program-info-log Program) # or check link status
      (prog
        (println "up-prg" (pack "Failed to load program: " Path-Bin))
        (delete-program Program)
        (dl-prg Paths-Vert Paths-Frag Path-Bin))
      Program)))
  

(de dl-prg (Paths-Vert Paths-Frag Path-Bin)
  
  (let (Program (init-prog-rast Paths-Vert Paths-Frag)
        Iv (native "@" "malloc" 'N 4))
  
    (get-program-iv Program PROGRAM-BINARY-LENGTH Iv)
    
    (let (Bufsize (car (struct Iv '(I . 1)))
          Binary-Format (native "@" "malloc" 'N 4)
          Binary (native "@" "malloc" 'N Bufsize))
          
      (get-program-binary Program Bufsize 0 Binary-Format Binary)

      (let File (native "@" "fopen" 'I Path-Bin "wb")
        (native "@" "fwrite"  NIL Binary-Format 1 4       File)
        (native "@" "fwrite"  NIL Binary        1 Bufsize File)
        (native "@" "fclose" NIL File))

      (native "@" "sync")

      #(prinl-info "dl-prg" (pack "Saved " (+ 4 Bufsize) " bytes to " Path-Bin))
      
      (native "@" "free" NIL Iv)
      (native "@" "free" NIL Binary-Format)
      (native "@" "free" NIL Binary))

    Program))
   
    
(mapc '((X) (def (car X) (hex (cadr X))))
      (quote
       
       (DEPTH-BUFFER-BIT                        "00000100")
       (STENCIL-BUFFER-BIT                      "00000400")
       (COLOR-BUFFER-BIT                        "00004000")
       (ATOMIC-COUNTER-BUFFER                   "92C0")
       (DRAW-INDIRECT-BUFFER                    "8F3F")
       (ELEMENT-ARRAY-BUFFER                    "8893")
       (SHADER-STORAGE-BUFFER                   "90D2")
       (UNIFORM-BUFFER                          "8A11")
       (TEXTURE-BUFFER                          "8C2A")
       (UNIFORM-BUFFER                          "8A11")
       (TEXTURE0                                "84C0")
       (RGBA8                                   "8058")
       (INFO-LOG-LENGTH                         "8B84")
       (SYNC-CONDITION                          "9113")
       (SYNC-STATUS                             "9114")
       (SYNC-FLAGS                              "9115")
       (SYNC-FENCE                              "9116")
       (SYNC-GPU-COMMANDS-COMPLETE              "9117")
       (UNSIGNALED                              "9118")
       (SIGNALED                                "9119")
       (ALREADY-SIGNALED                        "911A")
       (TIMEOUT-EXPIRED                         "911B")
       (CONDITION-SATISFIED                     "911C")
       (WAIT-FAILED                             "911D")
       (TIMEOUT-IGNORED                         "FFFFFFFFFFFFFFFFull")
       (SYNC-FLUSH-COMMANDS-BIT                 "00000001")
       (PROGRAM-BINARY-RETRIEVABLE-HINT         "8257")
       (PROGRAM-BINARY-LENGTH                   "8741")
       (NUM-PROGRAM-BINARY-FORMATS              "87FE")
       (PROGRAM-BINARY-FORMATS                  "87FF")
       (FUNC-ADD                                "8006")
       
       (FALSE                                   "0000")
       (TRUE                                    "0001")
       (POINTS                                  "0000")
       (LINES                                   "0001")
       (LINE-LOOP                               "0002")
       (LINE-STRIP                              "0003")
       (TRIANGLES                               "0004")
       (TRIANGLE-STRIP                          "0005")
       (TRIANGLE-FAN                            "0006")
       (QUADS                                   "0007")
       (PATCHES                                 "000E")
       (DEPTH-BUFFER-BIT                        "0100")
       (ZERO                                    "0000")
       (NONE                                    "0000")
       (ONE                                     "0001")
       (LESS                                    "0201")
       (EQUAL                                   "0202")
       (LEQUAL                                  "0203")
       (GREATER                                 "0204")
       (NOTEQUAL                                "0205")
       (GEQUAL                                  "0206")
       (ALWAYS                                  "0207")
       (SRC-COLOR                               "0300")
       (ONE-MINUS-SRC-COLOR                     "0301")
       (SRC-ALPHA                               "0302")
       (ONE-MINUS-SRC-ALPHA                     "0303")
       (DST-ALPHA                               "0304")
       (ONE-MINUS-DST-ALPHA                     "0305")
       (DST-COLOR                               "0306")
       (ONE-MINUS-DST-COLOR                     "0307")
       (SRC-ALPHA-SATURATE                      "0308")
       (SRC-ALPHA                               "0302")
       (ONE-MINUS-SRC-ALPHA                     "0303")
       (STENCIL-BUFFER-BIT                      "0400")
       (FRONT                                   "0404")
       (BACK                                    "0405")
       (FRONT-AND-BACK                          "0408")
       (CW                                      "0900")
       (CCW                                     "0901")
       (DEPTH-TEST                              "0B71")
       (CULL-FACE                               "0B44")
       (FRONT-FACE                              "0B46")
       (BLEND                                   "0BE2")
       (UNPACK-ROW-LENGTH                       "0CF2")
       (UNPACK-ALIGNMENT                        "0CF5")
       (TEXTURE-2D                              "0DE1")
       (UNSIGNED-BYTE                           "1401")
       (SHORT                                   "1402")
       (UNSIGNED-SHORT                          "1403")
       (INT                                     "1404")
       (UNSIGNED-INT                            "1405")
       (FLOAT                                   "1406")
       (DEPTH-COMPONENT                         "1902")
       (RED                                     "1903")
       (GREEN                                   "1904")
       (BLUE                                    "1905")
       (ALPHA                                   "1906")
       (RGB                                     "1907")
       (RGBA                                    "1908")
       (POINT                                   "1B00")
       (LINE                                    "1B01")
       (FILL                                    "1B02")
       (NEAREST                                 "2600")
       (LINEAR                                  "2601")
       (NEAREST-MIPMAP-NEAREST                  "2700")
       (LINEAR-MIPMAP-NEAREST                   "2701")
       (NEAREST-MIPMAP-LINEAR                   "2702")
       (LINEAR-MIPMAP-LINEAR                    "2703")
       (TEXTURE-MAG-FILTER                      "2800")
       (TEXTURE-MIN-FILTER                      "2801")
       (TEXTURE-WRAP-S                          "2802")
       (TEXTURE-WRAP-T                          "2803")
       (REPEAT                                  "2901")
       (COLOR-BUFFER-BIT                        "4000")
       (RGB8                                    "8051")
       (RGBA8                                   "8058")
       (TEXTURE-3D                              "806F")
       (TEXTURE-WRAP-R                          "8072")
       (BGR                                     "80E0")
       (BGRA                                    "80E1")
       (CLAMP-TO-EDGE                           "812F")
       (TEXTURE-BASE-LEVEL                      "813C")
       (TEXTURE-MAX-LEVEL                       "813D")
       (R8                                      "8229")
       (INTERNALFORMAT-PREFERRED                "8270")
       (COMPRESSED-RGB-S3TC-DXT1-EXT            "83F0")
       (COMPRESSED-RGBA-S3TC-DXT1-EXT           "83F1")
       (COMPRESSED-RGBA-S3TC-DXT3-EXT           "83F2")
       (COMPRESSED-RGBA-S3TC-DXT5-EXT           "83F3")
       (TEXTURE0                                "84C0")
       (ACTIVE-TEXTURE                          "84E0")
       (TEXTURE-CUBE-MAP                        "8513")
       (TEXTURE-CUBE-MAP-POSITIVE-X             "8515")
       (BUFFER-SIZE                             "8764")
       (DRAW-BUFFER0                            "8825")
       (ARRAY-BUFFER                            "8892")
       (ELEMENT-ARRAY-BUFFER                    "8893")
       (READ-ONLY                               "88B8")
       (WRITE-ONLY                              "88B9")
       (READ-WRITE                              "88BA")
       (STREAM-DRAW                             "88E0")
       (STREAM-READ                             "88E1")
       (STREAM-COPY                             "88E2")
       (STATIC-DRAW                             "88E4")
       (STATIC-READ                             "88E5")
       (STATIC-COPY                             "88E6")
       (DYNAMIC-DRAW                            "88E8")
       (DYNAMIC-READ                            "88E9")
       (DYNAMIC-COPY                            "88EA")
       (UNIFORM-BUFFER                          "8A11")
       (FRAGMENT-SHADER                         "8B30")
       (VERTEX-SHADER                           "8B31")
       (TEXTURE-2D-ARRAY                        "8C1A")
       (READ-FRAMEBUFFER                        "8CA8")
       (DRAW-FRAMEBUFFER                        "8CA9")
       (DEPTH-COMPONENT32F                      "8CAC")
       (FRAMEBUFFER-COMPLETE                    "8CD5")       
       (COLOR-ATTACHMENT0                       "8CE0")
       (FRAMEBUFFER                             "8D40")
       (RENDERBUFFER                            "8D41")
       (GEOMETRY-SHADER                         "8DD9")
       (COMPILE-STATUS                          "8B81")
       (LINK-STATUS                             "8B82")
       (PATCH-VERTICES                          "8E72")
       (TESS-EVALUATION-SHADER                  "8E87")
       (TESS-CONTROL-SHADER                     "8E88")
       (SYNC-GPU-COMMANDS-COMPLETE              "9117")
       (COMPUTE-SHADER                          "91B9")
       (MAP-READ-BIT                            "0001")
       (MAP-WRITE-BIT                           "0002")
       (MAP-INVALIDATE-RANGE-BIT                "0004")
       (MAP-INVALIDATE-BUFFER-BIT               "0008")
       (MAP-FLUSH-EXPLICIT-BIT                  "0010")
       (MAP-UNSYNCHRONIZED-BIT                  "0020")
       (MAP-PERSISTENT-BIT                      "0040")
       (MAP-COHERENT-BIT                        "0080")
       (DYNAMIC-STORAGE-BIT                     "0100")
       (CLIENT-STORAGE-BIT                      "0200")))
