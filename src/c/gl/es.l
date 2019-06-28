(symbols 'gl 'pico)


(load "/home/user/quicklisp/local-projects/protoform/src/c/gl/bo.l")


(de viewport (X Y Width Height)
    (native "libGLESv2.so" "glViewport" NIL X Y Width Height))


(de enable (Cap)
    (native "libGLESv2.so" "glEnable" NIL Cap))


(de disable (Cap)
    (native "libGLESv2.so" "glDisable" NIL Cap))


(de blend-func (Sfactor Dfactor)
    (native "libGLESv2.so" "glBlendFunc" NIL Sfactor Dfactor))


#Why does "clear" not work?
(de clear-mask (Mask)
    (native "libGLESv2.so" "glClear" NIL Mask))


(de clear-color (Red Green Blue Alpha)
    (native "libGLESv2.so" "glClearColor" NIL
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
    (native "libGLESv2.so" "glGetBooleanv" NIL Pname '(Ret (1) . 0))
    Ret)


(de get-boolean-v (Pname Data)
    (get-num (list "libGLESv2.so" "glGetBooleanv" NIL Pname)
	     (if Data Data (cons 1 'B))))
}#


(de get-boolean-v (Pname Data)
    (if Data
	(native "libGLESv2.so" "glGetBooleanv" NIL Pname Data)
	(let Data-2 (native "@" "malloc" 'N 2)
	     (native "libGLESv2.so" "glGetBooleanv" NIL Pname Data-2)
	     (let Ret (struct Data-2 'B)
		  (native "@" "free" NIL Data-2)
		  Ret))))


(de get-float-v (Pname Data)
    (native "libGLESv2.so" "glGetFloatv" NIL Pname Data))


(de get-integer-v (Pname Data)
    (native "libGLESv2.so" "glGetIntegerv" NIL Pname Data))


(de get-integer64-v (Pname Data)
    (native "libGLESv2.so" "glGetInteger64v" NIL Pname Data))


(de get-integer-i-v (Target Index Data)
    (native "libGLESv2.so" "glGetIntegeri_v" NIL Target Index Data))


(de get-integer64-i-v (Target Index Data)
    (native "libGLESv2.so" "glGetInteger64i_v" NIL Target Index Data))


(de get-string (Name)
    (native "libGLESv2.so" "glGetString" 'N Name))


(de get-string-i (Name Index)
    (native "libGLESv2.so" "glGetStringi" 'N Name Index))

#{
(de gen-buffers (N)
    (let Buffers (native "@" "malloc" 'N (* 4 N))
	 (native "libGLESv2.so" "glGenBuffers" NIL N Buffers)
	 (let Ret (struct Buffers '())
	      (native "@" "free" NIL Buffers)
	      Ret)))
}#

(de gen-buffers (N)
    #TODO: Use more idiomatic method
    (native "libGLESv2.so" "glGenBuffers" NIL N (list 'Buffers (cons (* 4 N) 'I N)))
    Buffers)


(de bind-buffer (Target Buffer)
    (native "libGLESv2.so" "glBindBuffer" NIL Target Buffer))


(de bind-texture (Target Texture)
    (native "libGLESv2.so" "glBindTexture" NIL Target Texture))


(de bind-buffer-base (Target Index Buffer)
    (native "libGLESv2.so" "glBindBufferBase" NIL Target Index Buffer))


(de buffer-storage (Target Size Data Flags)
    (native "libGLESv2.so" "glBufferStorage" NIL Target Size Data Flags))


(de unmap-buffer (Target)
    #Returns GLboolean = 1 byte
    (native "libGLESv2.so" "glUnmapBuffer" 'B Target))


(de map-buffer-range (Target Offset Length Access)
    (native "libGLESv2.so" "glMapBufferRange" NIL Target Offset Length Access))


(de buffer-data (Target Size Data Usage)
    (native "libGLESv2.so" "glBufferData" Target Size Data Usage))


(de active-texture (Texture)
    (native "libGLESv2.so" "glActiveTexture" NIL Texture))


(de tex-buffer (Target Internal-format Buffer)
    (native "libGLESv2.so" "glTexBuffer" NIL Target Internal-format Buffer))




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
       
       (FALSE                                   "0000")
       (TRUE                                    "0001")
       (POINTS                                  "0000")
       (LINES                                   "0001")
       (LINE_LOOP                               "0002")
       (LINE_STRIP                              "0003")
       (TRIANGLES                               "0004")
       (TRIANGLE_STRIP                          "0005")
       (TRIANGLE_FAN                            "0006")
       (QUADS                                   "0007")
       (PATCHES                                 "000E")
       (DEPTH_BUFFER_BIT                        "0100")
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
       (SRC_COLOR                               "0300")
       (ONE_MINUS_SRC_COLOR                     "0301")
       (SRC_ALPHA                               "0302")
       (ONE_MINUS_SRC_ALPHA                     "0303")
       (DST_ALPHA                               "0304")
       (ONE_MINUS_DST_ALPHA                     "0305")
       (DST_COLOR                               "0306")
       (ONE_MINUS_DST_COLOR                     "0307")
       (SRC_ALPHA_SATURATE                      "0308")
       (SRC_ALPHA                               "0302")
       (ONE_MINUS_SRC_ALPHA                     "0303")
       (STENCIL_BUFFER_BIT                      "0400")
       (FRONT                                   "0404")
       (BACK                                    "0405")
       (FRONT_AND_BACK                          "0408")
       (CW                                      "0900")
       (CCW                                     "0901")
       (DEPTH_TEST                              "0B71")
       (CULL_FACE                               "0B44")
       (FRONT_FACE                              "0B46")
       (BLEND                                   "0BE2")
       (UNPACK_ROW_LENGTH                       "0CF2")
       (UNPACK_ALIGNMENT                        "0CF5")
       (TEXTURE_2D                              "0DE1")
       (UNSIGNED_BYTE                           "1401")
       (SHORT                                   "1402")
       (UNSIGNED_SHORT                          "1403")
       (INT                                     "1404")
       (UNSIGNED_INT                            "1405")
       (FLOAT                                   "1406")
       (DEPTH_COMPONENT                         "1902")
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
       (NEAREST_MIPMAP_NEAREST                  "2700")
       (LINEAR_MIPMAP_NEAREST                   "2701")
       (NEAREST_MIPMAP_LINEAR                   "2702")
       (LINEAR_MIPMAP_LINEAR                    "2703")
       (TEXTURE_MAG_FILTER                      "2800")
       (TEXTURE_MIN_FILTER                      "2801")
       (TEXTURE_WRAP_S                          "2802")
       (TEXTURE_WRAP_T                          "2803")
       (REPEAT                                  "2901")
       (COLOR_BUFFER_BIT                        "4000")
       (RGB8                                    "8051")
       (RGBA8                                   "8058")
       (TEXTURE_3D                              "806F")
       (TEXTURE_WRAP_R                          "8072")
       (BGR                                     "80E0")
       (BGRA                                    "80E1")
       (CLAMP_TO_EDGE                           "812F")
       (TEXTURE_BASE_LEVEL                      "813C")
       (TEXTURE_MAX_LEVEL                       "813D")
       (R8                                      "8229")
       (INTERNALFORMAT_PREFERRED                "8270")
       (COMPRESSED_RGB_S3TC_DXT1_EXT            "83F0")
       (COMPRESSED_RGBA_S3TC_DXT1_EXT           "83F1")
       (COMPRESSED_RGBA_S3TC_DXT3_EXT           "83F2")
       (COMPRESSED_RGBA_S3TC_DXT5_EXT           "83F3")
       (TEXTURE0                                "84C0")
       (ACTIVE_TEXTURE                          "84E0")
       (TEXTURE_CUBE_MAP                        "8513")
       (TEXTURE_CUBE_MAP_POSITIVE_X             "8515")
       (BUFFER_SIZE                             "8764")
       (DRAW_BUFFER0                            "8825")
       (ARRAY_BUFFER                            "8892")
       (ELEMENT_ARRAY_BUFFER                    "8893")
       (READ_ONLY                               "88B8")
       (WRITE_ONLY                              "88B9")
       (READ_WRITE                              "88BA")
       (STREAM_DRAW                             "88E0")
       (STREAM_READ                             "88E1")
       (STREAM_COPY                             "88E2")
       (STATIC_DRAW                             "88E4")
       (STATIC_READ                             "88E5")
       (STATIC_COPY                             "88E6")
       (DYNAMIC_DRAW                            "88E8")
       (DYNAMIC_READ                            "88E9")
       (DYNAMIC_COPY                            "88EA")
       (UNIFORM_BUFFER                          "8A11")
       (FRAGMENT_SHADER                         "8B30")
       (VERTEX_SHADER                           "8B31")
       (TEXTURE_2D_ARRAY                        "8C1A")
       (READ_FRAMEBUFFER                        "8CA8")
       (DRAW_FRAMEBUFFER                        "8CA9")
       (DEPTH_COMPONENT32F                      "8CAC")
       (FRAMEBUFFER_COMPLETE                    "8CD5")       
       (COLOR_ATTACHMENT0                       "8CE0")
       (FRAMEBUFFER                             "8D40")
       (RENDERBUFFER                            "8D41")
       (GEOMETRY_SHADER                         "8DD9")
       (COMPILE_STATUS                          "8B81")
       (LINK_STATUS                             "8B82")
       (PATCH_VERTICES                          "8E72")
       (TESS_EVALUATION_SHADER                  "8E87")
       (TESS_CONTROL_SHADER                     "8E88")
       (SYNC_GPU_COMMANDS_COMPLETE              "9117")
       (COMPUTE_SHADER                          "91B9")
       (MAP_READ_BIT                            "0001")
       (MAP_WRITE_BIT                           "0002")
       (MAP_INVALIDATE_RANGE_BIT                "0004")
       (MAP_INVALIDATE_BUFFER_BIT               "0008")
       (MAP_FLUSH_EXPLICIT_BIT                  "0010")
       (MAP_UNSYNCHRONIZED_BIT                  "0020")
       (MAP_PERSISTENT_BIT                      "0040")
       (MAP_COHERENT_BIT                        "0080")
       (DYNAMIC_STORAGE_BIT                     "0100")
       (CLIENT_STORAGE_BIT                      "0200")))
