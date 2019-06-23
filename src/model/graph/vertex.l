#Node tex dims are all 96x96
(def '*dims-tex-node (list 96 96))

(def '*color-node-default (list 0.5137255 0.5803922 0.60784316 1.0 0.5137255 0.5803922 0.60784316 1.0
				0.5137255 0.5803922 0.60784316 1.0 0.5137255 0.5803922 0.60784316 1.0))


(class +Vertex)
(dm T (Char                    #Use to get offset, if char then data is char also
       Offset-tex
       UV
       RGBA
       MM)                      #Rename to transforms? -> Nah, MM is output so intermediates are members of it

    #Texture only
    (=: char Char)
    (=: offset-tex Offset-tex)
    (=: uv UV)

    #Pixel only
    (=: rgba RGBA)

    (=: mm MM))
