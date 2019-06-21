#Node tex dims are all 96x96
(def '*dims-tex-node (list 96 96))

(def '*color-node-default (list 0.5137255 0.5803922 0.60784316 1.0 0.5137255 0.5803922 0.60784316 1.0
				0.5137255 0.5803922 0.60784316 1.0 0.5137255 0.5803922 0.60784316 1.0))

#{
Each node represents one of the following:
- Num
- Sym/String
- List
...which are all built on cons cells

- Or should they all be strings
- A single cons cell can hold 7 bytes

Each node has either a texture or a pixel (no texture)
- Problem is one of these

Edges are successors of node
}#

(class +Drawing)
(dm T (Char                    #Use to get offset, if char then data is char also
       Offset-tex
       UV
       MM                      #Rename to transforms? -> Nah, MM is output so intermediates are members of it
       RGBA)
    (=: char Char)
    (=: offset-tex Offset-tex)
    (=: uv UV)
    (=: mm MM)
    (=: rgba RGBA))

(class +Vertex)
(dm T (Data
       Drawing
       Edges)
    (=: data Data)
    (=: drawing (new '(+Drawing)))
    (=: edges Edges))
