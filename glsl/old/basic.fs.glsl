//#version 320 es

precision mediump float;
precision mediump samplerBuffer;

uniform samplerBuffer msdf;

in rgba_t vertexRGBA;
flat in int vertexOffsetTex;
flat in ivec2 vertexDimsTex;
flat in vec2 vertexDimsTexOffset;
in uv_t vertexUV;

layout(location = 0) out vec4 color;

float median(float r, float g, float b) 
{
    return max(min(r, g), min(max(r, g), b));
}

void main()
{
    vec4 samp = filter_bilinear(msdf, 
                                vertexOffsetTex,
                                vertexDimsTex,
                                vertexUV,
                                vertexDimsTexOffset);
    float d = median(samp.r, samp.g, samp.b);
    float w = fwidth(d);
    float opacity = smoothstep(0.5 - w, 0.5 + w, d);
    
    color = vec4(vec3(vertexRGBA.r,
                       vertexRGBA.g,
                       vertexRGBA.b),
                 opacity*vertexRGBA.a);
}

// http://www.java-gaming.org/topics/solved-signed-distance-field-fonts-look-crappy-at-small-pt-sizes/33612/view.html
