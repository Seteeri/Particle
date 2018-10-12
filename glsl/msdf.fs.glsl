//#version 320 es

precision mediump float;
precision mediump samplerBuffer;

uniform samplerBuffer msdf;

in rgba_t vertexRGBA;
flat in int vertexOffsetTex;
flat in ivec2 vertexDimsTex;
in uv_t vertexUV;

layout(location = 0) out vec4 color;

float median(float r, float g, float b) 
{
    return max(min(r, g), min(max(r, g), b));
}  

void main()
{
    vec4 samp = filter_bilinear(msdf, vertexOffsetTex, vertexDimsTex, vertexUV);
    
    float sigDist = median(samp.r, samp.g, samp.b);
    float w = fwidth(sigDist);
    float opacity = smoothstep(0.5 - w, 0.5 + w, sigDist);

    color = vec4(vec3(vertexRGBA.r,
                       vertexRGBA.g,
                       vertexRGBA.b),
                 opacity*vertexRGBA.a);
}