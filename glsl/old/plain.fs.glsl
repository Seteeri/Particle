//#version 320 es

precision mediump float;
precision mediump samplerBuffer;

uniform samplerBuffer msdf;

in rgba_t vertexRGBA;
flat in int vertexOffsetTex;
flat in ivec2 vertexDimsTex;
flat in vec2 vertexDimsTexOffset;
in uv_t vertexUV;

in vec3 vBC;

layout(location = 0) out vec4 color;

void main()
{
    // Note UV's v coord flipped in vertex shader
    
    // base + ux + (vy * width)
    
    //vec2 size = vec2(57.0,112.0);
    ////vec2 texcoord = vec2(vertexUV.u, vertexUV.v) * size;
    //ivec2 coord = ivec2(texcoord);
    //color = texelFetch(msdf, coord.x + (coord.y * 58));
    
    // Switch B and R due to Pango layout
    vec4 samp = filter_bilinear(msdf, 
                                vertexOffsetTex,
                                vertexDimsTex,
                                vertexUV,
                                vertexDimsTexOffset);
    color = vec4(samp.b, samp.g, samp.r, samp.a);
}

vec4 sample_bilinear2(samplerBuffer samp,
                      int vertOffTex,
                      ivec2 vertexDimsTex,
                      ivec2 coordTexel)
{
    // https://github.com/WebGLSamples/WebGL2Samples/blob/master/samples/texture_fetch.html
    
    // Move this to vertex shader or CPU/passthrough
    // Subtract 1 since coords actually start at 0,0 otherwise visual artifacts will occur
    /* Make sure you offset your texture coordinates with 1/2 pixel, 
     * because in OpenGL the texel origin are defined to be the bottom 
     * left corner of a texel. That means that the exact center of a 
     * texel is located at [S'+0.5, T'+0.5] where S' and T' are the 
     * unnormalized texture coordinates.
     */
        
    // Below only works when textures are all the same size
    //int offsetTex = vertOffTex * vertexDimsTex.x * vertexDimsTex.y;
    // Otherwise offset has to be generated and passed here per glyph
    // Currently, vertOffTex is simply the char index

    int offTexel00 = vertOffTex + ( coordTexel.x       + (coordTexel.y       * vertexDimsTex.x));
    int offTexel10 = vertOffTex + ((coordTexel.x + 1)  + (coordTexel.y       * vertexDimsTex.x));
    int offTexel11 = vertOffTex + ((coordTexel.x + 1)  + ((coordTexel.y + 1) * vertexDimsTex.x));
    int offTexel01 = vertOffTex + ( coordTexel.x       + ((coordTexel.y + 1) * vertexDimsTex.x));
    
    vec4 texel00 = texelFetch(samp, offTexel00);
    vec4 texel10 = texelFetch(samp, offTexel10);
    vec4 texel11 = texelFetch(samp, offTexel11);
    vec4 texel01 = texelFetch(samp, offTexel01);

    vec2 sampleCoord = fract(f_coordTexel.xy);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    
    return mix(texel0, texel1, sampleCoord.x);
}
