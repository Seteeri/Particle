//#version 320 es

precision mediump float;
precision mediump samplerBuffer;

vec4 sample_bilinear(samplerBuffer msdf, 
                      int vertOffTex,
                      ivec2 vertexDimsTex,
                      uv_t vertexUV, 
                      vec2 vertexDimsTexOffset)
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
    //vec2 f_dims = vec2(float(vertexDimsTex.x-1), float(vertexDimsTex.y-1));
    vec2 f_coordTexel = vec2(vertexUV.u, vertexUV.v) * vertexDimsTexOffset;
    ivec2 coordTexel = ivec2(f_coordTexel);
        
    // Below only works when textures are all the same size
    //int offsetTex = vertOffTex * vertexDimsTex.x * vertexDimsTex.y;
    // Otherwise offset has to be generated and passed here per glyph
    // Currently, vertOffTex is simply the char index

    int offTexel00 = vertOffTex + ( coordTexel.x       + (coordTexel.y       * vertexDimsTex.x));
    int offTexel10 = vertOffTex + ((coordTexel.x + 1)  + (coordTexel.y       * vertexDimsTex.x));
    int offTexel11 = vertOffTex + ((coordTexel.x + 1)  + ((coordTexel.y + 1) * vertexDimsTex.x));
    int offTexel01 = vertOffTex + ( coordTexel.x       + ((coordTexel.y + 1) * vertexDimsTex.x));
    
    vec4 texel00 = texelFetch(msdf, offTexel00);
    vec4 texel10 = texelFetch(msdf, offTexel10);
    vec4 texel11 = texelFetch(msdf, offTexel11);
    vec4 texel01 = texelFetch(msdf, offTexel01);

    vec2 sampleCoord = fract(f_coordTexel.xy);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    
    return mix(texel0, texel1, sampleCoord.x);
}
