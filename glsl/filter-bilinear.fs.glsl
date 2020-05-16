//#version 320 es

precision mediump float;
precision mediump samplerBuffer;

vec4 sample_bilinear(samplerBuffer samp, 
                      int vertexOffsetTex,
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
    //int offsetTex = vertexOffsetTex * vertexDimsTex.x * vertexDimsTex.y;
    // Otherwise offset has to be generated and passed here per glyph
    // Currently, vertexOffsetTex is simply the char index

    int offsetTexel00 = vertexOffsetTex + ( coordTexel.x       + (coordTexel.y       * vertexDimsTex.x));
    int offsetTexel10 = vertexOffsetTex + ((coordTexel.x + 1)  + (coordTexel.y       * vertexDimsTex.x));
    int offsetTexel11 = vertexOffsetTex + ((coordTexel.x + 1)  + ((coordTexel.y + 1) * vertexDimsTex.x));
    int offsetTexel01 = vertexOffsetTex + ( coordTexel.x       + ((coordTexel.y + 1) * vertexDimsTex.x));
    
    vec4 texel00 = texelFetch(samp, offsetTexel00);
    vec4 texel10 = texelFetch(samp, offsetTexel10);
    vec4 texel11 = texelFetch(samp, offsetTexel11);
    vec4 texel01 = texelFetch(samp, offsetTexel01);

    vec2 sampleCoord = fract(f_coordTexel);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    
    return mix(texel0, texel1, sampleCoord.x);
}


vec4 sample_bilinear2(samplerBuffer samp,
                      int vertOffTex,
                      ivec2 vertexDimsTex,
                      vec2 coordTexel)
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
    
    ivec2 i_coordTexel = ivec2(coordTexel);

    int offTexel00 = vertOffTex + ( i_coordTexel.x       + (i_coordTexel.y       * vertexDimsTex.x));
    int offTexel10 = vertOffTex + ((i_coordTexel.x + 1)  + (i_coordTexel.y       * vertexDimsTex.x));
    int offTexel11 = vertOffTex + ((i_coordTexel.x + 1)  + ((i_coordTexel.y + 1) * vertexDimsTex.x));
    int offTexel01 = vertOffTex + ( i_coordTexel.x       + ((i_coordTexel.y + 1) * vertexDimsTex.x));
    
    vec4 texel00 = texelFetch(samp, offTexel00);
    vec4 texel10 = texelFetch(samp, offTexel10);
    vec4 texel11 = texelFetch(samp, offTexel11);
    vec4 texel01 = texelFetch(samp, offTexel01);

    vec2 sampleCoord = fract(coordTexel);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    
    return mix(texel0, texel1, sampleCoord.x);
}
