//#version 320 es

precision mediump float;
precision mediump samplerBuffer;

vec4 filter_bilinear(samplerBuffer msdf, int vertexOffsetTex, ivec2 vertexDimsTex, uv_t vertexUV)
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
    vec2 f_dims = vec2(float(vertexDimsTex.x-1), float(vertexDimsTex.y-1));
    vec2 f_coordTexel = vec2(vertexUV.u, vertexUV.v) * f_dims;
    ivec2 coordTexel = ivec2(f_coordTexel);
        
    // Below only works when textures are all the same size
    //int offsetTex = vertexOffsetTex * vertexDimsTex.x * vertexDimsTex.y;
        
    int offsetTexel00 = vertexOffsetTex + (coordTexel.x + (coordTexel.y * vertexDimsTex.x));
    int offsetTexel10 = vertexOffsetTex + ((coordTexel.x + 1) + (coordTexel.y * vertexDimsTex.x));
    int offsetTexel11 = vertexOffsetTex + ((coordTexel.x + 1) + ((coordTexel.y + 1) * vertexDimsTex.x));
    int offsetTexel01 = vertexOffsetTex + (coordTexel.x + ((coordTexel.y + 1) * vertexDimsTex.x));
    
    vec4 texel00 = texelFetch(msdf, offsetTexel00);
    vec4 texel10 = texelFetch(msdf, offsetTexel10);
    vec4 texel11 = texelFetch(msdf, offsetTexel11);
    vec4 texel01 = texelFetch(msdf, offsetTexel01);

    vec2 sampleCoord = fract(f_coordTexel.xy);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    
    return mix(texel0, texel1, sampleCoord.x);
}