//#version 320 es

precision highp float;
precision highp sampler2DArray;
precision highp samplerBuffer;
precision highp usamplerBuffer;

uniform samplerBuffer msdf;

in rgba_t vertexRGBA;
flat in int vertexOffsetTex;
flat in ivec2 vertexDimsTex;
in uv_t vertexUV;

out vec4 color;

// Implement filter_bicubic

vec4 filter_bilinear()
{
    // https://github.com/WebGLSamples/WebGL2Samples/blob/master/samples/texture_fetch.html
    
    vec2 f_dims = vec2(float(vertexDimsTex.x-1), float(vertexDimsTex.y-1));
    vec2 f_coordTexel = vec2(vertexUV.u, vertexUV.v) * f_dims;
    ivec2 coordTexel = ivec2(f_coordTexel);
        
    int offsetTex = vertexOffsetTex * vertexDimsTex.x * vertexDimsTex.y;
        
    int offsetTexel00 = offsetTex + (coordTexel.x + (coordTexel.y * vertexDimsTex.x));
    int offsetTexel10 = offsetTex + ((coordTexel.x + 1) + (coordTexel.y * vertexDimsTex.x));
    int offsetTexel11 = offsetTex + ((coordTexel.x + 1) + ((coordTexel.y + 1) * vertexDimsTex.x));
    int offsetTexel01 = offsetTex + (coordTexel.x + ((coordTexel.y + 1) * vertexDimsTex.x));
    
    vec4 texel00 = texelFetch(msdf, offsetTexel00);
    vec4 texel10 = texelFetch(msdf, offsetTexel10);
    vec4 texel11 = texelFetch(msdf, offsetTexel11);
    vec4 texel01 = texelFetch(msdf, offsetTexel01);

    vec2 sampleCoord = fract(f_coordTexel.xy);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    
    return mix(texel0, texel1, sampleCoord.x);
}    

/*
//uniform sampler2DArray msdf;
vec4 filter_bilinear_2d_array()
{
    // https://github.com/WebGLSamples/WebGL2Samples/blob/master/samples/texture_fetch.html
    vec2 size = vec2(textureSize(msdf, 0) - 1);
    vec2 texcoord = vec2(vertexUV.u, vertexUV.v) * size;
    ivec2 coord = ivec2(texcoord);
    vec4 texel00 = texelFetch(msdf, ivec3(coord + ivec2(0, 0), vertexW_UV), 0);
    vec4 texel10 = texelFetch(msdf, ivec3(coord + ivec2(1, 0), vertexW_UV), 0);
    vec4 texel11 = texelFetch(msdf, ivec3(coord + ivec2(1, 1), vertexW_UV), 0);
    vec4 texel01 = texelFetch(msdf, ivec3(coord + ivec2(0, 1), vertexW_UV), 0);
    vec2 sampleCoord = fract(texcoord.xy);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    return mix(texel0, texel1, sampleCoord.x);
}    
*/

void main()
{
    // Note UV's v coord flipped in vertex shader
    
    // base + ux + (vy * width)
    
    //vec2 size = vec2(57.0,112.0);
    ////vec2 texcoord = vec2(vertexUV.u, vertexUV.v) * size;
    //ivec2 coord = ivec2(texcoord);
    //color = texelFetch(msdf, coord.x + (coord.y * 58));
    
    // Switch B and R due to Pango layout
    vec4 color_filtered = filter_bilinear();
    color = vec4(color_filtered.b, color_filtered.g, color_filtered.r, color_filtered.a);
}