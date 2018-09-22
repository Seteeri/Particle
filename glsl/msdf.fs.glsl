//#version 320 es

precision highp float;
precision highp sampler2DArray;
precision highp samplerBuffer;
precision highp usamplerBuffer;

uniform samplerBuffer msdf;

in uv_t vertexUV;
flat in int vertexW_UV;
in rgba_t vertexRGBA;

out vec4 color;


float median(float r, float g, float b) 
{
    return max(min(r, g), min(max(r, g), b));
}

// Implement filter_bicubic

vec4 filter_bilinear()
{
    // https://github.com/WebGLSamples/WebGL2Samples/blob/master/samples/texture_fetch.html
    
    //vec2 size = vec2(textureSize(msdf, 0) - 1);
    vec2 size = vec2(57.0,112.0);
    vec2 texcoord = vec2(vertexUV.u, vertexUV.v) * size;
    ivec2 coord = ivec2(texcoord);
        
    // xy -> offset
    int offset_base_glyph = vertexW_UV * 58 * 113;
        
    int offset_texel00 = offset_base_glyph + (coord.x + (coord.y * 58));
    int offset_texel10 = offset_base_glyph + ((coord.x + 1) + (coord.y * 58));
    int offset_texel11 = offset_base_glyph + ((coord.x + 1) + ((coord.y + 1) * 58));
    int offset_texel01 = offset_base_glyph + (coord.x + ((coord.y + 1) * 58));
    
    vec4 texel00 = texelFetch(msdf, offset_texel00);
    vec4 texel10 = texelFetch(msdf, offset_texel10);
    vec4 texel11 = texelFetch(msdf, offset_texel11);
    vec4 texel01 = texelFetch(msdf, offset_texel01);

    vec2 sampleCoord = fract(texcoord.xy);
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
void main_msdf()
{
    vec4 samp = filter_bilinear();
    
    float sigDist = median(samp.r, samp.g, samp.b);
    float w = fwidth(sigDist);
    float opacity = smoothstep(0.5 - w, 0.5 + w, sigDist);

    color = vec4(vec3(vertexRGBA.r,
                      vertexRGBA.g,
                      vertexRGBA.b),
                 opacity*vertexRGBA.a);
}

void main()
{
    // Note UV's v coord flipped in vertex shader
    
    // base + ux + (vy * width)
    
    //vec2 size = vec2(57.0,112.0);
    ////vec2 texcoord = vec2(vertexUV.u, vertexUV.v) * size;
    //ivec2 coord = ivec2(texcoord);
    //color = texelFetch(msdf, coord.x + (coord.y * 58));
    
    color = filter_bilinear();
}