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

vec4 cubic(float v)
{
    vec4 n = vec4(1.0, 2.0, 3.0, 4.0) - v;
    vec4 s = n * n * n;
    float x = s.x;
    float y = s.y - 4.0 * s.x;
    float z = s.z - 4.0 * s.y + 6.0 * s.x;
    float w = 6.0 - x - y - z;
    return vec4(x, y, z, w);
}

// Implement filter_bicubic


vec4 filter_bilinear()
{
    // https://github.com/WebGLSamples/WebGL2Samples/blob/master/samples/texture_fetch.html
    
    //vec2 size = vec2(textureSize(msdf, 0) - 1);
    vec2 size = vec2(95.0,95.0);
    
    vec2 texcoord = vec2(vertexUV.u, vertexUV.v) * size;
    ivec2 coord = ivec2(texcoord);
        
    // xy -> offset
    int offset_base_glyph = vertexW_UV * 96 * 96;
        
    int offset_texel00 = offset_base_glyph + (coord.x + (coord.y * 96));
    int offset_texel10 = offset_base_glyph + ((coord.x + 1) + (coord.y * 96));
    int offset_texel11 = offset_base_glyph + ((coord.x + 1) + ((coord.y + 1) * 96));
    int offset_texel01 = offset_base_glyph + (coord.x + ((coord.y + 1) * 96));
    
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

//uniform sampler2DArray msdf;
vec3 filter_builtin()
{
    vec3 texCoords = vec3(vertexUV.u, 
                          vertexUV.v,
                          float(vertexW_UV));
    vec2 pos = texCoords.xy;
    
    return texture(msdf, texCoords).rgb;
    
    //return texelFetch(msdf, ivec3(int(texCoords.x*96.0),int(texCoords.y*96.0),texCoords.z), 0);
}
*/
void main()
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

void main2()
{
    color = vec4(1.0, 1.0, 1.0, 1.0);
}

/*
void main2() 
{
    float pxRange = 4.0;
    vec4 bgColor = vec4(0,0,0,1);
    vec4 fgColor = vec4(1,1,1,1);
    vec2 msdfUnit = pxRange/vec2(textureSize(msdf, 0).x,textureSize(msdf, 0).y);
    vec3 samp = texture(msdf, texCoords).rgb;
    float sigDist = median(samp.r, samp.g, samp.b) - 0.5;
    sigDist *= dot(msdfUnit, 0.5/fwidth(texCoords.xy));
    float opacity = clamp(sigDist + 0.5, 0.0, 1.0);
    color = mix(bgColor, fgColor, opacity);
}
*/

/*
void main() 
{
    vec3 texCoords2 = vec3(texCoords.x, texCoords.y, glyphIx);   
    // Bilinear sampling of the distance field
    vec3 s = texture(msdf, texCoords2).rgb;
    // Acquire the signed distance
    float d = median(s.r, s.g, s.b) - 0.5;
    // Weight between inside and outside (anti-aliasing)
    float w = clamp(d/fwidth(d) + 0.5, 0.0, 1.0);
    // Combining the background and foreground color
    //vec4 bgColor = vec4(1,1,1,1);
    //vec4 fgColor = vec4(0,0,0,1);
    //color = mix(bgColor, fgColor, w);
    color = vec4(fragColor.rgb, w);
    
    if(w < 0.5)
        discard;
}
*/
/*
vec2 safeNormalize( in vec2 v )
{
   float len = length( v );
   len = ( len > 0.0 ) ? 1.0 / len : 0.0;
   return v * len;
}

void main(void)
{
    // Convert normalized texcoords to absolute texcoords.
    vec2 uv = texCoords * vec2(96.0, 96.0);
    // Calculate derivates
    vec2 Jdx = dFdx( uv );
    vec2 Jdy = dFdy( uv );
    // Sample SDF texture (3 channels).
    //vec3 sample = texture( uTex0, TexCoord ).rgb;
    vec3 samp = texture(msdf, vec3(texCoords.x, texCoords.y, glyphIx)).rgb;
    // calculate signed distance (in texels).
    float sigDist = median( samp.r, samp.g, samp.b ) - 0.5;
    // For proper anti-aliasing, we need to calculate signed distance in pixels. We do this using derivatives.
    vec2 gradDist = safeNormalize( vec2( dFdx( sigDist ), dFdy( sigDist ) ) );
    vec2 grad = vec2( gradDist.x * Jdx.x + gradDist.y * Jdy.x, gradDist.x * Jdx.y + gradDist.y * Jdy.y );
    // Apply anti-aliasing.
    const float kThickness = 0.125;
    const float kNormalization = kThickness * 0.5 * sqrt( 2.0 );
    float afwidth = min( kNormalization * length( grad ), 0.5 );
    float opacity = smoothstep( 0.0 - afwidth, 0.0 + afwidth, sigDist );
    // Apply pre-multiplied alpha with gamma correction.
    color.a = pow( fragColor.a * opacity, 1.0 / 2.2 );

    if(color.a < 0.5)
        discard;

    color.rgb = fragColor.rgb * color.a;
}
*/
