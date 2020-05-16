//#version 320 es
/*
 * Copyright (c) 2017 Anton Stepin astiopin@gmail.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */                  

/*
 *  Subpixel coverage calculation
 *    
 *  v - edge slope    -1.0 to 1.0          triplet
 *  a - pixel coverage 0.0 to 1.0          coverage
 *                                       
 *      |<- glyph edge                      R  G  B
 *  +---+---+                             +--+--+--+
 *  |   |XXX| v = 1.0 (edge facing west)  |  |xx|XX|
 *  |   |XXX| a = 0.5 (50% coverage)      |  |xx|XX|
 *  |   |XXX|                             |  |xx|XX|
 *  +---+---+                             +--+--+--+
 *    pixel                                0  50 100
 *
 *
 *        R   G   B
 *         
 *   1.0        +--+   <- top (abs( v ))
 *              |  
 *       -+-----+--+-- <- ceil: 100% coverage
 *        |     |XX|     
 *   0.0  |  +--+XX|     
 *        |  |xx|XX|     
 *       -+--+--+--+-- <- floor: 0% coverage
 *           |
 *  -1.0  +--+         <-  -abs(v)
 *        |
 *        |
 *        |         
 *  -2.0  +            <- bottom: -abs(v)-1.0
 */

precision mediump float;
precision mediump samplerBuffer;

uniform samplerBuffer msdf;
uniform float hint_amount;
uniform float subpixel_amount;

in rgba_t vertexRGBA;
flat in int vertexOffsetTex;
flat in ivec2 vertexDimsTex;
flat in vec2 vertexDimsTexOffset;
in uv_t vertexUV;

varying vec2  tc0;
varying float doffset;
varying float sdf_texel;

layout(location = 0) out vec4 gl_FragColor;

float median(float r, float g, float b) 
{
    return max(min(r, g), min(max(r, g), b));
}

float lstep( float a, float b, float x )
{
    return clamp(( x - a ) / ( b - a ), 0.0, 1.0 );
}

vec4 sample_bilinear2(int vertOffTex,
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
    
    vec4 texel00 = texelFetch(msdf, offTexel00);
    vec4 texel10 = texelFetch(msdf, offTexel10);
    vec4 texel11 = texelFetch(msdf, offTexel11);
    vec4 texel01 = texelFetch(msdf, offTexel01);

    vec2 sampleCoord = fract(f_coordTexel.xy);
    vec4 texel0 = mix(texel00, texel01, sampleCoord.y);
    vec4 texel1 = mix(texel10, texel11, sampleCoord.y);            
    
    return mix(texel0, texel1, sampleCoord.x);
}    
    
void main() 
{
    
    ivec2 coordTexel = vec2(vertexUV.u, vertexUV.v) * vertexDimsTexOffset;

    float v_sdf = sample_bilinear2(msdf, 
                                   vertexOffsetTex,
                                   vertexDimsTex,
                                   coordTexel);
    float v_sdf_n = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     ivec2(coordTexel.x, coordTexel.y - 1));
    float v_sdf_e = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     ivec2(coordTexel.x + 1, coordTexel.y - 1));
                                
    // Sampling the texture, L pattern
    float sdf       = median(v_sdf.r,   v_sdf.g,   v_sdf.b);
    float sdf_north = median(v_sdf_n.r, v_sdf_n.g, v_sdf_n.b);
    float sdf_east  = median(v_sdf_e.r, v_sdf_e.g, v_sdf_e.b);

    // Estimating stroke direction by the distance field gradient vector
    vec2  sgrad     = vec2( sdf_east - sdf, sdf_north - sdf );
    float sgrad_len = max( length( sgrad ), 1.0 / 128.0 );
    vec2  grad      = sgrad / vec2( sgrad_len );
    float vgrad = abs( grad.y ); // 0.0 - vertical stroke, 1.0 - horizontal one

    float horz_scale  = 1.0 - subpixel_amount * 0.333; // Blurring vertical strokes along the X axis a bit
    float vert_scale  = 0.5; // While adding some contrast to the horizontal strokes
    float hdoffset    = mix( doffset * horz_scale, doffset * vert_scale, vgrad );
    float res_doffset = mix( doffset, hdoffset, hint_amount );

    vec2 offs = vec2( subpixel_amount * res_doffset * sdf_texel * 16.0, 0.0 ); // not sure about 16.0, sdf_size?

    float sdf_r = texture2D( font_tex, tc0 - offs ).r;
    float sdf_b = texture2D( font_tex, tc0 + offs ).r;

    float alpha_r = lstep( 0.5 - res_doffset, 0.5 + res_doffset, sdf_r );
    float alpha_g = lstep( 0.5 - res_doffset, 0.5 + res_doffset, sdf );
    float alpha_b = lstep( 0.5 - res_doffset, 0.5 + res_doffset, sdf_b );

    vec3 color = vec3( alpha_r, alpha_g, alpha_b );
    gl_FragColor = vec4( color, 1.0 );
}
