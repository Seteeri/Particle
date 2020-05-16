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
//uniform float hint_amount; 0 or 1
//uniform float subpixel_amount; 0 or 1

in rgba_t vertexRGBA;
flat in int vertexOffsetTex;
flat in ivec2 vertexDimsTex;
flat in vec2 vertexDimsTexOffset;
in uv_t vertexUV;

flat in float dOffset;
flat in vec2 sdfTexel;

layout(location = 0) out vec4 color;

float median(float r, float g, float b) 
{
    return max(min(r, g), min(max(r, g), b));
}

float lstep( float a, float b, float x )
{
    return clamp(( x - a ) / ( b - a ), 0.0, 1.0 );
}

vec3 subpixel( float v, float a ) 
{
    float vt      = 0.6 * v; // 1.0 will make your eyes bleed
    vec3  rgb_max = vec3( -vt, 0.0, vt );
    float top     = abs( vt );
    float bottom  = -top - 1.0;
    float cfloor  = mix( top, bottom, a );
    vec3  res     = clamp( rgb_max - vec3( cfloor ), 0.0, 1.0 );
    return res;
}

void main2() 
{
    const float hint_amount = 1.0;
    const float subpixel_amount = 1.0;
    
    vec2  f_coordTexel = vec2(vertexUV.u, vertexUV.v) * vertexDimsTexOffset;
    
    vec4 v_sdf = sample_bilinear2(msdf, 
                                   vertexOffsetTex,
                                   vertexDimsTex,
                                   f_coordTexel);
    vec4 v_sdf_n = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     f_coordTexel + vec2(0.0, sdfTexel.y));
    vec4 v_sdf_e = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     f_coordTexel + vec2(sdfTexel.x, 0.0));
                                
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
    float hdOffset    = mix( dOffset * horz_scale, dOffset * vert_scale, vgrad );
    float res_dOffset = mix( dOffset, hdOffset, hint_amount );

    float f_offs = subpixel_amount * res_dOffset * sdfTexel.x * 16.0;
    vec2 offs = vec2( f_offs, 0.0 ); // not sure about 16.0, sdf_size?
    
    vec4 v_sdf_r = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     f_coordTexel - offs);
    vec4 v_sdf_b = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     f_coordTexel + offs);

    float sdf_r = median(v_sdf_r.r, v_sdf_r.g, v_sdf_r.b);
    float sdf_b = median(v_sdf_b.r, v_sdf_b.g, v_sdf_b.b);
                                     
    float alpha_r = lstep( 0.5 - res_dOffset, 0.5 + res_dOffset, sdf_r );
    float alpha_g = lstep( 0.5 - res_dOffset, 0.5 + res_dOffset, sdf );
    float alpha_b = lstep( 0.5 - res_dOffset, 0.5 + res_dOffset, sdf_b );
    
    color = vec4( vec3( alpha_r, alpha_g, alpha_b ), 1.0 );
}


void main() 
{
    const float hint_amount = 1.0;
    const float subpixel_amount = 1.0;
    
    vec2 f_coordTexel = vec2(vertexUV.u, vertexUV.v) * vertexDimsTexOffset;
    
    vec4 v_sdf = sample_bilinear2(msdf, 
                                   vertexOffsetTex,
                                   vertexDimsTex,
                                   f_coordTexel);
    vec4 v_sdf_n = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     f_coordTexel + vec2(0.0, sdfTexel.y));
    vec4 v_sdf_e = sample_bilinear2(msdf, 
                                     vertexOffsetTex,
                                     vertexDimsTex,
                                     f_coordTexel + vec2(sdfTexel.x, 0.0));
                                
    // Sampling the texture, L pattern
    float sdf       = median(v_sdf.r,   v_sdf.g,   v_sdf.b);
    float sdf_north = median(v_sdf_n.r, v_sdf_n.g, v_sdf_n.b);
    float sdf_east  = median(v_sdf_e.r, v_sdf_e.g, v_sdf_e.b);
    
    // Estimating stroke direction by the distance field gradient vector
    vec2  sgrad     = vec2( sdf_east - sdf, sdf_north - sdf );
    float sgrad_len = max( length( sgrad ), 1.0 / 128.0 );
    vec2  grad      = sgrad / vec2( sgrad_len );
    float vgrad = abs( grad.y ); // 0.0 - vertical stroke, 1.0 - horizontal one
    
    float horz_scale  = 1.1; // Blurring vertical strokes along the X axis a bit
    float vert_scale  = 0.6; // While adding some contrast to the horizontal strokes
    float hdOffset    = mix( dOffset * horz_scale, dOffset * vert_scale, vgrad ); 
    float res_dOffset = mix( dOffset, hdOffset, hint_amount );
    
    
    float alpha       = smoothstep( 0.5 - res_dOffset, 0.5 + res_dOffset, sdf );
    // Additional contrast
    alpha             = pow( alpha, 1.0 + 0.2 * vgrad * hint_amount );
    // Unfortunately there is no support for ARB_blend_func_extended in WebGL.
    // Fortunately the background is filled with a solid color so we can do
    // the blending inside the shader.
    
    // Discarding pixels beyond a threshold to minimise possible artifacts.
    if ( alpha < 20.0 / 256.0 ) discard;
    
    vec3 channels = subpixel( grad.x * 0.5 * subpixel_amount, alpha );
    // For subpixel rendering we have to blend each color channel separately
    vec3 res = mix( vec3(0.0, 0.0, 0.0),
                    vec3(vertexRGBA.r,
                         vertexRGBA.g,
                         vertexRGBA.b),
                    channels);
    
    color = vec4( res, vertexRGBA.a );
}
