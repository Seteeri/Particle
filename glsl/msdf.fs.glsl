//#version 320 es

precision mediump float;
precision mediump samplerBuffer;

uniform samplerBuffer msdf;

in rgba_t vertexRGBA;
flat in int vertexOffsetTex;
flat in ivec2 vertexDimsTex;
flat in vec2 vertexDimsTexOffset;
in uv_t vertexUV;

layout(location = 0) out vec4 color;

float median(float r, float g, float b) 
{
    return max(min(r, g), min(max(r, g), b));
}

vec2 safeNormalize( in vec2 v )
{
   float len = length( v );
   len = ( len > 0.0 ) ? 1.0 / len : 0.0;
   return v * len;
}

void main(void)
{
    vec2 TexCoord = vec2(vertexUV.u, vertexUV.v) * vertexDimsTexOffset;
    ivec2 iTexCoord = ivec2(TexCoord);
    // Convert normalized texcoords to absolute texcoords.
    vec2 uv = TexCoord * vec2(vertexDimsTex);
    // Calculate derivates
    vec2 Jdx = dFdx( uv );
    vec2 Jdy = dFdy( uv );
    // Sample SDF texture (3 channels).
    vec4 samp = filter_bilinear(msdf, 
                                vertexOffsetTex,
                                vertexDimsTex,
                                vertexUV,
                                vertexDimsTexOffset);
    // calculate signed distance (in texels).
    float sigDist = median( samp.r, samp.g, samp.b ) - 0.5;
    // For proper anti-aliasing, we need to calculate signed distance in pixels. We do this using derivatives.
    vec2 gradDist = safeNormalize( vec2( dFdx( sigDist ), dFdy( sigDist ) ) );
    vec2 grad = vec2( gradDist.x * Jdx.x + gradDist.y * Jdy.x, gradDist.x * Jdx.y + gradDist.y * Jdy.y );
    // Apply anti-aliasing = 1.0/8.0
    //const float kThickness = 0.125;
    const float kThickness = 1.0/128.0;
    const float kNormalization = kThickness * 0.5 * sqrt( 2.0 );
    float afwidth = min( kNormalization * length( grad ), 0.5 );
    float opacity = smoothstep( 0.0 - afwidth, 0.0 + afwidth, sigDist );
    
    // Apply pre-multiplied alpha with gamma correction.
    vec4 uFgColor = vec4(vertexRGBA.r,
                         vertexRGBA.g,
                         vertexRGBA.b,
                         vertexRGBA.a);
    color.a = pow( uFgColor.a * opacity, 1.0 / 2.2 );
    color.rgb = uFgColor.rgb * color.a;
}
