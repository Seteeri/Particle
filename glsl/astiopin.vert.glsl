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

layout (std140, binding = 0) uniform projview
{
    mat4 proj;
    mat4 view;
};
layout (std140, binding = 1) uniform vertices
{
    vec4 vertex[4];
};
layout(std430, binding = 3) readonly buffer data_instances_out
{
    instance_t instances_out[];
};

out rgba_t vertexRGBA;
flat out int vertexOffsetTex;
flat out ivec2 vertexDimsTex;
flat out vec2 vertexDimsTexOffset;
out uv_t vertexUV;

void main()
{
    gl_Position = proj * view * instances_out[gl_InstanceID].model * vertex[gl_VertexID];
    
    vertexRGBA = instances_out[gl_InstanceID].rgbas[gl_VertexID];
    
    // Flip y since OpenGL stores bottom first (or do in compute shader)
    // Instead of here, fix during metrics calc
    vertexUV.u = instances_out[gl_InstanceID].uvs[gl_VertexID].u;
    vertexUV.v = 1.0 - instances_out[gl_InstanceID].uvs[gl_VertexID].v;
    
    vertexOffsetTex = instances_out[gl_InstanceID].w_flags[0];
    // Glyph dims are uniform; per program
    vertexDimsTex = ivec2(96, 96);
    vertexDimsTexOffset = vec2(float(vertexDimsTex.x-1), 
                                float(vertexDimsTex.y-1));
}

// Refactor to pass range from metrics to here
// - Place in extra UV coord

attribute float sdf_size;   // Signed distance field size in screen pixels

// outs
vec2 tc0 = vertexUV;
float doffset = 1.0 / sdf_size;       // Distance field delta for one screen pixel
vec2 sdf_texel = 1.0 / vertexDimsTex;

