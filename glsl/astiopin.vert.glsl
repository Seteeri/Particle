//#version 320 es           

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

flat out float dOffset;
flat out vec2 sdfTexel;

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
                                
    dOffset = 1.0 / instances_out[gl_InstanceID].uvs[gl_VertexID].s;
    sdfTexel = 1.0 / vec2(vertexDimsTex);
}

//attribute float sdf_size;   // Signed distance field size in screen pixels

// outs
//vec2 tc0 = vertexUV;
// float doffset = 1.0 / sdf_size;       // Distance field delta for one screen pixel
// vec2 sdf_texel = 1.0 / vertexDimsTex;
