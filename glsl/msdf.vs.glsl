//#version 320 es

layout (std140, binding = 0) uniform projview
{
    mat4 proj;
    mat4 view;
    vec4 positionVertex[4];
};
//
layout(std430, binding = 2) readonly buffer data_instances_out
{
    instance_t instances_out[];
};

out rgba_t vertexRGBA;
flat out int vertexW_UV;
out uv_t vertexUV;

void main()
{
    gl_Position = proj * view * instances_out[gl_InstanceID].model * positionVertex[gl_VertexID];
    
    vertexRGBA = instances_out[gl_InstanceID].rgbas[gl_VertexID];
    
    // Flip y since OpenGL stores bottom first (or do in compute shader)
    vertexUV.u = instances_out[gl_InstanceID].uvs[gl_VertexID].u;
    vertexUV.v = 1.0 - instances_out[gl_InstanceID].uvs[gl_VertexID].v;
    
    vertexW_UV = instances_out[gl_InstanceID].w_flags[0];
}
