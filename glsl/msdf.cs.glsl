//#version 320 es

// https://fgiesen.wordpress.com/2010/10/17/view-frustum-culling/
// https://cesium.com/blog/2017/02/02/tighter-frustum-culling-and-why-you-may-want-to-disregard-it/

// There is a general limitation on the local size dimensions, queried 
// with GL_MAX_COMPUTE_WORK_GROUP_SIZE in the same way as above. Note 
// that the minimum requirements here are much smaller: 
// 1024 for X and Y, and a mere 64 for Z.
// GL_MAX_COMPUTE_WORK_GROUP_SIZE = 1792, 1792, 1792

// (* x y z) : <= 1792 (GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS)
//
// That is, the product of the X, Y and Z components of the local size 
// must be less than GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS
layout (local_size_x=1792, local_size_y=1, local_size_z=1) in;

////////////////
// Input Buffers

layout (std140, binding = 0) uniform data_projview
{
    mat4 proj;
    mat4 view;
    vec4 position[4];
};
layout (std430, binding = 1) readonly buffer data_instances_in
{
    // Create compute-only buffer for these
    // Compute shader outputs to model
    //vec3 translation;
    //vec3 scale;
    //vec3 rotation;
    instance_t instances_in[];
};

//////////////////
// Output Buffers
// msdf.vs bindings should match these

layout(std430, binding = 2) writeonly buffer data_instances_out
{
    instance_t instances_out[];
};

layout(binding=3, offset=0) uniform atomic_uint counter;

/*
uvec3 gl_NumWorkGroups	global work group size we gave to glDispatchCompute()
uvec3 gl_WorkGroupSize	local work group size we defined with layout
uvec3 gl_WorkGroupID	position of current invocation in global work group
uvec3 gl_LocalInvocationID	position of current invocation in local work group
uvec3 gl_GlobalInvocationID	unique index of current invocation in global work group
uint gl_LocalInvocationIndex	1d index representation of gl_LocalInvocationID
*/

void main()
{        
    if (instances_in[gl_GlobalInvocationID.x].w_flags[1] == 1)
    {
        uint count = atomicCounterIncrement(counter);
       
        // primCount=count can end up being short depending on
        // which thread finishes first; 
        // so must set the primCount after all compute shader done
        // in CPU side

        instances_out[count] = instances_in[gl_GlobalInvocationID.x];
    }
}

// Frustum Culling
// 0. Calculate clip planes on CPU and set uniform
//    1. TODO: Implement here...only needs to be done once however
// 1. Calculate OOB
//    1. Use proj/view/model/vertices
//    2. Save transform into vbo for vertex shader (do later)
//       (Use drawarrays instead of elements)
//       (Create another vbo and use instead of model_matrix)
// 2. Cull
//    1. Copy drawable transforms into cull buffer
//    2. Copy non-drawable transforms into another buffer?
// 4. Modify command buffer
// 5. Draw
