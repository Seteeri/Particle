#version 320 es

precision mediump float;
precision mediump samplerBuffer;

// For padding
// https://community.khronos.org/t/ssbo-alignment-question/75614/4

// stpq
// TODO: use vec4 -> simpler
struct uv_t {
    float u;
    float v;
    float s;
    float t;
};

// TODO: use vec4 -> simpler
struct rgba_t {
    float r;
    float g;
    float b;
    float a;
};

struct instance_t {
    mat4 model;      // * 16 4 = 64 
    rgba_t rgbas[4]; // * 16 4 = 64
    uv_t uvs[4];     // * 16 4 = 64
    ivec4 w_flags;   // *  4 4 = 16
                     //        = 208 bytes
};


// Reduce size

struct instance_t_2 {
    mat4 model;      // * 16 4 = 64 
    rgba_t rgbas;    // * 4  4 = 16
    uv_t uvs[4];     // * 8  4 = 32
    int w_flags;     // * 4  1 = 4
                     //        = 116 bytes
};

// Other optimizations
// Store UVs in uniform buffer? Might not fit
// Use chars for rgba
// Pass ROT/POS/SCA or QUAT
// Half UVs since other half is 0

/*
struct instance_t_3 {
                     // cnt * sz
    mat4 model;      // * 16 4 = 64 
    rgba_t rgbas;    // * 4  1 = 4
    uv_t uvs[4];     // * 4  4 = 16
    int w_flags;     // * 4  1 = 4
    float rx;        // * 1  4 = 4
                     //        = 104 bytes
};
*/
