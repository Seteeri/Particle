#version 320 es

precision mediump float;
precision mediump samplerBuffer;

// MSDF
// UVs are the same
// Text dims are the same
// RGBAs are the same - likely to be utilized dynamically

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
