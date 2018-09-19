#version 320 es

// use vec4s?
struct uv_t {
    float u;
    float v;
    float s;
    float t;
};

// use vec4s?
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