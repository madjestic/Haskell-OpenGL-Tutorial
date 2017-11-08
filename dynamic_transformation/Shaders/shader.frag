#version 430 core

// Interpolated values from the vertex shaders
in vec3 clr;
in vec2 uv;
uniform sampler2D tex_00;
uniform sampler2D tex_01;

// Ouput data
out vec4 fColor;

void main()
{
  fColor = vec4( mix( texture(tex_00, uv).rgb,
                      texture(tex_01, uv).rgb, 0.5 ) * clr
               , 1.0 );
}
