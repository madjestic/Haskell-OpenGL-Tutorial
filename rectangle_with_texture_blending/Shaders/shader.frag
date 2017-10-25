#version 430 core

// Interpolated values from the vertex shaders
in vec2 uv;
uniform sampler2D tex_00;
uniform sampler2D tex_01;

// Ouput data
out vec4 fColor;

void main()
{
  fColor = vec4(texture(tex_01, uv).rgb, 1.0);
  //fColor = vec4(uv, 1.0, 1.0);
}
