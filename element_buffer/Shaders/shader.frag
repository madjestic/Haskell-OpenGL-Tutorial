#version 430 core

// Interpolated values from the vertex shaders
in vec2 uv;
uniform sampler2D tex;

// Ouput data
out vec4 fColor;

void main()
{
  fColor = vec4(texture(tex, uv).rgb, 1.0);
}
