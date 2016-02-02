#version 430 core

// Interpolated values from the vertex shaders
uniform sampler2D tex;

// Ouput data
out vec4 fColor;

void main()
{
	 fColor = vec4(0.0, 0.0, 1.0, 1.0);
}
