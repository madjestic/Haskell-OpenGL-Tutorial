#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec3 vColor;
layout(location = 2) in vec2 uvCoords;

// Output data ; will be interpolated for each fragment.
out vec3 clr;
out vec2 uv;

uniform mat4 transform;

void main()
{
  gl_Position = transform * vec4(vPosition, 1.0);

// The color of each vertex will be interpolated
// to produce the color of each fragment
  clr = vColor;
	uv  = uvCoords;
}
