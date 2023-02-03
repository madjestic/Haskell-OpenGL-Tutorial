#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec2 uvCoords;

uniform float fTime;
uniform mat4 transform;

// Output data ; will be interpolated for each fragment.
out vec2 uv;
out float time;

void main()
{
   gl_Position = transform * vec4(vPosition, 1.0);

// The color of each vertex will be interpolated
// to produce the color of each fragment
	 uv = uvCoords;
   time = fTime;
}
