#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec2 uvCoords;
uniform float fTime;

// Output data ; will be interpolated for each fragment.
out vec2 fragCoord;
out float time;

void main()
{
   gl_Position = vPosition;

// The color of each vertex will be interpolated
// to produce the color of each fragment
	 fragCoord = uvCoords;
   time = fTime;
}
