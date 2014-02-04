#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vertexColor;

// Output data ; will be interpolated for each fragment.
out vec4 fragmentColor;

void
main()
{
   gl_Position = vPosition;

	 						 // The color of each vertex will be interpolated
	 						 // to produce the color of each fragment
	 						 fragmentColor = vertexColor;
}