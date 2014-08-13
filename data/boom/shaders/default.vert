#version 330

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 textureCoordinates;

uniform mat4 mvp;

out vec2 textureCoordinatesFrag;

void main()
{
    gl_Position = mvp * vec4(position, 1);
	textureCoordinatesFrag = textureCoordinates;
}
