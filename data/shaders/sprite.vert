#version 330

in vec2 pos;
in vec2 mesh;
in vec2 origin;
in float rotation;
in int image;

in vec2 texCoords;

uniform mat4 view;
uniform mat4 projection;

out vec2 texCoordsFinal;
flat out int imageFinal;

void main()
{
  vec2 center = mesh - origin;
  vec2 newPos = vec2(center.x * cos (rotation) + center.y * sin(rotation),
                     center.x * (-sin(rotation)) + center.y * cos(rotation));
  gl_Position = projection*view*vec4(pos + newPos, 0, 1);
  texCoordsFinal = texCoords;
  imageFinal = image;
}
