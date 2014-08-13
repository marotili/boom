#version 330

in vec2 textureCoordinatesFrag;
uniform sampler2D texture;

out vec3 finalColor;

void main()
{
	finalColor = texture2D(texture, textureCoordinatesFrag).rgb;

	// DEBUG: show UV coordinates as red and green on models
	//finalColor = vec3(textureCoordinatesFrag.x, textureCoordinatesFrag.y, 0.0f);
}
