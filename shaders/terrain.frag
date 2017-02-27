#version 330 core

in vec2 vTexCoord;

uniform sampler2D groundTexture;

out vec4 color;

void main()
{
  // Flip the texture horizontally.
  color = texture(groundTexture, vec2(vTexCoord.s, 1.0 - vTexCoord.t));
  //color = vec4(0, 1, 0, 1);
}
