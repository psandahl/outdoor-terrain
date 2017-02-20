#version 330 core

in vec2 vTexCoord;

uniform sampler2D tex1;

out vec4 color;

void main()
{
  // Flip the texture horizontally.
  color = texture(tex1, vec2(vTexCoord.s, 1.0 - vTexCoord.t));
}
