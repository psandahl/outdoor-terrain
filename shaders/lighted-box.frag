#version 330 core

in vec2 vTexCoord;

uniform sampler2D tex1;

out vec4 color;

void main()
{
  color = texture(tex1, vTexCoord);
}
