#version 330 core

in vec3 vPosition;

out vec4 color;

const vec4 dark = vec4(135.0 / 255.0, 206.0 / 255.0, 235.0 / 255.0, 1);
const vec4 light = vec4(1);

void main()
{
  // The y coordinate goes from 1 to -1. By using the absolute value it can
  // be used as a gradient for the sky colors. The gradient will go from
  // dark - light - dark.
  float scale = abs(vPosition.y);
  color = mix(light, dark, scale);
}
