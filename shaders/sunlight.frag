#version 330 core

uniform vec3 sunColor;

out vec4 color;

const vec3 glow = vec3(255.0 / 255.0, 225.0 / 255.0, 150.0 / 255.0);

void main()
{
  vec2 point = gl_PointCoord - vec2(0.5);
  float len = length(point);
  if (len < 0.5)
  {
    color = vec4(mix(sunColor, glow, len), 1.0);
  }
  else
  {
    discard;
  }
}
