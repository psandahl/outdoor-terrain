#version 330 core

uniform vec3 sunColor;

out vec4 color;

const vec4 blue = vec4(135.0 / 255.0, 206.0 / 255.0, 235.0 / 255.0, 1);
const vec4 glow = vec4(255.0 / 255.0, 225.0 / 255.0, 200.0 / 255.0, 1.0);

vec4 skyColor()
{
  return mix(vec4(1), blue, 0.27);
}

void main()
{
  vec2 point = gl_PointCoord - vec2(0.5);
  float len = length(point);
  if (len < 0.12)
  {
    color = vec4(sunColor, 1);
  }
  else if (len > 0.5)
  {
    discard;
  }
  else
  {
    color = mix(glow, skyColor(), smoothstep(0.12, 0.5, len));
  }
}
