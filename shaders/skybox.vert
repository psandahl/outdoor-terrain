#version 330 core

layout (location = 0) in vec3 position;

uniform mat4 mvp;

out vec3 vPosition;

void main()
{
  // Just propagate/interpolate local coordinates.
  vPosition = position;

  gl_Position = mvp * vec4(position, 1.0);
}
