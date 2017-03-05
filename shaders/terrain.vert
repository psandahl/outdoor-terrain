#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec3 color;
layout (location = 3) in vec2 texCoord;

uniform mat4 mvp;
uniform mat4 model;

out vec3 vPosition;
out vec3 vNormal;
out vec3 vColor;
out vec2 vTexCoord;

void main()
{
  vPosition = (model * vec4(position, 1.0)).xyz;
  vNormal = (model * vec4(normal, 0.0)).xyz;
  vColor = color;
  vTexCoord = texCoord;

  gl_Position = mvp * vec4(position, 1.0);
}
