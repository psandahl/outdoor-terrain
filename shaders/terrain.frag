#version 330 core

in vec3 vNormal;
in vec2 vTexCoord;

uniform vec3 sunPosition;
uniform vec3 sunColor;

uniform sampler2D groundTexture; // Texture unit 0

out vec4 color;

const float ambientStrength = 0.1;

void main()
{
  vec4 ambientColor = vec4(sunColor * ambientStrength, 1);

  // Load texture color. Flip the texture horizontally.
  vec4 fragColor = texture(groundTexture, vec2(vTexCoord.s, 1.0 - vTexCoord.t));

  color = fragColor + ambientColor;
}
