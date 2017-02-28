#version 330 core

in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

uniform vec3 sunPosition;
uniform vec3 sunColor;

uniform sampler2D groundTexture; // Texture unit 0

out vec4 color;

const float ambientStrength = 0.1;
const float diffuseFactor = 0.4;

const vec4 groundColor = vec4(0, 104.0 / 255.0, 10.0 / 255.0, 1);

void main()
{
  // Calculate ambient color.
  vec4 ambientColor = vec4(sunColor * ambientStrength, 1);

  // Calculate diffuse color.
  vec3 normal = normalize(vNormal);
  vec3 sunDirection = normalize(sunPosition - vPosition);
  float diffuse = dot(sunDirection, normal);

  vec4 diffuseColor = vec4(0.0);
  if (diffuse > 0.0)
  {
    diffuseColor = vec4(sunColor * diffuse * diffuseFactor, 1.0);
  }

  // Load texture color. Flip the texture horizontally.
  vec4 fragColor = mix(groundColor, texture(groundTexture, vec2(vTexCoord.s, 1.0 - vTexCoord.t)), 0.9);

  color = fragColor + ambientColor + diffuseColor;
}
