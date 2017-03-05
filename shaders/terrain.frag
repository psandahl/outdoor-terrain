#version 330 core

in vec3 vPosition; // Interpolated fragment position
in vec3 vNormal; // Interpolated fragment normal
in vec3 vTangent; // Interpolated fragment tangent
in vec3 vColor; // Interpolated fragment color
in vec2 vTexCoord; // Interpolated texture coordinate

uniform vec3 sunPosition; // The sun's position in model space
uniform vec3 sunColor; // The sun's color
uniform vec3 eyePosition; // The eye's position in model space

uniform sampler2D groundTexture; // Texture unit 0
uniform sampler2D normalMap; // Texture unit 1

out vec4 color; // Final fragment color

// Lightning factors
const float ambientStrength = 0.1;
const float diffuseFactor = 0.4;

// Distance where bump map normals are used.
const float detailed = 3.0;

// The vista in model units before full fog kicks in.
const float vista = 160.0;

// Approximation of the skie's color, somewhere in the lighter part of
// the gradient.
vec4 fogColor()
{
  return mix(vec4(1), vec4(135.0 / 255.0, 206.0 / 255.0, 235.0 / 255.0, 1), 0.2);
}

// Calculate a linear fog strength. Value between 0 and 1.
float linearFogStrength(float dist)
{
  return min(dist / vista, 1.0);
}

// Calculate an exponential fog strength. Value between 0 and 1.
float exponentialFogStrength(float dist)
{
  return exp(1.0 - (1.0 / pow(linearFogStrength(dist), 2.0)));
}

// Calculate the normal from the normal map.
vec3 calculateNormal()
{
  vec3 normal = normalize(vNormal);
  vec3 tangent = normalize(vTangent);
  tangent = normalize(tangent - dot(tangent, normal) * normal);
  vec3 binormal = cross(tangent, normal);

  vec3 norm = texture(normalMap, vec2(vTexCoord.s, 1.0 - vTexCoord.t)).xyz;
  norm = norm * 2.0 - vec3(1.0);

  mat3 tbn = mat3(tangent, binormal, normal);

  return normalize(tbn * norm);
}

void main()
{
  // Calculate ambient color.
  vec4 ambientColor = vec4(sunColor * ambientStrength, 1);

  // Caluculate distance to eye.
  float dist = distance(eyePosition, vPosition);

  // Calculate diffuse color.
  vec3 normal = dist < detailed ? calculateNormal() : normalize(vNormal);
  vec3 sunDirection = normalize(sunPosition - vPosition);
  float diffuse = dot(sunDirection, normal);

  vec4 diffuseColor = vec4(0.0);
  if (diffuse > 0.0)
  {
    diffuseColor = vec4(sunColor * diffuse * diffuseFactor, 1.0);
  }

  // Load texture color and mix with vertex color. Flip the texture horizontally.
  vec4 texColor = texture(groundTexture, vec2(vTexCoord.s, 1.0 - vTexCoord.t));
  vec4 fragColor = mix(vec4(vColor, 1), texColor, 0.1);

  // The final color before applying fog.
  vec4 unfoggedColor = fragColor + ambientColor + diffuseColor;

  // Apply fog depending on distance.
  color = mix(unfoggedColor, fogColor(), exponentialFogStrength(dist));
}
