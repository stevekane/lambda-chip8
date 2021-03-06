#version 430 core

uniform vec4 color;
uniform vec4 bgcolor;
uniform sampler2D display;

in vec2 uv;

out vec4 fragColor;

void main() {
  float weight = texture(display, uv).r;
  fragColor = mix(bgcolor, color, weight);
}