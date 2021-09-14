#version 430 core

uniform vec4 color;
uniform vec4 backgroundColor;
uniform sampler2D display;

in vec2 uv;

out vec4 fragColor;

void main() {
  float weight = texture2D(display, uv).a;
  weight = ceil(weight);
  fragColor = mix(backgroundColor, color, weight);
}