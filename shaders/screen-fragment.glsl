#version 430 core

uniform vec4 color;
uniform vec4 backgroundColor;
uniform sampler2D display;

in vec2 uv;

out vec4 fragColor;

void main() {
  float weight = texture(display, uv).r;
  // fragColor = mix(color,backgroundColor,weight);
  fragColor = vec4(weight.r, 0, 0, 1);
}