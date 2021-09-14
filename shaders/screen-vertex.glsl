#version 430 core

layout(location = 0) in vec2 position;

out vec2 uv;

void main() {
  uv = position;
  uv.y *= -1.0;
  uv += vec2(1);
  uv *= 0.5;
  gl_Position = vec4(position, 0, 1);
}