#pragma once

#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#define WINDOWS_IGNORE_PACKING_MISMATCH

#include "assert.h"

#ifdef _DEBUG
#include <stdio.h>
#endif

#include <windows.h>
#include <winuser.h>
#include <mmsystem.h>
#include <mmreg.h>

#include <GL/gl.h>
#include "glext.h"

// #define USE_MINI

#define XRES 1600
#define YRES 900

extern "C" {
  #pragma data_seg(".fltused")
  int  _fltused = 0;

  static LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

  #pragma data_seg(".xres")
  static int xres = XRES;
  #pragma data_seg(".yres")
  static int yres = YRES;

  #pragma data_seg(".windowRect")
  static RECT windowRect {
    0
  , 0
  , XRES
  , YRES
  };

  #pragma data_seg(".pixelFormatDescriptor")
  static PIXELFORMATDESCRIPTOR pixelFormatSpecification {
      sizeof(PIXELFORMATDESCRIPTOR)                           // nSize
    , 1                                                       // nVersion
    , PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_DOUBLEBUFFER  // dwFlags
    , PFD_TYPE_RGBA                                           // iPixelType
    , 32                                                      // cColorBits
    , 0                                                       // cRedBits
    , 0                                                       // cRedShift
    , 0                                                       // cGreenBits
    , 0                                                       // cGreenShift
    , 0                                                       // cBlueBits
    , 0                                                       // cBlueShift
    , 8                                                       // cAlphaBits
    , 0                                                       // cAlphaShift
    , 0                                                       // cAccumBits
    , 0                                                       // cAccumRedBits
    , 0                                                       // cAccumGreenBits
    , 0                                                       // cAccumBlueBits
    , 0                                                       // cAccumAlphaBits
    , 32                                                      // cDepthBits
    , 0                                                       // cStencilBits
    , 0                                                       // cAuxBuffers
    , PFD_MAIN_PLANE                                          // iLayerType
    , 0                                                       // bReserved
    , 0                                                       // dwLayerMask
    , 0                                                       // dwVisibleMask
    , 0                                                       // dwDamageMask
  };

  #pragma data_seg(".windowClassSpecification")
  static WNDCLASSA windowClassSpecification {
        CS_OWNDC | CS_HREDRAW | CS_VREDRAW  // style
      , &WndProc                            // lpfnWndProc
      , 0                                   // cbClsExtra
      , 0                                   // cbWndExtra
      , 0                                   // hInstance
      , 0                                   // hIcon
      , 0                                   // hCursor
      , 0                                   // hbrBackground
      , 0                                   // lpszMenuName
      , "DEMO"                              // lpszClassName
  };

  #pragma data_seg(".glCreateShaderProgramv")
  static const char nm_glCreateShaderProgramv[] = "glCreateShaderProgramv";

  #pragma data_seg(".glUseProgram")
  static const char nm_glUseProgram[] = "glUseProgram";

  #pragma data_seg(".glUniform4f")
  static const char nm_glUniform4f[] = "glUniform4f";

  #pragma data_seg(".fragmentShaderProgram")
  static GLint fragmentShaderProgram;

  #pragma data_seg(".shader_fx")
  static GLchar const shader_fx[] =
#ifdef USE_MINI
#define SHADER_MINIFIER_IMPL
#include "shader.inl"
;
#else
  R"FS(
#version 430

uniform vec4 state;
out vec4 fcol;

const float PI        = acos(-1);
const float PI_2      = .5*PI;
const float TAU       = 2*PI;
const float planeDist = 1.0-0.75;
const int   furthest  = 12;
const int   fadeFrom  = max(furthest-4, 0);
const float fadeDist  = planeDist*float(furthest - fadeFrom);

float getTime() {
  return state.x;
}

vec2 getRes() {
  return state.yz;
}

mat2 rot(float a) {
  float c = cos(a);
  float s = sin(a);
  return mat2(c,s,-s,c);
}

// License: Unknown, author: Unknown, found: don't remember
vec4 alphaBlend(vec4 back, vec4 front) {
  float w = front.w + back.w*(1.0-front.w);
  vec3 xyz = (front.xyz*front.w + back.xyz*back.w*(1.0-front.w))/w;
  return w > 0.0 ? vec4(xyz, w) : vec4(0.0);
}

// License: Unknown, author: Unknown, found: don't remember
vec3 alphaBlend(vec3 back, vec4 front) {
  return mix(back, front.xyz, front.w);
}

// License: Unknown, author: Unknown, found: don't remember
float tanh_approx(float x) {
  //  Found this somewhere on the interwebs
  //  return tanh(x);
  float x2 = x*x;
  return clamp(x*(27.0 + x2)/(27.0+9.0*x2), -1.0, 1.0);
}

vec3 offset(float z) {
  float a = z*0.5;
  vec2 p = -0.25*(vec2(cos(a), sin(a*sqrt(2.0))) + vec2(cos(a*sqrt(0.75)), sin(a*sqrt(0.6))));
  return vec3(p, z);
}

vec3 doffset(float z) {
  const float eps = 0.05;
  return (offset(z + eps) - offset(z - eps))/(2.0*eps);
}

vec3 ddoffset(float z) {
  const float eps = 0.05;
  return (doffset(z + eps) - doffset(z - eps))/(2.0*eps);
}

vec3 skyColor(vec3 ro, vec3 rd) {
  return vec3(0.0);
}

// License: MIT, author: Pascal Gilcher, found: https://www.shadertoy.com/view/flSXRV
float atan_approx(float y, float x) {
  float cosatan2 = x / (abs(x) + abs(y));
  float t = PI_2 - cosatan2 * PI_2;
  return y < 0.0 ? -t : t;
}

vec2 toPolar(vec2 p) {
  return vec2(length(p), atan_approx(p.y, p.x));
}

vec2 toRect(vec2 p) {
  return vec2(p.x*cos(p.y), p.x*sin(p.y));
}

// License: MIT OR CC-BY-NC-4.0, author: mercury, found: https://mercury.sexy/hg_sdf/
float modMirror1(inout float p, float size) {
  float halfsize = size*0.5;
  float c = floor((p + halfsize)/size);
  p = mod(p + halfsize,size) - halfsize;
  p *= mod(c, 2.0)*2.0 - 1.0;
  return c;
}

// License: MIT, author: Inigo Quilez, found: https://www.iquilezles.org/www/articles/smin/smin.htm
float pmin(float a, float b, float k) {
  float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
  return mix(b, a, h) - k*h*(1.0-h);
}

float pabs(float a, float k) {
  return -pmin(a, -a, k);
}

float smoothKaleidoscope(inout vec2 p, float sm, float rep) {
  vec2 hp = p;

  vec2 hpp = toPolar(hp);
  float rn = modMirror1(hpp.y, TAU/rep);

  float sa = PI/rep - pabs(PI/rep - abs(hpp.y), sm);
  hpp.y = sign(hpp.y)*(sa);

  hp = toRect(hpp);

  p = hp;

  return rn;
}

vec3 palette( float t ) {
  return (1.0+cos(1.0*vec3(0.0, 1.0, 2.0)+TAU*t))*0.5;
}

// License: Unknown, author: kishimisu, found: https://www.shadertoy.com/view/mtyGWy
vec3 kishimisu(vec3 col, vec2 p, float tm, float n) {
  vec2 p0 = p;
  vec3 finalColor = vec3(0.0);

  vec2 p1 = p;
  for (float i = 0.0; i < 4.0; i++) {
    p1 = fract(p1 * 2.0+0.0125*tm) - 0.5;

    float d = length(p1) * exp(-length(p0));

    vec3 cc = palette(length(p0) + i*.4 + tm*.2);

    d = sin(d*8. + tm)/8.;
    d = abs(d);

    d -= 0.0025;
    d = max(d, 0.005);
    d = (0.0125 / d);
    d *= d;

    col += cc * d;
  }

  return 0.5*(col);
}

vec3 effect(vec2 p, float tm, float n) {
  vec3 col = vec3(0.0);
  vec2 kp = p;
  float kl = dot(kp, kp);
  float nn = 2.0*floor(mix(3.0, 16.0, 0.5+0.5*sin(0.1*n)));
  float kn = smoothKaleidoscope(kp, 0.1, nn);
  kp += 0.5*sin(vec2(1.0, sqrt(0.5))*tm*0.21);
  col = kishimisu(col, kp, tm, n);
  col -= 0.75*vec3(0.0, 1.0, 2.0).zyx*dot(p, p);
  col = clamp(col, 0.0, 4.0);
  return col;
}

vec4 plane(vec3 ro, vec3 rd, vec3 pp, vec3 off, float aa, float n) {
  vec2 p = (pp-off*vec3(1.0, 1.0, 0.0)).xy;
  float l = length(p);
  p *= mix(0.5, 0.75, 0.5+0.5*sin(n*0.071));
  float tm = 0.5*0.125*getTime()+0.125*n;
  p *= rot(-tm);
  float fade = smoothstep(0.1, 0.15, l);
  if (fade < 0.05) return vec4(0.0);
  vec4 col = vec4(0.0);

  col.xyz = effect(p, tm, n);
  float i = max(max(col.x, col.y), col.z)*0.75;
  col.w = (tanh_approx(0.5+l+max((i), 0.0))*fade);
  return col;
}

float pcos(float a) {
  return 0.5+0.5*cos(a);
}

vec3 color(vec3 ww, vec3 uu, vec3 vv, vec3 ro, vec2 p) {
  float lp = length(p);
  vec2 np = p + 1.0/getRes().xy;
  const float rdd_per   = 10.0;
  float rdd =  (1.75+0.75*pow(lp,1.5)*tanh_approx(lp+0.9*pcos(rdd_per*p.x)*pcos(rdd_per*p.y)));
//  float rdd = 2.0;

  vec3 rd = normalize(p.x*uu + p.y*vv + rdd*ww);
  vec3 nrd = normalize(np.x*uu + np.y*vv + rdd*ww);

  float nz = floor(ro.z / planeDist);

  vec3 skyCol = skyColor(ro, rd);


  vec4 acol = vec4(0.0);
  const float cutOff = 0.975;
  bool cutOut = false;

  float maxpd = 0.0;

  // Steps from nearest to furthest plane and accumulates the color
  for (int i = 1; i <= furthest; ++i) {
    float pz = planeDist*nz + planeDist*float(i);

    float pd = (pz - ro.z)/rd.z;

    if (pd > 0.0 && acol.w < cutOff) {
      vec3 pp = ro + rd*pd;
      maxpd = pd;
      vec3 npp = ro + nrd*pd;

      float aa = 3.0*length(pp - npp);

      vec3 off = offset(pp.z);

      vec4 pcol = plane(ro, rd, pp, off, aa, nz+float(i));

      float nz = pp.z-ro.z;
      float fadeIn = smoothstep(planeDist*float(furthest), planeDist*float(fadeFrom), nz);
      float fadeOut = smoothstep(0.0, planeDist*0.1, nz);
      pcol.w *= fadeOut*fadeIn;
      pcol = clamp(pcol, 0.0, 1.0);

      acol = alphaBlend(pcol, acol);
    } else {
      cutOut = true;
      acol.w = acol.w > cutOff ? 1.0 : acol.w;
      break;
    }

  }

  vec3 col = alphaBlend(skyCol, acol);
// To debug cutouts due to transparency
//  col += cutOut ? vec3(1.0, -1.0, 0.0) : vec3(0.0);
  return col;
}

vec3 effect(vec2 p, vec2 pp) {
  float tm  = planeDist*getTime();
  vec3 ro   = offset(tm);
  vec3 dro  = doffset(tm);
  vec3 ddro = ddoffset(tm);

  vec3 ww = normalize(dro);
  vec3 uu = normalize(cross(normalize(vec3(0.0,1.0,0.0)+ddro), ww));
  vec3 vv = cross(ww, uu);

  vec3 col = color(ww, uu, vv, ro, p);
  col *= smoothstep(0.0, 4.0, getTime());
  col = clamp(col, 0.0, 1.0);
  col = sqrt(col);
  return col;
}

void main() {
  vec2 r = getRes();
  vec2 q = gl_FragCoord.xy/r.xy;
  vec2 p = -1. + 2. * q;
  vec2 pp = p;
  p.x *= r.x/r.y;

  vec3 col = effect(p, pp);

  fcol = vec4(col, 1.0);
}
)FS";
#endif

  #pragma data_seg(".fragmentShaders")
  static char const * fragmentShaders[] = {shader_fx};

}

