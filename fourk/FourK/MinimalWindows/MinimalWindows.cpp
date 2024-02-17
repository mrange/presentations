
#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#define WINDOWS_IGNORE_PACKING_MISMATCH

#include "assert.h"

#include <stdio.h>

#include <windows.h>
#include <winuser.h>
#include <mmsystem.h>
#include <mmreg.h>
#include <GL/gl.h>

#pragma comment(lib, "opengl32.lib")

#pragma data_seg(".pixelFormatDescriptor")
static PIXELFORMATDESCRIPTOR pixelFormatSpecification {
    sizeof(PIXELFORMATDESCRIPTOR)                           // nSize;
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

WNDCLASS windowClassSpecification {
      0   // style
    , 0   // lpfnWndProc
    , 0   // cbClsExtra
    , 0   // cbWndExtra
    , 0   // hInstance
    , 0   // hIcon
    , 0   // hCursor
    , 0   // hbrBackground
    , 0   // lpszMenuName
    , 0   // lpszClassName
};

void init();

int __cdecl main() {

  auto dwStyle = WS_VISIBLE | WS_OVERLAPPEDWINDOW | WS_POPUP;
  auto hwnd = CreateWindowEx(
    0                        // dwExStyle
  , L"STATIC"                // lpClassName
  , nullptr                  // lpWindowName
  , dwStyle                  // dwStyle
  , 0                        // X
  , 0                        // Y
  , 1920                     // nWidth
  , 1080                     // nHeight
  , nullptr                  // hWndParent
  , nullptr                  // hMenu
  , nullptr                  // hInstance
  , nullptr                  // lpParam
  );
  assert(hwnd);

  auto hdc = GetDC(hwnd);
  assert(hdc);

  auto pixelFormat = ChoosePixelFormat(
    hdc
  , &pixelFormatSpecification
  );
  assert(pixelFormat);

  auto setOk = SetPixelFormat(
    hdc
  , pixelFormat
  , nullptr
  );
  assert(setOk);

  auto hglrc = wglCreateContext(hdc);
  assert(hglrc);

  auto makeOk = wglMakeCurrent(hdc, hglrc);
  assert(makeOk);

  init();

  auto done = false;

  MSG msg {};

  while(!done) {
    while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
    {
      if (msg.message == WM_QUIT) done = 1;
      TranslateMessage(&msg);
      DispatchMessage(&msg);
      printf("%d - %x\n", done, msg.message);
    }

  }

  return 0;
}

void init() {
}
