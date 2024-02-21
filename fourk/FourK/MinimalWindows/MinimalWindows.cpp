
#include "MinimalWindows.h"

extern "C" {

#ifdef _DEBUG
  void APIENTRY debugCallback(
      GLenum          source
    , GLenum          type
    , GLuint          id
    , GLenum          severity
    , GLsizei         length
    , GLchar const *  message
    , void const *    userParam
    )
  {
    printf(message);
    printf("\n");
  }
  char debugLog[0xFFFF];
#endif

  #pragma code_seg(".init")
  void init() {
#ifdef _DEBUG
    glEnable(GL_DEBUG_OUTPUT);
    ((PFNGLDEBUGMESSAGECALLBACKPROC)wglGetProcAddress("glDebugMessageCallback"))(debugCallback, 0);
#endif

  fragmentShaderProgram = ((PFNGLCREATESHADERPROGRAMVPROC)wglGetProcAddress(nm_glCreateShaderProgramv))(GL_FRAGMENT_SHADER, 1, fragmentShaders);

#ifdef _DEBUG
  ((PFNGLGETSHADERINFOLOGPROC)wglGetProcAddress("glGetProgramInfoLog"))(fragmentShaderProgram, sizeof(debugLog), NULL, debugLog);
  printf(debugLog);
  glDisable(GL_DEBUG_OUTPUT);
#endif
  }

  void draw(float time) {
    ((PFNGLUSEPROGRAMPROC)wglGetProcAddress(nm_glUseProgram))(fragmentShaderProgram);
    ((PFNGLUNIFORM4FPROC)wglGetProcAddress(nm_glUniform4f))(
        0 // Uniform location
      , time
      , XRES
      , YRES
      , 0
      );
    glRects(-1, -1, 1, 1);
  }

  #pragma code_seg(".WndProc")
  LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
  {
    if (uMsg == WM_SYSCOMMAND && (wParam == SC_SCREENSAVE || wParam == SC_MONITORPOWER))
      return 0;

    if (uMsg == WM_CLOSE || uMsg == WM_DESTROY || (uMsg == WM_KEYDOWN && wParam == VK_ESCAPE)) {
      PostQuitMessage(0);
      return 0;
    }

    if (uMsg == WM_SIZE)
    {
      glViewport(0, 0, LOWORD(lParam), HIWORD(lParam));
    }

    if (uMsg == WM_CHAR || uMsg == WM_KEYDOWN)
    {
      if (wParam == VK_ESCAPE)
      {
        PostQuitMessage(0);
        return 0;
      }
    }

    return(DefWindowProcA(hWnd, uMsg, wParam, lParam));
  }
}

#pragma code_seg(".main")
#ifdef USE_CRINKLER
void entrypoint() {
#else
int __cdecl main() {
#endif
  auto hinstance = GetModuleHandle(0);
  assert(hinstance);

  auto hbackground = CreateSolidBrush(RGB(0x66, 0x33, 0x99));
  assert(hbackground);

  windowClassSpecification.hInstance      = hinstance;
  windowClassSpecification.hbrBackground  = hbackground;

  auto regOk = RegisterClassA(&windowClassSpecification);
  assert(regOk);

  auto dwStyle = WS_VISIBLE | WS_OVERLAPPEDWINDOW | WS_POPUP;

	BOOL rectOk = AdjustWindowRect(&windowRect, dwStyle, 0);
  assert(rectOk);

  auto width  = windowRect.right  - windowRect.left;
  auto height = windowRect.bottom - windowRect.top;

  auto hwnd = CreateWindowExA(
    0                                             // dwExStyle
  , windowClassSpecification.lpszClassName        // lpClassName
  , nullptr                                       // lpWindowName
  , dwStyle                                       // dwStyle
  , (GetSystemMetrics(SM_CXSCREEN) - width) >> 1  // nX
  , (GetSystemMetrics(SM_CYSCREEN) - height) >> 1 // nY
  , width                                         // nWidth
  , height                                        // nHeight
  , nullptr                                       // hWndParent
  , nullptr                                       // hMenu
  , nullptr                                       // hInstance
  , nullptr                                       // lpParam
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

  auto before = timeGetTime();

  while(!done) {

    while (PeekMessageA(&msg, 0, 0, 0, PM_REMOVE)) {
      if (msg.message == WM_QUIT) done = 1;
      // Result intentionally ignored
      TranslateMessage(&msg);
      // Result intentionally ignored
      DispatchMessageA(&msg);
    }

    auto now = timeGetTime();
    draw((now - before)/1000.F);
    auto swapOk = SwapBuffers(hdc);
    assert(swapOk);

  }

#ifdef USE_CRINKLER
  ExitProcess(0);
#else
  return 0;
#endif
}

