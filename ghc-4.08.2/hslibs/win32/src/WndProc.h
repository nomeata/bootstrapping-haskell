#ifndef __WNDPROC_H
#define __WNDPROC_H
#include <windows.h>

#if TARGET_HUGS
DLLEXPORT(LRESULT CALLBACK) genericWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
#else
extern LRESULT CALLBACK genericWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

extern HWND    win32_hwnd;
extern UINT    win32_msg;
extern WPARAM  win32_wparam;
extern LPARAM  win32_lparam;
extern LRESULT win32_result;

#endif

#endif /* __WNDPROC_H */
