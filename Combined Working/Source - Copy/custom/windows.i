/* ====================================================================
   file      windows.i
   by        Jurjen Dijkstra, 1997
             mailto:jurjen@global-shared.com
             http://www.global-shared.com
   language  Progress 8.2A
   ==================================================================== */

&IF DEFINED(WINDOWS_I)=0 &THEN
&GLOBAL-DEFINE WINDOWS_I

&IF "{&OPSYS}":U="WIN32":U &THEN
   /* 32-bit definitions, Progress 8.2+ */

   &GLOB A A
   /* data types */
   &Glob HWND long
   &Glob BOOL long
   &Glob HINSTANCE long
   &Glob INT long
   &GLOB INTSIZE 4

   /* libraries */
   &GLOB USER     "user32"
   &GLOB KERNEL   "kernel32"
   &GLOB SHELL    "shell32"
   &GLOB MAPI     "mapi32"
   &GLOB GDI      "gdi32"
   &GLOB MMEDIA   "winmm"
   &GLOB WINSPOOL "winspool.drv"
   &GLOB ADVAPI   "advapi32"
   &GLOB A A

&ELSE
   /* 16-bit definitions, Progress 7 to 8.1 */

   /* data types */
   &Glob HWND short
   &Glob BOOL short
   &Glob HINSTANCE short
   &Glob INT short
   &GLOB INTSIZE 2

   /* libraries */
   &GLOB USER   "user.exe"
   &GLOB KERNEL "kernel.exe"
   &GLOB SHELL  "shell.dll"
   &GLOB MAPI   "mapi.dll"
   &GLOB GDI    "gdi.exe"
   &GLOB A

&ENDIF


/* messages */
&Glob WM_PAINT 15
&Glob WM_HSCROLL 276
&Glob WM_VSCROLL 277
&Glob WM_LBUTTONDOWN 513
&Glob WM_LBUTTONUP 514
&Glob WM_RBUTTONDOWN 516
&Glob WM_RBUTTONUP 517
&GLOB WM_USER 1024

/* mouse buttons */
&Glob MK_LBUTTON 1
&Glob MK_RBUTTON 2

/* scrollbars */
&Glob SB_HORZ 0
&Glob SB_VERT 1
&Glob SB_BOTH 3
&Glob SB_THUMBPOSITION 4

/* editors */
&IF "{&OPSYS}":U="WIN32":U &THEN
   &GLOB EM_SETPASSWORDCHAR 204
&ELSE
    &GLOB EM_SETPASSWORDCHAR {&WM_USER} + 28
&ENDIF

/* some window styles */
&GLOB GWL_STYLE -16
&GLOB WS_MAXIMIZEBOX 65536
&GLOB WS_MINIMIZEBOX 131072
&GLOB WS_THICKFRAME  262144
&GLOB WS_CAPTION 12582912
&GLOB WS_BORDER 8388608

/* some extended window styles */
&GLOB GWL_EXSTYLE -20
&GLOB WS_EX_CONTEXTHELP 1024
&GLOB WS_EX_PALETTEWINDOW 392

/* system commands/menu */
&GLOB SC_SIZE      61440  
&GLOB SC_MINIMIZE  61472
&GLOB SC_MAXIMIZE  61488  
&GLOB MF_BYCOMMAND 0

/* placement order (Z-order) */
&GLOB HWND_TOPMOST -1
&GLOB HWND_NOTOPMOST -2

 
/* window-positioning flags */
&GLOB SWP_NOSIZE 1
&GLOB SWP_NOMOVE 2
&GLOB SWP_NOZORDER 4
&GLOB SWP_NOACTIVATE 16 
&GLOB SWP_FRAMECHANGED 32
&GLOB SWP_SHOWWINDOW 64


/* registry */
&GLOB HKEY_CLASSES_ROOT -2147483648
&GLOB HKEY_CURRENT_USER -2147483647
&GLOB HKEY_LOCAL_MACHINE -2147483646
&GLOB HKEY_USERS -2147483645
&GLOB HKEY_PERFORMANCE_DATA -2147483644
&GLOB HKEY_CURRENT_CONFIG -2147483643
&GLOB HKEY_DYN_DATA -2147483642


&GLOB ERROR_SUCCESS 0
&GLOB ERROR_NO_MORE_ITEMS 259

&GLOB MAX_PATH 260

/* results from WaitForSingleObject */
&GLOB WAIT_ABANDONED 128
&GLOB WAIT_OBJECT_0 0

/* menu manipulation */
&GLOB MF_BYPOSITION 1024
&GLOB MF_REMOVE     256


/* get a handle to the procedure definitions */
&IF DEFINED(DONTDEFINE-HPAPI)=0 &THEN
   DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
   IF NOT VALID-HANDLE(hpApi) THEN RUN custom/Windows.p PERSISTENT SET hpApi.

   /* forward function declarations. Must not be included in windows.p : */
   {custom/winfunc.i}
&ENDIF

&ENDIF  /* &IF DEFINED(WINDOWS_I)=0 */

