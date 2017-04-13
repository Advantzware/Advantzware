/* ===========================================================================
   file    : WINFUNC.I
   by      : Jurjen Dijkstra, 1997
             mailto:jurjen@global-shared.com
             http://www.global-shared.com
   purpose : Forward declarations for functions that call API procedures.
             This include file is included in windows.i
   =========================================================================== */

/* prevent multiple inclusion: */
&IF DEFINED(WINFUNC_I)=0 &THEN
&GLOBAL-DEFINE WINFUNC_I

/* start persistent procedure holding the function implementations.
   The forward declarations are needed in winfunc.p, but the
   "run winfunc.p persistent" part must be prevented in winfunc.p : */
     
DEFINE NEW GLOBAL SHARED VARIABLE hppure4gltvWinFunc AS HANDLE NO-UNDO.
&IF DEFINED(DONTRUN-WINFUNC)=0 &THEN
  IF NOT VALID-HANDLE(hppure4gltvWinFunc) THEN RUN protools/abhack/pure4gltvWinfunc.p PERSISTENT SET hppure4gltvWinFunc.
&ELSE
  hppure4gltvWinFunc = this-procedure:handle.
&ENDIF

/* --- the forward declarations : --- */

FUNCTION GetLastError      /* 1:1 implementation of API */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hppure4gltvWinFunc.    

FUNCTION GetParent         /* 1:1 implementation of API */
         RETURNS INTEGER   /* = hWnd van parent */
         (input hwnd as INTEGER) 
         IN hppure4gltvWinFunc.    

FUNCTION ShowLastError     /* calls GetLastError and views it as alert-box */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hppure4gltvWinFunc.    

FUNCTION CreateProcess     /* wrapper for the big API definition */
         RETURNS INTEGER   /* = if success then hProcess else 0  */
         (input CommandLine as CHAR,
          input CurrentDir  as CHAR,
          input wShowWindow as INTEGER) 
         in hppure4gltvWinFunc.    

&ENDIF  /* &IF DEFINED(WINFUNC_I)=0 */
