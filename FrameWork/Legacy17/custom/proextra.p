/* ====================================================================
   file      ProExtra.p
   by        Jurjen Dijkstra, 1997
             mailto:jurjen@global-shared.com
             http://www.global-shared.com
   language  Progress 8.2A
   purpose   interface to ProExtra.DLL containing some extra routines.
   note      ProExtra.DLL is not part of Progress, it's custom made
             by myself using Delphi 2.01.
   ==================================================================== */

&IF "{&OPSYS}":U="WIN32":U &THEN
   &GLOB ProExtra "ProExtra.DLL"   

def var hProExtraDLL as integer no-undo.
hProExtraDLL = ?.

/* why use LoadLibrary and FreeLibrary?
   because it allows us to find the dll if we are not sure 
   about the location  */

DEF VAR chProExtraDLL as CHAR NO-UNDO.
FILE-INFO:FILE-NAME = 'custom/proextra.dll'.
chProExtraDLL = FILE-INFO:FULL-PATHNAME.
IF chProExtraDLL=? THEN 
   MESSAGE {&ProExtra} ' not found' VIEW-AS ALERT-BOX.
ELSE
   RUN LoadLibraryA (chProExtraDLL, output hProExtraDLL).

ON CLOSE OF THIS-PROCEDURE
DO:                       
  /* very important memory cleaner */
  DEF VAR ReturnValue as INTEGER NO-UNDO.
  if hProExtraDLL<>? then
     run FreeLibrary (hProExtraDLL, output ReturnValue).
  hProExtraDLL = ?.
END.

PROCEDURE LoadLibraryA EXTERNAL "kernel32" :
  DEFINE INPUT  PARAMETER libname AS CHAR.
  DEFINE RETURN PARAMETER hproc   AS LONG.
END PROCEDURE.

PROCEDURE FreeLibrary EXTERNAL "kernel32" :
  define input parameter hproc        as LONG.
  define return parameter ReturnValue as LONG.
END.

/* -------------- exported functions: ------------------- */

PROCEDURE Bit_Remove EXTERNAL {&ProExtra} :
  define input-output parameter Flags   as LONG.
  define input        parameter OldFlag as LONG.
END PROCEDURE.

PROCEDURE Bit_Or EXTERNAL {&ProExtra} :
  define input-output parameter Flags   as LONG.
  define input        parameter NewFlag as LONG.
END PROCEDURE.

PROCEDURE Bit_Xor EXTERNAL {&ProExtra} :
  define input-output parameter Flags   as LONG.
  define input        parameter NewFlag as LONG.
END PROCEDURE.

PROCEDURE Bit_And EXTERNAL {&ProExtra} :
  define input  parameter Flags        as LONG.
  define input  parameter TestFlag     as LONG.
  define return parameter FlagsAndTest as LONG.
END PROCEDURE.

/* BrowseForFolder was added to proextra.dll by Cyril O' Floinn */
PROCEDURE BrowseForFolder EXTERNAL {&ProExtra} :
  define input  parameter hWndOwner       as LONG.
  define input  parameter lpTitle         as CHAR.
  define input  parameter uiFlags         as LONG.
  define input  parameter lpInitialFolder as CHAR.
  define output parameter lpFolder        as CHAR.
  define return parameter BoolRetVal      as LONG.
END PROCEDURE.

/* =======================================================
   CloseProcessWindows
   sends a WM_CLOSE message to every top-level window
   created by the specified process. 
     Pid : process identifier
     ReturnValue : 0 when no windows are found.
   ------------------------------------------------------- */
PROCEDURE CloseProcessWindows EXTERNAL {&ProExtra} :
  DEFINE INPUT  PARAMETER Pid         AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

/* =======================================================
   GetProcessWindow
   returns the HWND of a top-level window created by
   the specified process.
     Pid  : process identifier
     HWND : window handle
   ------------------------------------------------------- */
PROCEDURE GetProcessWindow EXTERNAL {&ProExtra} :
  DEFINE INPUT  PARAMETER Pid     AS LONG.
  DEFINE RETURN PARAMETER HWND    AS LONG.
END PROCEDURE.

&ENDIF

