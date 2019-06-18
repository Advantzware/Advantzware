/* lib/windows32.p
 *
 * windows kernel calls
 *
 * use this when _progres is a 32 bit executable
 *
 */

session:add-super-procedure( this-procedure ).

return.


PROCEDURE CreateProcessA EXTERNAL "kernel32.dll":
  DEFINE INPUT  PARAMETER lpApplicationName    AS LONG.
  DEFINE INPUT  PARAMETER lpCommandline        AS CHAR.
  DEFINE INPUT  PARAMETER lpProcessAttributes  AS LONG.
  DEFINE INPUT  PARAMETER lpThreadAttributes   AS LONG.
  DEFINE INPUT  PARAMETER bInheritHandles      AS LONG.
  DEFINE INPUT  PARAMETER dCreationFlags       AS LONG.
  DEFINE INPUT  PARAMETER lpEnvironment        AS LONG.
  DEFINE INPUT  PARAMETER lpCurrentDirectory   AS LONG.
  DEFINE INPUT  PARAMETER lpStartupInfo        AS LONG.
  DEFINE INPUT  PARAMETER lpProcessInformation AS LONG.
  DEFINE RETURN PARAMETER bResult              AS LONG.
end procedure.

procedure OpenProcess external "kernel32.dll" :
  define input  parameter desiredAccess as long.
  define input  parameter inheritFlag   as long.
  define input  parameter pid           as long.
  define return parameter procHdl       as long.
end procedure.

procedure TerminateProcess external "kernel32.dll" :
  define input  parameter hProcess  as long.
  define input  parameter uExitCode as long.
  define return parameter retval    as long.
end procedure.

procedure CloseHandle external "kernel32.dll":
  define input  parameter procHdl  as long.
  define return parameter statcode as long.
end.


procedure spawn:

  DEFINE INPUT  PARAMETER CommandLine as character NO-UNDO.
  DEFINE INPUT  PARAMETER WorkingDir  as character NO-UNDO.
  DEFINE OUTPUT PARAMETER PID         AS INTEGER   NO-UNDO.

  define variable wShowWindow   AS INTEGER NO-UNDO INITIAL 0.
  define variable bResult       AS INTEGER NO-UNDO.
  define variable ReturnValue   AS INTEGER NO-UNDO.
  define variable lpStartupInfo AS MEMPTR  NO-UNDO.

  SET-SIZE ( lpStartupInfo )     = 68.

  PUT-LONG ( lpStartupInfo, 1 )  = 68.
  PUT-LONG ( lpStartupInfo, 45 ) = 1. /* = STARTF_USESHOWWINDOW */

  PUT-SHORT( lpStartupInfo, 49 ) = wShowWindow.

  define variable lpProcessInformation AS MEMPTR.
  SET-SIZE( lpProcessInformation )   = 16.

  define variable lpWorkingDirectory AS MEMPTR.
  IF WorkingDir NE "" THEN DO:
    SET-SIZE( lpWorkingDirectory )     = 256.
    PUT-STRING( lpWorkingDirectory, 1 ) = WorkingDir.
  END.

  RUN CreateProcessA(
    0,
    CommandLine,
    0,
    0,
    0,
    0,
    0,
    ( IF WorkingDir = "" THEN 0 ELSE GET-POINTER-VALUE( lpWorkingDirectory )),
    GET-POINTER-VALUE( lpStartupInfo ),
    GET-POINTER-VALUE( lpProcessInformation ),
    OUTPUT bResult
  ).

  IF bResult <> 0 THEN     /* release kernel-objects hProcess and hThread: */
    do:

      PID = GET-LONG( lpProcessInformation, 9 ).
      RUN CloseHandle ( input GET-LONG( lpProcessInformation, 1 ), OUTPUT ReturnValue ).
      RUN CloseHandle ( input GET-LONG( lpProcessInformation, 5 ), OUTPUT ReturnValue ).

    end.

  SET-SIZE( lpStartupInfo )        = 0.
  SET-SIZE( lpProcessInformation ) = 0.
  SET-SIZE( lpWorkingDirectory )   = 0.

END PROCEDURE.


procedure kill:

  define input parameter PID as integer no-undo.
  define variable processHandle as integer no-undo.
  define variable returnValue   as integer no-undo.

  run OpenProcess ( 1, 0, PID, OUTPUT ProcessHandle ).

  if processHandle NE 0 then
    do:
      run TerminateProcess ( processHandle, 0, output returnValue ).
      run CloseHandle ( processHandle, output returnValue ).
    end.

end.
