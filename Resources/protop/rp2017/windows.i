/* lib/windows.i
 *
 * windows kernel calls
 *
 */

PROCEDURE CreateProcessA EXTERNAL "kernel32.dll":
  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
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
  &ELSE
    &IF {&PROCESS-ARCHITECTURE} = 64 &THEN
      DEFINE INPUT  PARAMETER lpApplicationName    AS INT64.
      DEFINE INPUT  PARAMETER lpCommandline        AS CHAR.
      DEFINE INPUT  PARAMETER lpProcessAttributes  AS INT64.
      DEFINE INPUT  PARAMETER lpThreadAttributes   AS INT64.
      DEFINE INPUT  PARAMETER bInheritHandles      AS INT64.
      DEFINE INPUT  PARAMETER dCreationFlags       AS INT64.
      DEFINE INPUT  PARAMETER lpEnvironment        AS INT64.
      DEFINE INPUT  PARAMETER lpCurrentDirectory   AS INT64.
      DEFINE INPUT  PARAMETER lpStartupInfo        AS INT64.
      DEFINE INPUT  PARAMETER lpProcessInformation AS INT64.
      DEFINE RETURN PARAMETER bResult              AS INT64.
    &ELSE
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
    &ENDIF
  &ENDIF
end procedure.

procedure OpenProcess external "kernel32.dll" :

  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
    define input  parameter desiredAccess as long.
    define input  parameter inheritFlag   as long.
    define input  parameter pid           as long.
    define return parameter procHdl       as long.
  &ELSE
    &IF {&PROCESS-ARCHITECTURE} = 64 &THEN
      define input  parameter desiredAccess as int64.
      define input  parameter inheritFlag   as int64.
      define input  parameter pid           as int64.
      define return parameter procHdl       as int64.
    &ELSE
      define input  parameter desiredAccess as long.
      define input  parameter inheritFlag   as long.
      define input  parameter pid           as long.
      define return parameter procHdl       as long.
    &ENDIF
  &ENDIF

end procedure.

procedure TerminateProcess external "kernel32.dll" :
  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
    define input  parameter hProcess  as long.
    define input  parameter uExitCode as long.
    define return parameter retval    as long.
  &ELSE
    &IF {&PROCESS-ARCHITECTURE} = 64 &THEN
      define input  parameter hProcess  as int64.
      define input  parameter uExitCode as int64.
      define return parameter retval    as int64.
    &ELSE
      define input  parameter hProcess  as long.
      define input  parameter uExitCode as long.
      define return parameter retval    as long.
    &ENDIF
  &ENDIF

end procedure.

procedure CloseHandle external "kernel32.dll":
  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
    define input  parameter procHdl  as long.
    define return parameter statcode as long.
  &ELSE
    &IF {&PROCESS-ARCHITECTURE} = 64 &THEN
      define input  parameter procHdl  as int64.
      define return parameter statcode as int64.
    &ELSE
      define input  parameter procHdl  as long.
      define return parameter statcode as long.
    &ENDIF
  &ENDIF
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

  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
    PUT-LONG ( lpStartupInfo, 1 )  = 68.
    PUT-LONG ( lpStartupInfo, 45 ) = 1. /* = STARTF_USESHOWWINDOW */
  &ELSE
    &IF {&PROCESS-ARCHITECTURE} = 64 &THEN
      PUT-INT64 ( lpStartupInfo, 1 )  = 68.
      PUT-INT64 ( lpStartupInfo, 45 ) = 1. /* = STARTF_USESHOWWINDOW */
    &ELSE
      PUT-LONG ( lpStartupInfo, 1 )  = 68.
      PUT-LONG ( lpStartupInfo, 45 ) = 1. /* = STARTF_USESHOWWINDOW */
    &ENDIF
  &ENDIF

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

      &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
        PID = GET-LONG( lpProcessInformation, 9 ).
        RUN CloseHandle ( input GET-LONG( lpProcessInformation, 1 ), OUTPUT ReturnValue ).
        RUN CloseHandle ( input GET-LONG( lpProcessInformation, 5 ), OUTPUT ReturnValue ).
      &ELSE
        &IF {&PROCESS-ARCHITECTURE} = 64 &THEN
          PID = GET-INT64( lpProcessInformation, 9 ).
          RUN CloseHandle ( input GET-INT64( lpProcessInformation, 1 ), OUTPUT ReturnValue ).
          RUN CloseHandle ( input GET-INT64( lpProcessInformation, 5 ), OUTPUT ReturnValue ).
        &ELSE
          PID = GET-LONG( lpProcessInformation, 9 ).
          RUN CloseHandle ( input GET-LONG( lpProcessInformation, 1 ), OUTPUT ReturnValue ).
          RUN CloseHandle ( input GET-LONG( lpProcessInformation, 5 ), OUTPUT ReturnValue ).
        &ENDIF
      &ENDIF

    end.

  SET-SIZE( lpStartupInfo )        = 0.
  SET-SIZE( lpProcessInformation ) = 0.
  SET-SIZE( lpWorkingDirectory )   = 0.

END PROCEDURE.

procedure kill:

  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
    define input parameter PID as integer no-undo.
    define variable processHandle as integer no-undo.
    define variable returnValue   as integer no-undo.
  &ELSE
    &IF {&PROCESS-ARCHITECTURE} = 64 &THEN
      define input parameter PID as int64 no-undo.
      define variable processHandle as int64 no-undo.
      define variable returnValue   as int64 no-undo.
    &ELSE
      define input parameter PID as integer no-undo.
      define variable processHandle as integer no-undo.
      define variable returnValue   as integer no-undo.
    &ENDIF
  &ENDIF

  run OpenProcess ( 1, 0, PID, OUTPUT ProcessHandle ).

  if processHandle NE 0 then
    do:
      run TerminateProcess ( processHandle, 0, output returnValue ).
      run CloseHandle ( processHandle, output returnValue ).
    end.

end.

/*** end windows.i ***/

