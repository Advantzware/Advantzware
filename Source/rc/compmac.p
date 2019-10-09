/***************************************************************************\
*****************************************************************************
**  Program: D:\RPROREL\RC\COMPMAC.P
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF VAR source_Fid AS CHAR FORMAT 'x(30)' NO-UNDO.
DEF STREAM s-in.
IF SEARCH("compile.mac") = ? THEN
DO:
  BELL.
  MESSAGE "Cannot find compile.mac - Aborted".
  RETURN.
END.
ELSE
MESSAGE "Compiling from " SEARCH("compile.mac").
INPUT STREAM s-in FROM "compile.mac".
INPUT FROM "nul".
OUTPUT TO coerrs.lst NO-ECHO APPEND.
REPEAT:
  IMPORT stream s-in source_fid.
  PUT UNFORMATTED "=== " TODAY " @" STRING(TIME,"HH:MM")
    " Compiling "  + source_fid + " ===" SKIP.
  STATUS DEFAULT "Compiling " + source_fid.
  COMPILE VALUE(source_fid) SAVE xref rpro.xls append.
  PAUSE 0.
END.
STATUS DEFAULT.
OUTPUT CLOSE.
INPUT STREAM s-in CLOSE.
INPUT FROM TERMINAL.
/*
status default "Compiles are done; cross-referencing ...".
run pm/xref.p (input "rpro.xls").
status default.
*/
