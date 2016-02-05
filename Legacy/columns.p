/* columns.p */

DEFINE VARIABLE fld AS CHARACTER NO-UNDO.
DEFINE VARIABLE lbl AS CHARACTER NO-UNDO.
DEFINE VARIABLE fmt AS CHARACTER NO-UNDO.

OUTPUT TO 'columns.txt'.
PUT UNFORMATTED
  '~&ELSEIF ~'~{~&yellowColumnsName}~' EQ ~'~' &THEN' SKIP
  '~&SCOPED-DEFINE SORTBY-PHRASE BY ~~' SKIP.
INPUT FROM 'columns.dat' NO-ECHO.
REPEAT:
  IMPORT fld ^ lbl ^ fmt.
  IF fmt BEGINS 'x' THEN fmt = ''.
  IF fmt NE '' THEN
  ASSIGN
    fmt = REPLACE(fmt,'>','9')
    fmt = REPLACE(fmt,'<','9')
    fmt = REPLACE(fmt,',','').
  PUT UNFORMATTED '  IF sortColumn EQ ~'' lbl '~' THEN '.
  IF fmt NE '' THEN PUT UNFORMATTED 'STRING('.
  PUT UNFORMATTED fld.
  IF fmt NE '' THEN PUT UNFORMATTED ',~'' fmt '~')'.
  PUT UNFORMATTED ' ELSE ~~' SKIP.
END.
INPUT CLOSE.
PUT UNFORMATTED '/*lastField*/ ~~~{~&SORTED}' SKIP.
OUTPUT CLOSE.
OS-COMMAND NO-WAIT notepad columns.txt.
