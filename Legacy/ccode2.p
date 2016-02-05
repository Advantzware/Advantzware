&SCOPED-DEFINE asi_gui9 z:\asi_gui9\pco400\
&SCOPED-DEFINE asi9test z:\asi9test\rco400\
&SCOPED-DEFINE asi9test1 z:\asi9test\pco400\
&SCOPED-DEFINE asi9ship z:\asi9ship\rco400\

DEFINE VARIABLE rcode AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcode AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcode AS CHARACTER NO-UNDO.
DEFINE VARIABLE ccode AS CHARACTER NO-UNDO.

DEFINE STREAM s1.
DEFINE STREAM s2.

OUTPUT STREAM s1 TO ccode.p.
OUTPUT STREAM s2 TO ccode.bat.
PUT STREAM s1 UNFORMATTED 'OUTPUT TO ccode.log.' SKIP.
INPUT FROM rcode.lst NO-ECHO.
REPEAT:
  IMPORT rcode.
  ASSIGN
    pcode = '{&asi_gui9}' + REPLACE(rcode,'.r','.p')
    wcode = '{&asi_gui9}' + REPLACE(rcode,'.r','.w').
  IF SEARCH('{&asi9ship}' + rcode) NE ? THEN NEXT.
  IF SEARCH(pcode) EQ ? AND SEARCH(wcode) EQ ? THEN NEXT.
  ASSIGN
    ccode = IF SEARCH(pcode) NE ? THEN pcode ELSE wcode
    ccode = REPLACE(ccode,'{&asi_gui9}','').
  PUT STREAM s1 UNFORMATTED
    'PUT UNFORMATTED ~'Compiling ' ccode ' ...~' SKIP.' SKIP
    'COMPILE ' ccode ' SAVE.' SKIP.
  PUT STREAM s2 UNFORMATTED 'xcopy {&asi_gui9}' rcode ' {&asi9test}' rcode SKIP
    'xcopy {&asi_gui9}' rcode ' {&asi9ship}' rcode SKIP
    'xcopy {&asi_gui9}' ccode ' {&asi9test1}' ccode SKIP.
END. /* repeat */
INPUT CLOSE.
PUT STREAM s1 UNFORMATTED 'OUTPUT CLOSE.' SKIP
  'OS-COMMAND NO-CONSOLE notepad ccode.log.' SKIP.
OUTPUT STREAM s1 CLOSE.
OUTPUT STREAM s2 CLOSE.
