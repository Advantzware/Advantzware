DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE txt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prgmname AS CHARACTER NO-UNDO.
DEFINE VARIABLE prgmname2 AS CHARACTER NO-UNDO.

DEFINE STREAM s1.
DEFINE STREAM s2.

INPUT STREAM s1 FROM 'prgmname.txt' NO-ECHO.
REPEAT:
  IMPORT STREAM s1 prgmname.
  prgmname2 = REPLACE(prgmname,'lookups','lookups.fix').
  OUTPUT TO VALUE(prgmname2).
  INPUT STREAM s2 FROM VALUE(prgmname).
  REPEAT:
    txt = ''.
    IMPORT STREAM s2 UNFORMATTED txt.
    i = i + 1.
    IF i EQ 15 AND INDEX(txt,'varasgn.i') EQ 0 THEN
    txt = REPLACE(txt,'" ~~','~~~{sys/inc/varasgn.i} " ~~').
    ELSE
    IF i EQ 16 AND INDEX(txt,'var.i') EQ 0 THEN
    txt = REPLACE(txt,'" ~~','~~~{sys/inc/var.i new shared} " ~~').
    ELSE
    IF INDEX(txt,'top-include') NE 0 AND INDEX(txt,'varasgn.i') EQ 0 THEN
    txt = txt + ' ~~~{sys/inc/varasgn.i}'.
    ELSE
    IF INDEX(txt,'def-include') NE 0 AND INDEX(txt,'var.i') EQ 0 THEN
    txt = txt + ' ~~~{sys/inc/var.i new shared}'.
    ASSIGN
      txt = REPLACE(txt,'" ~~~{','"~{')
      txt = REPLACE(txt,'include  ~~~{','include ~~~{').
    IF txt EQ '' THEN
    PUT UNFORMATTED SKIP(1).
    ELSE
    PUT UNFORMATTED txt SKIP.
  END.
  OUTPUT CLOSE.
  i = 0.
END.
