DEFINE VARIABLE rcode AS CHARACTER NO-UNDO.
DEFINE VARIABLE elist AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

elist = '_sav,_bak,copy of,.orig.,_test,.ron.,_org,\tmp\,yoosun,searches,_new'.
OUTPUT TO rcode.lst.
INPUT FROM rcode.txt NO-ECHO.
repeatBlock:
REPEAT :
  IMPORT UNFORMATTED rcode.
  DO i = 1 TO NUM-ENTRIES(elist):
    IF INDEX(rcode,ENTRY(i,elist)) NE 0 THEN NEXT repeatBlock.
  END.
  rcode = REPLACE(rcode,'z:\asi9ship\rco300\','').
  EXPORT rcode.
END.
INPUT CLOSE.
OUTPUT CLOSE.
