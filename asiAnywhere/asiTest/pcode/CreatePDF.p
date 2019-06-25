DEFINE VARIABLE arglist     AS CHARACTER.
DEFINE VARIABLE numarg      AS INTEGER.

DEFINE VARIABLE cTmpFile    AS CHARACTER.
DEF STREAM O.

{d:\webapps\asinet\pcode\custom\xprint.i}
{d:\webapps\asinet\pcode\custom\vxprint.i}

arglist = SESSION:PARAMETER.
numarg = NUM-ENTRIES(arglist).

IF numarg = 1  THEN DO:
    cTmpFile  = ENTRY(1,arglist).
END.                            

OUTPUT STREAM O TO VALUE("c:\temp\vpxPrint.log") APPEND.
PUT STREAM O UNFORMATTED
    "-----------------------------------------------" SKIP
    "Log started     " TODAY " " STRING(TIME, "hh:mm:ss") SKIP 
    cTmpFile arglist numarg SKIP.

IF SEARCH(cTmpFile) = ? THEN RETURN.

RUN vprintFile(cTmpFile).

PUT STREAM O UNFORMATTED
    "After vprintFile " TODAY " " STRING(TIME, "hh:mm:ss") SKIP.

OUTPUT STREAM O CLOSE.
