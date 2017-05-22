DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cFileName AS CHARACTER   NO-UNDO.
cFileName = "protools\abhack\sampleCatDbs\abhack.df".
/* cFileName = "C:\wrk\101b\lar.df". */

OS-COPY VALUE(SEARCH(cFileName)) VALUE(SEARCH(cFileName) + ".orig").
INPUT FROM VALUE(SEARCH(cFileName) + ".orig").
OUTPUT TO VALUE(SEARCH(cFileName)).

REPEAT:
    IMPORT UNFORMATTED cLine.
    
    IF cLine = "" THEN DO:
        PUT SKIP(1).
        NEXT.
    END.
    
    IF cLine BEGINS "  AREA" THEN NEXT. /* 18-DEC-2007 sla:  */
    
    
    IF  LEFT-TRIM(cLine) BEGINS "MAX-WIDTH"
     OR LEFT-TRIM(cLine) BEGINS "sql-width"
      THEN NEXT.
    
    IF TRIM(cLine) = "PSC" THEN NEXT.
    IF TRIM(cLine) BEGINS "cpstream=" THEN NEXT.
    
    PUT UNFORMATTED cLine SKIP.
END.

INPUT CLOSE.
OUTPUT CLOSE.

OS-DELETE  VALUE(SEARCH("protools\abhack\sampleCatDbs\abhack.df") + ".orig").

