/* custom/ld-arinv.p   Import A/R data for SouthPak 
11/12/02  YSK */

/* need to create ar-inv,ar-invl,ar-cash and ar-cashl */
       
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR ls-input AS cha FORM "x(3000)" NO-UNDO.

INPUT FROM c:\tmp\oearch.ASC NO-ECHO.
OUTPUT TO c:\tmp\oearch.dat.

DO WHILE li-cnt < 10 :
       IMPORT UNFORMATTED ls-input.
       PUT ls-input SKIP.
       
       li-cnt = li-cnt + 1.
END.
INPUT CLOSE.
OUTPUT CLOSE.


INPUT FROM c:\tmp\oearcd.ASC NO-ECHO.
OUTPUT TO c:\tmp\oearcd.dat.
li-cnt = 0.

DO WHILE li-cnt < 10 :
       IMPORT UNFORMATTED ls-input.
       PUT ls-input SKIP.
       
       li-cnt = li-cnt + 1.
END.

