 {custom/xprint.i}
 DEF VAR li-file-name AS cha NO-UNDO.

 DEF VAR m AS MEMPTR NO-UNDO.
 DEF VAR a AS cha NO-UNDO.
 DEF VAR i AS INT NO-UNDO.


 SET-SIZE(m) = 256.
 RUN printerdialog (m).

 a = GET-STRING(m,1).
 SET-SIZE(m) = 0.

 li-file-name = "r:\tmp\prtquo10.txt".

 MESSAGE SESSION:PRINTER-PORT skip
     SESSION:PRINTER-NAME VIEW-AS ALERT-BOX.


 DO i = 1 TO int(replace(ENTRY(7,a),"COPIES=","")) :
     OS-COPY value(li-file-name) value(session:printer-port) 
                 /*VALUE(ENTRY(1,a)) */.
 END.
  

