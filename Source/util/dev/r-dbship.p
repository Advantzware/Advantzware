
FORM HEADER 
    SKIP(1)
    "*** Shipto File Field Definition ***           As of " TODAY
    "Page: " + STRING(PAGE-NUM,">>9") FORM "x(10)" TO 79 SKIP
    FILL("=",80) FORM "x(80)" 
    WITH FRAME pg-top PAGE-TOP NO-BOX NO-LABEL STREAM-IO.

OUTPUT TO c:\tmp\shipto.txt PAGE-SIZE 60.

VIEW FRAME pg-top.

FOR EACH _file NO-LOCK WHERE _file-name = "shipto",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.
