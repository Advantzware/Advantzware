
FORM HEADER 
    SKIP(1)
    "*** Order Invoice File Field Definition ***           As of " TODAY
    "Page: " + STRING(PAGE-NUM,">>9") FORM "x(10)" TO 79 SKIP
    FILL("=",80) FORM "x(80)" 
    WITH FRAME pg-top PAGE-TOP NO-BOX NO-LABEL STREAM-IO.

OUTPUT TO c:\tmp\invoice.txt PAGE-SIZE 60.

VIEW FRAME pg-top.
PUT "Header table " SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "inv-head",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.
PUT skip(1)
    "Line item table " SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "inv-line",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT skip(1)
    "Misc. item table " SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "inv-misc",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

