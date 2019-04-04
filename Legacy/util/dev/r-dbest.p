
FORM HEADER 
    SKIP(1)
    "*** Estimate File Field Definition ***           As of " TODAY
    "Page: " + STRING(PAGE-NUM,">>9") FORM "x(10)" TO 79 SKIP
    FILL("=",80) FORM "x(80)" 
    WITH FRAME pg-top PAGE-TOP NO-BOX NO-LABEL STREAM-IO.

OUTPUT TO c:\tmp\estimate.txt PAGE-SIZE 60.

VIEW FRAME pg-top.

PUT SKIP
    "Header file" SKIP.

FOR EACH _file NO-LOCK WHERE _file-name = "est",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT SKIP(1)
    "Quantity file" SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "est-qty",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT SKIP(1)
    "Form file" SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "ef",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT SKIP(1)
    "Blank file" SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "eb",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT SKIP(1)
    "Prep file" SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "est-prep",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT SKIP(1)
    "Routing file" SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "est-op",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT SKIP(1)
    "Film file" SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "est-file",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.
