
FORM HEADER 
    SKIP(1)
    "*** Inventory File Field Definition ***           As of " TODAY
    "Page: " + STRING(PAGE-NUM,">>9") FORM "x(10)" TO 79 SKIP
    FILL("=",80) FORM "x(80)" 
    WITH FRAME pg-top PAGE-TOP NO-BOX NO-LABEL STREAM-IO.

OUTPUT TO c:\tmp\inv.txt PAGE-SIZE 60.

VIEW FRAME pg-top.
PUT "FG Item table " SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "itemfg",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.
PUT skip(1)
    "Inventory table " SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "fg-bin",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT skip(1)
    "FG Receipts History header table " SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "fg-rcpth",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

PUT skip(1)
    "FG Receipts History detail table " SKIP.
FOR EACH _file NO-LOCK WHERE _file-name = "fg-rdtlh",
    EACH _field OF _file NO-LOCK.
    DISP _file-name LABEL "Table" FORM "x(10)"
         _field-name LABEL "Field" FORM "x(15)"
        _label LABEL "Label" FORM "x(25)"
        _format LABEL "Format" FORM "x(25)"
        WITH STREAM-IO DOWN NO-BOX .
END.

