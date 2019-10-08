/* -------------------------------------------------- fg/fg-mkbin.p 11/97 JLF */
/* finished goods bin rebuild program                                         */
/* -------------------------------------------------------------------------- */

    DEF INPUT PARAM rec-id AS RECID.

    {sys/inc/var.i NEW SHARED}

    DEF VAR li AS INT NO-UNDO.
    DEF VAR lv AS CHAR NO-UNDO.

    DISABLE TRIGGERS FOR LOAD OF fg-bin.

    FIND itemfg NO-LOCK WHERE 
        RECID(itemfg) EQ rec-id 
        NO-ERROR.

    IF NOT AVAIL itemfg THEN RETURN.

    ASSIGN
        cocode = itemfg.company.

    RUN fg/fgmkbin2.p (rec-id).

    IF TRIM(itemfg.i-no) NE "" THEN FOR EACH fg-bin WHERE 
        fg-bin.company EQ itemfg.company AND 
        fg-bin.i-no    EQ itemfg.i-no AND 
        fg-bin.cust-no GT "" AND 
        fg-bin.qty LE 0
        USE-INDEX co-ino:
        DELETE fg-bin.
    END.

