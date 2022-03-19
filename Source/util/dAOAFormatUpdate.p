// dAOAFormatUpdate.p - rstark - 2.3.2022

DEFINE VARIABLE cFormat LIKE dynSubjectColumn.fieldformat NO-UNDO.

FOR EACH dynSubjectColumn NO-LOCK:
    RUN pSetFormat (dynSubjectColumn.fieldName, dynSubjectColumn.fieldFormat, OUTPUT cFormat).
    IF cFormat NE "" AND cFormat NE dynSubjectColumn.fieldFormat THEN DO:
        FIND CURRENT dynSubjectColumn EXCLUSIVE-LOCK.
        dynSubjectColumn.fieldFormat = cFormat.
        FIND CURRENT dynSubjectColumn NO-LOCK.
    END.
END.

FOR EACH dynValueColumn NO-LOCK:
    RUN pSetFormat (dynValueColumn.colName, dynValueColumn.colFormat, OUTPUT cFormat).
    IF cFormat NE "" AND cFormat NE dynValueColumn.colFormat THEN DO:
        FIND CURRENT dynValueColumn EXCLUSIVE-LOCK.
        dynValueColumn.colFormat = cFormat.
        FIND CURRENT dynValueColumn NO-LOCK.
    END.
END.

PROCEDURE pSetFormat:
    DEFINE INPUT  PARAMETER ipcField  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFormat AS CHARACTER NO-UNDO.

    opcFormat = "".
    IF INDEX(ipcField,"job-no2") NE 0 OR
       INDEX(ipcField,"jobNo2")  NE 0 OR
       INDEX(ipcField,"jobSub")  NE 0 OR
       INDEX(ipcField,"job_Sub") NE 0 THEN
        opcFormat = ">>9".
    ELSE
    IF INDEX(ipcField,"job-no")    NE 0 OR
       INDEX(ipcField,"jobNumber") NE 0 OR
       INDEX(ipcField,"jobSub")    NE 0 THEN
        opcFormat = "x(9)".
    ELSE
    IF INDEX(ipcField,"ord-no")       NE 0 OR
       INDEX(ipcField,"ordNo")        NE 0 OR
       INDEX(ipcField,"orderNo")      NE 0 OR
       INDEX(ipcField,"ordOrdererNo") NE 0 OR
       INDEX(ipcField,"orderNumber")  NE 0 THEN
        opcFormat = ">>>>>>>9".
    ELSE
    IF INDEX(ipcField,"est-no") NE 0 OR
       INDEX(ipcField,"estNo")   NE 0 THEN
        opcFormat = "x(8)".

END PROCEDURE.
