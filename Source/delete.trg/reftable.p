&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME reftable

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEFINE VARIABLE cEstNo LIKE est.est-no NO-UNDO.
DEFINE VARIABLE idx      AS INTEGER    NO-UNDO.

IF {&TABLENAME}.reftable EQ "EST-COST" THEN DO:
    cEstNo = "".
    DO idx = 1 TO LENGTH({&TABLENAME}.code):
        IF SUBSTR(reftable.code,1,1) EQ "/" THEN LEAVE.
        cEstNo = cEstNo + SUBSTR({&TABLENAME}.code,1,1).
    END.
    cEstNo = FILL(" ",8 - LENGTH(TRIM(cEstNo))) + TRIM(cEstNo).
    IF cEstNo NE "" THEN DO:
        FIND FIRST est EXCLUSIVE-LOCK
            WHERE est.company EQ {&TABLENAME}.company
              AND est.est-no  EQ cEstNo
            NO-ERROR.
        IF AVAILABLE est THEN
        RUN update-est.
    END.
END.
ELSE DO:
    IF {&TABLENAME}.reftable EQ "PLATE/FOUNTAIN" OR 
       {&TABLENAME}.reftable EQ "cedepth" THEN DO:
        FIND FIRST est EXCLUSIVE-LOCK
            WHERE est.company EQ {&TABLENAME}.company
              AND est.est-no  EQ {&TABLENAME}.loc
            NO-ERROR.
        IF AVAILABLE est THEN
        RUN update-est.
    END.
    ELSE DO:
        IF {&TABLENAME}.reftable EQ "est/getqty.w" THEN DO:
            FIND FIRST est EXCLUSIVE-LOCK
                WHERE est.company EQ {&TABLENAME}.company
                  AND est.est-no  EQ {&TABLENAME}.code
                NO-ERROR.
            IF AVAILABLE est THEN
            RUN update-est.
        END.
    END.
END.
/* Clear out any error-status from find with no-error that is false */
DEFINE VARIABLE ll-error AS LOGICAL NO-UNDO.
ll-error = YES NO-ERROR.

RETURN.

/* Procedures */
PROCEDURE update-est.
    ASSIGN
        est.updated-date = TODAY
        est.updated-id   = USERID("ASI")
        .
END PROCEDURE.
