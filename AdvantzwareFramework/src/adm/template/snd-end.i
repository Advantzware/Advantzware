/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/* snd-end.i */
        OTHERWISE
        DO:
            RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                INPUT "RECORD-SOURCE":U, OUTPUT link-handle) NO-ERROR.
            IF link-handle NE "":U THEN
            DO:
                IF NUM-ENTRIES(link-handle) > 1 THEN
                    MESSAGE "send-records in ":U THIS-PROCEDURE:FILE-NAME
                            "encountered more than one RECORD-SOURCE.":U SKIP
                            "The first will be used.":U
                            VIEW-AS ALERT-BOX ERROR.
                RUN send-records IN WIDGET-HANDLE(ENTRY(1,link-handle))
                    (INPUT ENTRY(i, p-tbl-list), OUTPUT rowid-string).
                p-rowid-list = p-rowid-list + rowid-string.
            END.
            ELSE
            DO:
/*                MESSAGE "Requested table":U ENTRY(i, p-tbl-list)            */
/*                        "does not match tables in send-records":U           */
/*                        "in procedure":U THIS-PROCEDURE:FILE-NAME ".":U SKIP*/
/*                        "Check that objects are linked properly and that":U */
/*                        "database qualification is consistent.":U           */
/*                    VIEW-AS ALERT-BOX ERROR.                                */
                RETURN ERROR.
            END.
        END.
        END CASE.
    END.                 /* i = 1 to num-entries */


    /* Mike Fechner, Consultingwerk Ltd. 11.03.2017
       https://github.com/advantzwareWinKit/Advantzware/issues/38
       When the above did not return and rowid's query the own
       record-source instead */
    DEFINE VARIABLE hDataSource AS HANDLE NO-UNDO .

    IF p-rowid-list = "?" OR p-rowid-list = ? AND
       p-tbl-list = "{&FIRST-EXTERNAL-TABLE}" THEN DO:
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT hDataSource) .

        RUN send-records IN hDataSource
            (INPUT p-tbl-list, OUTPUT p-rowid-list) NO-ERROR.
    END.
