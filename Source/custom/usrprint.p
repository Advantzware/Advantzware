/* usrprint.p - yoosun */

DEFINE INPUT PARAMETER ipcProgramID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iphFrame     AS HANDLE   NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}

&SCOPED-DEFINE where-phrase ~
WHERE user-print.company    EQ cocode ~
  AND user-print.program-id EQ ipcProgramID ~
  AND user-print.batch      EQ ""

DEFINE TEMP-TABLE tt-user-print LIKE user-print.
DEFINE TEMP-TABLE tt-date 
    FIELD tt-name  AS CHARACTER
    FIELD tt-value AS CHARACTER
    .

RUN userPrint.

PROCEDURE userPrint:
    DEFINE VARIABLE hField       AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hGroup       AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER NO-UNDO.
    DEFINE VARIABLE lUpdateBatch AS LOGICAL NO-UNDO.

    DEFINE BUFFER user-print FOR user-print.
    DEFINE BUFFER bUserPrint FOR user-print.
    
    ASSIGN
        cocode = g_company
        locode = g_loc
        .
    EMPTY TEMP-TABLE tt-date.
    IF g_batch THEN
        FIND FIRST user-print NO-LOCK
             WHERE ROWID(user-print) EQ g_batch-rowid
             NO-ERROR.
    ELSE 
        FIND FIRST user-print NO-LOCK
             {&where-phrase}
             AND user-print.user-id EQ USERID("ASI")
             NO-ERROR.
    IF NOT AVAILABLE user-print THEN
        FIND FIRST user-print NO-LOCK
             {&where-phrase}
             AND user-print.user-id EQ ""
             NO-ERROR.
    CREATE tt-user-print.
    IF AVAILABLE user-print THEN
    BUFFER-COPY user-print TO tt-user-print.
    ASSIGN
        tt-user-print.company     = cocode
        tt-user-print.program-id  = ipcProgramID
        tt-user-print.user-id     = USERID("ASI")
        tt-user-print.batch       = ""
        tt-user-print.field-label = ""
        tt-user-print.field-name  = ""
        tt-user-print.field-value = ""
        hGroup                    = iphFrame:FIRST-CHILD
        hField                    = hGroup:FIRST-CHILD
        idx                       = 0
        .
    DO WHILE TRUE:
        idx = idx + 1.
        IF idx GT EXTENT(tt-user-print.field-name) OR
           NOT VALID-HANDLE(hField)         THEN LEAVE.
    
        ASSIGN
            tt-user-print.field-label[idx] = hField:LABEL
            tt-user-print.field-name[idx]  = hField:NAME
            tt-user-print.field-value[idx] = IF hField:NAME BEGINS "sl" THEN hField:LIST-ITEMS
                                            ELSE hField:SCREEN-VALUE
            NO-ERROR.
    
        IF g_batch THEN 
        DO:
            IF hField:TYPE NE "BROWSE" AND
                hField:TYPE NE "BUTTON" AND
                hField:DATA-TYPE EQ "Date" THEN 
            DO:
                CREATE tt-date.
                ASSIGN 
                    tt-date.tt-name  = hField:NAME
                    tt-date.tt-value = hField:SCREEN-VALUE.
                IF NOT lUpdateBatch AND AVAILABLE user-print THEN
                    lUpdateBatch = tt-date.tt-value NE user-print.field-value[idx].
            END.
        END.
        hField = hField:NEXT-SIBLING.
    END.
    
    IF g_batch THEN 
    DO:
        FIND CURRENT user-print EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE user-print THEN 
        DO:
            user-print.batch = "Batch".
            IF lUpdateBatch THEN 
                MESSAGE "Transfer New Date To All Batches With Same Program Name?" 
                    VIEW-AS ALERT-BOX
                    QUESTION BUTTON YES-NO
                    UPDATE ll-ans AS LOGICAL.
            IF ll-ans THEN 
            DO:
                FOR EACH bUserPrint
                    WHERE bUserPrint.company  EQ cocode         
                    AND bUserPrint.program-id EQ user-print.program-id 
                    AND bUserPrint.prgmName   EQ ipcProgramID
                    AND bUserPrint.batch      NE ""
                    AND ROWID(bUserPrint)     NE ROWID(user-print)
                    :
                    FOR EACH tt-date:
                        DO idx = 1 TO EXTENT(bUserPrint.field-name):
                            IF TRIM(bUserPrint.field-name[idx]) NE ""  AND 
                               bUserPrint.field-name[idx] EQ tt-date.tt-name THEN
                            bUserPrint.field-value[idx] = tt-date.tt-value.
                        END.
                    END. /* each tt-date*/
                END.  /* for each bf-user */
            END. /* ll-ans*/
        END.
    END.
    ELSE 
    DO:
        FIND CURRENT user-print EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE user-print THEN
        CREATE user-print.
        BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print.
        IF user-print.batch EQ "Batch" THEN
            RUN userBatch.
        RELEASE user-print.
        FOR EACH user-print NO-LOCK
            {&where-phrase}
            AND user-print.user-id EQ ""
            :
            FIND FIRST bUserPrint EXCLUSIVE-LOCK
                 WHERE ROWID(bUserPrint) EQ ROWID(user-print)
                 NO-ERROR NO-WAIT.
            IF AVAILABLE bUserPrint THEN
            DELETE bUserPrint.
        END.
        CREATE user-print.
        BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print
            ASSIGN user-print.user-id = "".
        IF user-print.batch EQ "Batch" THEN
            RUN userBatch.
    END.
    RELEASE user-print.
    RELEASE bUserPrint.
END PROCEDURE.

PROCEDURE userBatch:
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatchSeq AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiProgSeq  AS INTEGER   NO-UNDO.

    DEFINE BUFFER user-batch FOR user-batch.

    IF NOT CAN-FIND(FIRST user-batch
                    WHERE user-batch.company EQ ipcCompany
                    AND user-batch.batch-seq EQ ipiBatchSeq
                    AND user-batch.prog-seq  EQ ipiProgSeq) THEN 
    DO:
        CREATE user-batch.
        ASSIGN
            user-batch.company   = ipcCompany
            user-batch.batch-seq = ipiBatchSeq
            user-batch.prog-seq  = ipiProgSeq
            .
    END. /* not can-find user-batch */
    RELEASE user-batch.
END PROCEDURE.
