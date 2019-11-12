/* menutrig.i */

&IF "{&ITEM{1}}" NE "" &THEN
{methods/menus/acclrtrs.i "{&ITEM{1}}"}
ON CHOOSE OF MENU-ITEM m_{&ITEM{1}} OR
   CHOOSE OF MENU-ITEM p_{&ITEM{1}}
DO:
    RUN Select_{&ITEM{1}}.
END.

PROCEDURE Select_{&ITEM{1}}:
    &IF '{&ITEM{1}}' EQ 'List' AND DEFINED(setUserPrint) NE 0 &THEN
    RUN setUserPrint.
    &ENDIF
    
    &IF '{&ITEM{1}}' EQ 'Exit' AND DEFINED(setUserExit) NE 0 &THEN
    RUN setUserExit.
    &ENDIF
    
    &IF "{&ITEM{1}}" EQ "List" &THEN
    DEFINE VARIABLE adm-current-page AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBufferValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hBuffer          AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable           AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery           AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iSubjectID       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rRowID           AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bDynPrgrmsPage FOR dynPrgrmsPage.
    
    RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
    ASSIGN
        iDynSubjectPage = INTEGER(RETURN-VALUE)
        /* get current page subject id */
        iSubjectID      = b-prgrms.pageSubjectID[iDynSubjectPage + 1]
        /* if current page has subject id use it, otherwise use page 0 */
        iSubjectID      = IF iSubjectID NE 0 THEN iSubjectID
                          ELSE b-prgrms.pageSubjectID[1]
        cParamList      = ""
        cParamValue     = ""
        .    
    /* run dynamic subject if subject id ne 0 */
    IF iSubjectID NE 0 AND
       CAN-FIND(FIRST dynSubject WHERE dynSubject.subjectID EQ iSubjectID) THEN DO:
        /* check override parameters exists */
        &IF "{&FIRST-EXTERNAL-TABLE}" NE "" &THEN
        rRowID = ROWID({&FIRST-EXTERNAL-TABLE}).
        FOR EACH bDynPrgrmsPage
            WHERE bDynPrgrmsPage.prgmName  EQ b-prgrms.prgmName
              AND bDynPrgrmsPage.pageTab   EQ iDynSubjectPage
              AND bDynPrgrmsPage.subjectID EQ iSubjectID
            BREAK BY bDynPrgrmsPage.tableName
            :
            IF FIRST-OF(bDynPrgrmsPage.tableName) THEN DO:
                CREATE QUERY hQuery.
                CREATE BUFFER hBuffer FOR TABLE bDynPrgrmsPage.tableName.
                hQuery:ADD-BUFFER(hBuffer).
                hQuery:QUERY-PREPARE(
                    "FOR EACH " + bDynPrgrmsPage.tableName + " NO-LOCK " +
                    "WHERE ROWID(" + bDynPrgrmsPage.tableName + ") EQ TO-ROWID(~"" +
                    STRING(rRowID) + "~")"
                    ).
                hQuery:QUERY-OPEN().
                hQuery:GET-FIRST().
                hTable = hQuery:GET-BUFFER-HANDLE(bDynPrgrmsPage.tableName).
                FOR EACH dynPrgrmsPage NO-LOCK
                    WHERE dynPrgrmsPage.prgmName  EQ b-prgrms.prgmName
                      AND dynPrgrmsPage.pageTab   EQ iDynSubjectPage
                      AND dynPrgrmsPage.subjectID EQ iSubjectID
                      AND dynPrgrmsPage.tableName EQ bDynPrgrmsPage.tableName
                    :
                    IF dynPrgrmsPage.paramInitValue NE "" THEN
                    cBufferValue = dynPrgrmsPage.paramInitValue.
                    ELSE
                    cBufferValue = hTable:BUFFER-FIELD(dynPrgrmsPage.fieldName):BUFFER-VALUE().
                    ASSIGN
                        cParamList  = cParamList  + dynPrgrmsPage.paramName + "|"
                        cParamValue = cParamValue + cBufferValue + "|"
                        .
                END. /* each dynprgrmspage */
            END. /* if first-of */
            ASSIGN
                cParamList  = TRIM(cParamList,"|")
                cParamValue = TRIM(cParamValue,"|")
                .
            IF cParamList NE "" THEN
            RUN pInitDynParamValue (
                iSubjectID,
                USERID("ASI"),
                b-prgrms.prgmName,
                0,
                cParamList,
                cParamValue
                ).
        END. /* can-find first dynpargrmspage */
        &ENDIF
        run-proc = "AOA/Jasper.r".
        IF SEARCH(run-proc) EQ ? THEN
        run-proc = "AOA/Jasper.p".
        RUN VALUE(run-proc) (iSubjectID, USERID("ASI"), b-prgrms.prgmName, 0, YES).
    END. /* if subject id ne 0 */
    ELSE /* else run original framework report listing */
    &ENDIF
    DO:
        {&PROC{1}}
    END. /* else */

    &IF DEFINED(menutrig2) NE 0 &THEN
    IF VALID-HANDLE(h_b-jobapp) THEN DO:
        /*refresh browser after printing in JU2*/
        IF '{&ITEM{1}}' EQ 'List' AND
            INDEX(PROGRAM-NAME(1),"select_list jcinq/w-jobapp.") GT 0 AND
            AVAILABLE job-hdr THEN
        RUN reopen-query IN h_b-jobapp (INPUT ROWID(job-hdr)).
    END.
    &ENDIF

END PROCEDURE.
&ENDIF
