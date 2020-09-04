/* --------------------------------------------------- oe/autorel.p 11/96 JLF */
/* order entry - AUTO RELEASE ALL ORDER LINES                                 */
/* -------------------------------------------------------------------------- */
DEFINE NEW SHARED VARIABLE relh-recid AS RECID NO-UNDO.

{sys/inc/var.i shared}

DEFINE SHARED BUFFER xoe-ord  FOR oe-ord.
DEFINE SHARED BUFFER xoe-ordl FOR oe-ordl.

DEFINE SHARED VARIABLE v-auto            AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE fil_id            AS RECID     NO-UNDO.

DEFINE VARIABLE lAutoDef           AS LOG       NO-UNDO.
DEFINE VARIABLE cRelStat           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFirst             AS LOG       NO-UNDO.
DEFINE VARIABLE lMergeWithExisting AS LOG       NO-UNDO.
DEFINE VARIABLE iNumItemsReleased  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPromptMethod      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAbort             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iNumOrderLines     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNum-w-rel         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRelLogData        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParmList          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnValues      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMergeMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPromptEachItem    AS LOG       NO-UNDO.
DEFINE VARIABLE lMergeNew          AS LOG       NO-UNDO.
DEFINE VARIABLE cReturnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lActiveScope       AS LOGICAL   NO-UNDO.  
DEFINE VARIABLE lValidLocation     AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE hdOutboundProcs    AS HANDLE    NO-UNDO.    
DEFINE STREAM sRelErrorLog.

DEFINE BUFFER bf-oe-rel FOR oe-rel.

DEFINE TEMP-TABLE w-rel NO-UNDO 
    FIELD w-rowid AS ROWID.

PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEFINE INPUT PARAMETER mailTo AS CHARACTER.
    DEFINE INPUT PARAMETER mailsubject AS CHARACTER.
    DEFINE INPUT PARAMETER mailText AS CHARACTER.
    DEFINE INPUT PARAMETER mailFiles AS CHARACTER.
    DEFINE INPUT PARAMETER mailDialog AS LONG.
    DEFINE OUTPUT PARAMETER iReturnCode AS LONG.
END.

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

{oe/chkordl.i NEW}
{oe/relemail.i NEW}

DO TRANSACTION:
    {sys/inc/addrelse.i}
END.
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.

FIND xoe-ordl WHERE RECID(xoe-ordl) EQ fil_id NO-LOCK NO-ERROR.
IF NOT AVAILABLE xoe-ordl THEN 
DO: 
    RETURN. 
END.

FIND xoe-ord WHERE xoe-ord.company = cocode AND xoe-ord.ord-no = xoe-ordl.ord-no
    NO-LOCK NO-ERROR.
FIND FIRST eb  NO-LOCK 
    WHERE eb.company = xoe-ordl.company
      AND eb.est-no EQ xoe-ordl.est-no 
    NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "AUTOREL", "L" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cReturnChar, OUTPUT lRecFound).
v-auto = LOGICAL(cReturnChar) NO-ERROR.
RUN sys/ref/nk1look.p (INPUT cocode, "AUTOREL", "C" /* Char */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                       OUTPUT cReturnChar, OUTPUT lRecFound).
lAutoDef = INTEGER(cReturnChar) EQ 1 NO-ERROR.                       


IF v-auto THEN 
DO ON ENDKEY UNDO, RETURN:
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company EQ xoe-ord.company
        AND oe-rel.ord-no  EQ xoe-ord.ord-no
        AND oe-rel.link-no EQ 0
        AND oe-rel.tot-qty     GT 0
        ,
        FIRST xoe-ordl NO-LOCK OF oe-rel 
        :
      
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT cRelStat).
       
        IF INDEX("AB",cRelStat) EQ 0 THEN 
        DO:
            lMergeWithExisting = YES.
            LEAVE.
        END.
    
        RELEASE oe-rel.
    END.
END.

IF v-auto AND (xoe-ord.stat EQ "H" OR xoe-ord.priceHold) AND oe-ctrl.p-pick EQ NO THEN 
DO:
    MESSAGE "Can not release items for customers on Credit Hold or Price Hold." VIEW-AS ALERT-BOX
        ERROR.
    v-auto = NO.
END.

IF v-auto THEN 
DO:
    EMPTY TEMP-TABLE w-rel.
    lMergeWithExisting  = YES.
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company EQ xoe-ord.company
          AND oe-rel.ord-no  EQ xoe-ord.ord-no
          AND oe-rel.link-no EQ 0
          AND oe-rel.tot-qty     GT 0
        ,
        FIRST xoe-ordl NO-LOCK OF oe-rel 
        BREAK BY oe-rel.rel-date
        BY oe-rel.ship-id:
    
        IF FIRST-OF(oe-rel.ship-id) THEN lFirst = YES.

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT cRelStat).
        cRelLogData = cRelLogData + string(oe-rel.r-no) + "|" + oe-rel.stat + "|" + cRelStat + "|" + oe-rel.i-no + "," .
  
        IF INDEX("AB",cRelStat) EQ 0 THEN 
        DO:
            IF lFirst THEN 
            DO:
                ASSIGN
                    lFirst = NO
                    lMergeWithExisting  = YES
                    lcMergeMessage = "Create Release for Date-" + TRIM(STRING(oe-rel.rel-date)) + 
                                     " and ShipID-" +  TRIM(oe-rel.ship-id) + " ?" 
                    .
         
                cParmList = 
                    "type=literal,name=fi4,row=3,col=18,enable=false,width=58,scrval=" + lcMergeMessage + ",FORMAT=X(58)"
                    + "|type=toggle,name=tg1,row=4,col=18,enable=true,width=32,label=Prompt to merge each item?"
                    + "|type=toggle,name=tg2,row=5,col=18,enable=true,width=38,label=Merge items with existing release?"                    
                    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
                    + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=11".
    
                RUN custom/d-prompt.w (INPUT "yes-no", cParmList, "", OUTPUT cReturnValues).
    
                DO i = 1 TO NUM-ENTRIES(cReturnValues) BY 2.
                    IF ENTRY(i, cReturnValues) EQ "default" THEN
                        lMergeWithExisting = LOGICAL(ENTRY(i + 1, cReturnValues)) NO-ERROR.
                    IF ENTRY(i, cReturnValues) EQ "tg1" THEN
                        lPromptEachItem = LOGICAL(ENTRY(i + 1, cReturnValues)) NO-ERROR.
                    IF ENTRY(i, cReturnValues) EQ "tg2" THEN
                        lMergeNew = LOGICAL(ENTRY(i + 1, cReturnValues)) NO-ERROR.            
                END.
                cPromptMethod = (IF lPromptEachItem THEN "ALWAYS" ELSE "NEVER").
                /* If they check of merge to new release only, then set cPromptMethod to FIRST and use that to  */
                /* make sure it doesn't merge on the first time through, then change cPromptMethod back to 'ALWAYS' */
                IF NOT lPromptEachItem AND NOT lMergeNew THEN
                    cPromptMethod = "FIRST".                                                                       

            END. /*lFirst*/

            IF lMergeWithExisting THEN 
            DO:
                CREATE w-rel.
                w-rowid = ROWID(oe-rel).
                iNum-w-rel = iNum-w-rel + 1.
            END.
        END. /*cRelStat ne A or B*/

    END. /* Each oe-rel */

    iNumItemsReleased = 0.
    FOR EACH w-rel,
        FIRST oe-rel WHERE ROWID(oe-rel) EQ w-rowid NO-LOCK
        BREAK BY oe-rel.rel-date
        BY oe-rel.ship-id:

        IF AVAILABLE eb AND oe-rel.ship-id EQ "" THEN
            ASSIGN
                oe-rel.ship-id      = eb.ship-id
                oe-rel.ship-addr[1] = eb.ship-addr[1]
                oe-rel.ship-addr[2] = eb.ship-addr[2]
                oe-rel.ship-city    = eb.ship-city
                oe-rel.ship-state   = eb.ship-state
                oe-rel.ship-zip     = eb.ship-zip. 
         
        v-auto = NOT FIRST-OF(oe-rel.ship-id).

        RUN oe/actrel.p (RECID(oe-rel), INPUT-OUTPUT cPromptMethod).
        FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel) NO-LOCK NO-ERROR.
        IF AVAILABLE bf-oe-rel AND bf-oe-rel.rel-no GT 0 THEN
            iNumItemsReleased = iNumItemsReleased + 1.
    END.

    RUN send-email-proc.
END. /* If v-auto */

IF addrelse-cha = "No Tags" THEN 
DO:
    iNumOrderLines = 0.
    RUN CountOrderLines(BUFFER xoe-ord, 
        OUTPUT iNumOrderLines).
    IF iNumOrderLines GT iNumItemsReleased THEN 
    DO:
        RUN LogError.
        lAbort = NO.
        RUN PromptForIncompleteRelease(INPUT iNumOrderlines,
            INPUT iNumItemsReleased, 
            INPUT xoe-ord.ord-no,
            OUTPUT lAbort).
        IF lAbort THEN 
        DO:
            RUN DeleteIncompleteRelease.
        END.
    END.
    ELSE
        MESSAGE "Total # of items released: " iNumItemsReleased
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

PROCEDURE send-email-proc:
    DEFINE VARIABLE cToList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMailSubject AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMailBody    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iReturnCode  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-prgmname   LIKE prgrms.prgmname NO-UNDO.
   
    v-prgmname = "actrel.".

    FOR EACH tt-email,
        FIRST cust WHERE
        cust.company = cocode AND
        cust.cust-no = tt-email.cust-no AND
        cust.active = "E"
        NO-LOCK
        BREAK BY tt-email.ord-no:

        IF FIRST-OF(tt-email.ord-no) THEN
            cMailBody = "Order Number " +  STRING(tt-email.ord-no)
                + " has been released." + CHR(10).

        cMailBody = cMailBody 
            + "Item: " + STRING(tt-email.i-no,"X(15)")
            + " Qty: " + STRING(tt-email.rel-qty,"->>,>>>,>>9")
            + " Date: " + STRING(tt-email.rel-date,"99/99/99")
            + " PO#: "  + STRING(tt-email.po-no,"X(15)") + CHR(10).
       
        IF LAST-OF(tt-email.ord-no) THEN 
        DO:           
            {custom/emailList.i &recKey=cust.rec_key &emailList=cToList}

            IF cToList NE '' THEN 
            DO:
                ASSIGN 
                    cToList      = "To:" + cToList
                    cMailSubject = "Release Generated"
                    .
                RUN mail(cToList,cMailSubject,cMailBody,"",1,OUTPUT iReturnCode).
            END.
        END. /* last-of(tt-email.cust-no) */
    END.
END.

PROCEDURE CountOrderLines:
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER opiCount AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    
    opiCount = 0.
    FOR EACH bf-oe-ordl OF ipbf-oe-ord 
        NO-LOCK:
        FIND FIRST bf-itemfg 
            WHERE bf-itemfg.company EQ bf-oe-ordl.company
            AND bf-itemfg.i-no    EQ bf-oe-ordl.i-no
            NO-LOCK NO-ERROR.
        /* Don't count unassembled set header */
        IF bf-itemfg.alloc = YES THEN
            NEXT.
        opiCount = opiCount + 1.
    END.   
END PROCEDURE.

PROCEDURE DeleteIncompleteRelease:

    DEFINE BUFFER bf-oe-rel  FOR oe-rel.
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    FOR EACH w-rel,
        FIRST bf-oe-rel WHERE ROWID(bf-oe-rel) EQ w-rowid EXCLUSIVE-LOCK:

        /* oe-rell has link to oe-rel, so use that to get oe-relh */
        FIND FIRST bf-oe-rell NO-LOCK
            WHERE bf-oe-rell.company EQ bf-oe-rel.company
              AND bf-oe-rell.ord-no  EQ bf-oe-rel.ord-no      
              AND bf-oe-rell.i-no    EQ bf-oe-rel.i-no
              AND bf-oe-rell.line    EQ bf-oe-rel.line
              AND bf-oe-rell.link-no EQ bf-oe-rel.r-no
            NO-ERROR.
        IF AVAILABLE bf-oe-rell THEN
            FIND FIRST bf-oe-relh EXCLUSIVE-LOCK 
                WHERE bf-oe-relh.r-no EQ bf-oe-rell.r-no 
                NO-ERROR.
        IF AVAILABLE bf-oe-relh THEN 
        DO:
            FOR EACH bf-oe-rell EXCLUSIVE-LOCK
                WHERE bf-oe-rell.company EQ bf-oe-rel.company
                  AND bf-oe-rell.ord-no  EQ bf-oe-rel.ord-no            
                  AND bf-oe-rell.i-no    EQ bf-oe-rel.i-no
                  AND bf-oe-rell.line    EQ bf-oe-rel.line
                  AND bf-oe-rell.link-no EQ bf-oe-rel.r-no
                :
                DELETE bf-oe-rell.
            END. /* Each oe-rell */

            FIND FIRST bf-oe-rell NO-LOCK
                WHERE bf-oe-rell.company EQ bf-oe-relh.company
                  AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no
                USE-INDEX r-no NO-ERROR.
            IF NOT AVAILABLE bf-oe-rell THEN 
            DO:
                bf-oe-relh.posted = NO.
                DELETE bf-oe-relh.
            END. /* no more oe-rell's for this oe-relh */
            
            RELEASE bf-oe-relh.
        END. /* If avail oe-relh */

        bf-oe-rel.link-no = 0.
       
        /*update release qty on orderline*/
        FIND FIRST bf-oe-ordl EXCLUSIVE-LOCK
            WHERE bf-oe-ordl.company EQ bf-oe-rel.company
              AND bf-oe-ordl.ord-no  EQ bf-oe-rel.ord-no      
              AND bf-oe-ordl.i-no    EQ bf-oe-rel.i-no
            NO-ERROR.
        IF AVAILABLE bf-oe-ordl THEN 
        DO:
            bf-oe-ordl.t-rel-qty = bf-oe-ordl.t-rel-qty - bf-oe-rel.qty.
            RELEASE bf-oe-ordl.
        END.
    END. /*each w-rel*/
END PROCEDURE.

PROCEDURE PromptForIncompleteRelease:
    DEFINE INPUT PARAMETER  ipiOrderCount AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleaseCount AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrderNumber AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAbort AS LOGICAL     NO-UNDO.

    MESSAGE "WARNING: Order " ipiOrderNumber " has " ipiOrderCount " items, but only " ipiReleaseCount " were released.  Would you like to abort the release creation and try again?" SKIP(1)
        "Hit YES to delete the incomplete release and try again." SKIP
        "Hit NO to continue with incomplete release." SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE oplAbort.
END PROCEDURE.

PROCEDURE LogError:
    DEFINE VARIABLE iAuditID     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBeforeValue AS CHARACTER NO-UNDO.

    RUN spCreateAuditHdr (
        "LOG",          /* type  */
        "ASI",          /* db    */
        "oe/autorel.p", /* table */
        "",             /* key   */
        OUTPUT iAuditID
        ).
    DO idx = 1 TO 4:
        CASE idx:
            WHEN 1 THEN
                cBeforeValue = cRelLogData.
            WHEN 2 THEN
                cBeforeValue = STRING(iNumOrderLines) + " lines in order".
            WHEN 3 THEN
                cBeforeValue = STRING(iNumItemsReleased) + " lines released".
            WHEN 4 THEN
                cBeforeValue = STRING(iNum-w-rel) + " lines in w-rel".
        END CASE.
        RUN spCreateAuditDtl (
            iAuditID,     /* audit id     */
            "RelError",   /* field        */
            idx,          /* extent       */
            cBeforeValue, /* before value */
            "",           /* after value  */
            NO            /* index field  */
            ).
    END. /* do idx */
END PROCEDURE.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
