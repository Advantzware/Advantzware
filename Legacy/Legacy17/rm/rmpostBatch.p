/*------------------------------------------------------------------------

  File: r-rmpostNoUI.w

  Description: RM Edit List & Posting

  Author: JLF

  Created: 05/23/2002 
  
  Modified By : 

------------------------------------------------------------------------*/
/* Batch Input Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-post-eom-date AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ip-run-what AS CHARACTER NO-UNDO. /* "SETUP" from initial setup (addon/sshoot/sssetups.w), else "" */

/* Selections to Input */
DEFINE            VARIABLE lPostSelected       AS LOG       NO-UNDO.
DEFINE            VARIABLE cBeginUserid        AS CHARACTER FORMAT "X(8)":U NO-UNDO.
DEFINE            VARIABLE cEndUserid          AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE            VARIABLE fiAutoIssue         AS LOGICAL   FORMAT "yes/no":U INITIAL NO NO-UNDO.
DEFINE            VARIABLE dtRctDateFrom       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/1980 NO-UNDO.
DEFINE            VARIABLE dtRctDateTo         AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
DEFINE            VARIABLE v-post-date         AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO. /* used in include */
DEFINE            VARIABLE cFromJobID          AS CHARACTER FORMAT "X(256)":U NO-UNDO.
DEFINE            VARIABLE cToJobID            AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzz" NO-UNDO.
DEFINE            VARIABLE lIncludeAdjustments AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE            VARIABLE lIncludeIssues      AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE            VARIABLE lIncludeReceipts    AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE            VARIABLE lIncludeTransfers   AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE            VARIABLE cTypes              AS CHARACTER FORMAT "x(10)" NO-UNDO.

/* Assign parameters */
ASSIGN
    lPostSelected = YES
    v-post-date   = TODAY
    cBeginUserId  = ipcUserID
    cEndUserID    = ipcUserID
    cTypes        = ip-run-what.

/* Local Variable Definitions ---                                       */
DEFINE NEW SHARED VARIABLE pox                 LIKE po-ordl.po-no.


DEFINE            VARIABLE lAutoIssue          AS LOG.
DEFINE            VARIABLE lDunne              AS LOG       INIT NO.
DEFINE            VARIABLE v-rmtags-log        AS LOG       NO-UNDO.
DEFINE            VARIABLE iPo                 AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lFromSS             AS LOG       NO-UNDO.
DEFINE            VARIABLE iFirstJob2          AS INTEGER   NO-UNDO.
DEFINE            VARIABLE iLastJob2           AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lBadIssuesPrompted  AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-uid-sec           AS LOG       NO-UNDO.
DEFINE            VARIABLE v-access-close      AS LOG       NO-UNDO.
DEFINE            VARIABLE v-access-list       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-source-handle     AS HANDLE    NO-UNDO.
DEFINE            VARIABLE lValidPeriod        AS LOG       NO-UNDO.
DEFINE            VARIABLE lPrTotals           AS LOG       FORMAT "Y/N" NO-UNDO.
DEFINE            VARIABLE tran-period         AS INTEGER   FORMAT "99":U INITIAL 0 NO-UNDO.
DEFINE            VARIABLE lShowTotal          AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE TEMP-TABLE tt-bad-issues
    FIELD issue-rowid AS ROWID.
    
DEFINE TEMP-TABLE tt-rctd NO-UNDO LIKE rm-rctd 
    FIELD tt-row-id AS ROWID
    FIELD rm-row-id AS ROWID
    FIELD has-rec   AS LOG     INIT NO
    FIELD seq-no    AS INTEGER
    FIELD db-seq-no AS INTEGER 
    INDEX seq-no seq-no i-no.

DEFINE TEMP-TABLE tt-mat NO-UNDO 
    FIELD frm LIKE job-mat.frm
    FIELD qty LIKE job-mat.qty
    INDEX frm frm.

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD vend-no         AS CHARACTER 
    FIELD po-no           AS INTEGER
    FIELD item-no         AS CHARACTER
    FIELD item-name       AS CHARACTER
    FIELD po-qty          AS DECIMAL
    FIELD recvd-qty       AS DECIMAL
    FIELD total-recvd-qty AS DECIMAL
    FIELD cons-uom        AS CHARACTER
    FIELD overrun-pct     AS DECIMAL
    FIELD underrun-pct    AS DECIMAL
    FIELD allow-qty       AS DECIMAL  
    FIELD under-qty       AS DECIMAL
    FIELD undovr          AS CHARACTER
    INDEX po-no po-no ASC item-no ASC.

DEFINE BUFFER b-job-hdr FOR job-hdr.
DEFINE BUFFER b-rh      FOR rm-rcpth.
DEFINE BUFFER b-rd      FOR rm-rdtlh.

{methods/defines/hndldefs.i NEW}
/*{methods/prgsecur.i} */
{methods/defines/globdefs.i &NEW=NEW GLOBAL}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}
def  stream sDebug.  
output stream sDebug to c:\temp\debugrm.txt.  
OUTPUT TO c:\temp\rm-errs.txt.  

if g_loc = "" then   
g_loc = "main".
g_sysdate   = TODAY.
RUN pDefaultAsiValues.
ASSIGN
    g_company = ipcCompany    
    gcompany  = ipcCompany
    gLoc      = g_loc
    cocode    = gcompany
    locode    = gloc
    lPostSelected = YES
    .
    
{jc/jcgl-sh.i NEW}


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "AUTOISSU"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "AUTOISSU"
        sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs?".

END.
lAutoIssue = sys-ctrl.log-fld.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "RMTAGS"
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "RMTAGS"
        sys-ctrl.descrip  = "Number of RM Loadtags to Print & Create Wip Tags"
        sys-ctrl.char-fld = ""
        sys-ctrl.int-fld  = 1
        sys-ctrl.log-fld  = FALSE. /* true create wip/false do not */
END.

ASSIGN 
    v-rmtags-log = sys-ctrl.log-fld.

DO TRANSACTION:
    {sys/inc/rmpostgl.i}
END.

/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "RMPostUID",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT v-uid-sec, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Function Prototypes ********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEFINE INPUT PARAMETER mailTo AS CHARACTER.
    DEFINE INPUT PARAMETER mailsubject AS CHARACTER.
    DEFINE INPUT PARAMETER mailText AS CHARACTER.
    DEFINE INPUT PARAMETER mailFiles AS CHARACTER.
    DEFINE INPUT PARAMETER mailDialog AS LONG.
    DEFINE OUTPUT PARAMETER retCode AS LONG.
END.
      
/* ***************************  Main Block  *************************** */    
DEFINE VARIABLE choice  AS LOG NO-UNDO.
DEFINE VARIABLE ll-auto AS LOG NO-UNDO.


MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* security check need {methods/prgsecur.i} in definition section */

    DO TRANSACTION:  
        {sys/inc/rmemails.i}
        {sys/inc/postdate.i}
    END.
    
    ASSIGN 
        lFromSS    = TRUE. /* Always from from SS for this procedure */
        
/*        lIncludeReceipts    = INDEX(cTypes,"R") GT 0*/
/*        lIncludeIssues      = INDEX(cTypes,"I") GT 0*/
/*        lIncludeTransfers   = INDEX(cTypes,"T") GT 0*/
/*        lIncludeAdjustments = INDEX(cTypes,"A") GT 0*/

    ASSIGN 
       v-post-date = TODAY.
    
    RUN pCheckDate.
    IF NOT lValidPeriod THEN 
      RETURN. 
      put stream sdebug "Main" cocode cbeginuserid cenduserid  ip-run-what skip.
    RUN pMainProcess.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignPrepInfo C-Win 
PROCEDURE pAssignPrepInfo :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-tt-rctd FOR tt-rctd.
    FOR EACH bf-tt-rctd   
        WHERE bf-tt-rctd.seq-no EQ tt-rctd.seq-no 
        AND bf-tt-rctd.i-no   EQ tt-rctd.i-no:
        /*         BREAK BY bf-tt-rctd.seq-no   */
        /*               BY bf-tt-rctd.i-no     */
        /*               BY bf-tt-rctd.r-no     */
        /*               BY bf-tt-rctd.job-no   */
        /*               BY bf-tt-rctd.job-no2: */
        /*   IF FIRST-OF(bf-tt-rctd.job-no) THEN DO:                                      */
        /*       FOR EACH job-hdr NO-LOCK                                                 */
        /*       WHERE job-hdr.company EQ bf-tt-rctd.company                              */
        /*         AND job-hdr.job-no  EQ bf-tt-rctd.job-no                               */
        /*         AND job-hdr.job-no2 EQ bf-tt-rctd.job-no2:                             */
        /*       FIND FIRST itemfg                                                        */
        /*         WHERE itemfg.company EQ job-hdr.company                                */
        /*           AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.                 */
        /*       IF AVAIL itemfg THEN DO:                                                 */
        /*                                                                                */
        /*         IF itemfg.plate-no NE "" THEN DO:                                      */
        /*           FIND FIRST prep                                                      */
        /*              WHERE prep.company EQ cocode                                      */
        /*                AND prep.code    EQ itemfg.plate-no NO-ERROR.                   */
        /*            IF AVAIL prep THEN ASSIGN prep.received-date = bf-tt-rctd.rct-date. */
        /*         END.                                                                   */
        /*                                                                                */
        /*         IF itemfg.die-no NE "" THEN DO:                                        */
        /*           FIND FIRST prep                                                      */
        /*             WHERE prep.company EQ cocode                                       */
        /*               AND prep.code    EQ itemfg.die-no NO-ERROR.                      */
        /*           IF AVAIL prep THEN ASSIGN prep.received-date = bf-tt-rctd.rct-date.  */
        /*                                                                                */
        /*         END.                                                                   */
        /*         RELEASE prep.                                                          */
        /*                                                                                */
        /*       END. /* avail itemfg */                                                  */
        /*     END. /* EACH job-hdr */                                                    */
        /*     END. /*bf-tt-rctd.job ne ""*/                                              */
        /*   END. /* FIRST-OF */                                                          */
        /*Note (BPV) - code above was commented after discussing the simplicity of matching 
        the RM item number between receipt record and prep record via the i-no
        NOTE: this is not an indexed search but records in prep should not be a large # 
        wfk - 9/6 - Changing to join on prep.code so that customers who use the same rm
        in multiple prep codes are not affected. Also, should be indexed, so faster. */

        FOR EACH prep WHERE prep.company EQ cocode                          
            AND prep.CODE = bf-tt-rctd.i-no:
            ASSIGN
                prep.loc           = bf-tt-rctd.loc
                prep.loc-bin       = bf-tt-rctd.loc-bin
                prep.received-date = bf-tt-rctd.rct-date.
            IF bf-tt-rctd.job-no NE "" THEN
                ASSIGN
                    prep.last-job-no  = bf-tt-rctd.job-no
                    prep.last-job-no2 = bf-tt-rctd.job-no2.
        END. /* each prep */
    END. /* each tt-rctd   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckDate C-Win 
PROCEDURE pCheckDate :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    lValidPeriod = YES.

    FIND FIRST period                   
        WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
        NO-LOCK NO-ERROR.
    IF AVAILABLE period THEN tran-period = period.pnum.
      
    ELSE
        IF lPostSelected THEN 
        DO:
            lValidPeriod = NO.
        END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckIssuedQty C-Win 
PROCEDURE pCheckIssuedQty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iTotalI AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotBin AS INTEGER NO-UNDO.
    ASSIGN
        iTotalI  = 0
        iTotBin  = 0
        oplValid = YES.
    FOR EACH tt-rctd,
        FIRST rm-rctd NO-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id
        AND rm-rctd.rita-code = "I"
        BREAK BY rm-rctd.i-no
        BY rm-rctd.loc
        BY rm-rctd.loc-bin
        BY rm-rctd.tag:

        iTotalI = iTotalI + rm-rctd.qty.
        IF LAST-OF(rm-rctd.tag) THEN 
        DO:
            FIND ITEM NO-LOCK
                WHERE ITEM.company EQ rm-rctd.company
                AND ITEM.i-no EQ rm-rctd.i-no
                NO-ERROR.

            /* Only validate for stocked items */
            IF NOT ITEM.stocked THEN
                NEXT.

            FOR EACH rm-bin NO-LOCK WHERE rm-bin.company EQ rm-rctd.company
                AND rm-bin.i-no EQ rm-rctd.i-no
                AND rm-bin.tag  EQ rm-rctd.tag
                AND rm-bin.loc  EQ rm-rctd.loc
                AND rm-bin.loc-bin EQ rm-rctd.loc-bin
                .
                iTotBin = iTotBin + rm-bin.qty.
            END.
          
            IF iTotalI GT iTotBin THEN 
            DO:
                IF NOT lBadIssuesPrompted THEN 
                DO:
        
/*                    MESSAGE "Quantity Issued is greater than Bins On Hand + Unposted Issues for item " + rm-rctd.i-no*/
/*                        + " and Tag " + rm-rctd.tag + "." SKIP                                                       */
/*                        "Please update Issues to post."                                                              */
/*                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                                                    */
                    lBadIssuesPrompted = TRUE.
                END.
                oplValid = NO.
                CREATE tt-bad-issues.
                tt-bad-issues.issue-rowid = ROWID(rm-rctd).
            END.
            ASSIGN 
                iTotalI = 0
                iTotBin = 0
                .
        END.
    END.

    FOR EACH tt-bad-issues:
        FIND FIRST tt-rctd WHERE tt-rctd.rm-row-id = tt-bad-issues.issue-rowid
            NO-ERROR.
        IF AVAILABLE tt-rctd THEN
            DELETE tt-rctd.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE pDefaultAsiValues:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FIND FIRST usr WHERE usr.uid EQ ipcUserID NO-LOCK NO-ERROR.
    IF AVAILABLE usr THEN
    DO:
        FIND FIRST company WHERE company.company EQ ipcCompany NO-LOCK NO-ERROR.
        IF NOT AVAILABLE company THEN
            FIND FIRST company NO-LOCK NO-ERROR.

        IF AVAILABLE usr AND AVAILABLE company THEN
        DO:
            FIND FIRST loc WHERE loc.company EQ company.company 
                AND loc.loc EQ usr.loc NO-LOCK NO-ERROR.
            IF NOT AVAILABLE loc THEN
                FIND FIRST loc WHERE loc.company EQ company.company NO-LOCK NO-ERROR.
            IF AVAILABLE loc THEN
            DO:
                ASSIGN
                    g_company = company.company
                    g_loc     = loc.loc
                    g_sysdate = TODAY.

                FIND FIRST period WHERE period.company EQ g_company 
                    AND period.pstat EQ TRUE   
                    AND period.pst LE g_sysdate   
                    AND period.pend GE g_sysdate NO-LOCK NO-ERROR.
                IF NOT AVAILABLE period THEN
                    FIND LAST period WHERE period.company EQ g_company AND
                        period.pstat EQ TRUE NO-LOCK NO-ERROR.
                IF AVAILABLE period THEN
                    g_period = period.pnum.
            END.
        END. /* avail user and company */
    END. /* avail usr */



END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFinalSteps C-Win 
PROCEDURE pFinalSteps :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-tt-rctd    FOR tt-rctd.
    DEFINE BUFFER rec-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER rec-rm-rcpth FOR rm-rcpth.

    DEFINE VARIABLE iNextRNo       AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-qty-received AS DECIMAL NO-UNDO.
  
    IF rm-rctd.rita-code EQ "I" AND TRIM(rm-rctd.tag) NE "" THEN
        FOR EACH rec-rm-rdtlh NO-LOCK
            WHERE rec-rm-rdtlh.company   EQ rm-rctd.company
            AND rec-rm-rdtlh.tag       EQ rm-rctd.tag
            AND rec-rm-rdtlh.rita-code EQ "R"
            USE-INDEX tag,
            FIRST rec-rm-rcpth
            WHERE rec-rm-rcpth.r-no      EQ rec-rm-rdtlh.r-no
            AND rec-rm-rdtlh.rita-code EQ rec-rm-rdtlh.rita-code
            NO-LOCK:
           
            IF rm-rctd.po-no EQ "" THEN rm-rctd.po-no = rec-rm-rcpth.po-no.
    
            IF rm-rctd.job-no EQ "" THEN
                ASSIGN
                    rm-rctd.job-no  = rec-rm-rcpth.job-no
                    rm-rctd.job-no2 = rec-rm-rcpth.job-no2.
    
            LEAVE.
        END.

    IF v-rmtags-log AND TRIM(rm-rctd.tag) NE "" THEN 
    DO:
        FOR EACH wiptag WHERE wiptag.company = rm-rctd.company 
            AND wiptag.rm-tag-no = rm-rctd.tag EXCLUSIVE-LOCK:
            ASSIGN
                wiptag.sts = "On Hand" .
        END.
    END.
  
    {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */

    IF rm-rctd.rita-code EQ "R" THEN
    DO:
    {rm/rmemails.i}      
    END.

    DELETE rm-rctd.

    FOR EACH b-tt-rctd WHERE b-tt-rctd.tt-row-id EQ ROWID(tt-rctd):
        iNextRNo = 0.
        RUN sys/ref/asiseq.p (INPUT g_company, INPUT "rm_rcpt_seq", OUTPUT iNextRNo) NO-ERROR.
 
        CREATE rm-rctd.
        BUFFER-COPY b-tt-rctd TO rm-rctd
            ASSIGN
            rm-rctd.r-no        = iNextRNo
            b-tt-rctd.r-no      = rm-rctd.r-no
            b-tt-rctd.has-rec   = YES
            b-tt-rctd.rm-row-id = ROWID(rm-rctd).    

    END.

    DELETE tt-rctd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRecordLock C-Win
PROCEDURE pGetRecordLock:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iprRec AS RECID NO-UNDO.
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE wrecid  AS INTEGER NO-UNDO.
    DEFINE VARIABLE wtable  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTryNum AS INTEGER NO-UNDO.

    ASSIGN 
        wrecid = iprRec.
    FIND asi._file WHERE asi._file._file-name = ipcTable NO-LOCK.
    ASSIGN 
        wtable = asi._file._file-num.
        

    REPEAT:

        iTryNum = iTryNum + 1.

        &SCOP Filename cust
        &SCOP   lock        When "~{&FileName}" then do :           ~
                Find ~{&Filename} where RECID(~{&Filename}) = iprRec ~
                              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.  ~
              IF avail(~{&Filename}) THEN RETURN. end.
                
        CASE ipcTable :
        &SCOP Filename item
            {&lock}
        &SCOP Filename rm-rctd
            {&lock}             
            OTHERWISE 
            RETURN ERROR.
        END CASE.
    


        IF iTryNum EQ 1 THEN 
        DO:
            PAUSE 1.
            NEXT.
        END.

        IF iTryNum EQ 2 THEN 
        DO:
            PAUSE 3.
            NEXT.
        END.


        /* iTryNum is 3, let the user know about it */

        /* Use repeat loop - More efficient than FIND ... WHERE
           due to lack of suitable index on _lock table */
        REPEAT:
            FIND NEXT asi._lock NO-LOCK NO-ERROR.
            IF NOT AVAILABLE asi._lock THEN 
                FIND FIRST asi._lock NO-LOCK NO-ERROR.
            IF _lock-recid = wrecid AND
                _lock-table = wtable AND
                (_lock-flag MATCHES "*X*" OR _lock-flag MATCHES "*S*")
                /* typically we're interested in any form of exclusive lock
                    so we test for X lock flag */
                THEN LEAVE.

            IF _lock-recid = ? THEN 
            DO:
                RELEASE asi._lock.
                LEAVE.
            END.
        END.

        IF AVAILABLE(asi._lock) THEN 
        DO:
        
       
            IF AVAILABLE asi._lock THEN
                FIND FIRST asi._connect WHERE _connect-usr = asi._lock._lock-usr NO-LOCK NO-ERROR.
        
            IF AVAILABLE asi._connect AND AVAILABLE asi._lock THEN
                MESSAGE "Waiting for " ipcTable " record to be unlocked." SKIP
                    "Currently locked by: " asi._lock._lock-name " user# " asi._lock._lock-usr SKIP
                    "ON " _connect-device SKIP
                    "Click OK to try again."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            ELSE
                MESSAGE "Waiting for " ipcTable " record to be unlocked." SKIP
                    "Click OK to try again."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      
        END.
        iTryNum = 0.
        FIND FIRST asi._lock WHERE FALSE NO-LOCK NO-ERROR.
    END.



END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGLFromWork C-Win 
PROCEDURE pGLFromWork :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-run AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-trnum AS INTEGER NO-UNDO.
  
    DEFINE VARIABLE credits AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE debits  AS DECIMAL INIT 0 NO-UNDO. 

  
    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
        NO-LOCK.

    FOR EACH work-gl 
        WHERE (ip-run EQ 1 AND work-gl.job-no NE "")
        OR (ip-run EQ 2 AND work-gl.job-no EQ "")
        BREAK BY work-gl.actnum:
      
        ASSIGN
            debits  = debits  + work-gl.debits
            credits = credits + work-gl.credits.

        IF LAST-OF(work-gl.actnum) THEN 
        DO:
            CREATE gltrans.
            ASSIGN
                gltrans.company = cocode
                gltrans.actnum  = work-gl.actnum
                gltrans.jrnl    = "RMPOST"
                gltrans.period  = period.pnum
                gltrans.tr-amt  = debits - credits
                gltrans.tr-date = v-post-date
                gltrans.tr-dscr = IF work-gl.job-no NE "" THEN "RM Issue to Job"
                                                 ELSE "RM Receipt"
                gltrans.trnum   = ip-trnum
                debits          = 0
                credits         = 0.

            RELEASE gltrans.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMainProcess C-Win
PROCEDURE pMainProcess:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE lv-post   AS LOG NO-UNDO.
    DEFINE VARIABLE lv-r-no   LIKE rm-rctd.r-no NO-UNDO.
    DEFINE VARIABLE lValidQty AS LOG NO-UNDO.

    IF lPostSelected THEN 
    DO:
        FIND FIRST period
            WHERE period.company EQ cocode
            AND period.pst     LE v-post-date
            AND period.pend    GE v-post-date
            NO-LOCK NO-ERROR.
    /* TBD */
    /*     IF NOT AVAIL period THEN DO:                 */
    /*       MESSAGE "No period exists for this date..."*/
    /*               VIEW-AS ALERT-BOX ERROR.           */
    /*       APPLY "entry" TO v-post-date.              */
    /*       RETURN NO-APPLY.                           */
    /*     END.                                         */
    END.



    ASSIGN
        cFromJobID = FILL(" ",6 - length(TRIM(cFromJobID))) +
                trim(cFromJobID)
        cToJobID   = FILL(" ",6 - length(TRIM(cToJobID))) +
                trim(cToJobID)
        lPrTotals  = lShowTotal.

    FOR EACH work-gl:
        DELETE work-gl.
    END.
  
    /* pRunReport creates tt-rctd record baesd on selection criteria */  
    PUT STREAM sDebug unformatted "running run report" SKIP.
    RUN pRunReport (OUTPUT lValidQty). 
    PUT STREAM sDebug unformatted "after run report - valid?" lValidQty " post selected? " lPostSelected SKIP.
    IF NOT lValidQty THEN
        RETURN NO-APPLY.

    IF lPostSelected THEN 
    DO: 
        lv-post = CAN-FIND(FIRST tt-rctd WHERE tt-rctd.has-rec).
        PUT STREAM sDebug unformatted "after run report - lv-post?" lv-post SKIP.
        IF lv-post THEN
            FOR EACH tt-rctd
                WHERE tt-rctd.has-rec
                AND NOT CAN-FIND(FIRST rm-rctd WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id):
                lv-post = NO.
                LEAVE.
            END.
            
        /* TBD if lv-post i false then nothing to post */
        lv-post = TRUE.

        IF lv-post THEN 
        DO:
            /* r-no is made 0 and then restored, I assume to fire the DB trigger */
            FOR EACH tt-rctd
                WHERE tt-rctd.has-rec
                AND CAN-FIND(FIRST rm-rcpth WHERE rm-rcpth.r-no EQ tt-rctd.r-no),
                FIRST rm-rctd WHERE rm-rctd.r-no EQ tt-rctd.r-no:
                lv-r-no = rm-rctd.r-no.
                DO TRANSACTION:
                    rm-rctd.r-no = 0.
                END.
                DO TRANSACTION:
                    rm-rctd.r-no = lv-r-no.
                END.
                tt-rctd.r-no = rm-rctd.r-no.
            END.
        /* TBD */
        /*      FOR EACH tt-rctd                                                            */
        /*          WHERE tt-rctd.has-rec                                                   */
        /*            AND NOT CAN-FIND(FIRST rm-rctd WHERE rm-rctd.r-no EQ tt-rctd.r-no),   */
        /*          FIRST rm-rcpth NO-LOCK WHERE rm-rcpth.r-no EQ tt-rctd.r-no:             */
        /*        MESSAGE "Sorry, these RM Transactions cannot be processed because 1 or " +*/
        /*                "more have already been posted by UserID: " +                     */
        /*                TRIM(rm-rcpth.user-id) + "..."                                    */
        /*            VIEW-AS ALERT-BOX ERROR.                                              */
        /*                                                                                  */
        /*        lv-post = NO.                                                             */
        /*        LEAVE.                                                                    */
        /*      END.                                                                        */
        END.
        PUT STREAM sDebug unformatted "before pPostrm - valid?" lv-post SKIP.
        IF lv-post THEN 
        DO:       
            PUT STREAM sDebug UNFORMATTED "running pPostRM " lv-post SKIP.
            RUN pPostRM.

            lv-post = lDunne.
        /* TBD */
        /*      IF lv-post THEN MESSAGE "Posting Complete" VIEW-AS ALERT-BOX. */

        END.
    END.

    RUN util/fxissues.p.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPostRM C-Win 
PROCEDURE pPostRM :
    /* --------------------------------------------------- rm/rm-post.p 10/94 rd  */
    /* raw materials inventory control receipt maintenance                        */
    /* -------------------------------------------------------------------------- */

    DEFINE BUFFER xrm-rctd  FOR rm-rctd.
    DEFINE BUFFER xrm-bin   FOR rm-bin.
    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
    DEFINE BUFFER b-item    FOR item.
    DEFINE BUFFER b-po-ordl FOR po-ordl.
    DEFINE BUFFER b-job-mat FOR job-mat.

    DEFINE VARIABLE lAvgcost       AS LOG.
    DEFINE VARIABLE iNextRNo       LIKE rm-rctd.r-no.
    DEFINE VARIABLE dConvertedQty  AS DECIMAL.
    DEFINE VARIABLE dReduceQty     LIKE po-ordl.ord-qty.
    DEFINE VARIABLE iNumberItems   AS INTEGER NO-UNDO.
    DEFINE VARIABLE ld-cvt-qty     AS DECIMAL NO-UNDO. /* used in include */
    DEFINE VARIABLE iTrNum         LIKE gl-ctrl.trnum NO-UNDO.

    DEFINE VARIABLE v-r-qty        AS DECIMAL NO-UNDO. /* used in include */
    DEFINE VARIABLE v-i-qty        AS DECIMAL NO-UNDO. /* used in include */
    DEFINE VARIABLE v-t-qty        AS DECIMAL NO-UNDO. /* Used in include */
    DEFINE VARIABLE dcost          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOutQty        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBasisWeight   LIKE item.basis-w NO-UNDO.
    DEFINE VARIABLE dLength        LIKE item.s-len NO-UNDO.
    DEFINE VARIABLE dWidth         LIKE item.s-wid NO-UNDO.
    DEFINE VARIABLE dDepth         LIKE item.s-dep NO-UNDO.
    DEFINE VARIABLE v-recid        AS RECID   NO-UNDO.
    DEFINE VARIABLE li             AS INTEGER NO-UNDO.

    DEFINE VARIABLE v-rmemail-file AS cha     NO-UNDO.

    FIND FIRST rm-ctrl WHERE rm-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    lAvgcost = rm-ctrl.avg-lst-cst.
    FIND FIRST tt-rctd NO-ERROR.
     PUT STREAM sDebug unformatted "postrm avail tt-rctd?" avail(tt-rctd)  SKIP.
     IF AVAILABLE tt-rctd THEN 
     PUT STREAM sDebug unformatted "postrm avail tt-rctd.i-no " tt-rctd.i-no " cocode " cocode " ctypes " ctypes  SKIP.
    SESSION:SET-WAIT-STATE ("general").
    transblok:
    FOR EACH tt-rctd
        WHERE CAN-FIND(FIRST item WHERE item.company EQ cocode
        AND item.i-no    EQ tt-rctd.i-no)
        AND INDEX(cTypes,tt-rctd.rita-code) GT 0 
        BREAK BY tt-rctd.seq-no
        BY tt-rctd.i-no
        BY tt-rctd.r-no
        BY RECID(tt-rctd)
        TRANSACTION:
        PUT STREAM sDebug unformatted "postrm in tt-rctd loop"  SKIP.
        RELEASE rm-rctd.
        RELEASE item.

        FIND rm-rctd NO-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id.
        RUN pGetRecordLock (RECID(rm-rctd), "rm-rctd").
        FIND rm-rctd EXCLUSIVE-LOCK WHERE ROWID(rm-rctd) EQ tt-rctd.rm-row-id.
      
        FIND FIRST item NO-LOCK
            WHERE item.company EQ rm-rctd.company
            AND item.i-no    EQ rm-rctd.i-no
            USE-INDEX i-no NO-WAIT NO-ERROR.
        
        RUN pGetRecordLock (RECID(item), "item").
        FIND FIRST item EXCLUSIVE-LOCK
            WHERE item.company EQ rm-rctd.company
            AND item.i-no    EQ rm-rctd.i-no
            USE-INDEX i-no .        
          
        IF rm-rctd.rita-code EQ "I" AND INT(rm-rctd.po-no) NE 0 THEN
            FOR EACH xrm-rctd
                WHERE xrm-rctd.company   EQ cocode
                AND xrm-rctd.i-no      EQ rm-rctd.i-no
                AND xrm-rctd.rita-code EQ "R"
                AND xrm-rctd.po-no     EQ rm-rctd.po-no
                AND xrm-rctd.r-no      LT rm-rctd.r-no
                NO-LOCK:
            
                UNDO transblok, NEXT transblok.
            END.

        FIND FIRST job
            WHERE job.company EQ rm-rctd.company
            AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) + TRIM(rm-rctd.job-no)
            AND job.job-no2 EQ rm-rctd.job-no2
            NO-ERROR.

        /** Find Bin & if not avail then create it **/
        FIND FIRST rm-bin
            WHERE rm-bin.company EQ rm-rctd.company
            AND rm-bin.loc     EQ rm-rctd.loc
            AND rm-bin.i-no    EQ rm-rctd.i-no
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin
            AND rm-bin.tag     EQ rm-rctd.tag
            NO-ERROR.
        IF NOT AVAILABLE rm-bin THEN 
        DO:
            CREATE rm-bin.
            ASSIGN
                rm-bin.company = rm-rctd.company
                rm-bin.loc     = rm-rctd.loc
                rm-bin.loc-bin = rm-rctd.loc-bin
                rm-bin.tag     = rm-rctd.tag
                rm-bin.i-no    = rm-rctd.i-no
                .
        END. /* not avail rm-bin */

        ld-cvt-qty = rm-rctd.qty.
 
        iPo = INTEGER(rm-rctd.po-no) NO-ERROR.

        IF NOT ERROR-STATUS:ERROR AND NOT rm-rctd.rita-code EQ "T" AND NOT (rm-rctd.rita-code EQ "A" AND rm-rctd.qty LT 0) THEN
            rm-bin.po-no      = iPo.
        IF rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
            RUN sys/ref/convquom.p (rm-rctd.pur-uom, item.cons-uom,
                item.basis-w,
                (IF item.r-wid EQ 0 THEN item.s-len ELSE 12), 
                (IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid),
                item.s-dep,
                ld-cvt-qty, OUTPUT ld-cvt-qty).
        
        IF rm-rctd.rita-code EQ "R" THEN 
        DO:        /** RECEIPTS **/
            {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

            ASSIGN
                rm-bin.qty     = rm-bin.qty + ld-cvt-qty
                item.last-cost = rm-rctd.cost
                item.q-onh     = item.q-onh + ld-cvt-qty.

            {rm/rm-poupd.i 2}
          
            item.q-avail = item.q-onh + item.q-ono - item.q-comm.
        END. /* R */

        ELSE
            IF rm-rctd.rita-code EQ "I" THEN 
            DO:  /** ISSUES **/

                IF rm-rctd.tag NE "" THEN
                    FOR EACH b-rd FIELDS(r-no rita-code tag2) WHERE
                        b-rd.company   EQ cocode AND
                        b-rd.tag       EQ rm-rctd.tag AND
                        b-rd.loc       EQ rm-rctd.loc AND
                        b-rd.loc-bin   EQ rm-rctd.loc-bin AND
                        b-rd.rita-code EQ "R" AND
                        b-rd.tag2      NE ""
                        NO-LOCK
                        USE-INDEX tag,
                        FIRST b-rh WHERE
                        b-rh.r-no EQ b-rd.r-no AND
                        b-rh.rita-code EQ b-rd.rita-code AND
                        b-rh.i-no      EQ rm-rctd.i-no
                        NO-LOCK:
           
                        rm-rctd.tag2 = b-rd.tag2.
                    END.

                IF AVAILABLE job AND job.job-no NE "" THEN 
                DO:
                    RUN rm/mkjobmat.p (RECID(rm-rctd),rm-rctd.company, OUTPUT v-recid).
              
                    FIND job-mat WHERE RECID(job-mat) EQ v-recid NO-ERROR.
              
                    IF NOT AVAILABLE job-mat THEN 
                    DO:
                        /* TBD Error job-mat not found */
                        UNDO transblok, NEXT transblok.
                    END.
           
                    ASSIGN
                        dBasisWeight = job-mat.basis-w
                        dLength      = job-mat.len
                        dWidth       = job-mat.wid
                        dDepth       = item.s-dep
                        .
           
                    IF dLength EQ 0 THEN dLength = item.s-len.
           
                    IF dWidth EQ 0 THEN
                        dWidth = IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid.
           
                    IF dBasisWeight EQ 0 THEN dBasisWeight = item.basis-w.
           
                    IF INDEX("RL",job.stat) NE 0 THEN job.stat = "W".
              
                        {rm/rmmatact.i}            /* Create Actual Material */
              
                    dOutQty = rm-rctd.qty.
                    IF rm-rctd.pur-uom NE job-mat.qty-uom AND rm-rctd.pur-uom NE "" THEN
                        RUN sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.qty-uom,
                            dBasisWeight, dLength, dWidth, dDepth,
                            rm-rctd.qty, OUTPUT dOutQty).
           
                    dcost = rm-rctd.cost.
                    IF rm-rctd.pur-uom NE job-mat.sc-uom AND rm-rctd.pur-uom NE "" THEN
                        RUN sys/ref/convcuom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                            dBasisWeight, dLength, dWidth, dDepth,
                            rm-rctd.cost, OUTPUT dCost).
           
                    ASSIGN
                        mat-act.qty-uom = job-mat.qty-uom
                        mat-act.cost    = dCost
                        mat-act.qty     = mat-act.qty     + dOutQty
                        job-mat.qty-iss = job-mat.qty-iss + dOutQty
                        job-mat.qty-all = job-mat.qty-all - dOutQty
                        item.q-comm     = item.q-comm     - rm-rctd.qty
                        .
              
                    RUN sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                        dBasisWeight, dLength, dWidth, dDepth,
                        rm-rctd.qty, OUTPUT dOutQty).
           
                    mat-act.ext-cost = mat-act.ext-cost + (dcost * dOutQty).
           
                    /* Don't relieve more than were allocated */
                    IF job-mat.qty-all LT 0 THEN 
                    DO:
                        RUN sys/ref/convquom.p(job-mat.qty-uom, rm-rctd.pur-uom,
                            dBasisWeight, dLength, dWidth, dDepth,
                            job-mat.qty-all, OUTPUT dOutQty).
                        ASSIGN
                            job-mat.qty-all = 0
                            item.q-comm     = item.q-comm - dOutQty
                            .
                    END.
           
                    /*job-mat.all-flg = (job-mat.qty-all gt 0).*/
                    IF item.q-comm LT 0 THEN item.q-comm = 0.
           
                    IF item.mat-type EQ "B" THEN RUN rm/rm-addcr.p (ROWID(rm-rctd)).
                END.
           
                FIND FIRST rm-bin
                    WHERE rm-bin.company EQ rm-rctd.company
                    AND rm-bin.loc     EQ rm-rctd.loc
                    AND rm-bin.i-no    EQ rm-rctd.i-no
                    AND rm-bin.loc-bin EQ rm-rctd.loc-bin
                    AND rm-bin.tag     EQ rm-rctd.tag
                    NO-ERROR.
           
                ASSIGN
                    rm-bin.qty     = rm-bin.qty - ld-cvt-qty
                    item.q-onh     = item.q-onh - ld-cvt-qty
                    item.qlast-iss = rm-rctd.qty
                    item.dlast-iss = rm-rctd.rct-date
                    item.q-ytd     = item.q-ytd + rm-rctd.qty
                    item.q-ptd     = item.q-ptd + rm-rctd.qty
                    item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
                    item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
                    item.q-avail   = item.q-onh + item.q-ono - item.q-comm
                    .
            END.  /* I */

            ELSE
                IF rm-rctd.rita-code EQ "A" THEN 
                DO:  /** ADJUSTMENTS **/
                    IF rm-rctd.cost NE 0 THEN 
                    DO:
                       {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}
                    END.

                    ASSIGN
                        rm-bin.qty     = rm-bin.qty + ld-cvt-qty
                        item.last-cost = IF rm-rctd.cost NE 0 THEN rm-rctd.cost
                                               ELSE item.last-cost
                        item.q-onh     = item.q-onh + ld-cvt-qty
                        item.q-avail   = item.q-onh + item.q-ono - item.q-comm
                        .
                END. /* A */

                ELSE
                    IF rm-rctd.rita-code EQ "T" THEN 
                    DO:  /** TRANSFERS **/
                        ASSIGN
                            rm-bin.qty   = rm-bin.qty - rm-rctd.qty
                            rm-rctd.cost = rm-bin.cost.

                        /* This code is to handel the Transfer to quantity to increase the BIN
                           using a buffer record so current rm-bin record is not updated. */

                        FIND FIRST xrm-bin
                            WHERE xrm-bin.company EQ rm-rctd.company
                                AND xrm-bin.loc     EQ rm-rctd.loc2
                                AND xrm-bin.i-no    EQ rm-rctd.i-no
                                AND xrm-bin.loc-bin EQ rm-rctd.loc-bin2
                                AND xrm-bin.tag     EQ rm-rctd.tag2
                                NO-ERROR.
                        IF NOT AVAILABLE xrm-bin THEN 
                        DO:
                            CREATE xrm-bin.
                            ASSIGN
                                xrm-bin.company = rm-rctd.company
                                xrm-bin.loc     = rm-rctd.loc2
                                xrm-bin.loc-bin = rm-rctd.loc-bin2
                                xrm-bin.tag     = rm-rctd.tag2
                                xrm-bin.i-no    = rm-rctd.i-no
                                .
                        END.

                        {rm/rm-post.i "xrm-bin.qty" "xrm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

                        xrm-bin.qty = xrm-bin.qty + rm-rctd.qty.
                    END. /* T */

        /*       /** Delete Bins With Zero Quantities. **/ */
        /*       IF rm-bin.qty EQ 0 THEN DELETE rm-bin.    */

        RELEASE loadtag.
        IF TRIM(rm-rctd.tag) NE "" THEN
            FIND FIRST loadtag EXCLUSIVE-LOCK 
                WHERE loadtag.company     EQ rm-rctd.company
                    AND loadtag.item-type   EQ YES
                    AND loadtag.tag-no      EQ rm-rctd.tag
                    AND loadtag.i-no        EQ rm-rctd.i-no
                    AND loadtag.is-case-tag EQ NO
                    NO-ERROR.

        IF AVAILABLE loadtag THEN 
        DO:
            IF rm-rctd.rita-code EQ "T" THEN 
                ASSIGN
                    loadtag.loc     = rm-rctd.loc2
                    loadtag.loc-bin = rm-rctd.loc-bin2
                    .
            ELSE
                ASSIGN
                    loadtag.loc     = rm-rctd.loc
                    loadtag.loc-bin = rm-rctd.loc-bin
                    .

            li = INDEX("RI",rm-rctd.rita-code).

            IF li EQ 1 AND (NOT AVAILABLE rm-bin OR rm-bin.qty LT 0) THEN li = 3.

            IF li GT 0 THEN loadtag.sts = ENTRY(li,"Received,Issued,Deleted").
        END.

        IF LAST-OF(tt-rctd.i-no) THEN             /* Calculate average cost */
            FOR EACH rm-bin
                WHERE rm-bin.company EQ rm-rctd.company
                    AND rm-bin.i-no    EQ rm-rctd.i-no
                    NO-LOCK USE-INDEX i-no
                    BREAK BY rm-bin.i-no:

                IF FIRST(rm-bin.i-no) THEN
                    ASSIGN
                        v-i-qty = 0
                        dcost   = 0.

                v-r-qty = rm-bin.qty.

                IF v-r-qty LT 0 THEN v-r-qty = v-r-qty * -1.

                ASSIGN
                    v-i-qty = v-i-qty + v-r-qty
                    dcost   = dcost   + (v-r-qty * rm-bin.cost).

                IF dcost EQ ? THEN dcost = 0.

                IF LAST(rm-bin.i-no) AND v-i-qty NE 0 AND dcost NE 0  
                    AND v-i-qty NE ? AND dcost NE ? THEN 
                    item.avg-cost = dcost / v-i-qty.

            END. /* each rm-bin */      
     
        /* gdm - 10280903 - Assign prep code received date */
        RUN pAssignPrepInfo. 
        RUN pFinalSteps.

        FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
        FIND CURRENT item NO-LOCK NO-ERROR.
        FIND CURRENT loadtag NO-LOCK NO-ERROR.
        FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.
        FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
        FIND CURRENT mat-act NO-LOCK NO-ERROR.
        FIND CURRENT job NO-LOCK NO-ERROR.
        FIND CURRENT job-mat NO-LOCK NO-ERROR.
    END. /* for each rm-rctd */

    lDunne = YES.
    FOR EACH rm-rctd
        WHERE rm-rctd.company   EQ cocode
            AND rm-rctd.rita-code EQ "ADDER"
            /*AND rm-rctd.job-no    GE cFromJobID
            AND rm-rctd.job-no    LE cToJobID */
            AND ((cBeginUserid    LE "" AND cEndUserid      GE "") 
            OR (rm-rctd.user-id GE cBeginUserid AND rm-rctd.user-id LE cEndUserid))
            TRANSACTION:
        
        rm-rctd.rita-code = "I".
    END.     

    IF rmpostgl THEN 
    DO TRANSACTION:
        /* gdm - 11050906 */
        REPEAT:

            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN 
            DO:
                ASSIGN 
                    iTrNum        = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = iTrNum
                    .

                FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.

                RUN pGLFromWork (1, iTrNum).
                RUN pGLFromWork (2, iTrNum).
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */
    /* gdm - 11050906 */
    END. /* IF rmpostgl */
/* TBD */
/*    IF CAN-FIND(FIRST tt-email) THEN    */
/*      RUN send-rmemail (v-rmemail-file).*/
   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunReport C-Win 
PROCEDURE pRunReport :

    DEFINE OUTPUT PARAMETER oplValidQty AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cTypePrint     AS ch        FORMAT "X(11)" INIT "".
    DEFINE VARIABLE dExtCost       AS de.
    DEFINE VARIABLE dTotalQty      AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".
    DEFINE VARIABLE dTotalCost     AS DECIMAL   FORMAT "->,>>>>,>>9.99<<".
    DEFINE VARIABLE dGrdQuantity   AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".
    DEFINE VARIABLE dGrdCost       AS DECIMAL   FORMAT "->,>>>>,>>9.99<<".
    DEFINE VARIABLE cPoNumber      LIKE rm-rctd.po-no.
    DEFINE VARIABLE cWhse          LIKE rm-rctd.loc.
    DEFINE VARIABLE v-int          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll-one-item    AS LOG       NO-UNDO.
    DEFINE VARIABLE dTotMaterialQty             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAccountDscr   LIKE account.dscr.
    DEFINE VARIABLE cDisplayActNum LIKE account.actnum.
    DEFINE VARIABLE dDispAmt       AS DECIMAL   FORMAT ">>,>>>,>>9.99cr".

    DEFINE VARIABLE iItemcnt       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iGitMCnt       AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cTotRita       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTotalQtyI     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" NO-UNDO.
    DEFINE VARIABLE dTotalCostI    AS DECIMAL   FORMAT "->,>>>>,>>9.99<<" NO-UNDO.
    DEFINE VARIABLE dGrdQuantityI  AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" NO-UNDO.
    DEFINE VARIABLE dGrdCostI      AS DECIMAL   FORMAT "->,>>>>,>>9.99<<" NO-UNDO.
    DEFINE VARIABLE lCreateIssue   AS LOG       NO-UNDO.
    DEFINE VARIABLE lValidQty      AS LOG       NO-UNDO.

    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
    DEFINE BUFFER b-item    FOR ITEM.
    DEFINE BUFFER b-tt-rctd FOR tt-rctd.
    
    ASSIGN
        fiAutoIssue  = TRUE
        lCreateIssue = fiAutoIssue
        oplValidQty  = TRUE.
        
    FOR EACH tt-rctd:
        DELETE tt-rctd.
    END.
put stream sdebug unformatted "in run report gcom " cocode  " vportlist " cTypes 
  " begin_dt "  dtRctDateFrom " end-dt " dtRctDateTo " endino " cBeginUserid " cEndUserid" cEndUserId
 
skip.  
PUT STREAM sDebug unformatted "run reoprt from job" cFromJobID " to job " cToJobID  SKIP.
    FOR EACH rm-rctd  NO-LOCK
        WHERE  rm-rctd.company   EQ cocode 
           /* AND rm-rctd.job-no    GE cFromJobID
            AND rm-rctd.job-no    LE cToJobID 
            AND rm-rctd.rct-date  GE dtRctDateFrom
            AND rm-rctd.rct-date  LE dtRctDateTo */
             AND INDEX(cTypes,rm-rctd.rita-code) GT 0 
             
            AND rm-rctd.rita-code NE "C"
            AND rm-rctd.user-id GE cBeginUserid
            AND rm-rctd.user-id LE cEndUserID
            /*AND ((cBeginUserid    LE "" AND
            cEndUserid      GE "") OR
            (rm-rctd.user-id GE cBeginUserid AND
            rm-rctd.user-id LE cEndUserid)) */
        :
           
        CREATE tt-rctd.
        BUFFER-COPY rm-rctd TO tt-rctd
            ASSIGN
            tt-rctd.rm-row-id = ROWID(rm-rctd)
            tt-rctd.has-rec   = YES
            tt-rctd.db-seq-no = rm-rctd.r-no 
            tt-rctd.seq-no    = 1
            .
          PUT STREAM sDebug unformatted "run reoprt create tt-rctd " tt-rctd.rita-code SKIP.            
    END.

    /* Validate quantity on tags */
    RUN pCheckIssuedQty (OUTPUT lValidQty).

    IF INDEX(cTypes,"R") GT 0 THEN
        auto-issue:
        FOR EACH tt-rctd 
            WHERE tt-rctd.rita-code EQ "R"
                AND tt-rctd.job-no    NE ""
                NO-LOCK,
                FIRST ITEM NO-LOCK
                    WHERE item.company EQ cocode
                    AND item.i-no    EQ tt-rctd.i-no
            .

            RELEASE po-ordl.

            cPoNumber = TRIM(tt-rctd.po-no).
            IF cPoNumber NE "" THEN 
            DO:
                DO x = 1 TO LENGTH(cPoNumber):
                    IF substr(cPoNumber,x,1) LT "0" OR
                        substr(cPoNumber,x,1) GT "9" THEN NEXT auto-issue.
                END.

                FIND FIRST po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ cocode
                        AND po-ordl.i-no      EQ tt-rctd.i-no
                        AND po-ordl.po-no     EQ int(cPoNumber)
                        AND po-ordl.job-no    EQ tt-rctd.job-no
                        AND po-ordl.job-no2   EQ tt-rctd.job-no2
                        AND po-ordl.item-type EQ YES
                        USE-INDEX item-ordno NO-ERROR.
            END.
            IF item.mat-type NE "I" OR AVAILABLE po-ordl THEN
                IF (item.i-code EQ "E" AND NOT AVAILABLE po-ordl)      
                    OR (item.i-code EQ "R" AND NOT lAutoIssue) 
                    THEN 
                    NEXT auto-issue.
      
            EMPTY TEMP-TABLE tt-mat.
      
            RELEASE job.
            IF tt-rctd.job-no NE "" AND tt-rctd.s-num EQ ? THEN
                FIND FIRST job NO-LOCK
                    WHERE job.company EQ cocode
                        AND job.job-no  EQ tt-rctd.job-no
                        AND job.job-no2 EQ tt-rctd.job-no2
                        NO-ERROR.

            IF AVAILABLE job THEN 
            DO:
                dTotMaterialQty = 0.

                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company EQ job.company
                        AND job-mat.job     EQ job.job
                        AND job-mat.job-no  EQ job.job-no
                        AND job-mat.job-no2 EQ job.job-no2
                        AND job-mat.rm-i-no EQ tt-rctd.i-no                  
                        BY job-mat.frm:
                    CREATE tt-mat.
                    ASSIGN
                        tt-mat.frm = job-mat.frm
                        tt-mat.qty = job-mat.qty
                        dTotMaterialQty         = dTotMaterialQty + job-mat.qty.
                END.

                FOR EACH tt-mat:
                    tt-mat.qty = tt-rctd.qty * (tt-mat.qty / dTotMaterialQty).
                    IF tt-rctd.pur-uom EQ "EA" THEN 
                    DO:
                    {sys/inc/roundup.i tt-mat.qty} 
                    END.
                END.

                dTotMaterialQty = 0.
                FOR EACH tt-mat:
                    dTotMaterialQty = dTotMaterialQty + tt-mat.qty.
                END.

                IF dTotMaterialQty NE tt-rctd.qty THEN
                    FOR EACH tt-mat:
                        tt-mat.qty = tt-mat.qty + (tt-rctd.qty - dTotMaterialQty).
                        LEAVE.
                    END.
            END.

            ELSE 
            DO:
                CREATE tt-mat.
                ASSIGN
                    tt-mat.frm = tt-rctd.s-num
                    tt-mat.qty = tt-rctd.qty
                    .
            END.

            IF lCreateIssue OR (item.i-code EQ "E" AND tt-rctd.tag EQ "") THEN 
            DO:
                FOR EACH tt-mat:
        
                    CREATE b-tt-rctd.
                    BUFFER-COPY tt-rctd EXCEPT rec_key TO b-tt-rctd
                        ASSIGN
                        b-tt-rctd.rita-code = "I"
                        b-tt-rctd.tt-row-id = ROWID(tt-rctd)
                        b-tt-rctd.seq-no    = 2
                        b-tt-rctd.s-num     = tt-mat.frm
                        b-tt-rctd.qty       = tt-mat.qty.
                    DELETE tt-mat.
                END. /* FOR EACH tt-mat */

            END. /* IF lCreateIssue  */

        END. /* if index(cTypes,"R") gt 0 */

    issue-adder-for-board:
    FOR EACH tt-rctd
        WHERE tt-rctd.rita-code EQ "I"
        AND tt-rctd.job-no    NE ""
        NO-LOCK,
        FIRST job NO-LOCK
        WHERE job.company EQ cocode
            AND job.job-no  EQ tt-rctd.job-no
            AND job.job-no2 EQ tt-rctd.job-no2
        ,

        FIRST item NO-LOCK
        WHERE item.company  EQ cocode
            AND item.i-no     EQ tt-rctd.i-no
            AND item.mat-type EQ "B"
        :
        IF AVAILABLE b-tt-rctd THEN 
        DO:
        {rm/rm-addcr.i E b-tt-rctd b-tt-rctd b-}
        ASSIGN
         b-tt-rctd.tt-row-id = ROWID(tt-rctd)
         b-tt-rctd.seq-no    = 3.
         END.
        END.
    END.


    ASSIGN
        dGrdQuantity = 0
        dGrdCost     = 0.

    FOR EACH tt-rctd WHERE INDEX(cTypes,tt-rctd.rita-code) GT 0 
        BREAK BY tt-rctd.loc                                             
        BY tt-rctd.i-no                                            
        BY tt-rctd.job-no                                          
        BY tt-rctd.job-no2 
        BY tt-rctd.loc-bin                           
        BY tt-rctd.tag
        BY RECID(tt-rctd)
        :                                                   

        IF FIRST-OF(tt-rctd.loc) THEN 
            cWhse = tt-rctd.loc.       
      

        FIND FIRST item NO-LOCK
            WHERE item.company EQ cocode
            AND item.i-no    EQ tt-rctd.i-no
            NO-ERROR.

        RELEASE costtype.
        IF AVAILABLE item THEN
            FIND FIRST costtype NO-LOCK
                WHERE costtype.company   EQ cocode
                AND costtype.cost-type EQ item.cost-type
                NO-ERROR.

        RELEASE po-ord.
        IF int(tt-rctd.po-no) NE 0 AND tt-rctd.rita-code EQ "R" THEN                                         
            FIND FIRST po-ord NO-LOCK
                WHERE po-ord.company EQ cocode
                AND po-ord.po-no   EQ int(tt-rctd.po-no)
                NO-ERROR.

        RELEASE po-ordl.
        IF AVAILABLE po-ord THEN
            FIND FIRST po-ordl NO-LOCK
                WHERE po-ordl.company   EQ cocode
                    AND po-ordl.po-no     EQ po-ord.po-no
                    AND po-ordl.i-no      EQ tt-rctd.i-no
                    AND po-ordl.job-no    EQ tt-rctd.job-no
                    AND po-ordl.job-no2   EQ tt-rctd.job-no2
                    AND po-ordl.s-num     EQ tt-rctd.s-num
                    AND po-ordl.b-num     EQ tt-rctd.b-num
                    AND po-ordl.deleted   EQ NO
                    AND po-ordl.item-type EQ YES
                    NO-ERROR.

        dExtCost = tt-rctd.cost * tt-rctd.qty.
       
        IF rmpostgl AND AVAILABLE costtype AND costtype.inv-asset NE ""  AND
            dExtCost NE 0 AND dExtCost NE ?                       THEN 
        DO:

            IF tt-rctd.rita-code EQ "R"  AND  
                costtype.ap-accrued NE "" THEN 
            DO:

                /* Debit RM Asset */
                FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.inv-asset NO-LOCK NO-ERROR.
                IF NOT AVAILABLE work-gl THEN 
                DO:
                    CREATE work-gl.
                    work-gl.actnum = costtype.inv-asset.
                END.
                work-gl.debits = work-gl.debits + dExtCost.

                /* Credit RM AP Accrued */
                FIND FIRST work-gl WHERE work-gl.actnum EQ costtype.ap-accrued NO-LOCK NO-ERROR.
                IF NOT AVAILABLE work-gl THEN 
                DO:
                    CREATE work-gl.
                    work-gl.actnum = costtype.ap-accrued.
                END.
                work-gl.credits = work-gl.credits + dExtCost.
            END.

            ELSE
                IF tt-rctd.rita-code EQ "I" AND
                    tt-rctd.job-no NE ""     THEN 
                DO:

                    FOR EACH job-hdr NO-LOCK
                        WHERE job-hdr.company EQ cocode
                            AND job-hdr.job-no  EQ tt-rctd.job-no
                            AND job-hdr.job-no2 EQ tt-rctd.job-no2
                        ,
                        FIRST job  NO-LOCK OF job-hdr
                        BREAK BY job-hdr.frm:
                        ll-one-item = FIRST(job-hdr.frm) AND LAST(job-hdr.frm).
                        LEAVE.
                    END.

                    FOR EACH job-hdr NO-LOCK
                        WHERE job-hdr.company     EQ cocode
                            AND job-hdr.job-no      EQ tt-rctd.job-no
                            AND job-hdr.job-no2     EQ tt-rctd.job-no2
                            AND ((job-hdr.frm       EQ tt-rctd.s-num AND
                            (job-hdr.blank-no EQ tt-rctd.b-num OR tt-rctd.b-num EQ 0))
                            OR  ll-one-item)
                        ,
                        FIRST job  NO-LOCK OF job-hdr,
                        FIRST itemfg
                        WHERE itemfg.company EQ cocode
                            AND itemfg.i-no    EQ job-hdr.i-no
                        ,
                        FIRST prodl NO-LOCK
                        WHERE prodl.company EQ cocode
                            AND prodl.procat  EQ itemfg.procat
                        AND CAN-FIND(FIRST prod
                        WHERE prod.company EQ cocode
                            AND prod.prolin  EQ prodl.prolin)
                        ,
                        FIRST prod NO-LOCK
                        WHERE prod.company EQ cocode
                            AND prod.prolin  EQ prodl.prolin
                            AND prod.wip-mat NE ""
                        :

                        dTotMaterialQty = ROUND(dExtCost * (IF ll-one-item        OR
                            tt-rctd.b-num NE 0 OR
                            job-hdr.sq-in LE 0 OR
                            job-hdr.sq-in EQ ? THEN 1
                            ELSE (job-hdr.sq-in / 100)),2).

                        /* Debit FG Wip Material */
                        FIND FIRST work-gl NO-LOCK
                            WHERE work-gl.job     EQ job-hdr.job
                                AND work-gl.job-no  EQ job-hdr.job-no
                                AND work-gl.job-no2 EQ job-hdr.job-no2
                                AND work-gl.actnum  EQ prod.wip-mat 
                                NO-ERROR.
                        IF NOT AVAILABLE work-gl THEN 
                        DO:
                            CREATE work-gl.
                            ASSIGN
                                work-gl.job     = job-hdr.job
                                work-gl.job-no  = job-hdr.job-no
                                work-gl.job-no2 = job-hdr.job-no2
                                work-gl.actnum  = prod.wip-mat.
                        END.
                        work-gl.debits = work-gl.debits + dTotMaterialQty.

                        /* Credit RM Asset */
                        FIND FIRST work-gl NO-LOCK
                            WHERE work-gl.job     EQ job-hdr.job
                                AND work-gl.job-no  EQ job-hdr.job-no
                                AND work-gl.job-no2 EQ job-hdr.job-no2
                                AND work-gl.actnum  EQ costtype.inv-asset
                                NO-ERROR.
                        IF NOT AVAILABLE work-gl THEN 
                        DO:
                            CREATE work-gl.
                            ASSIGN
                                work-gl.job     = job-hdr.job
                                work-gl.job-no  = job-hdr.job-no
                                work-gl.job-no2 = job-hdr.job-no2
                                work-gl.actnum  = costtype.inv-asset.
                        END.
                        work-gl.credits = work-gl.credits + dTotMaterialQty.
                    END.
                END.
        END.
        IF tt-rctd.rita-code EQ "R" OR tt-rctd.rita-code EQ "A"
            THEN ASSIGN 
                dTotalQty  = dTotalQty + tt-rctd.qty
                dTotalCost = dTotalCost + (tt-rctd.cost * tt-rctd.qty)
                .
                
        IF tt-rctd.rita-code EQ "I" 
            THEN ASSIGN 
                dTotalQtyI  = dTotalQtyI  + tt-rctd.qty
                dTotalCostI = dTotalCostI + (tt-rctd.cost * tt-rctd.qty)
                .
        ASSIGN 
            iItemcnt = iItemcnt  + 1
            iGitMCnt = iGitMCnt + 1
            .

        IF LAST-OF(tt-rctd.i-no) THEN 
        DO:       

            ASSIGN
                dGrdQuantity  = dGrdQuantity   + dTotalQty 
                dGrdCost      = dGrdCost       + dTotalCost
                dGrdQuantityI = dGrdQuantityI  + dTotalQtyI 
                dGrdCostI     = dGrdCostI      + dTotalCostI
                dTotalQty     = 0
                dTotalCost    = 0
                dTotalQtyI    = 0
                dTotalCostI   = 0
                iItemcnt      = 0
                .      
        END. 
    END. /* each tt-rctd */
    


    FOR EACH work-gl BREAK BY work-gl.actnum:
        FIND FIRST account NO-LOCK
            WHERE account.company EQ cocode
                AND account.actnum  EQ work-gl.actnum
                NO-ERROR.
        
        ASSIGN
            cAccountDscr   = IF AVAILABLE account THEN account.dscr
                       ELSE "ACCOUNT NOT FOUND - " + work-gl.actnum
            cDisplayActNum = work-gl.actnum
            dDispAmt       = work-gl.debits - work-gl.credits
            .

    END. /* each work-job */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Function Implementations ***************** */

