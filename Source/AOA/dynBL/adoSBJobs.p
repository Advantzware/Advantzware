/*------------------------------------------------------------------------
  File:         adoSBJobs.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 11.16.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCompare     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLoadPrompt  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLocation    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hConnection  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFields      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hRecordSet   AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hTable       AS HANDLE     NO-UNDO EXTENT 2.
DEFINE VARIABLE iTotal       AS INTEGER    NO-UNDO.
DEFINE VARIABLE lNoChange    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lProgressBar AS LOGICAL    NO-UNDO.

/* Temp-Table Definitions ---                                           */
DEFINE TEMP-TABLE ttSBJobs NO-UNDO
    FIELD company      AS CHARACTER 
    FIELD location     AS CHARACTER 
    FIELD line         AS INTEGER
    FIELD m-code       AS CHARACTER
    FIELD seq-no       AS INTEGER
    FIELD job-no       AS CHARACTER 
    FIELD job-no2      AS INTEGER
    FIELD frm          AS INTEGER
    FIELD blank-no     AS INTEGER
    FIELD pass         AS INTEGER 
    FIELD qty          AS INTEGER
    FIELD i-no         AS CHARACTER
    FIELD i-name       AS CHARACTER
    FIELD cust-no      AS CHARACTER      
    FIELD due-date     AS DATE
    FIELD run-qty      AS INTEGER
    FIELD mr-hr        AS DECIMAL
    FIELD run-hr       AS DECIMAL
    FIELD speed        AS INTEGER
    FIELD qtyRecvd     AS INTEGER
    .
DEFINE TEMP-TABLE ttUniqueJob NO-UNDO
    FIELD company AS CHARACTER 
    FIELD job-no  AS CHARACTER 
    FIELD job-no2 AS INTEGER
    FIELD job     AS INTEGER
        INDEX ttUniqueJob IS PRIMARY
            company
            job-no
            job-no2
            .  
/* **********************  Internal Functions  ************************ */

FUNCTION fUniqueJNo RETURNS INTEGER ():
    DEFINE BUFFER job-hdr FOR job-hdr.
    
    FIND LAST job-hdr NO-LOCK NO-ERROR.
    RETURN IF AVAILABLE job-hdr THEN job-hdr.j-no + 1 ELSE 1.
END FUNCTION.

/* **********************  Main Block  ******************************** */

RUN spGetSessionParam ("adoSBJobs", OUTPUT cLoadPrompt).
IF cLoadPrompt NE "NO" THEN
MESSAGE
    "Refresh Jobs from ADO Source?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
UPDATE lRefresh AS LOGICAL.
ELSE lRefresh = YES.

IF lRefresh THEN
RUN pBusinessLogic.
IF RETURN-VALUE NE "" THEN
RETURN "ERROR".

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cADOClient AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cADOSBJobs AS CHARACTER NO-UNDO.

    RUN spGetSettingByName ("ADOClient", OUTPUT cADOClient).
    /* create ActiveX Data Object connection */
    CREATE "ADODB.Connection.6.0" hConnection.
    CASE cADOClient:
        WHEN "IndepedentII" THEN
        hConnection:ConnectionString = "{AOA/dynBL/IndepedentII.i}".
        WHEN "Sumter" THEN
        hConnection:ConnectionString = "{AOA/dynBL/Sumter.i}".
    END CASE.

    /* open ADO connection */
    hConnection:Open (,,,) NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    RETURN "ERROR".
    /* create record set connection */
    CREATE "ADODB.Recordset.6.0" hRecordSet.
    hRecordSet:LockType = 1. /* read only */
    
    RUN spGetSessionParam ("Company",   OUTPUT cCompany)   NO-ERROR.
    RUN spGetSessionParam ("adoSBJobs", OUTPUT cADOSBJobs) NO-ERROR.
    lProgressBar = cADOSBJobs NE "NO".
    RUN pADOSBJobs.
    
    hConnection:Close ().
    RELEASE OBJECT hRecordSet.
    RELEASE OBJECT hConnection.

    IF lProgressBar THEN
    RUN spProgressBar (?, ?, 100).

    RETURN "".

END PROCEDURE.

PROCEDURE pADOSBJobs:
    DEFINE VARIABLE cSelect AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.

    /* set sql select statement */
    RUN pSetSelect (OUTPUT cSelect).
    /* open record set */
    hRecordSet:Open (cSelect,hConnection,1,1,).
    /* grab all record set fields */
    hFields = hRecordSet:Fields.
    /* read all record set rows */

/*    OUTPUT TO c:\tmp\Amtech.txt.*/

    DO WHILE TRUE:
        IF hRecordSet:EOF THEN LEAVE.
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar ("ADO SB Jobs - Import Jobs", iCount, hRecordSet:RecordCount).
        RUN pCreatettSBJobs.
        hRecordSet:MoveNext.
    END. /* do while */

/*    OUTPUT CLOSE.                                    */
/*    OS-COMMAND NO-WAIT notepad.exe c:\tmp\Amtech.txt.*/

    RUN pSetLineOrder.
    RUN pSBJobs.
    /* close record set */
    hRecordSet:Close ().

END PROCEDURE.

PROCEDURE pCreatettSBJobs:
    DEFINE VARIABLE iDMICode AS INTEGER   NO-UNDO.

    iDMICode = hFields:Item("mach_no"):VALUE.
    FIND FIRST mach NO-LOCK
         WHERE mach.company     EQ cCompany
           AND mach.spare-int-2 EQ iDMICode
         NO-ERROR.
    IF AVAILABLE mach THEN DO:
        CREATE ttSBJobs.
        ASSIGN
            ttSBJobs.blank-no     = 1
            ttSBJobs.company      = cCompany
            ttSBJobs.cust-no      = hFields:Item("cscode"):Value
            ttSBJobs.due-date     = hFields:Item("due_date"):Value
            ttSBJobs.frm          = hFields:Item("form_no"):Value
            ttSBJobs.i-name       = hFields:Item("cust_ident"):Value
            ttSBJobs.i-no         = hFields:Item("item_no"):Value
            ttSBJobs.job-no       = hFields:Item("job_number"):Value
            ttSBJobs.job-no2      = 0
            ttSBJobs.line         = 0
            ttSBJobs.location     = cLocation
            ttSBJobs.m-code       = mach.m-code
            ttSBJobs.mr-hr        = hFields:Item("stnd_su_hrs"):Value
            ttSBJobs.pass         = hFields:Item("pass_no"):Value
            ttSBJobs.qty          = hFields:Item("qty_ordered"):Value
            ttSBJobs.run-hr       = hFields:Item("stnd_run_hrs"):Value
            ttSBJobs.run-qty      = hFields:Item("qty_to_run"):Value
            ttSBJobs.seq-no       = hFields:Item("mach_seq_no"):Value
            ttSBJobs.speed        = hFields:Item("mach_speed"):Value
            ttSBJobs.run-hr       = ttSBJobs.run-qty / ttSBJobs.speed
            ttSBJobs.qtyRecvd     = hFields:Item("qty_corr_rcvd"):Value
            iTotal                = iTotal + 1
            .

/*        IF hFields:Item("qty_corr_rcvd"):VALUE GT 0 THEN*/
/*        EXPORT                                          */
/*            hFields:Item("cscode"):Value                */
/*            hFields:Item("due_date"):Value              */
/*            hFields:Item("form_no"):Value               */
/*            hFields:Item("cust_ident"):Value            */
/*            hFields:Item("item_no"):Value               */
/*            hFields:Item("job_number"):Value            */
/*            hFields:Item("stnd_su_hrs"):Value           */
/*            hFields:Item("pass_no"):Value               */
/*            hFields:Item("qty_ordered"):Value           */
/*            hFields:Item("stnd_run_hrs"):Value          */
/*            hFields:Item("qty_to_run"):Value            */
/*            hFields:Item("mach_seq_no"):Value           */
/*            hFields:Item("mach_speed"):Value            */
/*            hFields:Item("qty_sheets_rcvd"):Value       */
/*            hFields:Item("qty_corr_rcvd"):Value         */
/*            .                                           */

    END. /* if avail */

END PROCEDURE.

PROCEDURE pGetInternalJob PRIVATE :
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJob     AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bJob    FOR job.
    DEFINE BUFFER bJobHdr FOR job-hdr.
    
    opiJob = 1.        
    FIND LAST bJob NO-LOCK
         WHERE bJob.company EQ ipcCompany 
         USE-INDEX job
         NO-ERROR.    
    FIND LAST bJobHdr NO-LOCK
         WHERE bJobHdr.company EQ ipcCompany
         USE-INDEX job
         NO-ERROR.    
    IF AVAILABLE bJob AND AVAILABLE bJobHdr THEN DO:
        IF bJobHdr.job GT bJob.job THEN
        opiJob = bJobHdr.job + 1.    
        IF bJob.job GE bJobHdr.job THEN
        opiJob = bJob.job + 1.
    END. /* if avail */

END PROCEDURE.

PROCEDURE pSBJobs:
    DEFINE VARIABLE cCEMenu   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIndustry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJob      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJNo      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lCEMenu   AS LOGICAL   NO-UNDO.

    DISABLE TRIGGERS FOR LOAD OF job.
    DISABLE TRIGGERS FOR LOAD OF job-hdr.
    DISABLE TRIGGERS FOR LOAD OF job-mch.                                                                                                                                 

    DEFINE BUFFER bJobMch FOR job-mch.

    RUN sys/ref/nk1look.p (
        cCompany,"CEMENU","C",NO,NO,"","",
        OUTPUT cCEMenu,OUTPUT lCEMenu
        ).
    IF lCEMenu AND cCEMenu NE "" THEN
    cIndustry = IF cCEMenu EQ "Foldware" THEN "1"
           ELSE IF cCEMenu EQ "Corrware" THEN "2"
           ELSE "".

    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company      EQ cCompany
          AND job-mch.run-complete EQ NO
        :
        IF CAN-FIND(FIRST ttSBJobs
                    WHERE ttSBJobs.company  EQ job-mch.company
                      AND ttSBJobs.line     EQ job-mch.line
                      AND ttSBJobs.m-code   EQ job-mch.m-code
                      AND ttSBJobs.job-no   EQ job-mch.job-no
                      AND ttSBJobs.job-no2  EQ job-mch.job-no2
                      AND ttSBJobs.frm      EQ job-mch.frm
                      AND ttSBJobs.blank-no EQ job-mch.blank-no
                      AND ttSBJobs.pass     EQ job-mch.pass) THEN
        NEXT.
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar ("Run Complete Job Routings", iCount, ?).
        FIND FIRST bJobMch EXCLUSIVE-LOCK
             WHERE ROWID(bJobMch) EQ ROWID(job-mch).
        bJobMch.run-complete = YES.
        RELEASE bJobMch.
    END. // each job-mch

    iCount = 0.
    FOR EACH ttSBJobs:
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar ("ADO SB Jobs - Load into Job Machine", iCount, iTotal).
        FIND FIRST ttUniqueJob
             WHERE ttUniqueJob.company EQ ttSBJobs.company
               AND ttUniqueJob.job-no  EQ ttSBJobs.job-no
               AND ttUniqueJob.job-no2 EQ ttSBJobs.job-no2
             NO-ERROR.
        IF NOT AVAILABLE ttUniqueJob THEN DO:
            RUN pGetInternalJob (
                ttSBJobs.company,
                OUTPUT iJob
                ). 
            CREATE ttUniqueJob.
            ASSIGN
                ttUniqueJob.company = ttSBJobs.company
                ttUniqueJob.job-no  = ttSBJobs.job-no
                ttUniqueJob.job-no2 = ttSBJobs.job-no2
                ttUniqueJob.job     = iJob
                .
        END. /* if not avail */
        FIND FIRST job EXCLUSIVE-LOCK
             WHERE job.company EQ ttSBJobs.company
               AND job.loc     EQ ttSBJobs.location
               AND job.job-no  EQ ttSBJobs.job-no
               AND job.job-no2 EQ ttSBJobs.job-no2
             NO-ERROR.
        IF NOT AVAILABLE job THEN DO:
            CREATE job.
            ASSIGN 
                job.company   = ttSBJobs.company
                job.loc       = ttSBJobs.location
                job.job       = ttUniqueJob.job
                job.job-no    = ttSBJobs.job-no
                job.job-no2   = ttSBJobs.job-no2
                job.stat      = "P"
                job.opened    = TRUE 
                job.orderType = "O"
                job.industry  = cIndustry
                .                
        END. /* if not avail */
        job.due-date   = ttSBJobs.due-date.                
    
        FIND FIRST job-hdr EXCLUSIVE-LOCK
             WHERE job-hdr.company  EQ ttSBJobs.company
               AND job-hdr.loc      EQ ttSBJobs.location
               AND job-hdr.job-no   EQ ttSBJobs.job-no
               AND job-hdr.job-no2  EQ ttSBJobs.job-no2
               AND job-hdr.frm      EQ ttSBJobs.frm
             NO-ERROR.
        IF NOT AVAILABLE job-hdr THEN DO:
            iJNo = fUniqueJNo().
            CREATE job-hdr.
            ASSIGN 
                job-hdr.company  = ttSBJobs.company
                job-hdr.loc      = ttSBJobs.location
                job-hdr.job-no   = ttSBJobs.job-no
                job-hdr.job-no2  = ttSBJobs.job-no2
                job-hdr.frm      = ttSBJobs.frm
                job-hdr.opened   = TRUE
                job-hdr.job      = ttUniqueJob.job
                job-hdr.j-no     = iJNo
                .
        END. /* if not avail */
        ASSIGN
            job-hdr.blank-no   = ttSBJobs.blank-no
            job-hdr.i-no       = ttSBJobs.i-no
            job-hdr.cust-no    = ttSBJobs.cust-no
            job-hdr.due-date   = ttSBJobs.due-date
            job-hdr.qty        = ttSBJobs.qty
            .
        FIND FIRST job-mch EXCLUSIVE-LOCK
             WHERE job-mch.company  EQ ttSBJobs.company
               AND job-mch.line     EQ ttSBJobs.line
               AND job-mch.m-code   EQ ttSBJobs.m-code
               AND job-mch.job-no   EQ ttSBJobs.job-no
               AND job-mch.job-no2  EQ ttSBJobs.job-no2
               AND job-mch.frm      EQ ttSBJobs.frm
               AND job-mch.blank-no EQ ttSBJobs.blank-no
               AND job-mch.pass     EQ ttSBJobs.pass
             NO-ERROR.
        IF NOT AVAILABLE job-mch THEN DO:
            CREATE job-mch.
            ASSIGN
                job-mch.company        = ttSBJobs.company
                job-mch.line           = ttSBJobs.line
                job-mch.m-code         = ttSBJobs.m-code
                job-mch.job-no         = ttSBJobs.job-no
                job-mch.job-no2        = ttSBJobs.job-no2
                job-mch.frm            = ttSBJobs.frm
                job-mch.blank-no       = ttSBJobs.blank-no
                job-mch.pass           = ttSBJobs.pass
                job-mch.est-op_rec_key = "None"
                job-mch.job            = ttUniqueJob.job
                .                    
        END. /* if not avail */
        ASSIGN
            job-mch.i-no    = ttSBJobs.i-no
            job-mch.i-name  = ttSBJobs.i-name 
            job-mch.run-qty = ttSBJobs.run-qty
            job-mch.mr-hr   = ttSBJobs.mr-hr
            job-mch.run-hr  = ttSBJobs.run-hr
            job-mch.speed   = ttSBJobs.speed
            .
        /* check if job has board */
        IF ttSBJobs.qtyRecvd NE 0 THEN DO:
            /* check if w/f board status exists */ 
            IF CAN-FIND(FIRST sbStatus
                        WHERE sbStatus.company     EQ ttSBJobs.company
                          AND sbStatus.m-code      EQ ttSBJobs.m-code
                          AND sbStatus.job-no      EQ ttSBJobs.job-no
                          AND sbStatus.job-no2     EQ ttSBJobs.job-no2
                          AND sbStatus.frm         EQ ttSBJobs.frm
                          AND sbStatus.sbStatus[1] EQ YES) THEN
            NEXT.
            /* find job's status record or create one */
            FIND FIRST sbStatus EXCLUSIVE-LOCK
                 WHERE sbStatus.company EQ ttSBJobs.company
                   AND sbStatus.m-code  EQ ttSBJobs.m-code
                   AND sbStatus.job-no  EQ ttSBJobs.job-no
                   AND sbStatus.job-no2 EQ ttSBJobs.job-no2
                   AND sbStatus.frm     EQ ttSBJobs.frm
                 NO-ERROR.
            IF NOT AVAILABLE sbStatus THEN DO:
                CREATE sbStatus.
                ASSIGN
                    sbStatus.company = ttSBJobs.company
                    sbStatus.m-code  = ttSBJobs.m-code
                    sbStatus.job-no  = ttSBJobs.job-no
                    sbStatus.job-no2 = ttSBJobs.job-no2
                    sbStatus.frm     = ttSBJobs.frm
                    . 
            END. /* not avail */
            sbStatus[1] = YES.
            RELEASE sbStatus.
        END. /* qtyrecvd ne 0 */
    END. /* each ttsbjobs */

    RELEASE job.
    RELEASE job-hdr.
    RELEASE job-mch.                                                                                                                                 

END PROCEDURE.

PROCEDURE pSetLineOrder:
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLine  AS INTEGER NO-UNDO.

    FOR EACH ttSBJobs
        BREAK BY ttSBJobs.company
              BY ttSBJobs.job-no
              BY ttSBJobs.seq-no
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar ("ADO SB Jobs - Set Line Order", iCount, iTotal).
        IF FIRST-OF(ttSBJobs.job-no) THEN
        iLine = 0.
        ASSIGN
            iLine = iLine + 1
            ttSBJobs.line = iLine
            .
    END. /* each ttSBJobs */

END PROCEDURE.

PROCEDURE pSetSelect:
    DEFINE OUTPUT PARAMETER opcSelect AS CHARACTER NO-UNDO.

    /* define sql section statement */
    opcSelect = "Select "
              + "Customer.cscode, "
              + "Ord_Mach_Ops.form_no, "
              + "Ord_Mach_Ops.mach_no, "
              + "Ord_Mach_Ops.mach_seq_no, "
              + "Ord_Mach_Ops.mach_speed, "
              + "Ord_Mach_Ops.pass_no, "
              + "Ord_Mach_Ops.qty_to_run, "
              + "Ord_Mach_Ops.stnd_run_hrs, "
              + "Ord_Mach_Ops.stnd_su_hrs, "
              + "Orders.completion_flg, "
              + "Orders.due_date, "
              + "Orders.for_invt_flg, "
              + "Orders.job_number, "
              + "Orders.order_no, "
              + "Orders.plt_no, "
              + "Orders.qty_ordered, "
/*              + "Ord_Part.qty_sheets_rcvd, "*/
              + "Ord_Part.qty_corr_rcvd, "
              + "Spec_File.cust_ident, "
              + "Spec_File.item_no "
              + "from Ord_Mach_Ops Ord_Mach_Ops "
              + "Inner Join ((Customer Customer "
              + "Inner Join Orders Orders on Customer.cscode=Orders.cscode) "
              + "Inner Join Spec_File Spec_File on ((Customer.cscode=Spec_File.cscode) "
              + "and (Orders.spec_no=Spec_File.spec_no)) "
              + "and (Orders.spec_part_no=Spec_File.spec_part_no)) "
              + "on Ord_Mach_Ops.order_no=Orders.order_no "
              + "Left Outer Join Ord_Part Ord_Part on (Orders.order_no=Ord_Part.order_no) "
              + "Where (Orders.completion_flg='O' or Orders.completion_flg='P') "
              + "and Orders.for_invt_flg in ('Y','N') "
              + "and ("
              .
    FOR EACH mach NO-LOCK
        WHERE mach.company EQ cCompany
          AND mach.spare-int-2 GT 0
        BREAK BY mach.spare-int-2
        :
        IF FIRST-OF(mach.spare-int-2) THEN
        opcSelect = opcSelect
                  + "Ord_Mach_Ops.mach_no="
                  + STRING(mach.spare-int-2)
                  + " or "
                  .
    END. /* each mach */
    opcSelect = TRIM(opcSelect," or ") + ")".

END PROCEDURE.
