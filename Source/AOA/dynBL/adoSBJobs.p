/*------------------------------------------------------------------------
  File:         adoSBJobs.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 11.16.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCompare    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLocation   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hConnection AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFields     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hRecordSet  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO EXTENT 2.
DEFINE VARIABLE lNoChange   AS LOGICAL    NO-UNDO.

/* Temp-Table Definitions ---                                           */
DEFINE TEMP-TABLE ttSBJobs NO-UNDO
    FIELD company  AS CHARACTER 
    FIELD location AS CHARACTER 
    FIELD line     AS INTEGER
    FIELD m-code   AS CHARACTER
    FIELD seq-no   AS INTEGER
    FIELD job-no   AS CHARACTER 
    FIELD job-no2  AS INTEGER
    FIELD frm      AS INTEGER
    FIELD blank-no AS INTEGER
    FIELD pass     AS INTEGER 
    FIELD qty      AS INTEGER
    FIELD i-no     AS CHARACTER
    FIELD i-name   AS CHARACTER
    FIELD cust-no  AS CHARACTER      
    FIELD due-date AS DATE
    FIELD run-qty  AS INTEGER
    FIELD mr-hr    AS DECIMAL
    FIELD run-hr   AS DECIMAL
    FIELD speed    AS INTEGER
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

MESSAGE
    "Refresh Jobs from ADO Source?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
UPDATE lRefresh AS LOGICAL.
IF lRefresh THEN
RUN pBusinessLogic.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    /* create ActiveX Data Object connection */
    CREATE "ADODB.Connection.6.0" hConnection.
    hConnection:ConnectionString = "{AOA/dynBL/IndepedentII.i}".
    /* open ADO connection */
    hConnection:Open (,,,).
    /* create record set connection */
    CREATE "ADODB.Recordset.6.0" hRecordSet.
    hRecordSet:LockType = 1. /* read only */
    
    RUN spGetSessionParam ("Company",  OUTPUT cCompany).
    RUN spGetSessionParam ("Location", OUTPUT cLocation).
    RUN pADOSBJobs.
    
    hConnection:Close ().
    RELEASE OBJECT hRecordSet.
    RELEASE OBJECT hConnection.

END PROCEDURE.

PROCEDURE pADOSBJobs:
    DEFINE VARIABLE cSelect AS CHARACTER NO-UNDO.

    /* set sql select statement */
    RUN pSetSelect (OUTPUT cSelect).
    /* open record set */
    hRecordSet:Open (cSelect,hConnection,1,1,).
    /* grab all record set fields */
    hFields = hRecordSet:Fields.
    /* read all record set rows */
    DO WHILE TRUE:
        IF hRecordSet:EOF THEN LEAVE.
        RUN pCreatettSBJobs.
        hRecordSet:MoveNext.
    END. /* do while */
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
            ttSBJobs.blank-no = 1
            ttSBJobs.company  = cCompany
            ttSBJobs.cust-no  = hFields:Item("cscode"):Value
            ttSBJobs.due-date = hFields:Item("due_date"):Value
            ttSBJobs.frm      = hFields:Item("form_no"):Value
            ttSBJobs.i-name   = hFields:Item("cust_ident"):Value
            ttSBJobs.i-no     = hFields:Item("item_no"):Value
            ttSBJobs.job-no   = hFields:Item("job_number"):Value
            ttSBJobs.job-no2  = 0
            ttSBJobs.line     = 0
            ttSBJobs.location = cLocation
            ttSBJobs.m-code   = mach.m-code
            ttSBJobs.mr-hr    = hFields:Item("stnd_su_hrs"):Value
            ttSBJobs.pass     = hFields:Item("pass_no"):Value
            ttSBJobs.qty      = hFields:Item("qty_ordered"):Value
            ttSBJobs.run-hr   = hFields:Item("stnd_run_hrs"):Value
            ttSBJobs.run-qty  = hFields:Item("qty_to_run"):Value
            ttSBJobs.seq-no   = hFields:Item("mach_seq_no"):Value
            ttSBJobs.speed    = hFields:Item("mach_speed"):Value
            ttSBJobs.run-hr   = ttSBJobs.run-qty / ttSBJobs.speed
            .
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
    DEFINE VARIABLE iJob      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJNo      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lCEMenu   AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (
        cCompany,"CEMENU","C",NO,NO,"","",
        OUTPUT cCEMenu,OUTPUT lCEMenu
        ).
    IF lCEMenu AND cCEMenu NE "" THEN
    cIndustry = IF cCEMenu EQ "Foldware" THEN "1"
           ELSE IF cCEMenu EQ "Corrware" THEN "2"
           ELSE "".

    FOR EACH ttSBJobs:
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
    END. /* each ttsbjobs */

    RELEASE job.
    RELEASE job-hdr.
    RELEASE job-mch.                                                                                                                                 

END PROCEDURE.

PROCEDURE pSetLineOrder:
    DEFINE VARIABLE iLine AS INTEGER NO-UNDO.

    FOR EACH ttSBJobs
        BREAK BY ttSBJobs.company
              BY ttSBJobs.job-no
              BY ttSBJobs.seq-no
        :
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
              + "Ord_Mach_Ops.mach_no, "
              + "Ord_Mach_Ops.mach_seq_no, "
              + "Orders.due_date, "
              + "Orders.completion_flg, "
              + "Orders.for_invt_flg, "
              + "Ord_Mach_Ops.stnd_run_hrs, "
              + "Ord_Mach_Ops.stnd_su_hrs, "
              + "Ord_Mach_Ops.mach_speed, "
              + "Ord_Mach_Ops.qty_to_run, "
              + "Customer.cscode, "
              + "Spec_File.item_no, "
              + "Orders.qty_ordered, "
              + "Ord_Mach_Ops.pass_no, "
              + "Orders.plt_no, "
              + "Ord_Mach_Ops.form_no, "
              + "Orders.job_number, "
              + "Orders.order_no, "
              + "Spec_File.cust_ident "
              + "from Ord_Mach_Ops Ord_Mach_Ops "
              + "Inner Join ((Customer Customer "
              + "Inner Join Orders Orders on Customer.cscode=Orders.cscode) "
              + "Inner Join Spec_File Spec_File on ((Customer.cscode=Spec_File.cscode) "
              + "and (Orders.spec_no=Spec_File.spec_no)) "
              + "and (Orders.spec_part_no=Spec_File.spec_part_no)) "
              + "on Ord_Mach_Ops.order_no=Orders.order_no "
              + "Where (Orders.completion_flg='O' or Orders.completion_flg='P') "
              + "and Orders.due_date>=~{ts '2020-10-15 00:00:00'} "
              + "and Orders.for_invt_flg<>'R'"
              + "and ("
              .
    FOR EACH mach NO-LOCK
        WHERE mach.company EQ cCompany
          AND mach.spare-int-2 GT 0
           BY mach.spare-int-2
        :
        opcSelect = opcSelect
                  + "Ord_Mach_Ops.mach_no="
                  + STRING(mach.spare-int-2)
                  + " or "
                  .
    END. /* each mach */
    opcSelect = TRIM(opcSelect," or ") + ")".

END PROCEDURE.
