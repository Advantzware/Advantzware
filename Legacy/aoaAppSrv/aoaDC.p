&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaDC.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Production Analysis.rpa */
DEFINE TEMP-TABLE ttProductionAnalysis NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD machine        AS CHARACTER LABEL "Machine"         FORMAT "x(6)"
    FIELD custNo         AS CHARACTER LABEL "Customer"        FORMAT "x(8)"
    FIELD custName       AS CHARACTER LABEL "Customer Name"   FORMAT "x(30)"
    FIELD itemNo         AS CHARACTER LABEL "Item No"         FORMAT "x(10)"
    FIELD partNo         AS CHARACTER LABEL "Part No"         FORMAT "x(12)"
    FIELD jobNo          AS CHARACTER LABEL "Job"             FORMAT "x(6)"
    FIELD jobNo2         AS INTEGER   LABEL "Run"             FORMAT ">9"
    FIELD frm            AS INTEGER   LABEL "Form"            FORMAT ">>9"
    FIELD blankNo        AS INTEGER   LABEL "Blank"           FORMAT ">9"
    FIELD pass           AS INTEGER   LABEL "Pass"            FORMAT ">>9"
    FIELD shift          AS INTEGER   LABEL "Shift"           FORMAT ">9"
    FIELD mrActHr        AS DECIMAL   LABEL "MR Act Hr"       FORMAT ">>>>9.99"
    FIELD runActHr       AS DECIMAL   LABEL "Run Act Hr"      FORMAT ">>>>9.99"
    FIELD dtActHr        AS DECIMAL   LABEL "Act D/T Hr"      FORMAT ">>>>9.99"
    FIELD totActHrs      AS DECIMAL   LABEL "Total Act Hrs"   FORMAT "->>>9.99"
    FIELD totLabHrs      AS DECIMAL   LABEL "Total Labor Hrs" FORMAT "->,>>>,>>>,>>9.99"
    FIELD pieces         AS DECIMAL   LABEL "Pieces"          FORMAT ">,>>>,>>9"
    FIELD piecesPerHr    AS DECIMAL   LABEL "Pieces/Hour"     FORMAT "->>>,>>>,>>9.99"
    FIELD piecesRunHr    AS DECIMAL   LABEL "Pieces/Run Hr"   FORMAT ">>,>>9.99"
    FIELD numberUp       AS INTEGER   LABEL "Number Up"       FORMAT ">>,>>>,>>9"
    FIELD kicksPerHrs    AS DECIMAL   LABEL "Kicks/Hour"      FORMAT "->>,>>>,>>9.99"
    FIELD piecesManHr    AS DECIMAL   LABEL "Pieces/Man Hr"   FORMAT "->>,>>>,>>>,>>9.99"
    FIELD sqFtPiece      AS DECIMAL   LABEL "SqFt Piece"      FORMAT ">>,>>9.99"
    FIELD totalMSF       AS DECIMAL   LABEL "Total MSF"       FORMAT ">>,>>9.99"
    FIELD msfPerHr       AS DECIMAL   LABEL "MSF/Hour"        FORMAT "->,>>>,>>9.99"
    FIELD totalTons      AS DECIMAL   LABEL "Total Tons"      FORMAT ">>,>>9.99"
    FIELD mrWaste        AS DECIMAL   LABEL "MR Waste"        FORMAT "->,>>9.99"
    FIELD runWaste       AS DECIMAL   LABEL "Run Waste"       FORMAT "->,>>9.99"
    FIELD totalWaste     AS DECIMAL   LABEL "Total Waste"     FORMAT "->>,>>,>>9.99"
    FIELD wastePct       AS DECIMAL   LABEL "Waste Pct"       FORMAT "->,>>9.99"
    FIELD crew           AS DECIMAL   LABEL "Crew"            FORMAT "->,>>>,>>9.99"
    FIELD qtyExpect      AS DECIMAL   LABEL "Qty Expected"    FORMAT ">,>>>,>>9"
    FIELD mrStdHr        AS DECIMAL   LABEL "MR Std Hr"       FORMAT ">>>>9.99"
    FIELD mrEffPct       AS DECIMAL   LABEL "MR Eff Pct"      FORMAT "->>>9.99"
    FIELD runStdHr       AS DECIMAL   LABEL "Run Std Hr"      FORMAT ">>>>9.99"
    FIELD runEffPct      AS DECIMAL   LABEL "Run Eff Pct"     FORMAT "->>>9.99"
    FIELD dtHrsEff       AS DECIMAL   LABEL "D/T Hrs Eff"     FORMAT "->>>9.99"
    FIELD totStdHrs      AS DECIMAL   LABEL "Total Std Hrs"   FORMAT "->>>9.99"
    FIELD totEffPct      AS DECIMAL   LABEL "Total Eff Pct"   FORMAT "->>>9.99"
    FIELD mrComp         AS CHARACTER LABEL "MR-C"            FORMAT "x(8)"
    FIELD runComp        AS CHARACTER LABEL "Run-C"           FORMAT "x(8)"
    FIELD dept           AS CHARACTER LABEL "Department"      FORMAT "x(2)"
    FIELD actMachine     AS CHARACTER LABEL "Act Machine"     FORMAT "x(6)"
    FIELD opDate         AS DATE      LABEL "Oper Date"       FORMAT "99/99/9999"
    FIELD startDate      AS DATE      LABEL "Start Date"      FORMAT "99/99/9999"
    FIELD startTime      AS CHARACTER LABEL "Start Time"      FORMAT "x(11)"
    FIELD xxTotMRHrs     AS DECIMAL   LABEL "Total MR Hrs"    FORMAT "->>>9.99"
    FIELD xxTotRunHrs    AS DECIMAL   LABEL "Total MR Hrs"    FORMAT "->>>9.99"
    FIELD xxJob          AS INTEGER   LABEL "Unique Job"      FORMAT ">>>>>>9"
    FIELD xxSort         AS CHARACTER LABEL "Sort"            FORMAT "x(100)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .
/* Production Analysis.rpa */

/* Machine Productivity.rpa */
DEFINE TEMP-TABLE ttMachineProductivity NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD msfPerHr       AS DECIMAL   LABEL "1000 Sq Ft per Total Mach Hr" FORMAT "->,>>>,>>9.999"
    FIELD sqFtPerSetup   AS DECIMAL   LABEL "1000 Sq Ft per Setup"         FORMAT "->,>>>,>>9.999"
    FIELD sqFtPiece      AS DECIMAL   LABEL "Sq Ft per Piece"              FORMAT "->,>>>,>>9.999"
    FIELD manHrPerSqFt   AS DECIMAL   LABEL "Man Hr per 1000 Sq Ft"        FORMAT "->,>>>,>>9.999"
    FIELD machine        AS CHARACTER LABEL "Machine Specs"                FORMAT "x(6)"
    FIELD piecesPerHr    AS DECIMAL   LABEL "Pieces per Total Mach Hr"     FORMAT "->,>>>,>>9.999"
    FIELD piecesRunHr    AS DECIMAL   LABEL "Pieces per Run Mach Hr"       FORMAT "->,>>>,>>9.999"
    FIELD piecesPerSetup AS DECIMAL   LABEL "Pieces per Setup"             FORMAT "->,>>>,>>9.999"
    FIELD minPerSetup    AS DECIMAL   LABEL "Minutes per Setup"            FORMAT "->,>>>,>>9.999"
    FIELD avgCrew        AS DECIMAL   LABEL "Avg Crew"                     FORMAT "->,>>>,>>9.999"
    FIELD downTimePct    AS DECIMAL   LABEL "Down Time Pct"                FORMAT "->,>>>,>>9.999"
    FIELD xxSetups       AS INTEGER   LABEL "Setups"                       FORMAT ">>>>>9"
    FIELD xxPieces       AS DECIMAL   LABEL "Pieces"                       FORMAT ">,>>>,>>9"
    FIELD xxTotalMSF     AS DECIMAL   LABEL "Total MSF"                    FORMAT ">>,>>9.99"
    FIELD xxDTActHr      AS DECIMAL   LABEL "Act D/T Hr"                   FORMAT ">>>>9.99"
    FIELD xxMRActHr      AS DECIMAL   LABEL "MR Act Hr"                    FORMAT ">>>>9.99"
    FIELD xxRunActHr     AS DECIMAL   LABEL "Run Act Hr"                   FORMAT ">>>>9.99"
    FIELD xxTotActHrs    AS DECIMAL   LABEL "Total Act Hrs"                FORMAT ">>>>9.99"
    FIELD xxTotLabHrs    AS DECIMAL   LABEL "Total Labor Hrs"              FORMAT "->,>>>,>>9.999"
    FIELD xxTotMRHrs     AS DECIMAL   LABEL "Total MR Hrs"                 FORMAT "->>>9.99"
    FIELD xxTotRunHrs    AS DECIMAL   LABEL "Total MR Hrs"                 FORMAT "->>>9.99"
        INDEX sortBy IS PRIMARY rowType machine
        .
/* Machine Productivity.rpa */

{sys/ref/CustList.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fMachineProductivity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fMachineProductivity Procedure 
FUNCTION fMachineProductivity RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fProductionAnalysis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fProductionAnalysis Procedure 
FUNCTION fProductionAnalysis RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Production Analysis.rpa
  Parameters:  Company, Use List?, Start Cust, End Cust, ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
              AND bCust.cust-no GE ipcStartCust
              AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pMachineProductivity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMachineProductivity Procedure 
PROCEDURE pMachineProductivity :
/*------------------------------------------------------------------------------
  Purpose:     Machine Productivity.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pMachineProductivity.i}
    
    /* local variables */
    DEFINE VARIABLE cStartDept AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndDept   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort      AS CHARACTER NO-UNDO.
    
    ASSIGN
        cStartDept = CHR(32)
        cEndDept   = CHR(255)
        cSort      = "Alphabetically"
        .
    
    RUN pProductionAnalysis1 (
        ipcCompany,
        cLocation,
        cStartDept,
        cEndDept,
        cStartMachine,
        cEndMachine,
        dtStartOpDate,
        cStartOpDateOption,
        dtEndOpDate,
        cEndOpDateOption,
        iStartShift,
        iEndShift,
        lPrintByScheduledMachine,
        lRoundDecimals,
        cSort
        ).

    FOR EACH ttProductionAnalysis
        WHERE ttProductionAnalysis.rowType EQ "Data"
        BREAK BY ttProductionAnalysis.machine
        :
        IF FIRST-OF(ttProductionAnalysis.machine) THEN DO:
            CREATE ttMachineProductivity.
            ttMachineProductivity.machine = ttProductionAnalysis.machine.
        END. /* first-of */

        ASSIGN
            ttMachineProductivity.xxSetups       = ttMachineProductivity.xxSetups    + 1
            ttMachineProductivity.sqFtPiece      = ttMachineProductivity.sqFtPiece   + ttProductionAnalysis.sqFtPiece
            ttMachineProductivity.avgCrew        = ttMachineProductivity.avgCrew     + ttProductionAnalysis.crew
            ttMachineProductivity.xxPieces       = ttMachineProductivity.xxPieces    + ttProductionAnalysis.pieces
            ttMachineProductivity.xxTotalMSF     = ttMachineProductivity.xxTotalMSF  + ttProductionAnalysis.totalMSF
            ttMachineProductivity.xxDTActHr      = ttMachineProductivity.xxDTActHr   + ttProductionAnalysis.dtActHr
            ttMachineProductivity.xxMRActHr      = ttMachineProductivity.xxMRActHr   + ttProductionAnalysis.mrActHr
            ttMachineProductivity.xxRunActHr     = ttMachineProductivity.xxRunActHr  + ttProductionAnalysis.runActHr
            ttMachineProductivity.xxTotActHrs    = ttMachineProductivity.xxTotActHrs + ttProductionAnalysis.totActHrs
            ttMachineProductivity.xxTotLabHrs    = ttMachineProductivity.xxTotLabHrs + ttProductionAnalysis.totLabHrs
            ttMachineProductivity.xxTotMRHrs     = ttMachineProductivity.xxTotMRHrs  + ttProductionAnalysis.xxTotMRHrs
            ttMachineProductivity.xxTotRunHrs    = ttMachineProductivity.xxTotRunHrs + ttProductionAnalysis.xxTotRunHrs
            .
    END. /* each ttMachineProductivity */

    FOR EACH ttMachineProductivity
        :
        ASSIGN
            ttMachineProductivity.msfPerHr       = ttMachineProductivity.xxTotalMSF  / ttMachineProductivity.xxTotActHrs
            ttMachineProductivity.sqFtPerSetup   = ttMachineProductivity.xxTotalMSF  / ttMachineProductivity.xxSetups
            ttMachineProductivity.sqFtPiece      = ttMachineProductivity.sqFtPiece   / ttMachineProductivity.xxPieces    * 1000
            ttMachineProductivity.manHrPerSqFt   = ttMachineProductivity.xxTotLabHrs / ttMachineProductivity.xxTotalMSF
            ttMachineProductivity.piecesPerHr    = ttMachineProductivity.xxPieces    / ttMachineProductivity.xxTotActHrs
            ttMachineProductivity.piecesRunHr    = ttMachineProductivity.xxPieces    / ttMachineProductivity.xxTotRunHrs
            ttMachineProductivity.piecesPerSetup = ttMachineProductivity.xxPieces    / ttMachineProductivity.xxSetups
            ttMachineProductivity.minPerSetup    = ttMachineProductivity.xxMRActHr   / ttMachineProductivity.xxSetups    * 60
            ttMachineProductivity.avgCrew        = ttMachineProductivity.avgCrew     / ttMachineProductivity.xxSetups
            ttMachineProductivity.downTimePct    = ttMachineProductivity.xxDTActHr   / ttMachineProductivity.xxTotActHrs * 100
            .
        IF ttMachineProductivity.msfPerHr       EQ ? THEN ttMachineProductivity.msfPerHr       = 0.
        IF ttMachineProductivity.sqFtPerSetup   EQ ? THEN ttMachineProductivity.sqFtPerSetup   = 0.
        IF ttMachineProductivity.sqFtPiece      EQ ? THEN ttMachineProductivity.sqFtPiece      = 0.
        IF ttMachineProductivity.manHrPerSqFt   EQ ? THEN ttMachineProductivity.manHrPerSqFt   = 0.
        IF ttMachineProductivity.piecesPerHr    EQ ? THEN ttMachineProductivity.piecesPerHr    = 0.
        IF ttMachineProductivity.piecesRunHr    EQ ? THEN ttMachineProductivity.piecesRunHr    = 0.
        IF ttMachineProductivity.piecesPerSetup EQ ? THEN ttMachineProductivity.piecesPerSetup = 0.
        IF ttMachineProductivity.minPerSetup    EQ ? THEN ttMachineProductivity.minPerSetup    = 0.
        IF ttMachineProductivity.avgCrew        EQ ? THEN ttMachineProductivity.avgCrew        = 0.
        IF ttMachineProductivity.downTimePct    EQ ? THEN ttMachineProductivity.downTimePct    = 0.
    END. /* each ttMachineProductivity */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pProductionAnalysis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProductionAnalysis Procedure 
PROCEDURE pProductionAnalysis :
/*------------------------------------------------------------------------------
  Purpose:     Production Analysis.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pProductionAnalysis.i}

    RUN pProductionAnalysis1 (
        ipcCompany,
        cLocation,
        cStartDept,
        cEndDept,
        cStartMachine,
        cEndMachine,
        dtStartOpDate,
        cStartOpDateOption,
        dtEndOpDate,
        cEndOpDateOption,
        iStartShift,
        iEndShift,
        lPrintByScheduledMachine,
        lRoundDecimals,
        cSort
        ).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pProductionAnalysis1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProductionAnalysis1 Procedure 
PROCEDURE pProductionAnalysis1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartDept AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndDept AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartMachine AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndMachine AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtStartOpDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartOpDateOption AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtEndOpDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndOpDateOption AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiStartShift AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiEndShift AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintByScheduledMachine AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRoundDecimals AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcSort AS CHARACTER NO-UNDO.
    
    /* local variables */
    DEFINE VARIABLE iTotalUp   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCheckCust AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cINo       AS CHARACTER NO-UNDO.

    DEFINE BUFFER bMchAct FOR mch-act.

    /* subject business logic */
    FOR EACH mch-act NO-LOCK
        WHERE mch-act.company EQ ipcCompany
          AND mch-act.op-date GE ipdtStartOpDate
          AND mch-act.op-date LE ipdtEndOpDate
          AND mch-act.shift   GE ipiStartShift
          AND mch-act.shift   LE ipiEndShift
        USE-INDEX dte-idx,
        FIRST mach NO-LOCK
        WHERE mach.company EQ mch-act.company
          AND mach.m-code  EQ mch-act.m-code
        BREAK BY mch-act.m-code:
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ mch-act.company
               AND job-hdr.job-no  EQ mch-act.job-no
               AND job-hdr.job-no2 EQ mch-act.job-no2
               AND job-hdr.frm     EQ mch-act.frm 
             NO-ERROR.
        IF AVAIL job-hdr AND NOT CAN-FIND(FIRST ttCustList
                                          WHERE ttCustList.cust-no EQ job-hdr.cust
                                            AND ttCustList.log-fld EQ TRUE) THEN NEXT.
        IF NOT ((iplPrintByScheduledMachine         AND
                 mach.sch-m-code NE ""            AND
                 mach.sch-m-code GE ipcStartMachine AND
                 mach.sch-m-code LE ipcEndMachine)  OR 
               ((NOT iplPrintByScheduledMachine     OR
                 mach.sch-m-code EQ "")           AND
                 mach.m-code GE ipcStartMachine     AND
                 mach.m-code LE ipcEndMachine))     THEN NEXT.
        IF (mch-act.dept GE ipcStartDept AND
            mch-act.dept LE ipcEndDept)  OR
           (mach.dept[2] NE ""         AND
            mach.dept[2] GE ipcStartDept AND
            mach.dept[2] LE ipcEndDept   AND
            NOT CAN-FIND(FIRST bMchAct
                         WHERE bMchAct.company EQ mch-act.company
                           AND bMchAct.job     EQ mch-act.job
                           AND bMchAct.job-no  EQ mch-act.job-no
                           AND bMchAct.job-no2 EQ mch-act.job-no2
                           AND bMchAct.frm     EQ mch-act.frm
                           AND bMchAct.m-code  NE mch-act.m-code
                           AND bMchAct.dept    EQ mach.dept[2])) OR
           (mach.dept[3] NE ""         AND
            mach.dept[3] GE ipcStartDept AND
            mach.dept[3] LE ipcEndDept   AND
            NOT CAN-FIND(FIRST bMchAct
                         WHERE bMchAct.company EQ mch-act.company
                           AND bMchAct.job     EQ mch-act.job
                           AND bMchAct.job-no  EQ mch-act.job-no
                           AND bMchAct.job-no2 EQ mch-act.job-no2
                           AND bMchAct.frm     EQ mch-act.frm
                           AND bMchAct.m-code  NE mch-act.m-code
                           AND bMchAct.dept    EQ mach.dept[3]))
                           OR  (mach.dept[4]   NE ""
                           AND mach.dept[4]    GE ipcStartDept
                           AND mach.dept[4]    LE ipcEndDept
                           AND NOT CAN-FIND(FIRST bMchAct
                         WHERE bMchAct.company EQ mch-act.company
                           AND bMchAct.job     EQ mch-act.job
                           AND bMchAct.job-no  EQ mch-act.job-no
                           AND bMchAct.job-no2 EQ mch-act.job-no2
                           AND bMchAct.frm     EQ mch-act.frm
                           AND bMchAct.m-code  NE mch-act.m-code
                           AND bMchAct.dept    EQ mach.dept[4])) THEN DO:
            FIND FIRST ttProductionAnalysis
                 WHERE ttProductionAnalysis.dept       EQ mch-act.dept
                   AND ttProductionAnalysis.machine    EQ (IF iplPrintByScheduledMachine AND mach.sch-m-code NE "" THEN
                                                           mach.sch-m-code ELSE mach.m-code)
                   AND ttProductionAnalysis.shift      EQ mch-act.shift
                   AND ttProductionAnalysis.jobNo      EQ mch-act.job-no
                   AND ttProductionAnalysis.jobNo2     EQ mch-act.job-no2
                   AND ttProductionAnalysis.frm        EQ mch-act.frm
                   AND ttProductionAnalysis.blankNo    EQ mch-act.blank-no
                   AND ttProductionAnalysis.pass       EQ mch-act.pass
                   AND ttProductionAnalysis.actMachine EQ mch-act.m-code
                 NO-ERROR.
            FIND job-code NO-LOCK
                 WHERE job-code.code EQ mch-act.code
                 NO-ERROR.
            IF NOT AVAILABLE job-code THEN NEXT.
            IF NOT AVAILABLE ttProductionAnalysis THEN DO:
                CREATE ttProductionAnalysis.
                ASSIGN
                    ttProductionAnalysis.dept       = mch-act.dept
                    ttProductionAnalysis.machine    = (IF iplPrintByScheduledMachine AND mach.sch-m-code NE "" THEN
                                                       mach.sch-m-code ELSE mach.m-code)
                    ttProductionAnalysis.shift      = mch-act.shift
                    ttProductionAnalysis.xxJob      = mch-act.job
                    ttProductionAnalysis.jobNo      = mch-act.job-no
                    ttProductionAnalysis.jobNo2     = mch-act.job-no2
                    ttProductionAnalysis.frm        = mch-act.frm
                    ttProductionAnalysis.blankNo    = mch-act.blank-no
                    ttProductionAnalysis.pass       = mch-act.pass
                    ttProductionAnalysis.actMachine = mch-act.m-code
                    ttProductionAnalysis.opDate     = mch-act.op-date
                    ttProductionAnalysis.startDate  = ttProductionAnalysis.opDate
                    .
              RUN pProRateMR.
            END. /* not avail tt */
            IF job-code.cat EQ "RUN" THEN DO:
                ASSIGN
                    ttProductionAnalysis.runActHr = ttProductionAnalysis.runActHr + mch-act.hours
                    ttProductionAnalysis.pieces   = ttProductionAnalysis.pieces
                                                  + IF mch-act.qty EQ ? THEN 0
                                                    ELSE mch-act.qty
                    .
                IF mch-act.qty NE ? AND mch-act.qty NE 0 THEN DO:
                    FIND FIRST job-hdr NO-LOCK
                         WHERE job-hdr.company EQ mch-act.company
                           AND job-hdr.job-no  EQ mch-act.job-no
                           AND job-hdr.job-no2 EQ mch-act.job-no2
                           AND job-hdr.frm     EQ mch-act.frm
                         NO-ERROR.
                    FIND FIRST eb NO-LOCK
                         WHERE eb.company  EQ job-hdr.company
                           AND eb.est-no   EQ job-hdr.est-no
                           AND eb.form-no  EQ job-hdr.frm
                           AND eb.blank-no EQ job-hdr.blank-no
                         NO-ERROR.
                    FIND ef OF eb NO-LOCK NO-ERROR.
                    IF AVAILABLE ef THEN
                    iTotalUP = IF ef.spare-int-1 EQ 0 THEN ef.n-out * ef.n-out-l * ef.n-out-d
                             ELSE ef.spare-int-1.

                    IF CAN-DO("R,S,B",mach.p-type) THEN DO:
                        FOR EACH job-mat NO-LOCK
                            WHERE job-mat.company EQ mch-act.company
                              AND job-mat.job     EQ mch-act.job
                              AND job-mat.job-no  EQ mch-act.job-no
                              AND job-mat.job-no2 EQ mch-act.job-no2
                              AND job-mat.frm     EQ mch-act.frm
                            USE-INDEX seq-idx,
                            FIRST item OF job-mat NO-LOCK
                            WHERE item.company  EQ job-mat.company 
                              AND item.i-no     EQ job-mat.rm-i-no
                              AND item.mat-type EQ "B"
                            :
                            IF CAN-DO("R,S",mach.p-type) THEN DO:
                                IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR mach.dept[3] = "PR" OR mach.dept[4] EQ "PR" THEN
                                ASSIGN
                                    ttProductionAnalysis.totalTons = ttProductionAnalysis.totalTons
                                                                   + (mch-act.qty * job-mat.wid * job-mat.len / 144000 * item.basis-w / 2000)
                                    ttProductionAnalysis.totalMSF  = ttProductionAnalysis.totalMSF
                                                                   + (mch-act.qty * job-mat.wid * job-mat.len / iTotalUp / 144000)
                                    .
                                ELSE
                                ASSIGN
                                    ttProductionAnalysis.totalTons = ttProductionAnalysis.totalTons
                                                                   + (mch-act.qty * job-mat.wid * job-mat.len / 144000 * item.basis-w / 2000)
                                    ttProductionAnalysis.totalMSF  = ttProductionAnalysis.totalMSF
                                                                   + (mch-act.qty * job-mat.wid * job-mat.len / 144000)
                                    .
                            END.
                            ELSE DO:
                                FIND FIRST itemfg NO-LOCK
                                     WHERE itemfg.company EQ job-hdr.company
                                       AND itemfg.i-no    EQ job-hdr.i-no
                                     NO-ERROR.
                                IF AVAILABLE itemfg THEN
                                ASSIGN
                                    ttProductionAnalysis.totalMSF  = ttProductionAnalysis.totalMSF
                                                                   + mch-act.qty * itemfg.t-sqin / 144000
                                    ttProductionAnalysis.totalTons = ttProductionAnalysis.totalTons
                                                                   + (mch-act.qty * itemfg.t-sqin / 144000 * item.basis-w / 2000) 
                                    .               
                            END.                                   
                            LEAVE.
                        END. /* each job-mat */
                    END. /* if p-type */
                    ASSIGN
                        ttProductionAnalysis.sqFtPiece    = ttProductionAnalysis.totalMSF * 1000 / ttProductionAnalysis.pieces
                        ttProductionAnalysis.piecesRunHr  = ttProductionAnalysis.pieces / ttProductionAnalysis.runActHr
                        .
                END. /* if qty */
            END. /* if cat run */
            ELSE
            IF job-code.cat EQ "MR" THEN
            ttProductionAnalysis.mrActHr = ttProductionAnalysis.mrActHr + mch-act.hours.
            ELSE
            ttProductionAnalysis.dtActHr = ttProductionAnalysis.dtActHr + mch-act.hours.
        END. /* if dept range */
    END. /* each mch-act */

    FOR EACH ttProductionAnalysis,
        FIRST job
        WHERE job.company EQ ipcCompany
          AND job.job     EQ ttProductionAnalysis.xxJob
          AND job.job-no  EQ ttProductionAnalysis.jobNo
          AND job.job-no2 EQ ttProductionAnalysis.jobNo2
        USE-INDEX job-no:
        cINo = "" .
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job     EQ job.job
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2
            BY job-hdr.blank-no DESCENDING
            BY job-hdr.frm      DESCENDING
            :
            cINo = job-hdr.i-no.
            IF job-hdr.frm       EQ ttProductionAnalysis.frm     AND
               (job-hdr.blank-no EQ ttProductionAnalysis.blankNo OR
                job-hdr.blank-no EQ 0) THEN LEAVE.
        END. /* each job-hdr */
        ttProductionAnalysis.itemNo = cINo.
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company   EQ ipcCompany
               AND job-mch.job       EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no    EQ ttProductionAnalysis.jobNo
               AND job-mch.job-no2   EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm       EQ ttProductionAnalysis.frm
               AND (job-mch.blank-no EQ ttProductionAnalysis.blankNo
                OR ttProductionAnalysis.blankNo EQ 0)
               AND job-mch.m-code    EQ ttProductionAnalysis.actMachine
               AND job-mch.pass      EQ ttProductionAnalysis.pass
             NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company   EQ ipcCompany
               AND job-mch.job       EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no    EQ ttProductionAnalysis.jobNo
               AND job-mch.job-no2   EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm       EQ ttProductionAnalysis.frm
               AND (job-mch.blank-no EQ ttProductionAnalysis.blankNo
                OR ttProductionAnalysis.blankNo EQ 0)
               AND job-mch.m-code    EQ ttProductionAnalysis.actMachine
             NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company EQ ipcCompany
               AND job-mch.job     EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no  EQ ttProductionAnalysis.jobNo
               AND job-mch.job-no2 EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm     EQ ttProductionAnalysis.frm
               AND job-mch.m-code  EQ ttProductionAnalysis.actMachine
               AND job-mch.speed   NE 0
             NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company EQ ipcCompany
               AND job-mch.job     EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no  EQ ttProductionAnalysis.jobNo
               AND job-mch.job-no2 EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm     EQ ttProductionAnalysis.frm
               AND job-mch.m-code  EQ ttProductionAnalysis.actMachine
             NO-ERROR.
        IF AVAILABLE job-mch THEN DO:
            IF ttProductionAnalysis.pieces NE 0 THEN DO:
                IF CAN-FIND(FIRST mach
                            WHERE mach.company EQ ipcCompany
                              AND mach.loc     EQ ipcLocation
                              AND mach.m-code  EQ job-mch.m-code
                              AND mach.therm   EQ YES
                              AND (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
                FOR EACH job-mat FIELDS(i-no len) NO-LOCK
                    WHERE job-mat.company EQ ipcCompany
                      AND job-mat.job EQ job.job
                      AND job-mat.frm EQ job-mch.frm
                      AND job-mat.frm GT 0
                      AND job-mat.len GT 0,
                    FIRST item FIELDS(mat-type) NO-LOCK
                    WHERE item.company EQ ipcCompany
                      AND item.i-no EQ job-mat.i-no
                    BREAK BY job-mat.frm
                          BY item.mat-type
                          BY job-mat.j-no
                          BY job-mat.rec_key
                    :
                    ttProductionAnalysis.runStdHr = (ttProductionAnalysis.pieces * job-mat.len / 12) / job-mch.speed.
                    LEAVE.
                END. /* each job-mat */
                ELSE
                ttProductionAnalysis.runStdHr = ttProductionAnalysis.pieces / job-mch.speed.
            END. /* if qty-prod */
            ELSE
            ttProductionAnalysis.runStdHr = job-mch.run-hr.
            ASSIGN
                ttProductionAnalysis.mrStdHr   = job-mch.mr-hr * (ttProductionAnalysis.mrActHr / ttProductionAnalysis.xxTotMRHrs)
                ttProductionAnalysis.qtyExpect = IF job-mch.speed NE 0 THEN
                                                (IF ttProductionAnalysis.runActHr NE 0 THEN ttProductionAnalysis.runActHr
                                                 ELSE ttProductionAnalysis.runStdHr) * job-mch.speed ELSE job-mch.run-qty
                ttProductionAnalysis.startDate = job-mch.start-date
                ttProductionAnalysis.startTime = STRING(job-mch.start-time,"hh:mm:ss am")
                .
            
        END. /* if avail job-mch */
        ttProductionAnalysis.xxSort = IF ipcSort EQ "Alphabetically" THEN
                                      (STRING(ttProductionAnalysis.machine,"x(6)")
                                    +  ttProductionAnalysis.jobNo + STRING(ttProductionAnalysis.jobNo2))
                                 ELSE (STRING(YEAR(ttProductionAnalysis.startDate),"9999")
                                    +  STRING(MONTH(ttProductionAnalysis.startDate),"99")
                                    +  STRING(DAY(ttProductionAnalysis.startDate),"99")
                                    +  STRING(ttProductionAnalysis.startTime,"99999")).
        IF ttProductionAnalysis.runStdHr EQ ? THEN ttProductionAnalysis.runStdHr = 0.
        IF ttProductionAnalysis.mrStdHr  EQ ? THEN ttProductionAnalysis.mrStdHr  = 0.
        IF iplRoundDecimals THEN DO:
            {sys/inc/roundup.i ttProductionAnalysis.qtyExpect}
        END. /* round decimals */
    END. /* each ttProductionAnalysis */

    RUN pProductionAnalysis2 (ipcCompany, ipdtStartOpDate, ipdtEndOpDate, ipiStartShift, ipiEndShift).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pProductionAnalysis2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProductionAnalysis2 Procedure 
PROCEDURE pProductionAnalysis2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtStartOpDate AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdtEndOpDate   AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiStartShift   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiEndShift     AS INTEGER   NO-UNDO.

    DEFINE VARIABLE dCalcMSF AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER NO-UNDO.

    DEFINE BUFFER bMchAct FOR mch-act.

    FOR EACH ttProductionAnalysis
        BREAK BY ttProductionAnalysis.machine
        :
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no    EQ ttProductionAnalysis.itemNo
             NO-ERROR.
        ASSIGN
            dCalcMSF = ttProductionAnalysis.pieces * (IF AVAILABLE itemfg THEN itemfg.t-sqft ELSE 1) / 1000
            ttProductionAnalysis.partNo = IF AVAILABLE itemfg THEN itemfg.part-no ELSE ""
            .
        FIND FIRST job NO-LOCK
             WHERE job.company EQ ipcCompany
               AND job.job-no  EQ SUBSTRING(ttProductionAnalysis.jobNo,1,6)
               AND job.job-no2 EQ ttProductionAnalysis.jobNo2
             NO-ERROR.
        ttProductionAnalysis.custNo = "" .
        IF AVAILABLE job THEN DO:
            FIND FIRST eb NO-LOCK
                 WHERE eb.company  EQ ipcCompany
                   AND eb.est-no   EQ job.est-no
                   AND eb.stock-no EQ ttProductionAnalysis.itemNo
                 NO-ERROR.
            IF AVAILABLE eb THEN ttProductionAnalysis.numberUp = eb.num-up.
            ELSE ttProductionAnalysis.numberUp = 0.
            FOR EACH misc-act FIELDS(cost) NO-LOCK
                WHERE misc-act.company EQ ipcCompany
                  AND misc-act.job     EQ job.job
                :
            END. /* each misc-act */
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ ipcCompany
                   AND job-hdr.i-no    EQ ttProductionAnalysis.itemNo
                   AND job-hdr.job-no  EQ job.job-no
                   AND job-hdr.job-no2 EQ job.job-no2
                 NO-ERROR.
            IF AVAILABLE job-hdr THEN ttProductionAnalysis.custNo = job-hdr.cust-no.
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ ipcCompany
                   AND cust.cust-no EQ ttProductionAnalysis.custNo
                 NO-ERROR.
            IF AVAILABLE cust THEN
            ttProductionAnalysis.custName = cust.name.
        END. /* avail job */
        IF ttProductionAnalysis.runActHr EQ 0 THEN ttProductionAnalysis.runStdHr = 0.
        ASSIGN                                                                                         
            ttProductionAnalysis.mrEffPct  = (ttProductionAnalysis.mrStdHr  / ttProductionAnalysis.mrActHr)  * 100.00                                   
            ttProductionAnalysis.runEffPct = (ttProductionAnalysis.runStdHr / ttProductionAnalysis.runActHr) * 100.00                  
            ttProductionAnalysis.totStdHrs = ttProductionAnalysis.mrStdHr + ttProductionAnalysis.runStdHr
            ttProductionAnalysis.totActHrs = ttProductionAnalysis.mrActHr + ttProductionAnalysis.runActHr
            ttProductionAnalysis.totEffPct = (ttProductionAnalysis.totStdHrs / ttProductionAnalysis.totActHrs) * 100.00
            ttProductionAnalysis.dtHrsEff  = (ttProductionAnalysis.dtActHr / ttProductionAnalysis.totActHrs) * 100.00
            .
        IF ttProductionAnalysis.mrEffPct  EQ ? THEN ttProductionAnalysis.mrEffPct = 0.
        IF ttProductionAnalysis.runEffPct EQ ? THEN ttProductionAnalysis.runEffPct = 0.
        IF ttProductionAnalysis.totEffPct EQ ? THEN ttProductionAnalysis.totEffPct = 0.
        IF ttProductionAnalysis.dtHrsEff  EQ ? THEN ttProductionAnalysis.dtHrsEff = 0.
    
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company   EQ ipcCompany
               AND job-mch.job       EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no    EQ SUBSTRING(ttProductionAnalysis.jobNo,1,6)
               AND job-mch.job-no2   EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm       EQ ttProductionAnalysis.frm
               AND (job-mch.blank-no EQ ttProductionAnalysis.blankNo
                OR mch-srt.blank-no  EQ 0)
               AND job-mch.m-code    EQ ttProductionAnalysis.machine
               AND job-mch.pass      EQ ttProductionAnalysis.pass
             NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company   EQ ipcCompany
               AND job-mch.job       EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no    EQ SUBSTRING(ttProductionAnalysis.jobNo,1,6)
               AND job-mch.job-no2   EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm       EQ ttProductionAnalysis.frm
               AND (job-mch.blank-no EQ ttProductionAnalysis.blankNo
                OR mch-srt.blank-no  EQ 0)
               AND job-mch.m-code    EQ ttProductionAnalysis.machine
             NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company EQ ipcCompany
               AND job-mch.job     EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no  EQ SUBSTRING(ttProductionAnalysis.jobNo,1,6)
               AND job-mch.job-no2 EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm     EQ ttProductionAnalysis.frm
               AND job-mch.m-code  EQ ttProductionAnalysis.machine
               AND job-mch.speed   NE 0
             NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company EQ ipcCompany
               AND job-mch.job     EQ ttProductionAnalysis.xxJob
               AND job-mch.job-no  EQ SUBSTRING(ttProductionAnalysis.jobNo,1,6)
               AND job-mch.job-no2 EQ ttProductionAnalysis.jobNo2
               AND job-mch.frm     EQ ttProductionAnalysis.frm
               AND job-mch.m-code  EQ ttProductionAnalysis.machine
             NO-ERROR.
        IF AVAILABLE job-mch THEN
        ASSIGN
            ttProductionAnalysis.mrComp  = STRING(job-mch.mr-complete)
            ttProductionAnalysis.runComp = string(job-mch.run-complete)
            .
        ASSIGN
            ttProductionAnalysis.runWaste  = 0
            ttProductionAnalysis.mrWaste   = 0
            ttProductionAnalysis.totLabHrs = 0
            ttProductionAnalysis.crew      = 0
            .
        FOR EACH bMchAct NO-LOCK
            WHERE bMchAct.company   EQ ipcCompany
              AND bMchAct.dept      EQ ttProductionAnalysis.dept
              AND bMchAct.m-code    EQ ttProductionAnalysis.machine
              AND bMchAct.job       EQ ttProductionAnalysis.xxJob
              AND bMchAct.job-no    EQ SUBSTRING(ttProductionAnalysis.jobNo,1,6)
              AND bMchAct.job-no2   EQ ttProductionAnalysis.jobNo2
              AND bMchAct.frm       EQ ttProductionAnalysis.frm
              AND (bMchAct.blank-no EQ ttProductionAnalysis.blankNo
               OR mach.p-type       NE "B"
               OR bMchAct.blank-no  EQ 0)
              AND bMchAct.pass      EQ ttProductionAnalysis.pass
              AND bMchAct.op-date   GE ipdtStartOpDate
              AND bMchAct.op-date   LE ipdtEndOpDate
              AND bMchAct.shift     GE ipiStartShift
              AND bMchAct.shift     LE ipiEndShift
            :
            FIND FIRST job-code NO-LOCK
                 WHERE job-code.code EQ bMchAct.code
                 NO-ERROR.
            IF NOT AVAILABLE job-code THEN NEXT.
            IF job-code.cat EQ "RUN" THEN
            ttProductionAnalysis.runWaste = ttProductionAnalysis.runWaste + bMchAct.waste.
            ELSE IF job-code.cat EQ "MR" THEN
                 ttProductionAnalysis.mrWaste = ttProductionAnalysis.mrWaste + bMchAct.waste.
            ttProductionAnalysis.totLabHrs = ttProductionAnalysis.totLabHrs + (bMchAct.hours * bMchAct.crew).
        END. /* bMchAct */
        IF ttProductionAnalysis.runActHr EQ 0 THEN ttProductionAnalysis.runStdHr = 0.
        ttProductionAnalysis.mrEffPct = (ttProductionAnalysis.mrStdHr  / ttProductionAnalysis.mrActHr)  * 100.00.
        IF ttProductionAnalysis.mrEffPct EQ ? THEN ttProductionAnalysis.mrEffPct = 0.
        ttProductionAnalysis.runEffPct = (ttProductionAnalysis.runStdHr / ttProductionAnalysis.runActHr) * 100.00.
        IF ttProductionAnalysis.runEffPct EQ ? THEN ttProductionAnalysis.runEffPct = 0.
        ASSIGN
            ttProductionAnalysis.totStdHrs = ttProductionAnalysis.mrStdHr + ttProductionAnalysis.runStdHr
            ttProductionAnalysis.totActHrs = ttProductionAnalysis.mrActHr + ttProductionAnalysis.runActHr
            .
        ttProductionAnalysis.dtHrsEff = (ttProductionAnalysis.dtActHr / ttProductionAnalysis.totActHrs) * 100.00.
        IF ttProductionAnalysis.dtHrsEff EQ ? THEN ttProductionAnalysis.dtHrsEff = 0.
        IF ttProductionAnalysis.numberUp EQ 0 THEN ttProductionAnalysis.numberUp = 1.
        ASSIGN
            ttProductionAnalysis.piecesPerHr = (ttProductionAnalysis.pieces / (ttProductionAnalysis.mrActHr + ttProductionAnalysis.runActHr + ttProductionAnalysis.dtActHr))
            ttProductionAnalysis.msfPerHr    = (dCalcMSF / (ttProductionAnalysis.mrActHr + ttProductionAnalysis.runActHr + ttProductionAnalysis.dtActHr))
            ttProductionAnalysis.kicksPerHrs = (ttProductionAnalysis.piecesPerHr / ttProductionAnalysis.numberUp)
            ttProductionAnalysis.wastePct    = ((ttProductionAnalysis.mrWaste + ttProductionAnalysis.runWaste) / ( ttProductionAnalysis.pieces)) * 100
            ttProductionAnalysis.piecesManHr = (ttProductionAnalysis.pieces / ttProductionAnalysis.totLabHrs)
            ttProductionAnalysis.totActHrs   = ttProductionAnalysis.mrActHr + ttProductionAnalysis.runActHr + ttProductionAnalysis.dtActHr
            ttProductionAnalysis.crew        = ttProductionAnalysis.totLabHrs / ttProductionAnalysis.totActHrs
            .
        IF ttProductionAnalysis.mrActHr     EQ ? THEN ttProductionAnalysis.mrActHr     = 0.
        IF ttProductionAnalysis.runActHr    EQ ? THEN ttProductionAnalysis.runActHr    = 0.
        IF ttProductionAnalysis.dtActHr     EQ ? THEN ttProductionAnalysis.dtActHr     = 0.
        IF ttProductionAnalysis.totActHrs   EQ ? THEN ttProductionAnalysis.totActHrs   = 0.
        IF ttProductionAnalysis.totLabHrs   EQ ? THEN ttProductionAnalysis.totLabHrs   = 0.
        IF ttProductionAnalysis.pieces      EQ ? THEN ttProductionAnalysis.pieces      = 0.
        IF ttProductionAnalysis.piecesPerHr EQ ? THEN ttProductionAnalysis.piecesPerHr = 0.
        IF ttProductionAnalysis.piecesRunHr EQ ? THEN ttProductionAnalysis.piecesRunHr = 0.
        IF ttProductionAnalysis.kicksPerHrs EQ ? THEN ttProductionAnalysis.kicksPerHrs = 0.
        IF ttProductionAnalysis.piecesManHr EQ ? THEN ttProductionAnalysis.piecesManHr = 0.
        IF ttProductionAnalysis.sqFtPiece   EQ ? THEN ttProductionAnalysis.sqFtPiece   = 0.
        IF ttProductionAnalysis.totalMSF    EQ ? THEN ttProductionAnalysis.totalMSF    = 0.
        IF ttProductionAnalysis.msfPerHr    EQ ? THEN ttProductionAnalysis.msfPerHr    = 0.
        IF ttProductionAnalysis.totalTons   EQ ? THEN ttProductionAnalysis.totalTons   = 0.
        IF ttProductionAnalysis.mrWaste     EQ ? THEN ttProductionAnalysis.mrWaste     = 0.
        IF ttProductionAnalysis.runWaste    EQ ? THEN ttProductionAnalysis.runWaste    = 0.
        IF ttProductionAnalysis.totalWaste  EQ ? THEN ttProductionAnalysis.totalWaste  = 0.
        IF ttProductionAnalysis.wastePct    EQ ? THEN ttProductionAnalysis.wastePct    = 0.
        IF ttProductionAnalysis.crew        EQ ? THEN ttProductionAnalysis.crew        = 0.
        IF ttProductionAnalysis.qtyExpect   EQ ? THEN ttProductionAnalysis.qtyExpect   = 0.
        IF ttProductionAnalysis.mrStdHr     EQ ? THEN ttProductionAnalysis.mrStdHr     = 0.
        IF ttProductionAnalysis.mrEffPct    EQ ? THEN ttProductionAnalysis.mrEffPct    = 0.
        IF ttProductionAnalysis.runStdHr    EQ ? THEN ttProductionAnalysis.runStdHr    = 0.
        IF ttProductionAnalysis.runEffPct   EQ ? THEN ttProductionAnalysis.runEffPct   = 0.
        IF ttProductionAnalysis.dtHrsEff    EQ ? THEN ttProductionAnalysis.dtHrsEff    = 0.
        IF ttProductionAnalysis.totStdHrs   EQ ? THEN ttProductionAnalysis.totStdHrs   = 0.
        IF ttProductionAnalysis.totEffPct   EQ ? THEN ttProductionAnalysis.totEffPct   = 0.
        IF ttProductionAnalysis.xxTotMRHrs  EQ ? THEN ttProductionAnalysis.xxTotMRHrs  = 0.
        IF ttProductionAnalysis.xxTotRunHrs EQ ? THEN ttProductionAnalysis.xxTotRunHrs = 0.
    END. /* each ttProductionAnalysis */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pProRateMR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProRateMR Procedure 
PROCEDURE pProRateMR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bMchAct  FOR mch-act.
  DEFINE BUFFER bJobCode FOR job-code.


  FOR EACH bMchAct NO-LOCK
      WHERE bMchAct.company  EQ mch-act.company
        AND bMchAct.job      EQ mch-act.job
        AND bMchAct.job-no   EQ mch-act.job-no
        AND bMchAct.job-no2  EQ mch-act.job-no2
        AND bMchAct.m-code   EQ mch-act.m-code
        AND bMchAct.dept     EQ mch-act.dept
        AND bMchAct.pass     EQ mch-act.pass
        AND bMchAct.frm      EQ mch-act.frm
        AND bMchAct.blank-no EQ mch-act.blank-no,
      FIRST bJobCode NO-LOCK
      WHERE bJobCode.code EQ bMchAct.code
      :
      IF bJobCode.cat EQ "RUN" THEN
      ttProductionAnalysis.xxTotRunHrs = ttProductionAnalysis.xxTotRunHrs + bMchAct.hours.
      ELSE IF bJobCode.cat EQ "MR" THEN
           ttProductionAnalysis.xxTotMRHrs = ttProductionAnalysis.xxTotMRHrs + bMchAct.hours.
  END. /* each bmchact */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Machine Productivity.rpa */
        WHEN "machprod." THEN
        RETURN TEMP-TABLE ttMachineProductivity:HANDLE.
        /* Production Analysis.rpa */
        WHEN "r-prodlys." THEN
        RETURN TEMP-TABLE ttProductionAnalysis:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fMachineProductivity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fMachineProductivity Procedure 
FUNCTION fMachineProductivity RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Machine Productivity.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttProductionAnalysis.
    EMPTY TEMP-TABLE ttMachineProductivity.

    RUN pMachineProductivity (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttMachineProductivity:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fProductionAnalysis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fProductionAnalysis Procedure 
FUNCTION fProductionAnalysis RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Machine Transactions.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttProductionAnalysis.

    RUN pProductionAnalysis (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttProductionAnalysis:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

