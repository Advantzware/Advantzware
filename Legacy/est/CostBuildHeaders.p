
/*------------------------------------------------------------------------
    File        : CostBuildHeaders.p
    Purpose     : 

    Syntax      :

    Description : Initializes the ttCostHeader records given

    Author(s)   : BV
    Created     : Wed Feb 07 09:21:04 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEst AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipriJob AS ROWID NO-UNDO.

{cec/print4.i SHARED SHARED}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST est NO-LOCK 
    WHERE ROWID(est) EQ ipriEst
    NO-ERROR.
FIND FIRST job NO-LOCK
    WHERE ROWID(job) EQ ipriJob
    NO-ERROR.
IF AVAILABLE est THEN 
DO:
    RUN pBuildMaster(BUFFER est, ipdQuantityMaster).
    RUN pBuildHeaders(BUFFER est, BUFFER job, ipdQuantityMaster).
    RUN pSetFormMasters.
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildHeaders:
    /*------------------------------------------------------------------------------
     Purpose: Builds the ttCostHeader TempTable based on the structure of the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL.

    DEFINE VARIABLE dtCalcTime   AS DATETIME.
    DEFINE VARIABLE cUserID      AS CHARACTER.
    DEFINE VARIABLE cRK          AS CHARACTER.
    DEFINE VARIABLE cFormRK      AS CHARACTER.
    DEFINE VARIABLE cSetRK       AS CHARACTER.
    DEFINE VARIABLE iFormCreated AS INTEGER.
    DEFINE VARIABLE cJobNo       AS CHARACTER.
    DEFINE VARIABLE iJobNo2      AS INTEGER.
    DEFINE VARIABLE lIsItem      AS LOGICAL.

    IF AVAILABLE job THEN 
        ASSIGN 
            cJobNo  = job.job-no
            iJobNo2 = job.job-no2
            .
    dtCalcTime = NOW.
    cUserID = USERID("nosweat").
    ASSIGN 
        cRK     = ""
        cFormRK = ""
        cSetRK  = ""
        .
    IF ipbf-est.est-type EQ 8 THEN 
    DO:
        /*create a separate header record for the combo/tandem estimate as a whole*/
        RUN pCreateHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
            "Combo/Tandem Master Header",
            ipdQty, 
            NO,  /*This is not an item, thus no job-hdr link*/
            cJobNo,
            iJobNo2,
            0, 
            0,  
            dtCalcTime, 
            cUserID, 
            cFormRK, 
            cSetRK,
            INPUT-OUTPUT cRK).
        ASSIGN  
            cSetRK = cRK  /*Set the "Set" RK for a summary total header*/
            .
    END.
    FOR EACH eb OF ipbf-est NO-LOCK
        BREAK BY eb.form-no  
        BY eb.blank-no DESCENDING:
        lIsItem = YES.
        FIND FIRST ef OF eb NO-LOCK NO-ERROR. /*Not available for Set Header Blank*/
        IF FIRST-OF(eb.form-no) AND eb.blank-no GT 1 AND  iFormCreated NE eb.form-no THEN /*Last Blank of Form*/
        DO:
            /*create a separate header record for the form as a whole - "Form Master"*/
            RUN pCreateHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
                "Combo Form #" + STRING(eb.form-no) + " Master",
                ipdQty, 
                NO,  /*This is not an item, thus no job-hdr link*/
                cJobNo,
                iJobNo2,
                eb.form-no, 
                0,  
                dtCalcTime, 
                cUserID, 
                cFormRK, 
                cSetRK,
                INPUT-OUTPUT cRK).
            ASSIGN  
                cFormRK      = cRK  /*Set the form RK for the next blanks of the form to the newly created form header rec_key*/
                iFormCreated = eb.form-no /*register it so as not to create another form header*/
                . 
        END.    
        IF cFormRK NE "" THEN 
        DO:
            /*create a separate header record for the blank item as a whole*/
            RUN pCreateHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
                "Form #" + STRING(eb.form-no) + " Blank #" + STRING(eb.blank-no) + " Item Header",
                ipdQty, 
                YES,  /*This will be an item header record to match job-hdr for item*/
                cJobNo,
                iJobNo2,
                eb.form-no, 
                eb.blank-no,
                dtCalcTime, 
                cUserID, 
                cFormRK, 
                cSetRK,
                INPUT-OUTPUT cRK).
            ASSIGN  
                lIsItem = NO.
        END.
        /*Create Rec_key for blank*/
        IF FIRST-OF(eb.blank-no) THEN 
            RUN pCreateHeader(BUFFER ipbf-est, BUFFER ef, BUFFER eb, 
                "Form #" + STRING(eb.form-no) + " Blank #" + STRING(eb.blank-no) + " Blank",
                ipdQty, 
                lIsItem,  /*Item by default, unless it is an factored combo item*/
                cJobNo,
                iJobNo2,
                eb.form-no, 
                eb.blank-no, 
                dtCalcTime, 
                cUserID, 
                cFormRK, 
                cSetRK,
                INPUT-OUTPUT cRK). 
        IF NOT AVAILABLE ef AND eb.form-no EQ 0 AND eb.blank-no EQ 0 THEN  /*Set Header Blank*/ 
            cSetRK = cRK.
        IF LAST-OF(eb.form-no) THEN
            ASSIGN 
                cFormRK      = ""
                iFormCreated = 0
                .
    END.     

END PROCEDURE.

PROCEDURE pBuildMaster:
/*------------------------------------------------------------------------------
 Purpose: Builds the master configuration variables for the cost calculation
 Notes: Holds CE Control Settings, NK1s
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-est FOR est.
DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.

FIND FIRST ce-ctrl NO-LOCK 
    WHERE ce-ctrl.company EQ ipbf-est.company
    NO-ERROR.
IF NOT AVAILABLE ce-ctrl THEN LEAVE.
 
CREATE ttCostMaster.
ASSIGN 
    ttCostMaster.company = ce-ctrl.company
    ttCostMaster.estimateNo = ipbf-est.est-no
    .



END PROCEDURE.

PROCEDURE pCreateHeader:
    /*------------------------------------------------------------------------------
     Purpose: Encapsulates the Creation of the Header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE PARAMETER BUFFER ipbf-ef  FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb  FOR eb.
    DEFINE INPUT PARAMETER cDescription AS CHARACTER.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL.
    DEFINE INPUT PARAMETER iplIsItem AS LOGICAL.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER.
    DEFINE INPUT PARAMETER ipiForm AS INTEGER. 
    DEFINE INPUT PARAMETER ipiBlank AS INTEGER.
    DEFINE INPUT PARAMETER ipdtTimeStamp AS DATETIME.
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER.
    DEFINE INPUT PARAMETER ipcFormRK AS CHARACTER.
    DEFINE INPUT PARAMETER ipcSetRK AS CHARACTER.
    DEFINE INPUT-OUTPUT PARAMETER iopcRK AS CHARACTER. 


    CREATE ttCostHeader.
    iopcRK = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999").
    ASSIGN 
        ttCostHeader.company           = ipbf-est.company
        ttCostHeader.headerDescription = cDescription
        ttCostHeader.isItem            = iplIsItem
        ttCostHeader.rec_key           = iopcRK
        ttCostHeader.rec_keyParentForm = IF ipcFormRK NE "" THEN ipcFormRK ELSE ttCostHeader.rec_key
        ttCostHeader.rec_keyParentSet  = IF ipcSetRK NE "" THEN ipcSetRK ELSE ttCostHeader.rec_key
        ttCostHeader.calculationTime   = ipdtTimeStamp 
        ttCostHeader.calculatedBy      = ipcUserID 
        ttCostHeader.quantityMaster    = ipdQty
        ttCostHeader.jobNo             = ipcJobNo
        ttCostHeader.jobNo2            = ipiJobNo2
        ttCostHeader.formNo            = ipiForm
        ttCostHeader.blankNo           = ipiBlank
        ttCostHeader.estimateNo        = ipbf-est.est-no
        ttCostHeader.industry          = IF ipbf-est.est-type < 5 THEN 1 ELSE 2
        ttCostHeader.estimateType      = ipbf-est.est-type
        ttCostHeader.factorForm        = 1
        ttCostHeader.factorSet         = 1
        .
    
    IF AVAILABLE ipbf-ef THEN 
        ASSIGN              
            ttCostHeader.lengthGross = ipbf-ef.gsh-len 
            ttCostHeader.widthGross  = ipbf-ef.gsh-wid 
            ttCostHeader.depthGross  = ipbf-ef.gsh-dep 
            ttCostHeader.lengthNet   = ipbf-ef.nsh-len             
            ttCostHeader.widthNet    = ipbf-ef.nsh-wid 
            ttCostHeader.depthNet    = ipbf-ef.nsh-dep
            .
    IF AVAILABLE ipbf-eb THEN 
        ASSIGN      
            ttCostHeader.quantityPerSet  = ipbf-eb.quantityPerSet
            ttCostHeader.quantityYield   = ipbf-eb.yld-qty
            ttCostHeader.quantityRequest = ipbf-eb.bl-qty 
            ttCostHeader.lengthBlank     = ipbf-eb.t-len
            ttCostHeader.widthBlank      = ipbf-eb.t-wid
            ttCostHeader.blankSquareFeet = ipbf-eb.t-sqft
            ttCostHeader.customerPartID  = ipbf-eb.part-no
            ttCostHeader.fgItemID        = ipbf-eb.stock-no
            .

END PROCEDURE.

PROCEDURE pSetFormMasters:
/*------------------------------------------------------------------------------
 Purpose:  For each header that is a form master
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

