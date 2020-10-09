
/*------------------------------------------------------------------------
    File        : FGLabelProcs.p
    Purpose     : 

    Syntax      :

    Description : Procedures for Generating the data file and executing a print for FG Labels

    Author(s)   : BV
    Created     : Wed Mar 27 14:59:24 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER ipcJobStart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobEnd AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiJob2Start AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiJob2End AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItemIDStart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItemIDEnd AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplPrompt AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER iplAutoPrint AS LOGICAL NO-UNDO.

{fg/ttFGLabels.i}

DEFINE VARIABLE hdOutputProcs AS HANDLE.
DEFINE VARIABLE gcPathDataFileDefault AS CHARACTER INITIAL "C:\BA\LABEL".
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetFGCustPart RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcFGItemID AS CHARACTER,
    ipcCustomerID AS CHARACTER,
    ipcDefault AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

RUN pBuildAndPrintFGLabels(ipcCompany, ipcJobStart, ipcJobEnd, ipiJob2Start, ipiJob2End, ipcFGItemIDStart, ipcFGItemIDEnd, iplPrompt, iplAutoPrint).

DELETE OBJECT hdOutputProcs.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildAndPrintFGLabels PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Main Function to call
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcJobStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob2Start AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob2End AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemIDStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemIDEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrompt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplAutoPrint AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cPathDataFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPathTemplate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCopies       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountRecords AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountLabels  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.

    RUN pGetSettings(ipcCompany, OUTPUT cPathDataFile, OUTPUT cPathTemplate, OUTPUT iCopies).
    RUN pBuildFGLabels(ipcCompany, ipcJobStart, ipcJobEnd, ipiJob2Start, ipiJob2End, ipcFGItemIDStart, ipcFGItemIDEnd, iCopies, OUTPUT iCountRecords, OUTPUT iCountLabels).
    IF iplPrompt THEN 
    DO:
        IF iplAutoPrint THEN 
            MESSAGE "Are you sure you want to print labels for the jobs and items in your selected range?" SKIP
                "This process will generate " STRING(iCountLabels) " printed labels for the " STRING(iCountRecords) " jobs/items in your range." SKIP(2)
                "Data file: " cPathDataFile SKIP 
                "Template file: " cPathTemplate
                VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Continue with print?" UPDATE lContinue.
        ELSE 
            MESSAGE "Are you sure you want to generate the data file for the jobs and items in your selected range?" SKIP
                "This process will generate a data file with " STRING(iCountLabels) " records for the " STRING(iCountRecords) " jobs/items in your range." SKIP 
                "Note:  This will not send a print job to an external label program." SKIP(2)
                "     Data file: " cPathDataFile
                VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Continue generating data file?" UPDATE lContinue.
    END.    
    IF lContinue THEN 
    DO:
        RUN Output_TempTableToCSV IN hdOutputProcs (
            INPUT TEMP-TABLE ttFGLabel:HANDLE, 
            INPUT cPathDataFile, 
            INPUT YES,
            INPUT FALSE, /* Auto increment File name */
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
        
        IF iplAutoPrint THEN 
        DO:
            IF SEARCH(cPathTemplate) NE ? THEN DO:
                RUN PrintLabelMatrixFile IN hdOutputProcs(ipcCompany, cPathTemplate, "FGLabel").
                IF iplPrompt THEN 
                    MESSAGE "Label Matrix print process has been started." SKIP
                        "This process will generate " STRING(iCountLabels) " printed labels for the " STRING(iCountRecords) " jobs/items in your range." SKIP(2)
                        "     Data file: " cPathDataFile SKIP 
                        "     Template file: " cPathTemplate
                        VIEW-AS ALERT-BOX TITLE "Print Confirmation".
            END.
            ELSE IF iplPrompt THEN 
                MESSAGE "Data file has been generated but system is unable to locate template file to print." SKIP (2)
                        "     Data file: " cPathDataFile SKIP
                        "     Template file: " cPathTemplate
                VIEW-AS ALERT-BOX TITLE "Invalid Template".
        END.
        ELSE 
            IF iplPrompt THEN 
                MESSAGE "Data file has been generated. " SKIP 
                        "     Data file: " cPathDataFile
                    VIEW-AS ALERT-BOX TITLE "Data File Confirmation" . 
    END. 
    
END PROCEDURE.

PROCEDURE pBuildFGLabels PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given range parameters, creates a temp-table of data for output to a CSV
     txt file
     
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcJobStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob2Start AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob2End AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemIDStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemIDEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCopies AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiRecords AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiLabels AS INTEGER NO-UNDO.

    DEFINE VARIABLE iCopy         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustomerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPart AS CHARACTER NO-UNDO.
 
    IF ipiCopies EQ 0 THEN ipiCopies = 1.
    IF ipcJobEnd EQ "" THEN 
        IF ipcJobStart NE "" THEN
            ipcJobEnd = ipcJobStart. 
        ELSE 
            ipcJobEnd = CHR(254).
    IF ipcFGItemIDEnd EQ "" THEN 
        IF ipcFGItemIDStart NE "" THEN
            ipcFGItemIDEnd = ipcFGItemIDStart. 
        ELSE 
            ipcFGItemIDEnd = CHR(254).
    ASSIGN 
        opiRecords = 0
        opiLabels  = 0
        .     
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ ipcCompany
        AND job-hdr.job-no GE ipcJobStart
        AND job-hdr.job-no LE ipcJobEnd
        AND job-hdr.job-no2 GE ipiJob2Start
        AND job-hdr.job-no2 LE ipiJob2End
        AND job-hdr.i-no GE ipcFGItemIDStart
        AND job-hdr.i-no LE ipcFGItemIDEnd
        ,
        FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no EQ job-hdr.i-no:
    
        ASSIGN 
            cCustomerID   = ""
            cCustomerPart = ""
            opiRecords    = opiRecords + 1
            .
        IF job-hdr.cust-no NE "" THEN 
            cCustomerID = job-hdr.cust-no.
        ELSE 
            cCustomerID = itemfg.cust-no.
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ itemfg.company
            AND cust.cust-no EQ cCustomerID
            NO-ERROR.
        cCustomerPart = fGetFGCustPart(ipcCompany, itemfg.i-no, cCustomerID, itemfg.part-no). 
        DO iCopy = 1 TO ipiCopies:
            opiLabels = opiLabels + 1.
            CREATE ttFGLabel.
            ASSIGN 
                ttFGLabel.cCAD              = itemfg.cad-no
                ttFGLabel.cCompany          = itemfg.company
                ttFGLabel.cCustomerID       = cCustomerID
                ttFGLabel.cCustomerName     = IF AVAILABLE cust THEN cust.name ELSE ""
                ttFGLabel.cDie              = itemfg.die-no
                ttFGLabel.cEstimate         = itemfg.est-no
                ttFGLabel.cFGCustPart       = cCustomerPart
                ttFGLabel.cFGItemDesc1      = itemfg.part-dscr1
                ttFGLabel.cFGItemDesc2      = itemfg.part-dscr2
                ttFGLabel.cFGItemDesc3      = itemfg.part-dscr3
                ttFGLabel.cFGItemID         = itemfg.i-no
                ttFGLabel.cFGItemName       = itemfg.i-name
                ttFGLabel.iForm             = job-hdr.frm
                ttFGLabel.iBlank            = job-hdr.blank-no
                ttFGLabel.cJobNumber        = job-hdr.job-no
                ttFGLabel.iJobID2           = job-hdr.job-no2
                ttFGLabel.cJobID            = job-hdr.job-no + "-" + STRING(job-hdr.job-no2, "99")
                ttFGLabel.cJobIDTrimmed     = TRIM(ttFGLabel.cJobID)
                ttFGLabel.cJobIDFull        = ttFGLabel.cJobID + "." + STRING(job-hdr.frm,"99") + "." + STRING(job-hdr.blank-no,"99")
                ttFGLabel.cJobIDFullTrimmed = TRIM(ttFGLabel.cJobIDFull) 
                .
        END.    
    END.
END PROCEDURE.

PROCEDURE pGetSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the key NK1 settings for printing FG Labels
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPathDataFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPathTemplate AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCopies AS INTEGER NO-UNDO.

    DEFINE VARIABLE cReturn AS CHARACTER.
    DEFINE VARIABLE lFound  AS LOGICAL.

    RUN sys/ref/nk1look.p (ipcCompany, "FGLabel", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound). 
    IF lFound THEN 
        opcPathTemplate = cReturn.

    RUN sys/ref/nk1look.p (ipcCompany, "FGLabel", "I", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound). 
    IF lFound THEN 
        opiCopies = INTEGER(cReturn).
    IF opiCopies EQ 0 THEN opiCopies = 1.
    
    RUN GetBarDirFilePath IN hdOutputProcs (ipcCompany, "FGLabel", OUTPUT opcPathDataFile).
    IF opcPathDataFile EQ "" THEN opcPathDataFile = gcPathDataFileDefault.
        

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetFGCustPart RETURNS CHARACTER PRIVATE
    ( ipcCompany AS CHARACTER, ipcFGItemID AS CHARACTER, ipcCustomerID AS CHARACTER, ipcDefault AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the appropriate customer part # given a FG Item and customer ID
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cCustPart AS CHARACTER NO-UNDO.
    
    FIND FIRST cust-part NO-LOCK 
        WHERE cust-part.company EQ ipcCompany
        AND cust-part.i-no EQ ipcFGItemID
        AND cust-part.cust-no EQ ipcCustomerID
        NO-ERROR.
    IF AVAILABLE cust-part THEN 
        cCustPart = cust-part.part-no.
    ELSE 
        cCustPart = ipcDefault.
    
    RETURN cCustPart.
	
END FUNCTION.

