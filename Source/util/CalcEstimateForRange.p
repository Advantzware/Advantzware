
/*------------------------------------------------------------------------
    File        : CalcEstimateForRange.p
    Purpose     : Process to calculate the Estimate

    Syntax      :

    Description : Used  in Import Utility.  It's working for a range of Estimate num

    Author(s)   : Sakshi SIngh
    Created     : Tue Sep 21 09:50:56 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiEstType       AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEstRangeStart AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEstRangeEnd   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcErrorMessage  AS CHARACTER NO-UNDO.

DEFINE VARIABLE lFound              AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPurge              AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdEstimateCalcProcs AS HANDLE  NO-UNDO.

DEFINE BUFFER bf-est FOR est.
DEFINE BUFFER bf-eb  FOR eb.


RUN est\EstimateCalcProcs.p PERSISTENT SET hdEstimateCalcProcs.

FOR EACH bf-est NO-LOCK
    WHERE bf-est.company EQ ipcCompany
    AND bf-est.est-type GE ipiEstType         
    AND bf-est.est-no GE ipcEstRangeStart
    AND bf-est.est-no LE ipcEstRangeEnd,
    FIRST bf-eb NO-LOCK
    WHERE bf-eb.company EQ ipcCompany
    AND bf-eb.form-no NE 0
    AND bf-eb.est-no EQ bf-est.est-no:
    
    lFound = YES. 
    RUN CalculateEstimate IN hdEstimateCalcProcs (ipcCompany, bf-est.est-no, lPurge).
END.

IF NOT lFound THEN
    opcErrorMessage = "Error calculating the Estimate. Record not found for given range.".
    
IF VALID-HANDLE(hdEstimateCalcProcs) THEN
    DELETE OBJECT hdEstimateCalcProcs.
        
