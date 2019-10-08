
/*------------------------------------------------------------------------
    File        : CostResetHeaders.p
    Purpose     : 

    Syntax      :

    Description : Resets the Cost Headers - Done on Purge Calculations and on Job Calculation

    Author(s)   : BV
    Created     : Wed Feb 07 23:00:22 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEst AS ROWID NO-UNDO.
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
RUN pDeleteHeaders(BUFFER est, BUFFER job).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pDeleteHeaders:
/*------------------------------------------------------------------------------
 Purpose: Deletes Headers for est/job scope
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-est FOR est.
DEFINE PARAMETER BUFFER ipbf-job FOR job.

DEFINE VARIABLE cJobNo AS CHARACTER.
DEFINE VARIABLE iJobNo2 AS INTEGER.

IF AVAILABLE ipbf-est THEN DO:
    IF AVAILABLE ipbf-job THEN 
        ASSIGN 
            cJobNo = ipbf-job.job-no
            iJobNo2 = ipbf-job.job-no2
            . 
    FOR EACH ttCostHeader EXCLUSIVE-LOCK
        WHERE ttCostHeader.company EQ ipbf-est.company 
        AND ttCostHeader.estimateNo EQ ipbf-est.est-no
        AND ttCostHeader.jobNo EQ cJobNo
        AND ttCostHeader.jobNo2 EQ iJobNo2
        :
        DELETE ttCostHeader. 
    END.
    FOR EACH costHeader EXCLUSIVE-LOCK 
        WHERE costHeader.company EQ ipbf-est.company
        AND costHeader.estimateNo EQ ipbf-est.est-no
        AND costHeader.jobNo EQ cJobNo
        AND costHeader.jobNo2 EQ iJobNo2
        :
        DELETE costHeader.
    END.
END.
ELSE 
    EMPTY TEMP-TABLE ttCostHeader.
    


END PROCEDURE.


