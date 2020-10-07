/*------------------------------------------------------------------------
    File        : RebuildJobs.p
    Purpose     : 

    Syntax      :

    Description : Rebuilds all open jobs with 0 production quantity

    Author(s)   : BV
    Created     : Fri Sep 25 15:20:15 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcBeginJob AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndJob AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiBeginJob2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiEndJob2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcRunMethods AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRunAllOpenJobs AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplProdQty AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opiCountTotals AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiCountProcess AS INTEGER NO-UNDO.

{sys/inc/var.i NEW SHARED} /* for jc-calc.p */
{custom/globdefs.i} /* for jc-calc.p */
ASSIGN 
    cocode = g_company
    locode = g_loc.
DEFINE NEW SHARED VARIABLE nufile AS LOG NO-UNDO.   /* for jc-calc.p */

DEFINE VARIABLE dQty     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lProdQty AS LOGICAL   NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */
   
FUNCTION fJobIsOK RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job FOR job) FORWARD.    

/* ***************************  Main Block  *************************** */
    MAIN-LOOP:   
    FOR EACH job NO-LOCK
            WHERE job.company EQ cocode
            AND job.opened EQ TRUE  /* Per Spec run for opened jobs */
            AND (job.job-no GE ipcBeginJob OR ipcRunAllOpenJobs EQ "ALL")
            AND (job.job-no LE ipcEndJob OR ipcRunAllOpenJobs EQ "ALL")
            AND (job.job-no2 GE ipiBeginJob2 OR ipcRunAllOpenJobs EQ "ALL")
            AND (job.job-no2 LE ipiEndJob2 OR ipcRunAllOpenJobs EQ "ALL")
            BREAK BY job.job-no
            BY job.job-no2
            :
            opiCountTotals = opiCountTotals + 1 .
            IF FIRST-OF(job.job-no2) THEN 
            DO:
                FIND FIRST job-hdr NO-LOCK
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2   
                    AND job-hdr.opened
                    NO-ERROR.
          
                IF NOT AVAILABLE job-hdr THEN 
                    NEXT MAIN-LOOP.
                                
                 IF iplProdQty THEN
                 DO:
                    lProdQty = fJobIsOK(BUFFER job).
                    IF NOT lProdQty THEN NEXT MAIN-LOOP. 
                     
                 END.
          
                STATUS DEFAULT job.job-no + "-" + STRING(job.job-no2, "99").               
                opiCountProcess = opiCountProcess + 1.
                IF ipcRunMethods EQ "Rebuild" THEN
                DO:
                    RUN jc/jc-calc.p (RECID(job), YES) NO-ERROR.                    
                END.                  
                ELSE 
                DO:                  
                    RUN jc/jc-calc.p (RECID(job), NO) NO-ERROR.          
                END.
                
            END. /* first of job-no2 */    
        END. /* Each job, each job-hdr */ 
    

/* ************************  Function Implementations ***************** */


FUNCTION fJobIsOK RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job FOR job):
    /*------------------------------------------------------------------------------
     Purpose:  Returns OK for all tests
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dProductionQuantity     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dProductionQuantityItem AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lJobIsOK                AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lBadItem                AS LOGICAL NO-UNDO.
    
    FOR EACH job-hdr NO-LOCK 
        WHERE job-hdr.company EQ ipbf-job.company
        AND job-hdr.job-no EQ ipbf-job.job-no
        AND job-hdr.job-no2 EQ ipbf-job.job-no2
        AND job-hdr.job EQ ipbf-job.job:
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no EQ job-hdr.i-no
            NO-ERROR.
        IF NOT AVAILABLE itemfg THEN 
        DO:
            lBadItem = YES.
            LEAVE. 
        END.
        RUN fg\GetProductionQty.p (job-hdr.company, job-hdr.job-no, job-hdr.job-no2, job-hdr.i-no, NO, OUTPUT dProductionQuantityItem).
        dProductionQuantity = dProductionQuantity + dProductionQuantityItem.
    END.
  
    lJobIsOK = NOT lBadITem AND dProductionQuantity EQ 0.

    RETURN 	lJobIsOK.

END FUNCTION.

