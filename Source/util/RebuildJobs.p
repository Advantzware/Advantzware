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
{sys/inc/var.i NEW SHARED} /* for jc-calc.p */
{custom/globdefs.i} /* for jc-calc.p */
ASSIGN 
    cocode = g_company
    locode = g_loc.
DEFINE NEW SHARED VARIABLE nufile AS LOG NO-UNDO.   /* for jc-calc.p */

DEFINE VARIABLE iJobCount AS INTEGER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fConfirm RETURNS LOGICAL PRIVATE
    (  ) FORWARD.

FUNCTION fJobIsOK RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job FOR job) FORWARD.


/* ***************************  Main Block  *************************** */
IF fConfirm() THEN 
    FOR EACH job NO-LOCK 
        WHERE job.company EQ g_company
        AND job.job-no NE ""
        AND job.create-date GE TODAY - 90
        AND job.opened:
        
        IF fJobIsOK(BUFFER job) THEN 
        DO:
            iJobCount = iJobCount + 1.
            RUN jc/jc-calc.p (RECID(job), YES).
        END.
    END.
MESSAGE "Process Completed." SKIP 
    "Jobs Rebuilt: " iJobCount
    VIEW-AS ALERT-BOX.

/* ************************  Function Implementations ***************** */


FUNCTION fConfirm RETURNS LOGICAL PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:  Prompts user to confirm to continue
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lConfirm AS LOGICAL NO-UNDO.

    MESSAGE "This utility will update all open jobs that have 0 production quantity." SKIP 
        "It will take several minutes (or hours) to complete." SKIP (1)
        "Continue?"
        VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lConfirm .
    RETURN lConfirm.
		
END FUNCTION.

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

