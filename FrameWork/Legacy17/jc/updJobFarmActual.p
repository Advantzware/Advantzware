&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT PARAMETER iprJobRow AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipcIno AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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

RUN updateJobFarm.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-updateJobFarm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateJobFarm Procedure 
PROCEDURE updateJobFarm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR dTotCost AS DEC NO-UNDO.
DEF VAR dTotIssue AS DEC NO-UNDO.
DEF VAR dTotExt AS DEC NO-UNDO.
DEF VAR cCostM AS DEC NO-UNDO.
DEF VAR dCostM AS DEC NO-UNDO.

FIND FIRST job WHERE ROWID(job) EQ iprJobRow NO-LOCK NO-ERROR.

FOR EACH job-farm WHERE job-farm.company EQ job.company   
      AND  job-farm.job EQ job.job
      AND (job-farm.i-no EQ ipcINo OR ipcINo = "")
    EXCLUSIVE-LOCK.
    /* Get cost, qty totals */
    ASSIGN dTotCost  = 0
           dTotIssue = 0
           dTotExt   = 0.
    FOR EACH job-farm-rctd WHERE job-farm-rctd.job-no EQ job-farm.job-no
          AND job-farm-rctd.job-no2 EQ job-farm.job-no2
          AND job-farm-rctd.company EQ job-farm.company
          AND job-farm-rctd.i-no    EQ job-farm.i-no
          AND job-farm-rctd.rita-code EQ "F"
        NO-LOCK.

    dCostM = job-farm-rctd.std-cost.
    IF job-farm-rctd.cost-uom NE "M" THEN
      run sys/ref/convcuom.p(job-farm-rctd.cost-uom, "M", 0, 0, 0, 0,
                             job-farm-rctd.std-cost, output dCostM). 
        ASSIGN
        dTotCost = dTotCost + dCostM
        dTotIssue = dTotIssue + job-farm-rctd.t-qty
        dTotExt   = dTotExt   + (dCostM * job-farm-rctd.t-qty / 1000).

    END.
    /* assign job-farm issued qty, total actual cost, actual cost /m */
    /* Qty Issued */
    job-farm.qty-iss = dTotIssue.
    /* Total Actual Cost */
    job-farm.act-tot-cost = dTotExt.
    /* Actual Cost /m */
    job-farm.act-cost = dTotExt / (dTotIssue /  1000).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

