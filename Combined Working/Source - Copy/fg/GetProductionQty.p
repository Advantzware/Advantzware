&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : Returns the Production Qty for a Given Job & Item
    Purpose     :  Use History records or FG-Act to calculate

    Syntax      : Run GetProductionQty (job, job-no2, item, output qty).

    Description :

    Author(s)   : BV
    Created     : 09/25/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobNo LIKE job.job-no NO-UNDO.
DEFINE INPUT PARAMETER ipiJobNo2 LIKE job.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER ipcINo LIKE itemfg.i-no NO-UNDO.
DEFINE INPUT PARAMETER iplUseFGAct AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdQty LIKE fg-rdtlh.qty NO-UNDO.

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

IF iplUseFGAct THEN DO:
    FIND FIRST itemfg
        WHERE itemfg.company EQ ipcCompany
          AND itemfg.i-no EQ ipcINo
        NO-LOCK NO-ERROR.
    FIND FIRST job 
        WHERE job.company EQ ipcCompany
          AND job.job-no EQ ipcJobNo
          AND job.job-no2 EQ ipiJobNo2
    NO-LOCK NO-ERROR.
    IF AVAIL job AND AVAIL itemfg THEN DO:
        FOR EACH fg-act 
            FIELDS(qty)
            WHERE fg-act.company EQ job.company
              AND fg-act.job  EQ job.job
              AND fg-act.job-no EQ job.job-no
              AND fg-act.job-no2 EQ job.job-no2
              AND fg-act.i-no    EQ itemfg.i-no
              AND (IF itemfg.alloc = ? THEN fg-act.qty GT 0
               ELSE TRUE)
            NO-LOCK:
            
            opdQty = opdQty + fg-act.qty.

        END. /*each fg-act */
    END. /*avail job*/
END. /*use FG Act*/
ELSE DO:
    FOR EACH fg-rcpth 
        FIELDS(r-no rita-code)
        WHERE fg-rcpth.company EQ ipcCompany
          AND fg-rcpth.job-no EQ ipcJobNo
          AND fg-rcpth.job-no2 EQ ipiJobNo2
          AND fg-rcpth.i-no EQ ipcINo
          AND fg-rcpth.rita-code EQ 'R' 
        USE-INDEX job
        NO-LOCK,
        EACH fg-rdtlh FIELDS(qty)
        WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            NO-LOCK:
            
            opdQty = opdQty + fg-rdtlh.qty.

    END.  /*each fg history*/    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


