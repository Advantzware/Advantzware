&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : BuildDefaultPreps.p   
    Purpose     :  Add all default preps to a given estimate

    Syntax      : Run est/BuildDefaultPreps.p (buffer est,
                                               buffer ef).

    Description :

    Author(s)   :  BV   
    Created     :  9/29/14
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

DEFINE PARAMETER BUFFER ipbf-est FOR est.
DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
DEFINE INPUT PARAMETER ipiForm LIKE eb.form-no NO-UNDO.
DEFINE INPUT PARAMETER ipiBlank LIKE eb.blank-no NO-UNDO.

DEFINE VARIABLE hPrepProcs AS HANDLE NO-UNDO.
RUN system/PrepProcs.p PERSISTENT SET hPrepProcs.


DEFINE BUFFER bf-eb FOR eb.
DEFINE BUFFER bf-estMisc FOR estMisc.
DEFINE BUFFER bf-estMiscControl FOR estMiscControl.

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

DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
DEFINE BUFFER bf-prep FOR prep.
DEFINE BUFFER bf-est-prep FOR est-prep.

FOR EACH bf-est-prep
    WHERE bf-est-prep.company EQ ipbf-est.company 
      AND bf-est-prep.est-no  EQ ipbf-est.est-no                      
    USE-INDEX est-qty NO-LOCK
    BY bf-est-prep.line DESC:

    LEAVE.

END.
iCount = (IF AVAIL bf-est-prep THEN bf-est-prep.line ELSE 0) + 1.

FOR EACH bf-prep 
    WHERE bf-prep.company EQ ipbf-est.company 
      AND bf-prep.dfault EQ YES 
    NO-LOCK:
      
    CREATE est-prep.
    ASSIGN 
        est-prep.e-num  = ipbf-est.e-num
        est-prep.company = bf-prep.company
        est-prep.est-no = ipbf-est.est-no
        est-prep.line   = iCount
        est-prep.s-num  = IF ipiForm GT 0 THEN ipiForm ELSE 1
        est-prep.b-num  = ipiBlank
        est-prep.code   = bf-prep.code
        est-prep.dscr   = bf-prep.dscr
        est-prep.cost   = bf-prep.cost
        est-prep.ml     = bf-prep.ml
        est-prep.simon  = bf-prep.simon
        est-prep.mkup   = bf-prep.mkup
        est-prep.amtz   = bf-prep.amtz
        est-prep.mat-type = bf-prep.mat-type
        est-prep.spare-dec-1 = bf-prep.price
        .
        RUN est/GetPrepQty.p(INPUT ROWID(ipbf-est),
                         INPUT est-prep.mat-type,
                         INPUT est-prep.s-num,
                         OUTPUT est-prep.qty).
                         
        iCount = iCount + 1.
        RUN pDisplayPrepDisposedMessage IN hPrepProcs (ROWID(bf-prep)).
END.


FIND FIRST bf-eb NO-LOCK
    WHERE bf-eb.company  EQ ipbf-est.company
    AND bf-eb.est-no   EQ ipbf-est.est-no
    AND bf-eb.form-no  EQ MAX(ipiForm,1)
    AND bf-eb.blank-no EQ MAX(ipiBlank,1) NO-ERROR.
          
IF AVAILABLE bf-eb THEN
    RUN Prep_ValidateAndDeletePlatePrep IN hPrepProcs (ROWID(bf-eb)).      

FOR EACH bf-estMiscControl NO-LOCK
    WHERE bf-estMiscControl.company EQ ipbf-est.company:
    FIND FIRST bf-estMisc NO-LOCK
         WHERE bf-estMisc.company       EQ bf-estMiscControl.company
           AND bf-estMisc.estimateNo    EQ ipbf-est.est-no
           AND bf-estMisc.estCostCalcBy EQ bf-estMiscControl.estCostCalcBy
           AND bf-estMisc.estCostCalcOn EQ bf-estMiscControl.estCostCalcOn
           AND bf-estMisc.estCostCalcTo EQ bf-estMiscControl.estCostCalcTo
         NO-ERROR.
    IF NOT AVAILABLE bf-estMisc THEN DO:
        CREATE bf-estMisc.
        BUFFER-COPY bf-estMiscControl EXCEPT rec_key TO bf-estMisc.
        
        ASSIGN
            bf-estMisc.estimateNo = ipbf-est.est-no
            .
    END.
END.

IF VALID-HANDLE(hPrepProcs) THEN 
 DELETE OBJECT hPrepProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


