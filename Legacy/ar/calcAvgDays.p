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
DEFINE INPUT PARAMETER ipCustRowID AS ROWID.

DEFINE INPUT PARAMETER iplIncremental AS LOG.  
/*YES for simply adding one more day to the average, NO to run full ar-inv calc*/

DEFINE INPUT PARAMETER ipiIncrementDays AS INT.
/*if iplIncremental is YES, this is the # of Days to add to the average*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getAgeDays) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgeDays Procedure 
FUNCTION getAgeDays RETURNS INTEGER
  ( ipiCompany AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAgeDaysExcludeFC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgeDaysExcludeFC Procedure 
FUNCTION getAgeDaysExcludeFC RETURNS LOGICAL
( ipiCompany AS CHAR )  FORWARD.

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
RUN calculateAvgDays.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-calculateAvgDays) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculateAvgDays Procedure 
PROCEDURE calculateAvgDays :
/*------------------------------------------------------------------------------
  Purpose: Calculate average days to pay for a given customer.
           If NK1 AGEDAYS is not zero or it isn't an incremental    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iDays AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE iInvCount AS INT INIT 0 NO-UNDO.
DEFINE VARIABLE iDayRange AS INT NO-UNDO.
DEFINE VARIABLE dAvgDays LIKE cust.avg-pay NO-UNDO.
DEFINE VARIABLE lExcludeFC AS LOG NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.
DEFINE BUFFER bf-ar-inv FOR ar-inv.

FIND FIRST bf-cust WHERE ROWID(bf-cust) EQ ipCustRowId NO-LOCK NO-ERROR.
IF AVAIL bf-cust THEN DO:
    
    iDayRange = getAgeDays(bf-cust.company).
    lExcludeFC = getAgeDaysExcludeFC(bf-cust.company).
    
    IF iDayRange EQ 0 AND iplIncremental THEN /*increment average*/
        
        dAvgDays = ((bf-cust.num-inv * bf-cust.avg-pay) +
                    ipiIncrementDays) /
                   (bf-cust.num-inv + 1).
    
    ELSE DO: /*calculate average across all invoices*/
        ASSIGN 
            iDays = 0
            iInvCount = 0.
        FOR EACH bf-ar-inv
          WHERE   bf-ar-inv.company   EQ bf-cust.company
            AND bf-ar-inv.posted    EQ YES
            AND bf-ar-inv.cust-no   EQ bf-cust.cust-no
            AND bf-ar-inv.due       LE 0
            AND bf-ar-inv.pay-date  NE ?
            AND (iDayRange EQ 0 OR (iDayRange NE 0 AND bf-ar-inv.pay-date  GE (TODAY - iDayRange)))
            AND (NOT lExcludeFC OR (lExcludeFC AND bf-ar-inv.terms NE "FCHG"))
          USE-INDEX posted-due NO-LOCK:
         
            ASSIGN
                iDays = iDays + (bf-ar-inv.pay-date - bf-ar-inv.inv-date)
                iInvCount = iInvCount + 1.

        END. /*each bf-ar-inv*/
     
        dAvgDays = iDays / iInvCount.

    END. /*else (Range Calculation)*/
    
    IF dAvgDays LT 1 OR dAvgDays EQ ? THEN
        dAvgDays = 1.
    
    /*un-lock customer record and assign*/
    FIND CURRENT bf-cust EXCLUSIVE-LOCK.
    bf-cust.avg-pay = dAvgDays.

END. /*avail bf-cust*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getAgeDays) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgeDays Procedure 
FUNCTION getAgeDays RETURNS INTEGER
  ( ipiCompany AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Return NK1 AGEDAYS integer value 
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReturn AS INTEGER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHAR NO-UNDO.
    DEFINE VARIABLE lFound AS LOG NO-UNDO.

    RUN sys/ref/nk1look.p (ipiCompany, "AGEDAYS", "I", NO, NO, "", "", 
                          OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN
        iReturn = INT(cReturn).
    ELSE
        iReturn = 0.

    RETURN iReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAgeDaysExcludeFC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgeDaysExcludeFC Procedure 
FUNCTION getAgeDaysExcludeFC RETURNS LOGICAL
( ipiCompany AS CHAR ) :
/*------------------------------------------------------------------------------
Purpose:  Return NK1 AGEDAYS integer value 
  Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cReturn AS CHAR NO-UNDO.
  DEFINE VARIABLE lFound AS LOG NO-UNDO.

  RUN sys/ref/nk1look.p (ipiCompany, "AGEDAYS", "C", NO, NO, "", "", 
                        OUTPUT cReturn, OUTPUT lFound).
  IF cReturn = "Exclude Finance Charges" THEN
      lReturn = YES.
  ELSE
      lReturn = NO.
 
  RETURN lReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

