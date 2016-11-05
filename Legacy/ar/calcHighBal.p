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

{ar/tt-arinq.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getHighBalDays) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHighBalDays Procedure 
FUNCTION getHighBalDays RETURNS INTEGER
  ( ipcCompany AS CHAR )  FORWARD.

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
RUN calculateHighBal.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-buildTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildTempTable Procedure 
PROCEDURE buildTempTable :
/*------------------------------------------------------------------------------
  Purpose: build tt-arinq for calculation of high balance    
  Parameters:  ipiDays - day range to calculate
               ipbf-cust - cust buffer
  Notes: code is similar to arinq/b-cusinq.w - AQ2   
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-cust FOR cust.

    /*var definitions for ar/ar-iact1.i-*/
    DEF VAR xsum        AS DEC NO-UNDO.
    DEF VAR v-open      AS LOG.
    DEF VAR t-check-no  AS CHAR.
    DEF VAR x-check-no  AS CHAR.
    DEF VAR t-credits   AS DEC.
    DEF VAR t-debits    AS DEC.
    DEF VAR t-balance   AS DEC.
    DEF VAR v-tot-due   AS DEC.
    DEF VAR v-pay-stat1 AS LOG.
    DEF VAR v-pay-stat2 AS CHAR.
    DEF VAR li-seq AS INT NO-UNDO.
    DEF VAR lv-cust-no LIKE ar-cashl.cust-no NO-UNDO.
    DEF VAR ll-valid AS LOG NO-UNDO.
    DEF VAR fi_fchk LIKE ar-cash.check-no NO-UNDO.
    DEF VAR fi_tchk LIKE ar-cash.check-no NO-UNDO.
    DEF VAR cocode LIKE ar-cash.company NO-UNDO.
    DEF VAR v-gltrans-desc AS CHAR NO-UNDO.
    DEFINE VARIABLE iDayRange AS INT NO-UNDO.

    FOR EACH tt-arinq:
        DELETE tt-arinq.
    END.
    
    ASSIGN
        cocode  = ipbf-cust.company
        v-open  = YES /*to match Account Balance calculation*/
        li-seq  = 0
        xsum    = 0
        fi_fchk = 0
        fi_tchk = 2147483647.

    iDayRange = getHighBalDays(cocode).
    IF iDayRange EQ 0 THEN iDayRange = 9999 .

    FIND FIRST cust WHERE RECID(cust) EQ RECID(ipbf-cust) NO-LOCK NO-ERROR.

    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company EQ ipbf-cust.company
        AND ar-inv.cust-no   EQ ipbf-cust.cust-no
        AND ar-inv.posted    EQ YES
        AND ar-inv.terms     NE "CASH"
        AND ar-inv.inv-date  GE (TODAY - iDayRange /*9999*/ ) /*THis is to match the limits on AQ2*/
        USE-INDEX ar-inv
        BY ar-inv.inv-date
        BY ar-inv.inv-no:

        {ar/ar-iact1.i 2} /*Option 2 creates running balance (ARINQ char val = Fibre)*/

    END.  /*each ar-inv record*/
        
    FOR EACH ar-cashl NO-LOCK
        WHERE ar-cashl.company  EQ ipbf-cust.company
        AND ar-cashl.posted     EQ YES
        AND ar-cashl.cust-no    EQ lv-cust-no
        AND ar-cashl.inv-no     EQ 0,      
        FIRST ar-cash NO-LOCK
            WHERE ar-cash.c-no  EQ ar-cashl.c-no
        BY ar-cash.check-date
        BY ar-cash.c-no:

        {ar/ar-iact2.i 2} /*Option 2 creates running balance (ARINQ char val = Fibre)*/

    END.    /* for each ar-cash record */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calculateHighBal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculateHighBal Procedure 
PROCEDURE calculateHighBal :
/*------------------------------------------------------------------------------
  Purpose: Calculate average days to pay for a given customer.
           If NK1 AGEDAYS is not zero or it isn't an incremental    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iDayRange AS INT NO-UNDO.
DEFINE VARIABLE dHighBal LIKE cust.hibal NO-UNDO.
DEFINE VARIABLE dtHighBalDate LIKE cust.hibal-date NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

FIND FIRST bf-cust WHERE ROWID(bf-cust) EQ ipCustRowId NO-LOCK NO-ERROR.
IF AVAIL bf-cust THEN DO:
    
    iDayRange = getHighBalDays(bf-cust.company).
    
    IF iDayRange NE 0 THEN DO: /*if 0, HighBal is already set*/
        RUN buildTempTable(BUFFER bf-cust).
        RUN getHighBalInfo(iDayRange, OUTPUT dHighBal, OUTPUT dtHighBalDate).
        
        FIND CURRENT bf-cust EXCLUSIVE-LOCK.
        ASSIGN bf-cust.hibal = dHighBal
               bf-cust.hibal-date = dtHighBalDate.

    END. /* range calculation */
    
    /*un-lock customer record and assign*/
    
END. /*avail bf-cust*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getHighBalInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getHighBalInfo Procedure 
PROCEDURE getHighBalInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipiDays AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opdHighBal AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdtHighBalDate AS DATE NO-UNDO.

/*temp table created with running balance*/
FOR EACH tt-arinq NO-LOCK USE-INDEX balance BY balance DESC:
    IF (TODAY - tt-arinq.tr-date) LE ipiDays THEN DO:
        ASSIGN
            opdHighBal = tt-arinq.balance
            opdtHighBalDate = tt-arinq.tr-date.
        LEAVE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getHighBalDays) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHighBalDays Procedure 
FUNCTION getHighBalDays RETURNS INTEGER
  ( ipcCompany AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Return NK1 AGEDAYS integer value 
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iReturn AS INTEGER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHAR NO-UNDO.
    DEFINE VARIABLE lFound AS LOG NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "HighBalDays", "I", NO, NO, "", "", 
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

