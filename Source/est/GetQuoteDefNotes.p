&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : est/GetQuoteDefNotes.p
    Purpose     : Gets Quote Default Quote Notes

    Syntax      : RUN est/GetQuoteDefNotes.p(INPUT cocode,
                                             OUTPUT cNotes).
                                           
    Description :  Returns notes array from either first quote in system or 
                 by quote number defined by N-K QuoteNotes

    Author(s)   : BV
    Created     : 02/15
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcNotes LIKE quotehd.comment   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetNKQuoteNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetNKQuoteNotes Procedure 
FUNCTION GetNKQuoteNotes RETURNS INTEGER
  ( ipcCompany AS CHARACTER )  FORWARD.

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
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
    RUN GetQuoteNotes (INPUT ipcCompany,
                       OUTPUT opcNotes).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetQuoteNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQuoteNotes Procedure 
PROCEDURE GetQuoteNotes :
/*------------------------------------------------------------------------------
  Purpose:   Main procedure for returning Quote Notes  
  Parameters:  input company, output notes array of 5
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcNotes LIKE quotehd.comment   NO-UNDO.

DEFINE VARIABLE iQuoteNumber AS INTEGER     NO-UNDO.
DEFINE VARIABLE lQuoteFound AS LOGICAL     NO-UNDO.

iQuoteNumber = GetNKQuoteNotes(INPUT ipcCompany).
IF iQuoteNumber NE 0 THEN
    FIND FIRST quotehd 
        WHERE quotehd.company EQ ipcCompany
          AND quotehd.q-no EQ iQuoteNumber
        NO-LOCK NO-ERROR.
IF NOT AVAIL quotehd THEN
    FIND FIRST quotehd 
        WHERE quotehd.company EQ ipcCompany
        USE-INDEX q-no
        NO-LOCK NO-ERROR.
IF AVAIL quotehd THEN
    ASSIGN
        opcNotes[1] = quotehd.comment[1]
        opcNotes[2] = quotehd.comment[2]
        opcNotes[3] = quotehd.comment[3]
        opcNotes[4] = quotehd.comment[4]
        opcNotes[5] = quotehd.comment[5]
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetNKQuoteNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetNKQuoteNotes Procedure 
FUNCTION GetNKQuoteNotes RETURNS INTEGER
  ( ipcCompany AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the quote number of the quote specified in quote notes
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iQuoteNum AS INTEGER     NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

RUN sys/ref/NK1Look.p (INPUT ipcCompany,
                       INPUT "QuoteNotes",
                       INPUT "L",
                       INPUT NO, 
                       INPUT NO, 
                       INPUT "", 
                       INPUT "", 
                       OUTPUT cReturn, 
                       OUTPUT lFound).
IF lFound AND LOGICAL(cReturn) THEN DO:
    RUN sys/ref/NK1Look.p (INPUT ipcCompany,
                       INPUT "QuoteNotes",
                       INPUT "I",
                       INPUT NO, 
                       INPUT NO, 
                       INPUT "", 
                       INPUT "", 
                       OUTPUT cReturn, 
                       OUTPUT lFound).
    IF lFound THEN
        iQuoteNum = INTEGER(cReturn).

END.
ELSE
    iQuoteNum = 0.

RETURN iQuoteNum.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

