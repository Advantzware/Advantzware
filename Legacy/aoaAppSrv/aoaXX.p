&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaXX.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Template.rpa */
DEFINE TEMP-TABLE ttTemplate NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD field1 AS CHARACTER LABEL "Character Field"
    FIELD field2 AS INTEGER   LABEL "Integer Field"
    FIELD field3 AS DECIMAL   LABEL "Decimal Field"
    FIELD field4 AS DATE      LABEL "Date Field"      FORMAT "99/99/9999"
    FIELD field5 AS LOGICAL   LABEL "Logical Field"
    .
{sys/ref/CustList.i NEW}
/* Template.rpa */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTemplate Procedure 
FUNCTION fTemplate RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Template.rpa
  Parameters:  Company, Use List?, Start Cust, End Cust, ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
              AND bCust.cust-no GE ipcStartCust
              AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTemplate Procedure 
PROCEDURE pTemplate :
/*------------------------------------------------------------------------------
  Purpose:     Template.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pTemplate.i}
    
    /* local variables */
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    /* subject business logic */
    DO idx = 1 TO 5:
        CREATE ttTemplate.
        ASSIGN
            ttTemplate.field1 = STRING(idx)
            ttTemplate.field2 = idx
            ttTemplate.field3 = idx * 1.1
            ttTemplate.field4 = TODAY + idx
            ttTemplate.field5 = idx MOD 2 EQ 0
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Template.rpa */
        WHEN "template." THEN
        RETURN TEMP-TABLE ttTemplate:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fTemplate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTemplate Procedure 
FUNCTION fTemplate RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Template.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttTemplate.

    RUN pTemplate (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttTemplate:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

