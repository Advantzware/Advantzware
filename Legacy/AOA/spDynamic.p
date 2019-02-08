&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : spDynamic.p
    Purpose     : super-procedures for dynamic subject/query/parameters

    Syntax      : AOA/spDynamic.p

    Description : Super Procedures used by Dynamic Subjects/Querys/Parameters

    Author(s)   : Ron Stark
    Created     : 1.28.2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

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

&IF DEFINED(EXCLUDE-spCalcField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCalcField Procedure
PROCEDURE spCalcField:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iphQuery     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCalcProc  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCalcParam AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDataType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValue     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE hField AS HANDLE NO-UNDO.
    DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
    
/*    MESSAGE                              */
/*        "ipcCalcProc:" ipcCalcProc SKIP  */
/*        "ipcCalcParam:" ipcCalcParam SKIP*/
/*        "ipcDataType:" ipcDataType SKIP  */
/*        "ipcFormat:" ipcFormat           */
/*    VIEW-AS ALERT-BOX.                   */
    
    opcValue = STRING(TIME,"hh:mm:ss am").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spGetCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spGetCompany Procedure
PROCEDURE spGetCompany:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN cCompany.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spSetCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSetCompany Procedure
PROCEDURE spSetCompany:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    cCompany = ipcCompany.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


/* ************************  Function Implementations ***************** */
