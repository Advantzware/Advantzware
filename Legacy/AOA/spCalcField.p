&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : spCalcField.p
    Purpose     : super-procedures for dynamic calculated fields

    Syntax      : AOA/spCalcField.p

    Description : Super Procedures used by Calculated Fields

    Author(s)   : Ron Stark
    Created     : 1.28.2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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

&IF DEFINED(EXCLUDE-calcShiftEndTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcShiftEndTime Procedure
PROCEDURE calcShiftEndTime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplUseTime    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStartShift AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEndShift   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTime       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue  AS CHARACTER NO-UNDO.
    
    opcCalcValue = "86400000".

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcShiftStartTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcShiftStartTime Procedure
PROCEDURE calcShiftStartTime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplUseTime    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStartShift AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEndShift   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTime       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCalcValue  AS CHARACTER NO-UNDO.
    
    opcCalcValue = "0".

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcStringDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcStringDateTime Procedure
PROCEDURE calcStringDateTime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipdtDate     AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTime      AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    opcCalcValue = STRING(ipdtDate,"99/99/9999") + " "
                 + STRING(ipiTime,"hh:mm:ss am")
                 .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcStringTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcStringTime Procedure
PROCEDURE calcStringTime:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiTime      AS INTEGER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.
    
    opcCalcValue = STRING(ipiTime,"hh:mm:ss am").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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
    DEFINE OUTPUT PARAMETER opcCalcValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hField AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable AS HANDLE    NO-UNDO.
    
    /* parse parameter string, replace fields with actual values */
    DO idx = 1 TO NUM-ENTRIES(ipcCalcParam,"|"):
        cParam = ENTRY(idx,ipcCalcParam,"|").
        IF INDEX(cParam,".") NE 0 THEN DO:
            ASSIGN
                cTable = ENTRY(1,cParam,".")
                cField = ENTRY(2,cParam,".")
                hTable = iphQuery:GET-BUFFER-HANDLE(cTable)
                cValue = hTable:BUFFER-FIELD(cField):BUFFER-VALUE()
                ENTRY(idx,ipcCalcParam,"|") = TRIM(cValue)
                .
        END. /* if database field */
    END. /* do idx */
    /* list case when values alphabetically */
    CASE ipcCalcProc:
        WHEN "calcShiftEndTime"   OR
        WHEN "calcShiftStartTime" THEN
        RUN VALUE(ipcCalcProc) (
            ENTRY(1,ipcCalcParam,"|") EQ "yes",
            ENTRY(2,ipcCalcParam,"|"),
            ENTRY(3,ipcCalcParam,"|"),
            ENTRY(4,ipcCalcParam,"|"),
            OUTPUT opcCalcValue).
        WHEN "calcStringDateTime" THEN
        RUN VALUE(ipcCalcProc) (
            DATE(ENTRY(1,ipcCalcParam,"|")),
            INTEGER(ENTRY(2,ipcCalcParam,"|")),
            OUTPUT opcCalcValue).
        WHEN "calcStringTime" THEN
        RUN VALUE(ipcCalcProc) (
            INTEGER(ipcCalcParam),
            OUTPUT opcCalcValue).
    END CASE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
