&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sys/ref/GetBarDir.p
    Purpose     : Retrieve the BarDir directory

    Syntax      : 

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcType AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opcDirectory AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opcDB AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER oplUserSpecific AS LOG NO-UNDO.

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
DEFINE VARIABLE iUserSpecific AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDirectory AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDBName AS CHARACTER   NO-UNDO.


RUN GetBarDirNKValues(OUTPUT opcDirectory,
                      OUTPUT iUserSpecific).

IF iUserSpecific EQ 1 THEN DO:
    RUN GetUserDir(OUTPUT cDirectory).
    IF cDirectory  NE "" AND cDirectory NE opcDirectory THEN 
        ASSIGN
            oplUserSpecific = YES.
            opcDirectory = cDirectory.
END.
IF ipcType NE "" THEN DO:
    RUN GetTypeInfo(INPUT ipcType,
                OUTPUT opcDB,
                OUTPUT cDBName).
    opcDirectory = TRIM(opcDirectory,"\").
    opcDirectory = opcDirectory + "\" + cDBName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetBarDirNKValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBarDirNKValues Procedure 
PROCEDURE GetBarDirNKValues :
/*------------------------------------------------------------------------------
  Purpose:  Return BarDir and    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opcBarDir AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opiUserDir AS INTEGER NO-UNDO.

DEFINE VARIABLE cResult AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

RUN sys/ref/nk1look.p(INPUT ipcCompany,
                      INPUT "BARDIR",
                      INPUT "I",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT cResult,
                      OUTPUT lFound).
IF lFound THEN
    opiUserDir = INT(cResult).
RUN sys/ref/nk1look.p(INPUT ipcCompany,
                      INPUT "BARDIR",
                      INPUT "DS",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT opcBarDir,
                      OUTPUT lFound).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetTypeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTypeInfo Procedure 
PROCEDURE GetTypeInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcType AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opcDB AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER opcFile AS CHAR NO-UNDO.

CASE ipcType:
    WHEN "loadtag" THEN
        ASSIGN 
            opcDB = "loadtag"
            opcFile = "loadtag.txt".
    OTHERWISE
        ASSIGN 
            opcDB = ipcType
            opcFile = ipcType + ".txt".
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetUserDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetUserDir Procedure 
PROCEDURE GetUserDir :
/*------------------------------------------------------------------------------
  Purpose: Returns the BarDir for the current user  
  Parameters:  outputs the directory if it exists
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opcDirectory AS CHAR NO-UNDO.

DEFINE BUFFER bf-users FOR users.

FIND FIRST bf-users 
    WHERE bf-users.user_id = USERID("nosweat")  
    NO-LOCK NO-ERROR.
IF AVAIL bf-users AND bf-users.user_program[3] <> "" THEN
    opcDirectory = bf-users.user_program[3].
ELSE
    opcDirectory = "".

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

