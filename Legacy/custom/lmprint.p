&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*----------------------------------------------------------------------
Program     : custom/lmprint.p

Description : Wrapper for call of label matrix executable

Copyright(c): Advanced Software Services Inc. 2013
Author      : Wade Kaldawi
Created     : 03/06/2013 
Notes       :

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions     ----------------------------------------- */
DEF INPUT PARAMETER ipcLabelFile AS CHAR NO-UNDO.  /* QDF Template File */
/*##PN these parameter is used in the case of a user-defined c:ba\label\ dir*/
/*##PN leave blank to let template dictate datafile*/
DEF INPUT PARAMETER ipcDataName AS CHAR NO-UNDO.  /*optional datafile type*/
DEF INPUT PARAMETER ipcDataFile AS CHAR NO-UNDO. /*optional .txt location*/


DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER   NO-UNDO.


/* Include Files              ----------------------------------------- */
{custom/windows.i}
{sys/inc/var.i shared}

DEF VAR lLabelMatrixLock AS LOG NO-UNDO.
DEF STREAM sLock.

RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "lmLock",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cResult,
                       OUTPUT lFound).
IF lFound THEN
  lLabelMatrixLock = LOGICAL(cResult).
/* Function Forwards          ----------------------------------------- */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-win_normalizePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD win_normalizePath Procedure 
FUNCTION win_normalizePath RETURNS CHARACTER
  (  pcPath AS CHAR )  FORWARD.

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

DEF VAR iResult AS INT NO-UNDO.
DEF VAR cFileName AS CHAR NO-UNDO.
DEF VAR cPath AS CHARACTER NO-UNDO.

DEF VAR cProtocol        AS CHAR NO-UNDO.
DEF VAR cComputerName    AS CHAR NO-UNDO.
DEF VAR cSharedFolder    AS CHAR NO-UNDO.
DEF VAR cDrive           AS CHAR NO-UNDO.
DEF VAR cDir             AS CHAR NO-UNDO.
DEF VAR cFile            AS CHAR NO-UNDO.
DEF VAR cExt             AS CHAR NO-UNDO.



/* Pass the lock file name for labelMatrix to remove  */
IF  lLabelMatrixLock THEN DO:


    RUN win_breakPath (INPUT ipcLabelFile,
         OUTPUT cProtocol        ,
         OUTPUT cComputerName    ,
         OUTPUT cSharedFolder    ,
         OUTPUT cDrive           ,
         OUTPUT cDir             ,
         OUTPUT cFile            ,
         OUTPUT cExt             ).
      /* Put the lock file in the label matrix path */
    

END.

RUN pGetExecutionPath(OUTPUT cPath). /*Gets the LMPrint.exe execution path*/
  
  cFileName = "/L=" + ipcLabelFile.

/* Specify a "lock" file for LM to remove when done printing */
IF lLabelMatrixLock THEN DO:
  OUTPUT STREAM sLock TO VALUE(cDrive + cDir + "lm.lock").
  OUTPUT STREAM sLock CLOSE.
  cFileName = cFileName + " /F=" + cDrive + cDir + "lm.lock".
END.



IF ipcDataFile NE "" AND ipcDataName NE "" THEN
    cFileName = cFileName + " /file=" + ipcDataName + "," + ipcDataFile.


/* Original Call Failed with newer version 8.7 of label matrix*/
/*                                                                             */
/*           RUN WinExec (INPUT cPath + CHR(32) + cFileName , INPUT 1, OUTPUT */
/*                        iResult).                                              */

RUN ShellExecute{&A} IN hpApi(0,
  "open",
  cPath,
  cFileName,
  "",
  1,
  OUTPUT iResult).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pGetExecutionPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetExecutionPath Procedure
PROCEDURE pGetExecutionPath:
/*------------------------------------------------------------------------------
 Purpose: Finds the correct call to the execution path for LMWprint.exe
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opcPath AS CHARACTER NO-UNDO.

RUN custom/getregvalue.p (INPUT "HKEY_LOCAL_MACHINE", 
                          INPUT "SOFTWARE",
                          INPUT "Teklynx\Label Matrix",
                          INPUT "PATH",
                          OUTPUT opcPath).
IF opcPath NE ? THEN DO:
    ASSIGN
        opcPath = TRIM(opcPath,"\")
        opcPath = opcPath + "\lmwprint.exe ".
END.
ELSE DO:  /*Newer Operating Systems*/
    RUN custom/getregvalue.p (INPUT "HKEY_LOCAL_MACHINE", 
                              INPUT "SOFTWARE",
                              INPUT "Classes\Label Matrix Document\shell\open\command\print\command",
                              INPUT "",
                              OUTPUT opcPath).
    opcPath = TRIM(opcPath,"%1").
  
END.
IF opcPath EQ ? THEN 
    MESSAGE "The Label Matrix print program cannot be located or launched on this computer - Reinstall or contact your system administrator." VIEW-AS ALERT-BOX ERROR.  

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-win_breakPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE win_breakPath Procedure 
PROCEDURE win_breakPath :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT    PARAM pcPath            AS CHAR NO-UNDO.
    DEFINE OUTPUT   PARAM pcProtocol        AS CHAR NO-UNDO.
    DEFINE OUTPUT   PARAM pcComputerName    AS CHAR NO-UNDO.
    DEFINE OUTPUT   PARAM pcSharedFolder    AS CHAR NO-UNDO.
    DEFINE OUTPUT   PARAM pcDrive           AS CHAR NO-UNDO.
    DEFINE OUTPUT   PARAM pcDir             AS CHAR NO-UNDO.
    DEFINE OUTPUT   PARAM pcFile            AS CHAR NO-UNDO.
    DEFINE OUTPUT   PARAM pcExt             AS CHAR NO-UNDO.

    DEFINE VAR i AS INT NO-UNDO.

    ASSIGN
        pcProtocol      = ""
        pcComputerName  = ""
        pcSharedFolder  = ""
        pcDrive         = ""
        pcDir           = ""
        pcFile          = ""
        pcExt           = "".

    /* assumes that if the call is from another procedure or function with in this library then the path has already been normalized. */

    IF pcPath = ? THEN
        RETURN.

    IF SOURCE-PROCEDURE <> THIS-PROCEDURE THEN
       pcPath = win_normalizePath( pcPath ).

    IF pcPath BEGINS "~\~\" THEN DO:

        ASSIGN
            pcProtocol = substr( pcPath, 1, 2 )
            substr( pcPath, 1, 2 ) = "".

        i = INDEX( pcPath, "~\", 3 ).
        IF i = 0 THEN i = LENGTH( pcPath ) + 1.

        ASSIGN
            pcComputerName = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

        i = INDEX( pcPath, "~\", 2 ).
        IF i = 0 THEN i = LENGTH( pcPath ) + 1.

        ASSIGN
            pcSharedFolder = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

    END. /* pcPath begins "\\" */

    ELSE
    IF  substr( pcPath, 1, 1 ) >= "a"
    AND substr( pcPath, 1, 1 ) <= "z"
    AND substr( pcPath, 2, 1 )  = ":" THEN DO:

        ASSIGN
            pcDrive = substr( pcPath, 1, 2 )
            substr( pcPath, 1, 2 ) = "".

    END. /* else */

    i = R-INDEX( pcPath, "~\" ). 
    IF i > 0 THEN

    ASSIGN
        pcDir = substr( pcPath, 1, i )
        substr( pcPath, 1, i ) = "".

    i = R-INDEX( pcPath, "." ).
    IF i > 0 THEN

    ASSIGN
        pcExt = substr( pcPath, i )
        substr( pcPath, i, LENGTH( pcExt ) ) = "".

    pcFile = pcPath.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-win_normalizePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION win_normalizePath Procedure 
FUNCTION win_normalizePath RETURNS CHARACTER
  (  pcPath AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


    DEFINE VAR cPath    AS CHAR NO-UNDO.
    DEFINE VAR cRoot    AS CHAR NO-UNDO.
    DEFINE VAR cDir     AS CHAR NO-UNDO.
    DEFINE VAR iDir     AS INT NO-UNDO.

    DEFINE VAR str      AS CHAR NO-UNDO.
    DEFINE VAR i        AS INT NO-UNDO.

    pcPath = TRIM( pcPath ).

    IF pcPath = ""
    OR pcPath = ? THEN
        RETURN pcPath.

    pcPath = REPLACE( pcPath, "/", "~\" ).

    DO WHILE INDEX( pcPath, "~\~\", 2 ) <> 0:
        substr( pcPath, 2, LENGTH( pcPath ) - 1 ) = REPLACE( substr( pcPath, 2, LENGTH( pcPath ) - 1 ), "~\~\", "~\" ).
    END.

    DO WHILE INDEX( pcPath, "::" ) <> 0:
        pcPath = REPLACE( pcPath, "::", ":" ).
    END.

    IF LOOKUP( ".", pcPath, "~\" ) > 0 OR lookup( "..", pcPath, "~\" ) > 0 THEN DO:

        ASSIGN
            cRoot = ""
            cPath = "".

        IF pcPath BEGINS "~\~\" THEN DO:

            i = INDEX( pcPath, "~\", 3 ).
            IF i = 0 THEN i = LENGTH( pcPath ).

            ASSIGN
                cRoot = substr( pcPath, 1, i )
                substr( pcPath, 1, i ) = "".

            i = INDEX( pcPath, "~\" ). 
            IF i > 0 THEN

            ASSIGN
                cRoot = cRoot + substr( pcPath, 1, i )
                substr( pcPath, 1, i ) = "".

        END. /* pcPath begins "\\" */

        ELSE
        IF  substr( pcPath, 1, 1 ) >= "a"
        AND substr( pcPath, 1, 1 ) <= "z"
        AND substr( pcPath, 2, 1 )  = ":" THEN DO:

            ASSIGN
               cRoot = substr( pcPath, 1, 2 )
               substr( pcPath, 1, 2 ) = "".

            IF substr( pcPath, 1, 1 ) = "~\" THEN
            ASSIGN
               cRoot = cRoot + substr( pcPath, 1, 1 )
               substr( pcPath, 1, 1 ) = "".

        END. /* substr = ":" */



        DO iDir = 1 TO NUM-ENTRIES( pcPath, "~\" ):

            cDir = ENTRY( iDir, pcPath, "~\" ).

            IF cDir = "." THEN DO:

                IF cPath <> "" OR cRoot <> "" THEN
                    NEXT.

                ELSE
                cPath = cPath
                      + ( IF cPath <> "" THEN "~\" ELSE "" )
                      + cDir.

            END. /* cDir = "." */

            ELSE
            IF cDir = ".." THEN DO:

                IF cPath <> "" AND entry( NUM-ENTRIES( cPath, "~\" ), cPath, "~\" ) <> ".." THEN DO:

                    str = "".

                    DO i = 1 TO NUM-ENTRIES( cPath, "~\" ) - 1:

                        str = str
                            + ( IF str <> "" THEN "~\" ELSE "" )
                            + entry( i, cPath, "~\" ).

                    END. /* 1 to num-entries */

                    cPath = str.

                END. /* else */

                ELSE
                cPath = cPath
                      + ( IF cPath <> "" THEN "~\" ELSE "" )
                      + cDir.

            END. /* cDir = ".." */

            ELSE
            cPath = cPath
                  + ( IF cPath <> "" THEN "~\" ELSE "" )
                  + cDir.

        END. /* 1 to num-entries */

        pcPath = cPath.

        IF cRoot <> "" THEN
            pcPath = cRoot + pcPath.

    END. /* lookup( ".." ) > 0 */

    RETURN pcPath.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

