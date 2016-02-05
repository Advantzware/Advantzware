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
                       INPUT "LabelMatrix",
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
  (  pcPath as char )  FORWARD.

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

def var cProtocol        as char no-undo.
def var cComputerName    as char no-undo.
def var cSharedFolder    as char no-undo.
def var cDrive           as char no-undo.
def var cDir             as char no-undo.
def var cFile            as char no-undo.
def var cExt             as char no-undo.



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


RUN custom/getregvalue.p (INPUT "HKEY_LOCAL_MACHINE", 
                          INPUT "SOFTWARE",
                          INPUT "Teklynx\Label Matrix",
                          INPUT "PATH",
                          OUTPUT cPath).

cPath = TRIM(cPath,"\").


ASSIGN
  cPath = cPath + "\lmwprint.exe "
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

&IF DEFINED(EXCLUDE-win_breakPath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE win_breakPath Procedure 
PROCEDURE win_breakPath :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    define input    param pcPath            as char no-undo.
    define output   param pcProtocol        as char no-undo.
    define output   param pcComputerName    as char no-undo.
    define output   param pcSharedFolder    as char no-undo.
    define output   param pcDrive           as char no-undo.
    define output   param pcDir             as char no-undo.
    define output   param pcFile            as char no-undo.
    define output   param pcExt             as char no-undo.

    define var i as int no-undo.

    assign
        pcProtocol      = ""
        pcComputerName  = ""
        pcSharedFolder  = ""
        pcDrive         = ""
        pcDir           = ""
        pcFile          = ""
        pcExt           = "".

    /* assumes that if the call is from another procedure or function with in this library then the path has already been normalized. */

    if pcPath = ? then
        return.

    if source-procedure <> this-procedure then
       pcPath = win_normalizePath( pcPath ).

    if pcPath begins "~\~\" then do:

        assign
            pcProtocol = substr( pcPath, 1, 2 )
            substr( pcPath, 1, 2 ) = "".

        i = index( pcPath, "~\", 3 ).
        if i = 0 then i = length( pcPath ) + 1.

        assign
            pcComputerName = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

        i = index( pcPath, "~\", 2 ).
        if i = 0 then i = length( pcPath ) + 1.

        assign
            pcSharedFolder = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

    end. /* pcPath begins "\\" */

    else
    if  substr( pcPath, 1, 1 ) >= "a"
    and substr( pcPath, 1, 1 ) <= "z"
    and substr( pcPath, 2, 1 )  = ":" then do:

        assign
            pcDrive = substr( pcPath, 1, 2 )
            substr( pcPath, 1, 2 ) = "".

    end. /* else */

    i = r-index( pcPath, "~\" ). 
    if i > 0 then

    assign
        pcDir = substr( pcPath, 1, i )
        substr( pcPath, 1, i ) = "".

    i = r-index( pcPath, "." ).
    if i > 0 then

    assign
        pcExt = substr( pcPath, i )
        substr( pcPath, i, length( pcExt ) ) = "".

    pcFile = pcPath.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-win_normalizePath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION win_normalizePath Procedure 
FUNCTION win_normalizePath RETURNS CHARACTER
  (  pcPath as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


    define var cPath    as char no-undo.
    define var cRoot    as char no-undo.
    define var cDir     as char no-undo.
    define var iDir     as int no-undo.

    define var str      as char no-undo.
    define var i        as int no-undo.

    pcPath = trim( pcPath ).

    if pcPath = ""
    or pcPath = ? then
        return pcPath.

    pcPath = replace( pcPath, "/", "~\" ).

    do while index( pcPath, "~\~\", 2 ) <> 0:
        substr( pcPath, 2, length( pcPath ) - 1 ) = replace( substr( pcPath, 2, length( pcPath ) - 1 ), "~\~\", "~\" ).
    end.

    do while index( pcPath, "::" ) <> 0:
        pcPath = replace( pcPath, "::", ":" ).
    end.

    if lookup( ".", pcPath, "~\" ) > 0 or lookup( "..", pcPath, "~\" ) > 0 then do:

        assign
            cRoot = ""
            cPath = "".

        if pcPath begins "~\~\" then do:

            i = index( pcPath, "~\", 3 ).
            if i = 0 then i = length( pcPath ).

            assign
                cRoot = substr( pcPath, 1, i )
                substr( pcPath, 1, i ) = "".

            i = index( pcPath, "~\" ). 
            if i > 0 then

            assign
                cRoot = cRoot + substr( pcPath, 1, i )
                substr( pcPath, 1, i ) = "".

        end. /* pcPath begins "\\" */

        else
        if  substr( pcPath, 1, 1 ) >= "a"
        and substr( pcPath, 1, 1 ) <= "z"
        and substr( pcPath, 2, 1 )  = ":" then do:

            assign
               cRoot = substr( pcPath, 1, 2 )
               substr( pcPath, 1, 2 ) = "".

            if substr( pcPath, 1, 1 ) = "~\" then
            assign
               cRoot = cRoot + substr( pcPath, 1, 1 )
               substr( pcPath, 1, 1 ) = "".

        end. /* substr = ":" */



        do iDir = 1 to num-entries( pcPath, "~\" ):

            cDir = entry( iDir, pcPath, "~\" ).

            if cDir = "." then do:

                if cPath <> "" or cRoot <> "" then
                    next.

                else
                cPath = cPath
                      + ( if cPath <> "" then "~\" else "" )
                      + cDir.

            end. /* cDir = "." */

            else
            if cDir = ".." then do:

                if cPath <> "" and entry( num-entries( cPath, "~\" ), cPath, "~\" ) <> ".." then do:

                    str = "".

                    do i = 1 to num-entries( cPath, "~\" ) - 1:

                        str = str
                            + ( if str <> "" then "~\" else "" )
                            + entry( i, cPath, "~\" ).

                    end. /* 1 to num-entries */

                    cPath = str.

                end. /* else */

                else
                cPath = cPath
                      + ( if cPath <> "" then "~\" else "" )
                      + cDir.

            end. /* cDir = ".." */

            else
            cPath = cPath
                  + ( if cPath <> "" then "~\" else "" )
                  + cDir.

        end. /* 1 to num-entries */

        pcPath = cPath.

        if cRoot <> "" then
            pcPath = cRoot + pcPath.

    end. /* lookup( ".." ) > 0 */

    return pcPath.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

