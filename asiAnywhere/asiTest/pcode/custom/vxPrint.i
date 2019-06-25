&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : vxprint.i
    Purpose     : procedure to call the vpxprint viewer

    Syntax      :

    Description :

    Author(s)   : M. FONDACCI
    Created     :
    Notes       : This procedure calls 'xCaller.exe'.
                  xCaller.exe acts as vpxPrint.exe but calls
                  xPrint.dll instead of the viewer (vpxprint.exe).
                  xCaller.exe can be renamed vpxprint.exe.
                  
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
   &Glob HWND long    
   &Glob INT LONG
   &GLOB SHELL    "shell32"

PROCEDURE ShellExecuteA EXTERNAL {&SHELL} :
  DEFINE INPUT  PARAMETER hwnd          AS {&HWND}.
  DEFINE INPUT  PARAMETER lpOperation   AS CHAR.
  DEFINE INPUT  PARAMETER lpFile        AS CHAR.
  DEFINE INPUT  PARAMETER lpParameters  AS CHAR.
  DEFINE INPUT  PARAMETER lpDirectory   AS CHAR.
  DEFINE INPUT  PARAMETER nShowCmd      AS {&INT}.
  DEFINE RETURN PARAMETER hInstance     AS {&INT}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vprintFile Include 
PROCEDURE vprintFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iFileName AS CHAR NO-UNDO.
DEF VAR             hInstance AS INT  NO-UNDO.

            FILE-INFO:FILE-NAME = ".".        /* Current directory */

            RUN ShellExecuteA
                              (0,
                              "open",
                              "xCaller.exe",
                              iFileName,
                              FILE-INFO:FULL-PATHNAME,           /* starting directory */
                              1,
                              OUTPUT hInstance).

            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

