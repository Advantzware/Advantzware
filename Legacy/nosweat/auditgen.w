&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: auditgen.w

  Description: Auto Generate Audit Programs

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 07/29/98
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE NEW SHARED STREAM s_audit_gen.

DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS db-names Btn_OK Btn_Cancel compile-audit 
&Scoped-Define DISPLAYED-OBJECTS db-names compile-audit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE db-names AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 36 BY 5 NO-UNDO.

DEFINE VARIABLE compile-audit AS LOGICAL INITIAL no 
     LABEL "Compile Audit Programs" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     db-names AT ROW 2.67 COL 1 HELP
          "Select Database Names to Generate Audit Programs" NO-LABEL
     Btn_OK AT ROW 4.57 COL 38 HELP
          "Create Audit Programs for Selected Database Names"
     Btn_Cancel AT ROW 6.05 COL 38 HELP
          "Cancel Audit Program Generation"
     compile-audit AT ROW 7.67 COL 1 HELP
          "Set Compile Audit Programs Indicator"
     "Select Database Name(s) to" VIEW-AS TEXT
          SIZE 33 BY .62 AT ROW 1.24 COL 2
          FONT 6
     "Generate Audit Programs" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 1.95 COL 4
          FONT 6
     SPACE(19.00) SKIP(6.10)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Generate Audit Programs"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Generate Audit Programs */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN Gen-Programs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN Get-DBs.
  {methods/nowait.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY db-names compile-audit 
      WITH FRAME Dialog-Frame.
  ENABLE db-names Btn_OK Btn_Cancel compile-audit 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gen-Programs Dialog-Frame 
PROCEDURE Gen-Programs :
/*------------------------------------------------------------------------------
  Purpose:     Generate Prorams
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(24)" NO-UNDO.
  DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.

  {methods/wait.i}
  OUTPUT STREAM s_audit_gen TO "custom/audit.i".
  PUT STREAM s_audit_gen UNFORMATTED "/* audit.i - generated "
    TODAY FORMAT "99/99/9999" " - " STRING(TIME,"HH:MM:SS") " by "
    USERID("NOSWEAT") " */" SKIP(1)
    "/* this program was generated by auditgn1.w and should not be modified */" SKIP
    "/* modifications should be made to the auto generation program         */" SKIP(1).
  DO i = 1 TO db-names:NUM-ITEMS IN FRAME {&FRAME-NAME}:
    IF db-names:IS-SELECTED(i) THEN
    DO:
      CREATE ALIAS "dictdb" FOR DATABASE VALUE(db-names:ENTRY(i)).
      RUN Get_Procedure IN PERSISTENT-HANDLE (INPUT "auditgn1.",OUTPUT run-proc,yes).
    END.
  END.
  OUTPUT STREAM s_audit_gen CLOSE.
  IF compile-audit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes" THEN
  DO:
    INPUT FROM OS-DIR("auditprg") NO-ECHO.
    REPEAT:
      IMPORT DELIMITER " " file-name ^ attr-list.
      IF attr-list = "F" AND INDEX(file-name,".p") NE 0 THEN
      COMPILE VALUE("auditprg/" + file-name) SAVE.
    END.
    INPUT CLOSE.
  END.
  {methods/nowait.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-DBs Dialog-Frame 
PROCEDURE Get-DBs :
/* -----------------------------------------------------------
  Purpose: Populate db-names selection list with connected database names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE list-items AS CHARACTER NO-UNDO.
  
  RUN Get_Procedure IN Persistent-Handle (INPUT "db_list.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT list-items).
  ASSIGN
    db-names:LIST-ITEMS IN FRAME {&FRAME-NAME} = list-items
    db-names:SCREEN-VALUE = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus Dialog-Frame 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i Btn_Cancel}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


