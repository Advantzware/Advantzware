&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: util\DPUtils.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-25 lv-file Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lv-file AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-file AT ROW 4.1 COL 8 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 7.19 COL 15
     Btn_Cancel AT ROW 7.19 COL 43
     "Enter Config Data Output Folder" VIEW-AS TEXT
          SIZE 35 BY 1.1 AT ROW 2.43 COL 7
          FGCOLOR 9 
     "(Make sure this folder has an addon subfolder)" VIEW-AS TEXT
          SIZE 55 BY .62 AT ROW 5.29 COL 12 WIDGET-ID 8
          FGCOLOR 12 
     RECT-25 AT ROW 3.14 COL 5 WIDGET-ID 6
     SPACE(1.59) SKIP(4.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Dump Utilities"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Dump Utilities */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN lv-file.
/*    IF SEARCH(fiPFFileLocation + "\addon\" + "nosweat.pf") EQ ? THEN DO:   */
/*        MESSAGE 'Please run from Advantzware.  addon\nosweat.pf is missing'*/
/*        VIEW-AS ALERT-BOX.                                                 */
/*        RETURN.                                                            */
/*    END.                                                                   */
    
/*    IF NOT CONNECTED("nosweat-addon") THEN DO:                                           */
/*        CONNECT -pf VALUE(fiPFFileLocation + "\addon\" + "nosweat.pf") -ld nosweat-addon.*/
/*    END. 
    file-info                                                   */
    FILE-INFO:FILE-NAME = lv-file.
    
    IF not FILE-INFO:file-type begins "D"  THEN DO:
        MESSAGE "The folder " lv-file  skip
        "does not exist, so can't continue"
        VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    
        FILE-INFO:FILE-NAME = lv-file + "\addon".
    
    IF not FILE-INFO:file-type begins "D"  THEN DO:
        MESSAGE "The folder "  lv-file + "/addon" skip
        "does not exist, so can't continue"
        VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    
    IF CONNECTED("nosweat") THEN
        DISCONNECT nosweat.

    CONNECT -db nosweat -S 3810 -H asidc.
    CONNECT -db nosweat -ld "ASINOS" -S 3802 -H asidc.        

    RUN util/dumpConfig.p (INPUT lv-file).
    IF CONNECTED("nosweat") THEN
      DISCONNECT nosweat.
      
    IF CONNECTED("asinos") THEN 
      DISCONNECT asinos.
      
    CONNECT -db nosweat -S 3802 -H asidc.
    
    MESSAGE "Dump completed." VIEW-AS ALERT-BOX.
    APPLY "go" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file Dialog-Frame
ON HELP OF lv-file IN FRAME Dialog-Frame
DO:
   DEF VAR ls-filename AS cha NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.
   
   SYSTEM-DIALOG GET-FILE ls-filename 
                 TITLE "Select File to insert"
                 FILTERS "Data Files    (*.dat)" "*.dat",
                         "Text files (*.txt)" "*.txt",                         
                         "All Files    (*.*) " "*.*"
                 INITIAL-DIR "boximage\"
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN SELF:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

IF CONNECTED("jobs") THEN DO:
    MESSAGE "Please run this utility from Advantzware."
    VIEW-AS ALERT-BOX.
END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  lv-file = "c:\temp".
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY lv-file 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-25 lv-file Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

