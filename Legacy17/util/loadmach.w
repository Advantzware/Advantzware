&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-mach LIKE mach.
DEF TEMP-TABLE tt-mstd LIKE mstd.
DEF TEMP-TABLE tt-mmty LIKE mmty.
DEF TEMP-TABLE tt-mmtx LIKE mmtx.
DEF TEMP-TABLE tt-mmtx2 LIKE mmtx2.
DEF TEMP-TABLE tt-cal LIKE mach-calendar.
DEF STREAM st-input.
DEF TEMP-TABLE tt-mmtx-new 
     FIELD mmtx-no LIKE tt-mmtx2.mmtx-no
     FIELD new-mmtx-no LIKE tt-mmtx2.mmtx-no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS lv-file v-status 

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

DEFINE VARIABLE lv-file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\machine.dat" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-file AT ROW 3.38 COL 2 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 8.86 COL 14
     Btn_Cancel AT ROW 8.86 COL 43
     v-status AT ROW 11.24 COL 3 NO-LABEL
     "Enter Machine Data File Path" VIEW-AS TEXT
          SIZE 32 BY 1.1 AT ROW 1.95 COL 4
     SPACE(29.13) SKIP(9.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Import Machine"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-file IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Import Machine */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    

    SESSION:SET-WAIT-STATE("general").

    ASSIGN lv-file.

    /* save current data */
    OUTPUT TO value("c:\tmp\machsave.d" + string(TIME)) .
    FOR EACH mach NO-LOCK:
            EXPORT mach.
    END.
    OUTPUT CLOSE.
    OUTPUT TO value("c:\tmp\mstdsave.d" + string(TIME)) .
    FOR EACH mstd NO-LOCK:
            EXPORT mstd.
    END.
    OUTPUT CLOSE.
    OUTPUT TO value("c:\tmp\mmtysave.d" + string(TIME)) .
    FOR EACH mmty NO-LOCK:
            EXPORT mmty.
    END.
    OUTPUT CLOSE.
    OUTPUT TO value("c:\tmp\mmtxsave.d" + string(TIME)) .
    FOR EACH mmtx NO-LOCK:
            EXPORT mmtx.
    END.
    OUTPUT CLOSE.
    OUTPUT TO value("c:\tmp\mmtx2save.d" + string(TIME)) .
    FOR EACH mmtx2 NO-LOCK:
            EXPORT mmtx2.
    END.
    OUTPUT CLOSE.

    INPUT FROM value(lv-file) NO-ECHO.
    REPEAT:
        CREATE tt-mach.
        IMPORT tt-mach.
        FIND FIRST mach WHERE mach.company = tt-mach.company
                          AND mach.m-code = tt-mach.m-code NO-ERROR.
        IF NOT AVAIL mach THEN CREATE mach.
        BUFFER-COPY tt-mach TO mach.
        v-status = "Machine: " + mach.m-code + " " + mach.m-dscr.                
        DISP v-status WITH FRAME {&FRAME-NAME}.
    END.
    INPUT CLOSE.

    INPUT STREAM st-input FROM value("c:\tmp\machine.dat1") NO-ECHO.
    REPEAT:
            CREATE tt-mstd.
            IMPORT STREAM st-input tt-mstd.
            FIND FIRST mstd WHERE mstd.company = tt-mstd.company
                              AND mstd.loc = tt-mstd.loc
                              AND mstd.m-code = tt-mstd.m-code 
                              AND mstd.dept = tt-mstd.dept
                              AND mstd.style = tt-mstd.style NO-ERROR.
            IF NOT AVAIL mstd THEN CREATE mstd.
            BUFFER-COPY tt-mstd TO mstd.
            v-status:SCREEN-VALUE = "Setup: " + mstd.m-code .                
    END.
    INPUT STREAM st-input CLOSE.

    INPUT STREAM st-input FROM value("c:\tmp\machine.dat2") NO-ECHO.
    REPEAT:
            CREATE tt-mmty.
            IMPORT STREAM st-input tt-mmty.
            FIND FIRST mmty WHERE mmty.company = tt-mmty.company
                              AND mmty.loc = tt-mmty.loc
                              AND mmty.m-code = tt-mmty.m-code 
                              AND mmty.dept = tt-mmty.dept
                              AND mmty.style = tt-mmty.style 
                              AND mmty.page-no = tt-mmty.page-no
                              AND mmty.across-no = tt-mmty.across-no NO-ERROR.
            IF NOT AVAIL mmty THEN CREATE mmty.
            BUFFER-COPY tt-mmty TO mmty.
            v-status:SCREEN-VALUE = "Run: " + mmty.m-code.                
    END.
    INPUT STREAM st-input CLOSE.
    FOR EACH tt-mmtx-new:
        DELETE tt-mmtx-new.
    END.
    INPUT STREAM st-input FROM value("c:\tmp\machine.dat3") NO-ECHO.
    REPEAT:
            CREATE tt-mmtx.
            IMPORT STREAM st-input tt-mmtx.
            FIND FIRST mmtx WHERE mmtx.mmtx-no = tt-mmtx.mmtx-no NO-LOCK NO-ERROR.
            IF AVAIL mmtx THEN DO:
               find LAST mmtx use-index mmtx-no no-lock no-error.
               CREATE tt-mmtx-new.
               ASSIGN tt-mmtx-new.mmtx-no = tt-mmtx.mmtx-no.
               tt-mmtx.mmtx-no = if avail mmtx then mmtx.mmtx-no + 1 else 1.
               tt-mmtx-new.new-mmtx-no = tt-mmtx.mmtx-no.
            END.
            FIND FIRST mmtx WHERE mmtx.company = tt-mmtx.company
                              AND mmtx.loc = tt-mmtx.loc
                              AND mmtx.m-code = tt-mmtx.m-code 
                              AND mmtx.dept = tt-mmtx.dept
                              AND mmtx.style = tt-mmtx.style 
                              AND mmtx.mr-run = tt-mmtx.mr-run
                              AND mmtx.page-no = tt-mmtx.page-no NO-ERROR.
            IF NOT AVAIL mmtx THEN CREATE mmtx.
            BUFFER-COPY tt-mmtx TO mmtx.
            v-status:SCREEN-VALUE = "Spoilage: " + mmtx.m-code.                
    END.
    INPUT STREAM st-input CLOSE.
    INPUT STREAM st-input FROM value("c:\tmp\machine.dat4") NO-ECHO.
    REPEAT:
            CREATE tt-cal.
            IMPORT STREAM st-input tt-cal.
            FIND FIRST mach-calendar WHERE mach-calendar.company = tt-cal.company
                                       AND mach-calendar.m-code = tt-cal.m-code 
                                       AND mach-calendar.m-date = tt-cal.m-date
                                       AND mach-calendar.seq = tt-cal.seq NO-ERROR.
            IF NOT AVAIL mach-calendar THEN CREATE mach-calendar.
            BUFFER-COPY tt-cal TO mach-calendar.
            v-status:SCREEN-VALUE = "Capacity: " + mach-calendar.m-code + " " + STRING(mach-calendar.m-date).                
    END.
    INPUT STREAM st-input CLOSE.
    INPUT STREAM st-input FROM value("c:\tmp\machine.dat5") NO-ECHO.
    REPEAT:
            CREATE tt-mmtx2.
            IMPORT STREAM st-input tt-mmtx2.
            FIND FIRST mmtx2 WHERE mmtx2.mmtx-no = tt-mmtx2.mmtx-no NO-ERROR.
            IF NOT AVAIL mmtx2 THEN CREATE mmtx2.
            BUFFER-COPY tt-mmtx2 TO mmtx2.
            FIND FIRST tt-mmtx-new WHERE tt-mmtx-new.mmtx-no = tt-mmtx2.mmtx-no NO-ERROR.
            IF AVAIL tt-mmtx-new THEN
                mmtx2.mmtx-no = tt-mmtx-new.new-mmtx-no.
    END.
    INPUT STREAM st-input CLOSE.
    
    SESSION:SET-WAIT-STATE("").
    MESSAGE "Loading machine is completed." VIEW-AS ALERT-BOX.
    APPLY "go" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-file Dialog-Frame
ON HELP OF lv-file IN FRAME Dialog-Frame
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   
   system-dialog get-file ls-filename 
                 title "Select File to insert"
                 filters "Data Files    (*.dat)" "*.dat",
                         "Text files (*.txt)" "*.txt",                         
                         "All Files    (*.*) " "*.*"
                 initial-dir "boximage\"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.
      
    IF ll-ok THEN self:screen-value = ls-filename.
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
  DISPLAY lv-file v-status 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

