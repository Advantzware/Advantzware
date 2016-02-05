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

DEF TEMP-TABLE tt-item LIKE ITEM.
DEF TEMP-TABLE tt-eitem LIKE e-item.
DEF TEMP-TABLE tt-vend LIKE e-item-vend.


DEF STREAM st-eitem.
DEF STREAM st-vend.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-file Btn_OK Btn_Cancel 
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     lv-file AT ROW 3.38 COL 2 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 10.05 COL 15
     Btn_Cancel AT ROW 10.05 COL 43
     "Enter Style Data File Path" VIEW-AS TEXT
          SIZE 32 BY 1.1 AT ROW 1.95 COL 4
     SPACE(29.13) SKIP(9.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Load Ink"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Load Ink */
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
    DEF VAR lv-file2 AS cha NO-UNDO.
    DEF VAR lv-file3 AS cha NO-UNDO.

    DEF VAR v1 LIKE box-design-hdr.company NO-UNDO.
    DEF VAR v2 LIKE box-design-hdr.design-no NO-UNDO.
    DEF VAR v3 LIKE box-design-hdr.box-image NO-UNDO.
    DEF VAR v4 LIKE box-design-hdr.box-3d-image NO-UNDO.

    ASSIGN lv-file.
    /* save current data */
    OUTPUT TO value("c:\tmp\itemink.d" + string(TIME)) .

    OUTPUT STREAM st-eitem TO VALUE ("c:\tmp\itemeitm.d" + string(TIME)).
    OUTPUT STREAM st-vend TO VALUE ("c:\tmp\itemvend.d" + string(TIME))  .

    FOR EACH ITEM WHERE ITEM.mat-type = "I" NO-LOCK:
            EXPORT item.
            FOR EACH e-item OF ITEM NO-LOCK.
                EXPORT STREAM st-eitem e-item.
                FOR EACH e-item-vend OF e-item NO-LOCK:
                    EXPORT STREAM st-vend e-item-vend.
                END.
            END.
    END.
    OUTPUT CLOSE.
    OUTPUT STREAM st-eitem CLOSE.
    OUTPUT STREAM st-vend CLOSE.
      
    INPUT FROM value(lv-file) NO-ECHO.
    REPEAT:
     
        CREATE tt-item.
        IMPORT tt-item.
        FIND FIRST item WHERE item.company = tt-item.company
                         AND item.i-no = tt-item.i-no NO-ERROR.
        IF NOT AVAIL item THEN DO:
           CREATE item.
        END.
        BUFFER-COPY tt-item TO item.
    END.

    lv-file2 = lv-file.
    SUBSTRING(lv-file2,LENGTH(lv-file),1) = "2".  /* e-item */
    INPUT FROM value(lv-file2) NO-ECHO.
    REPEAT:
        CREATE tt-eitem.
        IMPORT tt-eitem.
        find first e-item where e-item.company = tt-eitem.company
                            AND e-item.i-no = tt-eitem.i-no
                        no-error.
        IF NOT AVAIL e-item THEN CREATE e-item.
        BUFFER-COPY tt-eitem TO e-item.
    END.

    lv-file2 = lv-file.
    SUBSTRING(lv-file2,LENGTH(lv-file),1) = "3".  /* e-item-vend */
    INPUT FROM value(lv-file2) NO-ECHO.
    REPEAT:
        CREATE tt-vend.
        IMPORT tt-vend.
        find first e-item-vend where e-item-vend.company = tt-vend.company
                                 AND e-item-vend.i-no = tt-vend.i-no
                                 AND e-item-vend.vend-no = tt-vend.vend-no
                        no-error.
        IF NOT AVAIL e-item-vend THEN CREATE e-item-vend.
        BUFFER-COPY tt-vend TO e-item-vend.
    END.

    SESSION:SET-WAIT-STATE("").
    MESSAGE "Ink Loading is completed." VIEW-AS ALERT-BOX.
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
  lv-file = ".\inks.dat".
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
  ENABLE lv-file Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

