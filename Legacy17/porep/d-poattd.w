&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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
DEF INPUT PARAM ip-cocode   AS CHAR NO-UNDO.
DEF INPUT PARAM ip-start-po AS CHAR NO-UNDO.
DEF INPUT PARAM ip-end-po   AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-outfile AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF BUFFER bf-attach FOR attach.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME br_attach

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ATTACH

/* Definitions for BROWSE br_attach                                     */
&Scoped-define FIELDS-IN-QUERY-br_attach attach.attach-file   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_attach   
&Scoped-define SELF-NAME br_attach
&Scoped-define QUERY-STRING-br_attach FOR EACH ATTACH NO-LOCK     WHERE attach.company EQ ip-cocode       AND attach.est-no  GE ip-start-po       AND attach.est-no  LE ip-end-po
&Scoped-define OPEN-QUERY-br_attach OPEN QUERY {&SELF-NAME} FOR EACH ATTACH NO-LOCK     WHERE attach.company EQ ip-cocode       AND attach.est-no  GE ip-start-po       AND attach.est-no  LE ip-end-po.
&Scoped-define TABLES-IN-QUERY-br_attach ATTACH
&Scoped-define FIRST-TABLE-IN-QUERY-br_attach ATTACH


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-br_attach}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_attach RECT-8 RECT-9 RECT-10 tgl_select ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tgl_select 

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

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 2.14.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 12.86.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 2.38.

DEFINE VARIABLE tgl_select AS LOGICAL INITIAL yes 
     LABEL "Select All file" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY 1.43 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_attach FOR 
      ATTACH SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_attach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_attach Dialog-Frame _FREEFORM
  QUERY br_attach DISPLAY
      attach.attach-file
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-ASSIGN NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 71 BY 12.86
         TITLE "Select Attachment Files" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     br_attach AT ROW 1 COL 1 WIDGET-ID 200
     tgl_select AT ROW 14.48 COL 5.4 WIDGET-ID 8
     Btn_OK AT ROW 14.57 COL 35.2
     Btn_Cancel AT ROW 14.57 COL 52.8
     RECT-8 AT ROW 1 COL 1 WIDGET-ID 2
     RECT-9 AT ROW 13.86 COL 1 WIDGET-ID 4
     RECT-10 AT ROW 14.1 COL 2 WIDGET-ID 10
     SPACE(39.79) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select attachments"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
/* BROWSE-TAB br_attach 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_attach
/* Query rebuild information for BROWSE br_attach
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ATTACH NO-LOCK
    WHERE attach.company EQ ip-cocode
      AND attach.est-no  GE ip-start-po
      AND attach.est-no  LE ip-end-po.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_attach */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select attachments */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:

   ASSIGN tgl_select.

   IF tgl_select 
     THEN RUN get_all_attach.
     
     ELSE RUN get_sel_attach.     
   
   LEAVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgl_select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl_select Dialog-Frame
ON VALUE-CHANGED OF tgl_select IN FRAME Dialog-Frame /* Select All file */
DO:
    ASSIGN tgl_select.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_attach
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
  DISPLAY tgl_select 
      WITH FRAME Dialog-Frame.
  ENABLE br_attach RECT-8 RECT-9 RECT-10 tgl_select Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get_all_attach Dialog-Frame 
PROCEDURE get_all_attach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH bf-attach NO-LOCK
    WHERE bf-attach.company EQ ip-cocode
      AND TRIM(bf-attach.est-no) GE ip-start-po
      AND TRIM(bf-attach.est-no) LE ip-end-po:

    ASSIGN 
        op-outfile = op-outfile + TRIM(bf-attach.attach-file) + "," .

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get_sel_attach Dialog-Frame 
PROCEDURE get_sel_attach :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-lcnt AS INT NO-UNDO.


IF br_attach:NUM-SELECTED-ROWS IN FRAME Dialog-Frame GT 0 THEN
  DO v-lcnt = 1 TO br_attach:NUM-SELECTED-ROWS IN FRAME Dialog-Frame:

    {&browse-name}:FETCH-SELECTED-ROW (v-lcnt) NO-ERROR.

    IF AVAIL attach THEN DO:

        ASSIGN 
             op-outfile = op-outfile + TRIM(attach.attach-file) + ",".

       END.

   END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

