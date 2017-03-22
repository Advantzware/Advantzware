&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\upd-box.p

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-t v-3d tg_estimates BtnCancel BtnOK 
&Scoped-Define DISPLAYED-OBJECTS v-t v-3d tg_estimates 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE v-3d AS CHARACTER FORMAT "X(256)":U 
     LABEL "3D Image" 
     VIEW-AS FILL-IN 
     SIZE 65.2 BY 1 NO-UNDO.

DEFINE VARIABLE v-t AS CHARACTER FORMAT "X(256)":U 
     LABEL "2D Image" 
     VIEW-AS FILL-IN 
     SIZE 65.2 BY 1 NO-UNDO.

DEFINE VARIABLE tg_estimates AS LOGICAL INITIAL no 
     LABEL "Update Estimates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     v-t AT ROW 2.14 COL 12 COLON-ALIGNED
     v-3d AT ROW 3.67 COL 12 COLON-ALIGNED
     tg_estimates AT ROW 4.81 COL 15 WIDGET-ID 2
     BtnCancel AT ROW 5.81 COL 52.2
     BtnOK AT ROW 5.91 COL 19.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 7.14
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Change Folder Name for Box Image"
         HEIGHT             = 7.14
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Change Folder Name for Box Image */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Change Folder Name for Box Image */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
DO:  
   DEF VAR v-image-file AS cha NO-UNDO.
  
   ASSIGN {&displayed-objects}.

   IF v-t = "" OR v-3d = "" THEN DO:
      MESSAGE "Folder name must be entered." VIEW-AS ALERT-BOX ERROR.
      IF v-t = "" THEN APPLY "entry" TO v-t.
      ELSE APPLY "entry" TO v-3d.
      RETURN.
   END.
   IF substring(v-t,LENGTH(v-t),1) <> "\" AND
      substring(v-t,LENGTH(v-t),1) <> "/" THEN do:
      MESSAGE "Enter back slash(\) at the end." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO v-t.
      RETURN.
   END.
   ELSE IF substring(v-3d,LENGTH(v-3d),1) <> "\" AND
      substring(v-3d,LENGTH(v-3d),1) <> "/" THEN do:
      MESSAGE "Enter back slash(\) at the end." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO v-3d.
      RETURN.
   END.


    MESSAGE "Are you ready to update?" VIEW-AS ALERT-BOX QUESTION
        BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN
    DO:
    
    IF tg_estimates = NO THEN
    FOR EACH box-design-hdr WHERE
        box-design-hdr.design-no NE 0:

        IF box-design-hdr.box-image <> "" THEN DO: 
        IF R-INDEX(box-design-hdr.box-image,"/") <> 0 THEN
           v-image-file = SUBSTRING(box-design-hdr.box-image,R-INDEX(box-design-hdr.box-image,"/") + 1,LENGTH(box-design-hdr.box-image)).
        else IF R-INDEX(box-design-hdr.box-image,"\") <> 0 THEN
           v-image-file = SUBSTRING(box-design-hdr.box-image,R-INDEX(box-design-hdr.box-image,"\") + 1,LENGTH(box-design-hdr.box-image)).

        ASSIGN box-design-hdr.box-image = v-t + v-image-file.
     END.

     IF box-design-hdr.box-3d-image <> "" THEN DO:
        IF R-INDEX(box-design-hdr.box-3d-image,"/") <> 0 THEN
           v-image-file = SUBSTRING(box-design-hdr.box-3d-image,R-INDEX(box-design-hdr.box-3d-image,"/") + 1,LENGTH(box-design-hdr.box-3d-image)).
        else IF R-INDEX(box-design-hdr.box-3d-image,"\") <> 0 THEN
           v-image-file = SUBSTRING(box-design-hdr.box-3d-image,R-INDEX(box-design-hdr.box-3d-image,"\") + 1,LENGTH(box-design-hdr.box-3d-image)).

         ASSIGN box-design-hdr.box-3d-image = v-3d + v-image-file.
     END.

     DISP box-design-hdr.box-image FORM "x(40)".
     PAUSE 0.

    END.
    ELSE
    FOR EACH box-design-hdr:
        IF box-design-hdr.box-image <> "" THEN DO: 
           IF R-INDEX(box-design-hdr.box-image,"/") <> 0 THEN
              v-image-file = SUBSTRING(box-design-hdr.box-image,R-INDEX(box-design-hdr.box-image,"/") + 1,LENGTH(box-design-hdr.box-image)).
           else IF R-INDEX(box-design-hdr.box-image,"\") <> 0 THEN
              v-image-file = SUBSTRING(box-design-hdr.box-image,R-INDEX(box-design-hdr.box-image,"\") + 1,LENGTH(box-design-hdr.box-image)).

           ASSIGN box-design-hdr.box-image = v-t + v-image-file.
        END.

        IF box-design-hdr.box-3d-image <> "" THEN DO:
           IF R-INDEX(box-design-hdr.box-3d-image,"/") <> 0 THEN
              v-image-file = SUBSTRING(box-design-hdr.box-3d-image,R-INDEX(box-design-hdr.box-3d-image,"/") + 1,LENGTH(box-design-hdr.box-3d-image)).
           else IF R-INDEX(box-design-hdr.box-3d-image,"\") <> 0 THEN
              v-image-file = SUBSTRING(box-design-hdr.box-3d-image,R-INDEX(box-design-hdr.box-3d-image,"\") + 1,LENGTH(box-design-hdr.box-3d-image)).

            ASSIGN box-design-hdr.box-3d-image = v-3d + v-image-file.
        END.

        DISP box-design-hdr.box-image FORM "x(40)".
        PAUSE 0.
    END.
    END.
    MESSAGE "Process is completed." VIEW-AS ALERT-BOX INFORMATION.
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY v-t v-3d tg_estimates 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE v-t v-3d tg_estimates BtnCancel BtnOK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

