&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : util/UpdateImagePath.w

  Description       : Updates File Path of a Box Image.

  Input Parameters  : <none>

  Output Parameters : <none>

  Author            : Dennis G. Dizon

  Created           : Apr 2, 2007

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
&SCOPED-DEFINE DontValidateError /* added by script _dontValidatePanels.p */

/* Variables */
DEFINE VARIABLE vcStartingPath  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcNewImagePath  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE viRecordCount   AS INTEGER    NO-UNDO.
DEFINE VARIABLE vcBegEstNo      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcEndEstNo      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcStatusDefault AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-41 RECT-42 cbImageType fi_BegEstNo ~
fi_EndEstNo fi_OldPath fi_NewPath BtnCancel btnOk 
&Scoped-Define DISPLAYED-OBJECTS cbImageType fi_BegEstNo fi_EndEstNo ~
fi_OldPath fi_NewPath 

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

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbImageType AS CHARACTER FORMAT "X(256)":U INITIAL "CAD" 
     LABEL "Image Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "CAD","DIE" 
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_BegEstNo AS CHARACTER FORMAT "x(8)":U 
     LABEL "From Est. #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_EndEstNo AS CHARACTER FORMAT "x(8)":U INITIAL "ZZZZZZZZ" 
     LABEL "To Est. #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_NewPath AS CHARACTER FORMAT "x(40)":U INITIAL "c:~\asiimage" 
     LABEL "New Path" 
     VIEW-AS FILL-IN 
     SIZE 41.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi_OldPath AS CHARACTER FORMAT "x(40)":U INITIAL "z:~\rcode~\asiimage" 
     LABEL "Old Path" 
     VIEW-AS FILL-IN 
     SIZE 41.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 5.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 3.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbImageType AT ROW 2.91 COL 25 COLON-ALIGNED
     fi_BegEstNo AT ROW 4.14 COL 25 COLON-ALIGNED
     fi_EndEstNo AT ROW 5.38 COL 25 COLON-ALIGNED
     fi_OldPath AT ROW 8.62 COL 14.6 COLON-ALIGNED
     fi_NewPath AT ROW 9.91 COL 14.6 COLON-ALIGNED
     BtnCancel AT ROW 12.14 COL 36
     btnOk AT ROW 12.19 COL 14
     " Estimate Range" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.95 COL 6.6
     " Directory" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 7.57 COL 6.6
     RECT-41 AT ROW 2.19 COL 5
     RECT-42 AT ROW 7.86 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.4 BY 12.62.


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
         TITLE              = "Update Image Path"
         HEIGHT             = 12.62
         WIDTH              = 64.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN
       BtnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       btnOk:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Image Path */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Image Path */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* OK */
DO:
  /* Clean up input data. */
  assign 
    vcBegEstNo      = fill (" ",8 - length (trim (fi_BegEstNo:screen-value))) +
                                            trim (fi_BegEstNo:screen-value)
    vcEndEstNo      = fill (" ",8 - length (trim (fi_EndEstNo:screen-value))) +
                                            trim (fi_EndEstNo:screen-value)
    vcStartingPath  = right-trim (fi_OldPath:screen-value, '~/~\')
    vcNewImagePath  = right-trim (fi_NewPath:screen-value, '~/~\')
    viRecordCount   = 0.

  /* Go through all matching records. */
  if cbImageType:screen-value = 'CAD' then
  do:
    for each box-design-hdr
       where box-design-hdr.box-image > ''
         and box-design-hdr.box-image begins vcStartingPath,
        each eb no-lock
       where eb.company = box-design-hdr.company
         and eb.est-no  = box-design-hdr.est-no
         and eb.form-no = box-design-hdr.form-no
         and eb.cad-no <> '':
      run UpdateImagePath.
    end.
  end.
  else
  do:
    for each box-design-hdr
       where box-design-hdr.box-image > ''
         and box-design-hdr.box-image begins vcStartingPath,
        each eb no-lock
       where eb.company = box-design-hdr.company
         and eb.est-no  = box-design-hdr.est-no
         and eb.form-no = box-design-hdr.form-no
         and eb.die-no <> '':
      run UpdateImagePath.
    end.
  end.

  /* Let user know how many records were updated. */
/*   vcStatusDefault = 'Modified ' + string (viRecordCount) + ' records.'. */
/*   status default vcStatusDefault.                                       */
  MESSAGE 'Modified ' viRecordCount ' record(s)!'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbImageType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbImageType C-Win
ON VALUE-CHANGED OF cbImageType IN FRAME DEFAULT-FRAME /* Image Type */
DO:
  find first sys-ctrl no-lock
       where sys-ctrl.company > ''
         and sys-ctrl.name = cbImageType:screen-value + 'FILE' no-error.

  if avail sys-ctrl then
    assign fi_NewPath:screen-value = sys-ctrl.char-fld.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  apply 'value-changed':u to cbImageType.

    {methods/setButton.i BtnCancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btnOk "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY cbImageType fi_BegEstNo fi_EndEstNo fi_OldPath fi_NewPath 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-41 RECT-42 cbImageType fi_BegEstNo fi_EndEstNo fi_OldPath 
         fi_NewPath BtnCancel btnOk 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateImagePath C-Win 
PROCEDURE UpdateImagePath :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if  not (vcBegEstNo = ''  or
           vcEndEstNo = '') and
      not (box-design-hdr.est-no >= vcBegEstNo  and
           box-design-hdr.est-no <= vcEndEstNo) then next.

  /* Keep track of record count and inform user. */
  assign 
    viRecordCount   = viRecordCount + 1
    vcStatusDefault = 'Record(s) : ' + string (viRecordCount) + 
                      ' Est #: '     + box-design-hdr.est-no  + 
                      ' Path: ' + box-design-hdr.box-image.
  status default vcStatusDefault.

  /* Update the Box Image Path. */
  box-design-hdr.box-image = caps (vcNewImagePath) + substring (box-design-hdr.box-image, length (vcStartingPath) + 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

