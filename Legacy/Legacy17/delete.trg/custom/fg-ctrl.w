&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg-ctrl.w.w

  Description: F/G Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-ctrl

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME fg-ctrl.inv-meth 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH fg-ctrl SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH fg-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME fg-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME fg-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 Btn_Update Btn_Close 
&Scoped-Define DISPLAYED-FIELDS fg-ctrl.inv-meth 
&Scoped-define DISPLAYED-TABLES fg-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE fg-ctrl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 fg-ctrl.inv-meth 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 1.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      fg-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fg-ctrl.inv-meth AT ROW 1.24 COL 28 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Average", "A":U,
"Last", "L":U,
"Standard", "S":U
          SIZE 33.2 BY .81
     Btn_Update AT ROW 2.43 COL 2 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 2.43 COL 18 HELP
          "Cancel Update or Close Window"
     "Finished Goods Valuation:" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.24 COL 2
     RECT-15 AT ROW 2.19 COL 1
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 61.2 BY 2.91.


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
         TITLE              = "F/G Control"
         HEIGHT             = 2.91
         WIDTH              = 61.2
         MAX-HEIGHT         = 2.91
         MAX-WIDTH          = 61.2
         VIRTUAL-HEIGHT     = 2.91
         VIRTUAL-WIDTH      = 61.2
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
   FRAME-NAME                                                           */
ASSIGN 
       Btn_Close:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       Btn_Update:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR RADIO-SET fg-ctrl.inv-meth IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RECTANGLE RECT-15 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "ASI.fg-ctrl"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* F/G Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* F/G Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    RUN enable_UI.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME DEFAULT-FRAME /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    {methods/setButton.i Btn_Update "Save"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Close "Cancel"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    APPLY "ENTRY" TO fg-ctrl.inv-meth.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1}.
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    ASSIGN {&LIST-1}.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND fg-ctrl WHERE fg-ctrl.company = gcompany NO-LOCK NO-ERROR.
  RUN enable_UI.
  {methods/nowait.i}
    {methods/setButton.i Btn_Close "Close"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {methods/setButton.i Btn_Update "Update"} /* added by script _nonAdm1Images1.p on 04.07.2017 @  2:07:13 pm */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.07.2017 @  2:06:29 pm */
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE fg-ctrl THEN 
    DISPLAY fg-ctrl.inv-meth 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-16 Btn_Update Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

