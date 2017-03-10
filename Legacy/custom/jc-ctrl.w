&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jc-ctrl.w.w

  Description: J/C Control File

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
&Scoped-define FRAME-NAME jc-ctrl

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-15 RECT-16 Btn_Update Btn_Close 
&Scoped-Define DISPLAYED-FIELDS jc-ctrl.post[1] jc-ctrl.post[16] ~
jc-ctrl.post[2] jc-ctrl.post[17] jc-ctrl.post[3] jc-ctrl.post[18] ~
jc-ctrl.post[4] jc-ctrl.post[19] jc-ctrl.post[5] jc-ctrl.post[20] ~
jc-ctrl.post[6] jc-ctrl.post[21] jc-ctrl.post[7] jc-ctrl.post[22] ~
jc-ctrl.post[8] jc-ctrl.post[23] jc-ctrl.post[9] jc-ctrl.post[24] ~
jc-ctrl.post[10] jc-ctrl.post[25] jc-ctrl.post[11] jc-ctrl.post[12] ~
jc-ctrl.post[13] jc-ctrl.post[14] jc-ctrl.post[15] 
&Scoped-define DISPLAYED-TABLES jc-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE jc-ctrl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 jc-ctrl.post[1] jc-ctrl.post[16] jc-ctrl.post[2] ~
jc-ctrl.post[17] jc-ctrl.post[3] jc-ctrl.post[18] jc-ctrl.post[4] ~
jc-ctrl.post[19] jc-ctrl.post[5] jc-ctrl.post[20] jc-ctrl.post[6] ~
jc-ctrl.post[21] jc-ctrl.post[7] jc-ctrl.post[22] jc-ctrl.post[8] ~
jc-ctrl.post[23] jc-ctrl.post[9] jc-ctrl.post[24] jc-ctrl.post[10] ~
jc-ctrl.post[25] jc-ctrl.post[11] jc-ctrl.post[12] jc-ctrl.post[13] ~
jc-ctrl.post[14] jc-ctrl.post[15] 

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
     SIZE 40 BY 13.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME jc-ctrl
     jc-ctrl.post[1] AT ROW 1.95 COL 3.4
          LABEL "Case"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     jc-ctrl.post[16] AT ROW 2 COL 18.2 HELP
          "" WIDGET-ID 6
          LABEL "Exp. Polystyrene"
          VIEW-AS TOGGLE-BOX
          SIZE 19.8 BY .81
     jc-ctrl.post[2] AT ROW 2.76 COL 3.4
          LABEL "Pallet"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     jc-ctrl.post[17] AT ROW 2.81 COL 18.2 HELP
          "" WIDGET-ID 8
          LABEL "Polystyrene"
          VIEW-AS TOGGLE-BOX
          SIZE 15.8 BY .81
     jc-ctrl.post[3] AT ROW 3.57 COL 3.4
          LABEL "Foil"
          VIEW-AS TOGGLE-BOX
          SIZE 8 BY .81
     jc-ctrl.post[18] AT ROW 3.62 COL 18.2 HELP
          "" WIDGET-ID 10
          LABEL "Layer Pad"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     jc-ctrl.post[4] AT ROW 4.38 COL 3.4
          LABEL "Glue"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     jc-ctrl.post[19] AT ROW 4.43 COL 18.2 HELP
          "" WIDGET-ID 12
          LABEL "Divider"
          VIEW-AS TOGGLE-BOX
          SIZE 11.8 BY .81
     jc-ctrl.post[5] AT ROW 5.19 COL 3.4
          LABEL "Ink"
          VIEW-AS TOGGLE-BOX
          SIZE 8 BY .81
     jc-ctrl.post[20] AT ROW 5.24 COL 18.2 HELP
          "" WIDGET-ID 14
          LABEL "Printing Die/ Plates"
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .81
     jc-ctrl.post[6] AT ROW 6 COL 3.4
          LABEL "Laminate"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     jc-ctrl.post[21] AT ROW 6.05 COL 18.2 HELP
          "" WIDGET-ID 16
          LABEL "Cutting Dies"
          VIEW-AS TOGGLE-BOX
          SIZE 15.8 BY .81
     jc-ctrl.post[7] AT ROW 6.81 COL 3.4
          LABEL "Misc."
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     jc-ctrl.post[22] AT ROW 6.86 COL 18.2 HELP
          "" WIDGET-ID 18
          LABEL "Wood"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     jc-ctrl.post[8] AT ROW 7.62 COL 3.4
          LABEL "Paper"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .81
     jc-ctrl.post[23] AT ROW 7.67 COL 18.2 HELP
          "" WIDGET-ID 20
          LABEL "Rotary Die"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     jc-ctrl.post[9] AT ROW 8.43 COL 3.4
          LABEL "Die Rule"
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .81
     jc-ctrl.post[24] AT ROW 8.48 COL 18.2 HELP
          "" WIDGET-ID 22
          LABEL "Flat Die"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     jc-ctrl.post[10] AT ROW 9.24 COL 3.4
          LABEL "Tray"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     jc-ctrl.post[25] AT ROW 9.24 COL 18.2 HELP
          "Should this Material Type be Automatically Posted." WIDGET-ID 24
          LABEL "@ Misc."
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     jc-ctrl.post[11] AT ROW 10.05 COL 3.4
          LABEL "Varnish"
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .81
     jc-ctrl.post[12] AT ROW 10.86 COL 3.4
          LABEL "Window"
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .81
     jc-ctrl.post[13] AT ROW 11.67 COL 3.4
          LABEL "Real Board"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 40.2 BY 15.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME jc-ctrl
     jc-ctrl.post[14] AT ROW 12.43 COL 3.4 HELP
          "Should this Material Type be Automatically Posted." WIDGET-ID 2
          LABEL "Polyethylene"
          VIEW-AS TOGGLE-BOX
          SIZE 16.6 BY .81
     jc-ctrl.post[15] AT ROW 13.29 COL 3.4 HELP
          "Should this Material Type be Automatically Posted." WIDGET-ID 4
          LABEL "Polyurethane"
          VIEW-AS TOGGLE-BOX
          SIZE 17.6 BY .81
     Btn_Update AT ROW 14.57 COL 3 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 14.57 COL 19 HELP
          "Cancel Update or Close Window"
     "Automatically Post Material's" VIEW-AS TEXT
          SIZE 33 BY .62 AT ROW 1.24 COL 2
          FONT 6
     RECT-15 AT ROW 14.43 COL 2
     RECT-16 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 40.2 BY 15.1.


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
         TITLE              = "J/C Control"
         HEIGHT             = 15.1
         WIDTH              = 40.2
         MAX-HEIGHT         = 33
         MAX-WIDTH          = 272.8
         VIRTUAL-HEIGHT     = 33
         VIRTUAL-WIDTH      = 272.8
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME jc-ctrl
   FRAME-NAME                                                           */
ASSIGN
       Btn_Close:PRIVATE-DATA IN FRAME jc-ctrl     = 
                "ribbon-button".


ASSIGN
       Btn_Update:PRIVATE-DATA IN FRAME jc-ctrl     = 
                "ribbon-button".


/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[10] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[11] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[12] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[13] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[14] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[15] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[16] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[17] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[18] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[19] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[1] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[20] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[21] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[22] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[23] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[24] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[25] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-HELP                                       */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[2] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[3] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[4] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[5] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[6] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[7] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[8] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX jc-ctrl.post[9] IN FRAME jc-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* J/C Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* J/C Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME jc-ctrl /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Close"
      Btn_Update:LABEL = "&Update".
    RUN enable_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME jc-ctrl /* Update */
DO:
  IF {&SELF-NAME}:LABEL EQ "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Save"
      Btn_Close:LABEL    = "&Cancel".
    APPLY "ENTRY" TO jc-ctrl.post[1].
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Update"
      Btn_Close:LABEL    = "&Close".
    FIND CURRENT jc-ctrl EXCLUSIVE-LOCK.
    ASSIGN {&LIST-1}.
    FIND CURRENT jc-ctrl NO-LOCK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.

  IF NOT CAN-FIND(FIRST jc-ctrl WHERE jc-ctrl.company EQ gcompany) THEN DO:
    CREATE jc-ctrl.
    jc-ctrl.company = gcompany.
  END.
  FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ gcompany NO-LOCK NO-ERROR.

  RUN enable_UI.
  {methods/nowait.i}
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
  IF AVAILABLE jc-ctrl THEN 
    DISPLAY jc-ctrl.post[1] jc-ctrl.post[16] jc-ctrl.post[2] jc-ctrl.post[17] 
          jc-ctrl.post[3] jc-ctrl.post[18] jc-ctrl.post[4] jc-ctrl.post[19] 
          jc-ctrl.post[5] jc-ctrl.post[20] jc-ctrl.post[6] jc-ctrl.post[21] 
          jc-ctrl.post[7] jc-ctrl.post[22] jc-ctrl.post[8] jc-ctrl.post[23] 
          jc-ctrl.post[9] jc-ctrl.post[24] jc-ctrl.post[10] jc-ctrl.post[25] 
          jc-ctrl.post[11] jc-ctrl.post[12] jc-ctrl.post[13] jc-ctrl.post[14] 
          jc-ctrl.post[15] 
      WITH FRAME jc-ctrl IN WINDOW C-Win.
  ENABLE RECT-15 RECT-16 Btn_Update Btn_Close 
      WITH FRAME jc-ctrl IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-jc-ctrl}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

