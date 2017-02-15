&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/fixorders.w

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
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = gcompany.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS from-order to-order from-item to-item btnOk ~
BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS from-order to-order from-item to-item 

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
     LABEL "Run" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE from-item AS CHARACTER FORMAT "X(15)":U 
     LABEL "From FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE from-order AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "From Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE to-item AS CHARACTER FORMAT "X(15)":U 
     LABEL "To FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE to-order AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "To Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     from-order AT ROW 2.43 COL 18 COLON-ALIGNED
     to-order AT ROW 2.43 COL 53.6 COLON-ALIGNED
     from-item AT ROW 4.57 COL 17 COLON-ALIGNED WIDGET-ID 2
     to-item AT ROW 4.57 COL 53.6 COLON-ALIGNED WIDGET-ID 4
     btnOk AT ROW 7.67 COL 20
     BtnCancel AT ROW 7.67 COL 51
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.8 BY 8.62.


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
         TITLE              = "Update Order Line Item's Invoiced Qty"
         HEIGHT             = 8.62
         WIDTH              = 83.8
         MAX-HEIGHT         = 8.62
         MAX-WIDTH          = 83.8
         VIRTUAL-HEIGHT     = 8.62
         VIRTUAL-WIDTH      = 83.8
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Order Line Item's Invoiced Qty */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Order Line Item's Invoiced Qty */
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
    {src/WinKit/triggerend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Run */
DO:
   DEF VAR iCount AS INT NO-UNDO.
   DEF VAR iInvQty AS INT NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      SESSION:SET-WAIT-STATE ("general").

      ASSIGN from-order to-order from-item to-item.
      IF from-order = 0 AND to-order = 0 THEN
         ASSIGN to-order = 999999
                to-order:SCREEN-VALUE = "999999".

      FOR EACH oe-ordl WHERE oe-ordl.company EQ cocode AND
             oe-ordl.ord-no >= from-order AND
             oe-ordl.ord-no <= to-order AND
             oe-ordl.i-no >= from-item AND
             oe-ordl.i-no <= to-item EXCLUSIVE-LOCK.

          iInvQty = 0.
          FOR EACH ar-invl WHERE ar-invl.company = cocode 
                             AND ar-invl.ord-no = oe-ordl.ord-no
                             AND ar-invl.i-no = oe-ordl.i-no NO-LOCK:
              iInvQty = iInvQty + ar-invl.inv-qty.
          END.
          FOR EACH inv-line WHERE inv-line.company = cocode
                              AND inv-line.ord-no = oe-ordl.ord-no
                              AND inv-line.i-no = oe-ordl.i-no NO-LOCK:
              iInvQty = iInvQty + inv-line.inv-qty.
          END.
          ASSIGN oe-ordl.t-inv-qty = iInvQty
                 oe-ordl.inv-qty = iInvQty
                 iCount = iCount + 1.

      END.


      MESSAGE iCount "Order Line Items Were Update."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      SESSION:SET-WAIT-STATE ("").
   END.
    {src/WinKit/triggerend.i}
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
   {Advantzware/WinKit/closewindow-nonadm.i}
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

    {Advantzware/WinKit/embedfinalize-nonadm.i}
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
  DISPLAY from-order to-order from-item to-item 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE from-order to-order from-item to-item btnOk BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

