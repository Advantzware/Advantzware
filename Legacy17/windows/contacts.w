&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : windows\contacts.w

  Description       : Customer Contacts Browser

  Input Parameters  : None

  Output Parameters : None

  Author            : Dennis G. Dizon

  Created           : 05/30/2007

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

/* Parameters */
&IF DEFINED (UIB_Is_Running) = 0 &THEN
/*   DEFINE INPUT PARAM icCompany      AS CHARACTER  NO-UNDO. */
/*   DEFINE INPUT PARAM icCustNo       AS CHARACTER  NO-UNDO. */
  DEFINE INPUT PARAM icShipRecKey   AS CHARACTER  NO-UNDO.
&ELSE
/*   DEFINE VARIABLE    icCompany      AS CHARACTER  NO-UNDO INIT '001'.     */
/*   DEFINE VARIABLE    icCustNo       AS CHARACTER  NO-UNDO INIT 'HER1000'. */
  DEFINE VARIABLE    icShipRecKey   AS CHARACTER  NO-UNDO INIT '0305200200044404'.
&ENDIF

/* Temp-Tables */
DEFINE TEMP-TABLE ttContacts        LIKE NOSWEAT.phone.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brContacts

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttContacts

/* Definitions for BROWSE brContacts                                    */
&Scoped-define FIELDS-IN-QUERY-brContacts ttContacts.attention ttContacts.e_mail ttContacts.titlcode ttContacts.phone_ctry_code ttContacts.phone_city_code ttContacts.phone ttContacts.phone_ext /* ttContacts.table_rec_key */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brContacts   
&Scoped-define SELF-NAME brContacts
&Scoped-define QUERY-STRING-brContacts FOR EACH ttContacts NO-LOCK
&Scoped-define OPEN-QUERY-brContacts OPEN QUERY {&SELF-NAME}   FOR EACH ttContacts NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brContacts ttContacts
&Scoped-define FIRST-TABLE-IN-QUERY-brContacts ttContacts


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brContacts}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 brContacts btnSelect btnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSelect 
     LABEL "&Add" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 1.91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brContacts FOR 
      ttContacts SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brContacts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brContacts C-Win _FREEFORM
  QUERY brContacts NO-LOCK DISPLAY
      ttContacts.attention      FORMAT 'x(20)'
      ttContacts.e_mail         FORMAT 'x(40)'
      ttContacts.titlcode       FORMAT 'x(12)'
      ttContacts.phone_ctry_code                 
      ttContacts.phone_city_code                 
      ttContacts.phone                           
      ttContacts.phone_ext                       
/*       ttContacts.table_rec_key  FORMAT 'x(20)' */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 113 BY 12.38
         TITLE "Customer Contacts" FIT-LAST-COLUMN TOOLTIP "Click on Contacts you wish to add. Press and hold 'CTRL' key to multi-select.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brContacts AT ROW 1.81 COL 4
     btnSelect AT ROW 14.81 COL 41
     btnCancel AT ROW 14.81 COL 59
     RECT-5 AT ROW 14.43 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119 BY 15.76
         CANCEL-BUTTON btnCancel.


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
         TITLE              = "Customer Contacts Multi-Selector"
         HEIGHT             = 15.76
         WIDTH              = 119
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 119
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 119
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
       btnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       btnSelect:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


/* BROWSE-TAB brContacts RECT-5 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brContacts
/* Query rebuild information for BROWSE brContacts
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
  FOR EACH ttContacts NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brContacts */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer Contacts Multi-Selector */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer Contacts Multi-Selector */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect C-Win
ON CHOOSE OF btnSelect IN FRAME DEFAULT-FRAME /* Add */
DO:
  DEFINE VARIABLE i   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE X   AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE i2  AS INTEGER    NO-UNDO.

  DO i = 1 TO brContacts:NUM-SELECTED-ROWS:
     x = brContacts:FETCH-SELECTED-ROW(i).
     IF x THEN DO:
       CREATE NOSWEAT.phone.
       BUFFER-COPY ttContacts TO NOSWEAT.phone.
       i2 = i2 + 1.
     END.
 END.

 MESSAGE 'Added ' i2 ' contact(s).'
   VIEW-AS ALERT-BOX INFO BUTTONS OK.

 APPLY 'CLOSE':U TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brContacts
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

  STATUS DEFAULT "For Multi-Select: Press and hold 'CTRL' key before clicking.".

  RUN enable_UI.

  RUN Initialize (icShipRecKey).

    {methods/setButton.i btnCancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btnSelect "Add"} /* added by script _nonAdm1Images1.p */
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
  ENABLE RECT-5 brContacts btnSelect btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialize C-Win 
PROCEDURE Initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   DEFINE INPUT PARAM icCompany    AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAM icCustNo     AS CHARACTER NO-UNDO. */
  DEFINE INPUT PARAM icShipRecKey AS CHARACTER  NO-UNDO.
  DEFINE BUFFER bf-cust FOR asi.cust.

  FIND FIRST bf-cust WHERE bf-cust.rec_key = icShipRecKey NO-LOCK NO-ERROR.
  IF AVAIL bf-cust THEN DO:
      FOR EACH  ASI.cust             NO-LOCK
          WHERE  ASI.cust.company   = bf-cust.company
            AND ASI.cust.active    = 'X',
           EACH  NOSWEAT.phone        NO-LOCK
          WHERE  NOSWEAT.phone.TABLE_rec_key = ASI.cust.rec_key:

        IF NOT CAN-FIND (FIRST ttContacts 
                         WHERE ttContacts.table_rec_key = icShipRecKey
                           AND ttContacts.attention     = NOSWEAT.phone.attention) 
        THEN DO:
             CREATE ttContacts.
             BUFFER-COPY NOSWEAT.phone TO ttContacts.
             ASSIGN ttContacts.table_rec_key = icShipRecKey.
        END.
      END.
  END.
  ELSE DO:
      FOR FIRST  ASI.shipto           NO-LOCK
          WHERE  ASI.shipto.rec_key = icShipRecKey,
           EACH  ASI.cust             NO-LOCK
          WHERE  ASI.cust.company   = ASI.shipto.company
            AND (ASI.cust.cust-no   = ASI.shipto.cust-no
             /*OR  ASI.cust.active    = 'X'*/),
           EACH  NOSWEAT.phone        NO-LOCK
          WHERE  NOSWEAT.phone.TABLE_rec_key = ASI.cust.rec_key:

        IF NOT CAN-FIND (FIRST ttContacts 
                         WHERE ttContacts.table_rec_key = icShipRecKey
                           AND ttContacts.attention     = NOSWEAT.phone.attention) 
        THEN DO:
             CREATE ttContacts.
             BUFFER-COPY NOSWEAT.phone TO ttContacts.
             ASSIGN ttContacts.table_rec_key = icShipRecKey.
        END.
      END.
  END.
  {&OPEN-QUERY-brContacts}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

