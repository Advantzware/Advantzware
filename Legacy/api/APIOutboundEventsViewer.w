&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: api/APIOutboundEventsViewer.w

  Description: Display all the API Outbound events

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Vishnu Vellanki

  Created: 06th June 2019

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
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES APIOutboundEvent

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 APIOutboundEvent.apiID APIOutboundEvent.callingProgram APIOutboundEvent.requestDateTime APIOutboundEvent.success   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH APIOutboundEvent NO-LOCK     WHERE (IF cbAPIID:SCREEN-VALUE EQ "All" THEN                TRUE            ELSE                APIOutboundEvent.apiID       EQ cbAPIID:SCREEN-VALUE)       AND (IF fiRequestDate:SCREEN-VALUE EQ ?  OR               fiRequestDate:SCREEN-VALUE EQ "" OR               fiRequestDate:SCREEN-VALUE EQ "/  /" THEN                TRUE            ELSE                DATE(APIOutboundEvent.requestDateTime) EQ DATE(fiRequestDate:SCREEN-VALUE))       AND (IF cbSuccess:SCREEN-VALUE = "ALL" THEN                TRUE            ELSE IF cbSuccess:SCREEN-VALUE = "SUCCESS" THEN                APIOutboundEvent.success = TRUE            ELSE                APIOutboundEvent.success = FALSE)       BY APIOutboundEvent.requestDateTime DESCENDING
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH APIOutboundEvent NO-LOCK     WHERE (IF cbAPIID:SCREEN-VALUE EQ "All" THEN                TRUE            ELSE                APIOutboundEvent.apiID       EQ cbAPIID:SCREEN-VALUE)       AND (IF fiRequestDate:SCREEN-VALUE EQ ?  OR               fiRequestDate:SCREEN-VALUE EQ "" OR               fiRequestDate:SCREEN-VALUE EQ "/  /" THEN                TRUE            ELSE                DATE(APIOutboundEvent.requestDateTime) EQ DATE(fiRequestDate:SCREEN-VALUE))       AND (IF cbSuccess:SCREEN-VALUE = "ALL" THEN                TRUE            ELSE IF cbSuccess:SCREEN-VALUE = "SUCCESS" THEN                APIOutboundEvent.success = TRUE            ELSE                APIOutboundEvent.success = FALSE)       BY APIOutboundEvent.requestDateTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 APIOutboundEvent
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 APIOutboundEvent


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExit btFilter cbAPIId fiRequestDate ~
cbSuccess BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fiAPIIDLabel cbAPIId fiRequestDatelabel ~
fiRequestDate fiSuccessLabel cbSuccess 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     LABEL "Exit" 
     SIZE 11 BY 2.14.

DEFINE BUTTON btFilter 
     LABEL "Filter" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbAPIId AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","AddCustomer","AddVendor","AddProduct","AddPurchaseOrder","AddPicklist" 
     DROP-DOWN-LIST
     SIZE 20.2 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE cbSuccess AS CHARACTER FORMAT "X(256)":U INITIAL "ALL" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ALL","FAILED","SUCCESS" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fiAPIIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API ID:" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestDate AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiSuccessLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Success:" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FONT 35 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      APIOutboundEvent SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      APIOutboundEvent.apiID COLUMN-LABEL "API ID" FORMAT "x(20)":U
            WIDTH 21.2
      APIOutboundEvent.callingProgram COLUMN-LABEL "Calling Program" FORMAT "x(45)":U
            WIDTH 46.2
      APIOutboundEvent.requestDateTime COLUMN-LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 29.2
      APIOutboundEvent.success COLUMN-LABEL "Success" FORMAT "SUCCESS/FAILED":U
            WIDTH 31.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 135 BY 13.71
         FONT 34 ROW-HEIGHT-CHARS .7 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExit AT ROW 1.52 COL 130.2 WIDGET-ID 2
     btFilter AT ROW 2.1 COL 111 WIDGET-ID 18
     fiAPIIDLabel AT ROW 2.19 COL 4.4 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     cbAPIId AT ROW 2.19 COL 16.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiRequestDatelabel AT ROW 2.19 COL 38.2 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiRequestDate AT ROW 2.19 COL 59.6 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiSuccessLabel AT ROW 2.19 COL 76.6 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cbSuccess AT ROW 2.19 COL 90.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     BROWSE-2 AT ROW 4.71 COL 6 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.2 BY 17.91 WIDGET-ID 100.


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
         TITLE              = "API Outbound Event"
         HEIGHT             = 17.91
         WIDTH              = 144.2
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 144.2
         VIRTUAL-HEIGHT     = 21
         VIRTUAL-WIDTH      = 144.2
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
/* BROWSE-TAB BROWSE-2 cbSuccess DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fiAPIIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSuccessLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH APIOutboundEvent NO-LOCK
    WHERE (IF cbAPIID:SCREEN-VALUE EQ "All" THEN
               TRUE
           ELSE
               APIOutboundEvent.apiID       EQ cbAPIID:SCREEN-VALUE)
      AND (IF fiRequestDate:SCREEN-VALUE EQ ?  OR
              fiRequestDate:SCREEN-VALUE EQ "" OR
              fiRequestDate:SCREEN-VALUE EQ "/  /" THEN
               TRUE
           ELSE
               DATE(APIOutboundEvent.requestDateTime) EQ DATE(fiRequestDate:SCREEN-VALUE))
      AND (IF cbSuccess:SCREEN-VALUE = "ALL" THEN
               TRUE
           ELSE IF cbSuccess:SCREEN-VALUE = "SUCCESS" THEN
               APIOutboundEvent.success = TRUE
           ELSE
               APIOutboundEvent.success = FALSE)
      BY APIOutboundEvent.requestDateTime DESCENDING.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Outbound Event */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Outbound Event */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE APIOutboundEvent THEN DO:
        RUN api\ResponseDataViewer.w (
            ROWID(APIOutboundEvent)
            ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    APPLY "WINDOW-CLOSE" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFilter C-Win
ON CHOOSE OF btFilter IN FRAME DEFAULT-FRAME /* Filter */
DO:
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
  DISPLAY fiAPIIDLabel cbAPIId fiRequestDatelabel fiRequestDate fiSuccessLabel 
          cbSuccess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btExit btFilter cbAPIId fiRequestDate cbSuccess BROWSE-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

