&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: sysCtrlUsage.w

  Description: Show User sys-ctrl Usage

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 3.27.2018

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

DEFINE TEMP-TABLE ttSysCtrlUsage NO-UNDO
    FIELD prgmName     AS CHARACTER                      LABEL "Program"         FORMAT "x(22)"
    FIELD company      LIKE sys-ctrl.company             
    FIELD module       LIKE sys-ctrl.module              LABEL "Module"
    FIELD name         LIKE sys-ctrl.name                                        FORMAT "x(20)"
    FIELD char-fld     LIKE sys-ctrl.char-fld            LABEL "Character Value" FORMAT "x(30)"
    FIELD date-fld     LIKE sys-ctrl.date-fld            LABEL "Date"
    FIELD dec-fld      LIKE sys-ctrl.dec-fld             LABEL "Decimal"
    FIELD int-fld      LIKE sys-ctrl.int-fld             LABEL "Integer"
    FIELD log-fld      LIKE sys-ctrl.log-fld             LABEL "Logical"
    FIELD descrip      LIKE sys-ctrl.descrip                                     FORMAT "x(80)"
    FIELD user-id      AS CHARACTER                      LABEL "User ID"         FORMAT "x(15)"
    FIELD usageNow     AS DATETIME                       LABEL "Date/Time"
    FIELD category     LIKE sys-ctrl-shipto.category
    FIELD cust-vend    LIKE sys-ctrl-shipto.cust-vend
    FIELD cust-vend-no LIKE sys-ctrl-shipto.cust-vend-no
    FIELD seqNo        LIKE sys-ctrl-shipto.seqNo
    FIELD ship-id      LIKE sys-ctrl-shipto.ship-id
    FIELD subCategory  LIKE sys-ctrl-shipto.subCategory
    FIELD sysCtrlID    LIKE sys-ctrl-shipto.sysCtrlID
    FIELD typeCode     LIKE sys-ctrl-shipto.typeCode
        INDEX ttSysCtrlUsage IS PRIMARY
            prgmName company module name
            .
DEFINE TEMP-TABLE bttSysCtrlUsage NO-UNDO LIKE ttSysCtrlUsage.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME sysCtrlUsage

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSysCtrlUsage

/* Definitions for BROWSE sysCtrlUsage                                  */
&Scoped-define FIELDS-IN-QUERY-sysCtrlUsage ttSysCtrlUsage   
&Scoped-define ENABLED-FIELDS-IN-QUERY-sysCtrlUsage   
&Scoped-define SELF-NAME sysCtrlUsage
&Scoped-define QUERY-STRING-sysCtrlUsage FOR EACH ttSysCtrlUsage
&Scoped-define OPEN-QUERY-sysCtrlUsage OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrlUsage.
&Scoped-define TABLES-IN-QUERY-sysCtrlUsage ttSysCtrlUsage
&Scoped-define FIRST-TABLE-IN-QUERY-sysCtrlUsage ttSysCtrlUsage


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-sysCtrlUsage}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS sysCtrlUsage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY sysCtrlUsage FOR 
      ttSysCtrlUsage SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE sysCtrlUsage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS sysCtrlUsage C-Win _FREEFORM
  QUERY sysCtrlUsage DISPLAY
      ttSysCtrlUsage
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 28.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sysCtrlUsage AT ROW 1 COL 1 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57 WIDGET-ID 100.


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
         TITLE              = "User SysCtrl Usage"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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
/* BROWSE-TAB sysCtrlUsage 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE sysCtrlUsage
/* Query rebuild information for BROWSE sysCtrlUsage
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrlUsage.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE sysCtrlUsage */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User SysCtrl Usage */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User SysCtrl Usage */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* User SysCtrl Usage */
DO:
    RUN pReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME sysCtrlUsage
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
  RUN pGetSysCtrlUsage.
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
  ENABLE sysCtrlUsage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSysCtrlUsage C-Win 
PROCEDURE pGetSysCtrlUsage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE bttSysCtrlUsage.
    INPUT FROM VALUE("users\" + USERID(LDBNAME(1)) + "\sysCtrlFind.dat") NO-ECHO.
    REPEAT:
        IMPORT bttSysCtrlUsage.
        FIND FIRST ttSysCtrlUsage
             WHERE ttSysCtrlUsage.prgmName EQ bttSysCtrlUsage.prgmName
               AND ttSysCtrlUsage.company  EQ bttSysCtrlUsage.company
               AND ttSysCtrlUsage.module   EQ bttSysCtrlUsage.module
               AND ttSysCtrlUsage.name     EQ bttSysCtrlUsage.name
             NO-ERROR.
        IF NOT AVAILABLE ttSysCtrlUsage THEN
        CREATE ttSysCtrlUsage.
        BUFFER-COPY bttSysCtrlUsage TO ttSysCtrlUsage.
    END. /* repeat */
    INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReSize C-Win 
PROCEDURE pReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FRAME {&FRAME-NAME}:HIDDEN = YES.
    ASSIGN
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:WIDTH = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
        BROWSE {&BROWSE-NAME}:WIDTH = FRAME {&FRAME-NAME}:WIDTH
        BROWSE {&BROWSE-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
        .
    FRAME {&FRAME-NAME}:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

