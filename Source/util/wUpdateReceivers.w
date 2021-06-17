&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i new shared}

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
&Scoped-define INTERNAL-TABLES fg-rctd fg-rdtlh

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 fg-rctd.po-no fg-rctd.po-line ~
fg-rdtlh.receiver-no fg-rctd.i-no fg-rctd.i-name fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.tag fg-rctd.rct-date 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 fg-rdtlh.receiver-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 fg-rdtlh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 fg-rdtlh
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH fg-rctd ~
      WHERE fg-rctd.company EQ gcompany AND ~
fg-rctd.po-no NE "" AND ~
fg-rctd.po-no EQ fiPoNo:SCREEN-VALUE NO-LOCK, ~
      EACH fg-rdtlh WHERE TRUE /* Join to fg-rctd incomplete */ ~
      AND fg-rdtlh.company eq fg-rctd.company AND ~
fg-rdtlh.r-no eq fg-rctd.r-no and ~
fg-rdtlh.loc eq fg-rctd.loc and ~
fg-rdtlh.loc-bin eq fg-rctd.loc-bin and ~
fg-rdtlh.tag eq fg-rctd.tag NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH fg-rctd ~
      WHERE fg-rctd.company EQ gcompany AND ~
fg-rctd.po-no NE "" AND ~
fg-rctd.po-no EQ fiPoNo:SCREEN-VALUE NO-LOCK, ~
      EACH fg-rdtlh WHERE TRUE /* Join to fg-rctd incomplete */ ~
      AND fg-rdtlh.company eq fg-rctd.company AND ~
fg-rdtlh.r-no eq fg-rctd.r-no and ~
fg-rdtlh.loc eq fg-rctd.loc and ~
fg-rdtlh.loc-bin eq fg-rctd.loc-bin and ~
fg-rdtlh.tag eq fg-rctd.tag NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 fg-rctd fg-rdtlh
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 fg-rctd
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 fg-rdtlh


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bExit fiPoNo bCheck BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fiPoNo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bCheck 
     LABEL "Check PO" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U
     LABEL "Exit" 
     SIZE 8 BY 1.67 TOOLTIP "Exit"
     FONT 6.

DEFINE VARIABLE fiPoNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enter PO Number to check" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      fg-rctd, 
      fg-rdtlh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      fg-rctd.po-no FORMAT "x(9)":U
      fg-rctd.po-line FORMAT ">>9":U
      fg-rdtlh.receiver-no FORMAT "x(20)":U
      fg-rctd.i-no FORMAT "x(10)":U WIDTH 18.2
      fg-rctd.i-name FORMAT "x(30)":U WIDTH 32.2
      fg-rctd.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U WIDTH 8
      fg-rctd.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 11
      fg-rctd.tag COLUMN-LABEL "Tag" FORMAT "x(21)":U WIDTH 24.6
      fg-rctd.rct-date FORMAT "99/99/9999":U
  ENABLE
      fg-rdtlh.receiver-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 148 BY 14.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bExit AT ROW 1.24 COL 142
     fiPoNo AT ROW 1.48 COL 30 COLON-ALIGNED
     bCheck AT ROW 1.48 COL 51
     BROWSE-2 AT ROW 3.14 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 153 BY 17.14.


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
         TITLE              = "Update Receiver Info for PO"
         HEIGHT             = 17.14
         WIDTH              = 153
         MAX-HEIGHT         = 17.14
         MAX-WIDTH          = 153
         VIRTUAL-HEIGHT     = 17.14
         VIRTUAL-WIDTH      = 153
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
/* BROWSE-TAB BROWSE-2 bCheck DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "asi.fg-rctd,asi.fg-rdtlh WHERE asi.fg-rctd ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "fg-rctd.company EQ gcompany AND
fg-rctd.po-no NE """" AND
fg-rctd.po-no EQ fiPoNo:SCREEN-VALUE"
     _Where[2]         = "fg-rdtlh.company eq fg-rctd.company AND
fg-rdtlh.r-no eq fg-rctd.r-no and
fg-rdtlh.loc eq fg-rctd.loc and
fg-rdtlh.loc-bin eq fg-rctd.loc-bin and
fg-rdtlh.tag eq fg-rctd.tag"
     _FldNameList[1]   = asi.fg-rctd.po-no
     _FldNameList[2]   = asi.fg-rctd.po-line
     _FldNameList[3]   > asi.fg-rdtlh.receiver-no
"fg-rdtlh.receiver-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.fg-rctd.i-no
"fg-rctd.i-no" ? ? "character" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.fg-rctd.i-name
"fg-rctd.i-name" ? ? "character" ? ? ? ? ? ? no ? no no "32.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.fg-rctd.loc
"fg-rctd.loc" "Whse" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.fg-rctd.loc-bin
"fg-rctd.loc-bin" "Bin" ? "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.fg-rctd.tag
"fg-rctd.tag" "Tag" "x(21)" "character" ? ? ? ? ? ? no ? no no "24.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = asi.fg-rctd.rct-date
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Receiver Info for PO */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Receiver Info for PO */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCheck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCheck C-Win
ON CHOOSE OF bCheck IN FRAME DEFAULT-FRAME /* Check PO */
OR RETURN OF fiPoNo
DO:

    OPEN QUERY browse-2 FOR EACH asi.fg-rctd
        WHERE fg-rctd.company EQ gcompany AND
        fg-rctd.po-no EQ fiPoNo:SCREEN-VALUE NO-LOCK,
        EACH asi.fg-rdtlh EXCLUSIVE WHERE fg-rdtlh.company eq fg-rctd.company AND
        fg-rdtlh.r-no eq fg-rctd.r-no and
        fg-rdtlh.loc eq fg-rctd.loc and
        fg-rdtlh.loc-bin eq fg-rctd.loc-bin and
        fg-rdtlh.tag eq fg-rctd.tag
        BY fg-rdtlh.receiver DESCENDING.
        

    browse-2:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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
  
  APPLY 'entry' TO fiPoNo.
  
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
  DISPLAY fiPoNo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE bExit fiPoNo bCheck BROWSE-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

