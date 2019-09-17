&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: comp_loc.w

  Description: Select Company/Location

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

DEFINE VARIABLE onlyone AS LOGICAL NO-UNDO.
DEFINE VARIABLE save-rowid AS ROWID NO-UNDO.
DEF BUFFER b-usercomp FOR usercomp .
DEF BUFFER c-usercomp FOR usercomp .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME companies

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES usercomp company loc

/* Definitions for BROWSE companies                                     */
&Scoped-define FIELDS-IN-QUERY-companies company.company company.name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-companies 
&Scoped-define QUERY-STRING-companies FOR EACH usercomp ~
      WHERE usercomp.user_id = USERID('NOSWEAT') AND ~
usercomp.loc = '' AND ~
(IF g_init THEN usercomp.company_default = yes ~
 ELSE TRUE) NO-LOCK, ~
      EACH company WHERE TRUE /* Join to usercomp incomplete */ ~
      AND company.company = usercomp.company NO-LOCK
&Scoped-define OPEN-QUERY-companies OPEN QUERY companies FOR EACH usercomp ~
      WHERE usercomp.user_id = USERID('NOSWEAT') AND ~
usercomp.loc = '' AND ~
(IF g_init THEN usercomp.company_default = yes ~
 ELSE TRUE) NO-LOCK, ~
      EACH company WHERE TRUE /* Join to usercomp incomplete */ ~
      AND company.company = usercomp.company NO-LOCK.
&Scoped-define TABLES-IN-QUERY-companies usercomp company
&Scoped-define FIRST-TABLE-IN-QUERY-companies usercomp
&Scoped-define SECOND-TABLE-IN-QUERY-companies company


/* Definitions for BROWSE locations                                     */
&Scoped-define FIELDS-IN-QUERY-locations loc.loc loc.dscr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-locations 
&Scoped-define QUERY-STRING-locations FOR EACH usercomp ~
      WHERE usercomp.user_id = USERID("NOSWEAT") AND ~
usercomp.company = company.company AND ~
usercomp.loc NE "" AND ~
(IF g_init THEN usercomp.loc_default = yes ~
 ELSE TRUE) NO-LOCK, ~
      EACH loc OF usercomp NO-LOCK
&Scoped-define OPEN-QUERY-locations OPEN QUERY locations FOR EACH usercomp ~
      WHERE usercomp.user_id = USERID("NOSWEAT") AND ~
usercomp.company = company.company AND ~
usercomp.loc NE "" AND ~
(IF g_init THEN usercomp.loc_default = yes ~
 ELSE TRUE) NO-LOCK, ~
      EACH loc OF usercomp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-locations usercomp loc
&Scoped-define FIRST-TABLE-IN-QUERY-locations usercomp
&Scoped-define SECOND-TABLE-IN-QUERY-locations loc


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-companies}~
    ~{&OPEN-QUERY-locations}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS companies Btn_Cancel locations Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS sysdate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&OK" 
     SIZE 8 BY 1.91 TOOLTIP "OK".

DEFINE VARIABLE sysdate AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 18 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY companies FOR 
      usercomp, 
      company SCROLLING.

DEFINE QUERY locations FOR 
      usercomp, 
      loc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE companies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS companies C-Win _STRUCTURED
  QUERY companies NO-LOCK DISPLAY
      company.company FORMAT "x(3)":U
      company.name FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 6.67.

DEFINE BROWSE locations
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS locations C-Win _STRUCTURED
  QUERY locations NO-LOCK DISPLAY
      loc.loc FORMAT "x(5)":U
      loc.dscr FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     companies AT ROW 1 COL 1 HELP
          "Select Company"
     Btn_Cancel AT ROW 5.29 COL 100 HELP
          "CANCEL Select Company/Department"
     locations AT ROW 1 COL 46 HELP
          "Select Department"
     Btn_OK AT ROW 5.29 COL 92 HELP
          "Select Company/Department"
     sysdate AT ROW 2.43 COL 90 COLON-ALIGNED HELP
          "Enter System Date" NO-LABEL
     "System Date" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.71 COL 94
     RECT-1 AT ROW 5.05 COL 91 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.8 BY 6.71.


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
         TITLE              = "Select Company/Department"
         HEIGHT             = 6.71
         WIDTH              = 108.8
         MAX-HEIGHT         = 6.71
         MAX-WIDTH          = 108.8
         VIRTUAL-HEIGHT     = 6.71
         VIRTUAL-WIDTH      = 108.8
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB companies TEXT-1 DEFAULT-FRAME */
/* BROWSE-TAB locations Btn_Cancel DEFAULT-FRAME */
ASSIGN 
       Btn_Cancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

ASSIGN 
       Btn_OK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sysdate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE companies
/* Query rebuild information for BROWSE companies
     _TblList          = "ASI.usercomp,ASI.company WHERE ASI.usercomp ..."
     _Options          = "NO-LOCK"
     _Where[1]         = "usercomp.user_id = USERID('NOSWEAT') AND
usercomp.loc = '' AND
(IF g_init THEN usercomp.company_default = yes
 ELSE TRUE)"
     _Where[2]         = "company.company = usercomp.company"
     _FldNameList[1]   = ASI.company.company
     _FldNameList[2]   = ASI.company.name
     _Query            is OPENED
*/  /* BROWSE companies */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE locations
/* Query rebuild information for BROWSE locations
     _TblList          = "ASI.usercomp,ASI.loc OF ASI.usercomp"
     _Options          = "NO-LOCK"
     _Where[1]         = "usercomp.user_id = USERID(""NOSWEAT"") AND
usercomp.company = company.company AND
usercomp.loc NE """" AND
(IF g_init THEN usercomp.loc_default = yes
 ELSE TRUE)"
     _FldNameList[1]   = ASI.loc.loc
     _FldNameList[2]   = ASI.loc.dscr
     _Query            is OPENED
*/  /* BROWSE locations */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Select Company/Department */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Select Company/Department */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  IF usercomp.loc = "" THEN
      GET FIRST locations.

  RUN Set-comp_loc.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME companies
&Scoped-define SELF-NAME companies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL companies C-Win
ON DEFAULT-ACTION OF companies IN FRAME DEFAULT-FRAME
DO:
  APPLY 'CHOOSE' TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL companies C-Win
ON VALUE-CHANGED OF companies IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-locations}
  FIND FIRST c-usercomp NO-LOCK WHERE 
    c-usercomp.user_id EQ USERID("asi") AND 
    c-usercomp.company EQ usercomp.company AND 
    c-usercomp.loc_default EQ TRUE
    NO-ERROR.
  IF AVAIL c-usercomp THEN 
    REPOSITION locations TO ROWID ROWID(c-usercomp).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME locations
&Scoped-define SELF-NAME locations
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL locations C-Win
ON DEFAULT-ACTION OF locations IN FRAME DEFAULT-FRAME
DO:
  APPLY 'CHOOSE' TO Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME companies
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
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  GET LAST companies.
  save-rowid = ROWID(usercomp).
  GET FIRST companies.
  onlyone = IF save-rowid = ROWID(usercomp) THEN yes ELSE no.
  IF onlyone THEN DO:
    GET LAST locations.
    save-rowid = ROWID(usercomp).
    GET FIRST locations.
    onlyone = IF save-rowid = ROWID(usercomp) THEN yes ELSE no.  
  END.

  IF g_init THEN
  DO:
    RUN Set-comp_loc.
    {methods/nowait.i}
    RETURN.
  END.
  RUN enable_UI.
  sysdate = g_sysdate.
  DISPLAY sysdate WITH FRAME {&FRAME-NAME}.
  {methods/enhance.i}
  {methods/nowait.i}
  APPLY 'value-changed' TO BROWSE companies.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF onlyone THEN
  APPLY "CHOOSE" TO Btn_OK IN FRAME {&FRAME-NAME}.
  APPLY "focus" TO locations IN FRAME {&FRAME-NAME} .
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
  DISPLAY sysdate 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE companies Btn_Cancel locations Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-comp_loc C-Win 
PROCEDURE Set-comp_loc :
/*------------------------------------------------------------------------------
  Purpose:     Set Company/Department
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF g_init THEN do: /* Task 07081401*/
       FIND FIRST  b-usercomp WHERE b-usercomp.user_id = USERID("NOSWEAT") 
          AND b-usercomp.company = company.company AND 
          b-usercomp.loc NE "" AND
          b-usercomp.loc_default = YES NO-LOCK NO-ERROR.
      IF NOT AVAIL b-usercomp THEN DO:
        FIND FIRST  b-usercomp WHERE b-usercomp.user_id = USERID("NOSWEAT") 
            AND b-usercomp.company = company.company AND 
            b-usercomp.loc = "MAIN" EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL b-usercomp THEN 
            do: ASSIGN  b-usercomp.loc_default = YES .
        FIND FIRST loc WHERE loc.company = company.company 
            AND loc.loc =  b-usercomp.loc NO-LOCK NO-ERROR.
        END.
      END.
    END.

  IF NOT AVAILABLE loc THEN
  DO:
    MESSAGE
      "No Locations Exist for this Company, Please Select different Company"
        VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.
  IF VALID-HANDLE(g_mainmenu) THEN
  RUN Set-comp_loc IN g_mainmenu
      (company.company,
       company.name,
       loc.loc,
       loc.dscr).
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      sysdate
      g_sysdate = IF sysdate NE ? THEN sysdate ELSE g_sysdate
      g_init = no.
  END.
  RUN Set-Period.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Period C-Win 
PROCEDURE Set-Period :
/*------------------------------------------------------------------------------
  Purpose:     Set System Period
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND period
       WHERE period.company = g_company
         /*AND period.yr = YEAR(g_sysdate)*/
         AND period.pst LE g_sysdate
         AND period.pend GE g_sysdate
       NO-LOCK NO-ERROR.
  IF AVAILABLE period THEN
  g_period = period.pnum.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

