&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/CnvtFGAI

  Description: Converts the FG status field from reftable to itemgf.stat field

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: BV

  Created: 01/28/2014

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
/* {methods/prgsecur.i}         */
/*                              */
/* {custom/gcompany.i}          */
/* {custom/getcmpny.i}          */
/* {custom/gloc.i}              */
/* {custom/getloc.i}            */
/*                              */
/* {sys/inc/var.i new shared}   */
/*                              */
/* assign                       */
/*  cocode = gcompany           */
/*  locode = gloc.              */
/*                              */

DEFINE VARIABLE glProcess AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_i-no end_i-no begin_cust ~
end_cust tb_force tb_delete btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no begin_cust end_cust ~
tb_force tb_delete 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.67.

DEFINE VARIABLE tb_delete AS LOGICAL INITIAL no 
     LABEL "Purge Reftable" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .95 NO-UNDO.

DEFINE VARIABLE tb_force AS LOGICAL INITIAL no 
     LABEL "Only update if DB field is blank" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_i-no AT ROW 6.95 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 6.95 COL 66 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cust AT ROW 8.14 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 8.14 COL 66 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_force AT ROW 9.33 COL 27 WIDGET-ID 2
     tb_delete AT ROW 10.29 COL 27 WIDGET-ID 4
     btn-process AT ROW 11.62 COL 21
     btn-cancel AT ROW 11.62 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Convert Active/Inactive to DB Field"
         HEIGHT             = 12.33
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Convert Active/Inactive to DB Field */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Convert Active/Inactive to DB Field */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  glProcess = NO.

  MESSAGE "Are you sure you want to" TRIM(c-win:TITLE) +
          " within the selection parameters?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE glProcess.

  IF glProcess THEN RUN RunProcess.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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
  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-process "Start"} /* added by script _nonAdm1Images1.p */
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
  DISPLAY begin_i-no end_i-no begin_cust end_cust tb_force tb_delete 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_i-no end_i-no begin_cust end_cust tb_force tb_delete 
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProcess C-Win 
PROCEDURE RunProcess :
/* ------------------------------------------------  */
/* Convert FGStatus Reftable to itemfg.stat                                   */
/* -------------------------------------------------------------------------- */
DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-reftable FOR reftable.
DEFINE BUFFER bf-company FOR company.

DEFINE VARIABLE cCustNoStart AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustNoEnd AS CHARACTER INIT "ZZZZZZZZ"  NO-UNDO.
DEFINE VARIABLE cItemNoStart AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cItemNoEnd AS CHARACTER INIT "ZZZZZZZZZZZZZZZ"  NO-UNDO.
DEFINE VARIABLE lUpdateOnlyIfBlank AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPurgeReftable AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cStat AS CHARACTER   NO-UNDO.



DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        begin_i-no
        end_i-no
        begin_cust
        end_cust
        tb_force
        tb_delete.
END.

ASSIGN
    cItemNoStart = begin_i-no
    cItemNoEnd = end_i-no
    cCustNoStart = begin_cust
    cCustNoEnd = end_cust
    lUpdateOnlyIfBlank = tb_force
    lPurgeReftable = tb_delete.


SESSION:SET-WAIT-STATE("General").
DISABLE TRIGGERS FOR LOAD OF bf-itemfg.  

FOR EACH bf-company NO-LOCK,
    EACH bf-itemfg
    WHERE bf-itemfg.company    EQ bf-company.company
      AND bf-itemfg.cust-no    GE cCustNoStart
      AND bf-itemfg.cust-no    LE cCustNoEnd
      AND bf-itemfg.i-no       GE cItemNoStart
      AND bf-itemfg.i-no       LE cItemNoEnd
      AND TRIM(bf-itemfg.i-no) NE ""
    USE-INDEX customer EXCLUSIVE-LOCK:

    STATUS DEFAULT "Processing Customer#/FG#: " +
                 TRIM(bf-itemfg.cust-no) + "/" + TRIM(bf-itemfg.i-no).
    cStat = "".
    FOR EACH bf-reftable
        WHERE bf-reftable.reftable EQ "FGSTATUS"
          AND bf-reftable.company  EQ bf-itemfg.company
          AND bf-reftable.loc      EQ ""   
          AND bf-reftable.CODE     EQ bf-itemfg.i-no
         EXCLUSIVE-LOCK:
        IF cStat NE "A" THEN 
            cStat = IF bf-reftable.code2 NE "" THEN bf-reftable.code2 ELSE "A".
        IF lPurgeReftable THEN
             DELETE bf-reftable.
    END.
    IF cStat EQ "" THEN
        cStat = "A".
    IF (lUpdateOnlyIfBlank AND bf-itemfg.stat EQ "") OR NOT lUpdateOnlyIfBlank THEN DO:
        bf-itemfg.stat = cStat.  
    END.

END. /* each itemfg */

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("General").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

