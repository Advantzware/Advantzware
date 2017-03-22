&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

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
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 btn-process btn-cancel 

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

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btn-process AT ROW 15.76 COL 21
     btn-cancel AT ROW 15.76 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 3.76 COL 1
          BGCOLOR 11 
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
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
         TITLE              = "Create FG Receipts from BOLs"
         HEIGHT             = 17.71
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create FG Receipts from BOLs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Create FG Receipts from BOLs */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  DEF VAR v-process AS LOG NO-UNDO.


  MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE)
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
          
  IF v-process THEN RUN run-process.
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
  ENABLE RECT-17 btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------ util/smancomm.p 08/00 FWK */
/* Update Order Line Salesman Info from Order Header                          */
/* -------------------------------------------------------------------------- */

DEF VAR lv-bol-qty AS INT INIT 0 NO-UNDO.
DEF VAR lv-rec-qty AS INT INIT 0 NO-UNDO.
DEF VAR lv-cost AS DEC NO-UNDO.
DEF VAR li AS INT NO-UNDO.


SESSION:SET-WAIT-STATE("General").

FOR EACH oe-bolh
    WHERE oe-bolh.company EQ cocode
      AND oe-bolh.posted  EQ YES
    NO-LOCK,

    EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no
    NO-LOCK,

    FIRST itemfg
    WHERE itemfg.company EQ oe-boll.company
      AND itemfg.i-no    EQ oe-boll.i-no
    NO-LOCK

    BREAK BY oe-boll.i-no
          BY oe-boll.job-no
          BY oe-boll.job-no2
          BY oe-boll.loc
          BY oe-boll.loc-bin
          BY oe-boll.tag
          BY oe-bolh.bol-date DESC:

  lv-bol-qty = lv-bol-qty + oe-boll.qty.

  IF LAST-OF(oe-boll.tag) THEN DO TRANSACTION:
    lv-rec-qty = 0.

    FOR EACH fg-rcpth
        WHERE fg-rcpth.company   EQ oe-boll.company
          AND fg-rcpth.i-no      EQ oe-boll.i-no
          AND fg-rcpth.job-no    EQ oe-boll.job-no
          AND fg-rcpth.job-no2   EQ oe-boll.job-no2
          AND fg-rcpth.rita-code EQ "R"
        NO-LOCK,

        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
          AND fg-rdtlh.loc       EQ oe-boll.loc
          AND fg-rdtlh.loc-bin   EQ oe-boll.loc-bin
          AND fg-rdtlh.tag       EQ oe-boll.tag
        NO-LOCK:

      lv-rec-qty = lv-rec-qty + fg-rdtlh.qty.
    END.

    FOR EACH fg-rctd
        WHERE fg-rctd.company   EQ oe-boll.company
          AND fg-rctd.i-no      EQ oe-boll.i-no
          AND fg-rctd.job-no    EQ oe-boll.job-no
          AND fg-rctd.job-no2   EQ oe-boll.job-no2
          AND fg-rctd.rita-code EQ "R"
          AND fg-rctd.loc       EQ oe-boll.loc
          AND fg-rctd.loc-bin   EQ oe-boll.loc-bin
          AND fg-rctd.tag       EQ oe-boll.tag
        NO-LOCK:

      lv-rec-qty = lv-rec-qty + fg-rctd.t-qty.
    END.

    IF lv-rec-qty LT lv-bol-qty THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ oe-boll.company
            AND fg-bin.i-no    EQ oe-boll.i-no
            AND fg-bin.loc     EQ oe-boll.loc
            AND fg-bin.loc-bin EQ oe-boll.loc-bin
            AND fg-bin.tag     EQ oe-boll.tag
            AND fg-bin.job-no  EQ oe-boll.job-no
            AND fg-bin.job-no2 EQ oe-boll.job-no2
          NO-LOCK NO-ERROR.

      FIND FIRST fg-rctd
          WHERE fg-rctd.company   EQ oe-boll.company
            AND fg-rctd.i-no      EQ oe-boll.i-no
            AND fg-rctd.job-no    EQ oe-boll.job-no
            AND fg-rctd.job-no2   EQ oe-boll.job-no2
            AND fg-rctd.rita-code EQ "R"
            AND fg-rctd.loc       EQ oe-boll.loc
            AND fg-rctd.loc-bin   EQ oe-boll.loc-bin
            AND fg-rctd.tag       EQ oe-boll.tag
            AND fg-rctd.rct-date  EQ oe-bolh.bol-date
          NO-ERROR.
      IF NOT AVAIL fg-rctd THEN DO:
        li = 0.
        FOR EACH fg-rctd WHERE fg-rctd.company EQ oe-bolh.company NO-LOCK
             BY fg-rctd.r-no DESC:
          li = fg-rctd.r-no.
          LEAVE.
        END.

        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.

        CREATE fg-rctd.
        ASSIGN
         fg-rctd.company    = oe-boll.company
         fg-rctd.r-no       = li + 1
         fg-rctd.i-no       = oe-boll.i-no
         fg-rctd.job-no     = oe-boll.job-no
         fg-rctd.job-no2    = oe-boll.job-no2
         fg-rctd.rita-code  = "R"
         fg-rctd.loc        = oe-boll.loc
         fg-rctd.loc-bin    = oe-boll.loc-bin
         fg-rctd.tag        = oe-boll.tag
         fg-rctd.rct-date   = oe-bolh.bol-date
         fg-rctd.trans-time = TIME
         fg-rctd.cost-uom   = IF itemfg.pur-man THEN itemfg.pur-uom
                                                ELSE itemfg.prod-uom
         fg-rctd.pur-uom    = fg-rctd.cost-uom
         fg-rctd.cases-unit = 1.

        FIND FIRST job-hdr
            WHERE job-hdr.company EQ oe-boll.company
              AND job-hdr.ord-no  EQ oe-boll.ord-no
              AND job-hdr.job-no  EQ oe-boll.job-no
              AND job-hdr.job-no2 EQ oe-boll.job-no2
              AND job-hdr.i-no    EQ oe-boll.i-no
            USE-INDEX ord-no NO-LOCK NO-ERROR.
        
        fg-rctd.std-cost = 
            IF AVAIL job-hdr THEN job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                  job-hdr.std-fix-cost + job-hdr.std-var-cost
                             ELSE itemfg.std-mat-cost + itemfg.std-lab-cost +
                                  itemfg.std-fix-cost + itemfg.std-var-cost.
      END.

      ASSIGN
       fg-rctd.t-qty    = fg-rctd.t-qty + (lv-bol-qty - lv-rec-qty)
       fg-rctd.qty-case = oe-boll.qty-case.

      IF fg-rctd.qty-case EQ 0 THEN fg-rctd.qty-case = 1.

      ASSIGN
       fg-rctd.partial  = oe-boll.partial
       fg-rctd.cases    = TRUNC((fg-rctd.t-qty - fg-rctd.partial) / fg-rctd.qty-case,0).

      IF fg-rctd.cost-uom EQ "EA" THEN
        lv-cost = fg-rctd.std-cost.
      ELSE
        RUN sys/ref/convcuom.p(fg-rctd.cost-uom, "EA", 0, 0, 0, 0,
                               fg-rctd.std-cost, OUTPUT lv-cost).

      fg-rctd.ext-cost = lv-cost * fg-rctd.t-qty.
    END.

    lv-bol-qty = 0.
  END.
END.
    
SESSION:SET-WAIT-STATE("").
    
MESSAGE TRIM(c-win:TITLE) + " Process Complete..." VIEW-AS ALERT-BOX.
    
APPLY "close" TO THIS-PROCEDURE.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

