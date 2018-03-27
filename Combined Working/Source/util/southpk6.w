&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_file btn-process btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS fi_file 

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U 
     LABEL "Input from" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_file AT ROW 4.81 COL 14 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     btn-process AT ROW 8.14 COL 21
     btn-cancel AT ROW 8.14 COL 53
     RECT-17 AT ROW 1 COL 1
     "This process is used to import an FG Physical Count file (inven2)" VIEW-AS TEXT
          SIZE 77 BY .71 AT ROW 2.19 COL 7
          FONT 6
     "from a comma separated ASCII file to Advantzware (fg-rcpts & fg-rdtl)" VIEW-AS TEXT
          SIZE 78 BY .62 AT ROW 3.14 COL 7
          FONT 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.


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
         TITLE              = "Load ShipTo"
         HEIGHT             = 9.57
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Load ShipTo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Load ShipTo */
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
  DEF VAR ll-process AS LOG NO-UNDO.


  ll-process = NO.
  MESSAGE "Are you sure you wish to continue?"
          VIEW-AS ALERT-BOX QUESTION BUTTON yes-no
          UPDATE ll-process.

  IF ll-process THEN DO:
    SESSION:SET-WAIT-STATE ("general").

    RUN run-process.

    SESSION:SET-WAIT-STATE("").

    MESSAGE trim(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
    APPLY "close" TO THIS-PROCEDURE.
  END.
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
  fi_file = "inven2.asc".
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
  DISPLAY fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE fi_file btn-process btn-cancel RECT-17 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------ util/southpk6.p 05/02 JLF */
/* Import FG Cycle Count for Southpak                                         */
/* -------------------------------------------------------------------------- */                       

def var v-file as char.
def var v-ip-file as   char format "x(40)".
def var v-op-file like v-ip-file.
def var v-int as int.
def var v-err as int.

def var v-uom-qty like fg-rdtl.t-qty no-undo.


  do with frame {&frame-name}:
    ASSIGN
     fi_file
     v-ip-file = fi_file
     v-op-file = "inven2.quo".
  end.

  if opsys eq "UNIX" then
    unix silent quoter -c 1-3000 value(search(v-ip-file)) > value(v-op-file).
  else
    dos  silent quoter -c 1-3000 value(search(v-ip-file)) > value(v-op-file).

  input from value(v-op-file).

  output to value("inven2.err").

  repeat:
    assign
     v-file = ""
     v-int  = v-int + 1.

    import v-file.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq substr(entry(1,v-file),1,15)
        no-lock no-error.

    status default "Processing Item: " + trim(substr(entry(1,v-file),1,15)).

    if not avail itemfg then do:
      v-err = v-err + 1.
      put unformatted v-int " No FG Record " v-file skip.
    end.

    else do:
      find first fg-rcpts
          where fg-rcpts.company      eq cocode
            and fg-rcpts.i-no         eq substr(entry(1,v-file),1,15)
            and fg-rcpts.job-no       eq ""
            and fg-rcpts.job-no2      eq 0
            and fg-rcpts.trans-date   eq today
            and fg-rcpts.rita-code    eq "C"
          no-lock no-error.

      if not avail fg-rcpts then do:
        find last fg-rcpts use-index r-no no-lock no-error.
        i = if avail fg-rcpts then fg-rcpts.r-no else 0.

        find last fg-rcpth where fg-rcpth.r-no gt i
            use-index r-no no-lock no-error.
        if avail fg-rcpth then i = fg-rcpth.r-no.

        create fg-rcpts.
        assign
         fg-rcpts.r-no         = i + 1 
         fg-rcpts.company      = cocode
         fg-rcpts.loc          = locode
         fg-rcpts.i-no         = substr(entry(1,v-file),1,15)
         fg-rcpth.i-name       = itemfg.i-name
         fg-rcpts.trans-date   = today
         fg-rcpts.rita-code    = "C"
         fg-rcpts.cust-no      = itemfg.cust-no
         fg-rcpts.user-id      = userid("asi")
         fg-rcpts.pur-uom      = itemfg.sell-uom.
      end.

      create fg-rdtl.
      assign
       fg-rdtl.r-no          = fg-rcpts.r-no
       fg-rdtl.company       = fg-rcpts.company
       fg-rdtl.loc           = "MAIN"
       fg-rdtl.loc-bin       = "FLOOR"
       fg-rdtl.tag           = ""
       fg-rdtl.rita-code     = fg-rcpts.rita-code
       fg-rdtl.uom           = itemfg.prod-uom
       fg-rdtl.t-qty         = dec(entry(38,v-file)) +
                               dec(entry(39,v-file)) +
                               dec(entry(40,v-file)) +
                               dec(entry(41,v-file)) +
                               dec(entry(42,v-file)).

      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.tag     eq fg-rdtl.tag
            and fg-bin.i-no    eq fg-rcpts.i-no
            and fg-bin.loc     eq fg-rdtl.loc
            and fg-bin.loc-bin eq fg-rdtl.loc-bin
            and fg-bin.job-no  eq fg-rcpts.job-no
            and fg-bin.job-no2 eq fg-rcpts.job-no2
          use-index tag no-lock no-error.
      if avail fg-bin then
        assign
         fg-rdtl.std-cost = fg-bin.std-tot-cost
         fg-rdtl.uom      = fg-bin.pur-uom.

      v-uom-qty = fg-rdtl.t-qty.

      if fg-rdtl.uom ne "EA" then
        run sys/ref/convquom.p("EA", fg-rdtl.uom, 0, 0, 0, 0,
                               v-uom-qty, output v-uom-qty).

      fg-rdtl.ext-cost = fg-rdtl.std-cost * v-uom-qty.
    end.
  end.

  output close.
  input close.

  status default "".

  if v-err gt 0 then message "Errors output to inven2.err" VIEW-AS ALERT-BOX.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

