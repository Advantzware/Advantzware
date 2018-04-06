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
     "This process is used to import the Vendor Master file (apmstr)" VIEW-AS TEXT
          SIZE 74 BY .71 AT ROW 2.19 COL 10
          FONT 6
     "from a comma separated ASCII file to Advantzware (vend)" VIEW-AS TEXT
          SIZE 68 BY .62 AT ROW 3.14 COL 10
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
         TITLE              = "Load Vendors"
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
ON END-ERROR OF C-Win /* Load Vendors */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Load Vendors */
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
  fi_file = "apmstr.asc".
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
/* ------------------------------------------------ util/southpk3.p 03/02 JLF */
/* Import Vendor for Southpak                                                 */
/* -------------------------------------------------------------------------- */                        

def var v-file as char.
def var v-ip-file as   char format "x(40)".
def var v-op-file like v-ip-file.
def var v-int as int.
def var v-err as int.


  do with frame {&frame-name}:
    ASSIGN
     fi_file
     v-ip-file = fi_file
     v-op-file = "apmstr.quo".
  end.

  if opsys eq "UNIX" then
    unix silent quoter -c 1-3000 value(search(v-ip-file)) > value(v-op-file).
  else
    dos  silent quoter -c 1-3000 value(search(v-ip-file)) > value(v-op-file).

  input from value(v-op-file).

  output to value("apmstr.err").

  repeat:
    assign
     v-file = ""
     v-int  = v-int + 1.

    import v-file.

    find first vend
        where vend.company eq cocode
          and vend.vend-no eq substr(entry(1,v-file),1,8)
        no-lock no-error.

    status default "Processing Vendor: " + trim(substr(entry(1,v-file),1,8)).

    if avail vend then do:
      v-err = v-err + 1.
      put unformatted v-int " " v-file skip.
    end.

    else do:
      create vend.
      assign
       vend.company        = cocode
       vend.vend-no        = substr(entry(1,v-file),1,8)
       vend.name           = substr(entry(2,v-file),1,30)
       vend.add1           = substr(entry(3,v-file),1,30)
       vend.add2           = substr(entry(4,v-file),1,30)
       vend.city           = substr(entry(5,v-file),1,16)
       vend.state          = substr(entry(6,v-file),1,2)
       vend.country        = substr(entry(7,v-file),1,10)
       vend.zip            = substr(entry(8,v-file),1,10)
       vend.area-code      = substr(entry(9,v-file),2,3)
       vend.phone          = substr(entry(9,v-file),6,3) +
                             substr(entry(9,v-file),10,4)
       vend.fax-area       = substr(entry(10,v-file),2,3)
       vend.fax            = substr(entry(10,v-file),6,3) +
                             substr(entry(10,v-file),10,4)
       vend.remit          = substr(entry(18,v-file),1,30)
       vend.r-add1         = substr(entry(19,v-file),1,30)
       vend.r-add2         = substr(entry(20,v-file),1,30)
       vend.r-city         = substr(entry(21,v-file),1,16)
       vend.r-state        = substr(entry(22,v-file),1,2)
       vend.r-country      = substr(entry(23,v-file),1,10)
       vend.r-zip          = substr(entry(24,v-file),1,10)
       vend.terms          = substr(entry(26,v-file),1,5)
       vend.active         = "A"
       vend.fob-code       = "DEST"
       vend.frt-pay        = "P".

      if vend.remit eq "" then vend.remit = vend.name.

      if vend.r-add1    eq "" and
         vend.r-add2    eq "" and
         vend.r-city    eq "" and
         vend.r-state   eq "" and
         vend.r-zip     eq "" and
         vend.r-country eq "" and
         vend.r-postal  eq "" then
        assign
         vend.r-add1    = vend.add1
         vend.r-add2    = vend.add2
         vend.r-city    = vend.city
         vend.r-state   = vend.state
         vend.r-zip     = vend.zip
         vend.r-country = vend.country
         vend.r-postal  = vend.postal.
    end.
  end.

  output close.
  input close.

  status default "".

  if v-err gt 0 then message "Errors output to apmstr.err" VIEW-AS ALERT-BOX.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

