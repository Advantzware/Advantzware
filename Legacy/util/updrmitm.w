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

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_rm-i-no end_rm-i-no btn-process ~
btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-i-no end_rm-i-no 

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

DEFINE VARIABLE begin_rm-i-no AS CHARACTER FORMAT "X(11)":U 
     LABEL "Old RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-i-no AS CHARACTER FORMAT "x(10)":U 
     LABEL "New RM Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-i-no AT ROW 9.57 COL 19 COLON-ALIGNED
     end_rm-i-no AT ROW 12.19 COL 19 COLON-ALIGNED
     btn-process AT ROW 16.71 COL 21
     btn-cancel AT ROW 16.71 COL 53
     RECT-17 AT ROW 6.71 COL 1
     " '!' to capitalize all)" VIEW-AS TEXT
          SIZE 44 BY .95 AT ROW 10.52 COL 43
          FONT 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 7.19 COL 5
     "('*' to replace ',' with '-' or" VIEW-AS TEXT
          SIZE 45 BY .95 AT ROW 9.57 COL 43
          FONT 6
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.67 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "These files must be mannually updated!" VIEW-AS TEXT
          SIZE 50 BY .95 AT ROW 5.29 COL 19
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "Please Note:  Procedure will NOT update files in use by other users!" VIEW-AS TEXT
          SIZE 81 BY .95 AT ROW 4.33 COL 5
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .81 AT ROW 3.76 COL 1
          BGCOLOR 11 
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.71 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 1
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 5.81
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
         TITLE              = "Update RM Item Number"
         HEIGHT             = 17.71
         WIDTH              = 90.2
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
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


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update RM Item Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update RM Item Number */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-i-no C-Win
ON LEAVE OF begin_rm-i-no IN FRAME FRAME-A /* Old RM Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-begin_rm-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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
  RUN valid-begin_rm-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  IF begin_rm-i-no EQ "/" OR begin_rm-i-no eq "*" OR
     (begin_rm-i-no BEGINS "!" AND
      NOT CAN-FIND(FIRST item
                   WHERE item.company EQ cocode
                     AND item.i-no    EQ begin_rm-i-no)) THEN
    ASSIGN
     end_rm-i-no              = ""
     end_rm-i-no:SCREEN-VALUE = end_rm-i-no.

  else
  if end_rm-i-no eq "" then do:
    message "ERROR: The new RM# cannot be spaces" view-as alert-box error.
    return no-apply.
  end.

  else
  if can-find(first item
              where item.company eq cocode
                and item.i-no    eq end_rm-i-no) then do:
    v-process = no.

    message "The new RM# already exists, merge old RM# into new RM#?"
            view-as alert-box question button yes-no update v-process.

    if not v-process then return no-apply.
  end.

  run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-i-no C-Win
ON LEAVE OF end_rm-i-no IN FRAME FRAME-A /* New RM Item# */
DO:
  {&self-name}:screen-value = caps({&self-name}:screen-value).
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
     RETURN .
  END.

  RUN enable_UI.
  apply "entry" to begin_rm-i-no.
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
  DISPLAY begin_rm-i-no end_rm-i-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_rm-i-no end_rm-i-no btn-process btn-cancel RECT-17 
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
/* ------------------------------------------------ util/updfgitm.p 09/99 JLF */
/*  Update FG Item with a new No or Est No                                    */
/* -------------------------------------------------------------------------- */

def buffer b-item for item.

def var v-item      like item.i-no init "".
def var v-new-item  like item.i-no.
def var v-char      as   char.


assign
 v-item     = begin_rm-i-no
 v-new-item = caps(end_rm-i-no)
 v-process  = no.

/*if v-item eq "/" then
  message "Are you sure you wish to update each RM Item# with its Estimate#?"
      view-as alert-box question button yes-no
      update v-process.

else*/
if v-item eq "*" then
  message "Are you sure you wish to replace commas in all RM Item#'s with"
          "dashes?"
      view-as alert-box question button yes-no
      update v-process.

else
IF v-item BEGINS "!"                            AND
   NOT CAN-FIND(FIRST item
                WHERE item.company EQ cocode
                  AND item.i-no    EQ v-item) THEN DO:
  assign
   v-new-item = substr(v-item,2,15)
   v-item     = substr(v-item,1,1).

  message "Are you sure you wish to capitalize all RM Item#'s" +
          (if v-new-item ne "" then " that begin " + trim(v-new-item) else "") + "?"
      view-as alert-box question button yes-no
      update v-process.
end.

else
  message "Are you sure you want change RM Item#" trim(caps(v-item))
          "to" trim(caps(v-new-item)) + "?"       
      view-as alert-box question button yes-no update v-process.

if v-process then do:
  session:set-wait-state("General").

  if index("*!",v-item) gt 0 then
  for each item
      where item.company           eq cocode
        and ((v-item               eq "*" and
              index(item.i-no,",") gt 0)              or
             (v-item               eq "!" and
              item.i-no            begins v-new-item))
      no-lock:

    STATUS DEFAULT "Processing RM Item#: " + TRIM(item.i-no).

    if v-item eq "*" then
    do i = 1 to length(trim(item.i-no)):
      v-char = substr(item.i-no,i,1).

      if v-char eq "," then v-char = "-".

      substr(v-new-item,i,1) = v-char.
    end.

    run rm/updrmitm.p (recid(item), if v-item eq "!" then "!" else v-new-item).
  end.

  else
  for each item
      where item.company eq cocode
        and item.i-no    eq v-item
      no-lock:

    STATUS DEFAULT "Processing RM Item#: " + TRIM(item.i-no).

    run rm/updrmitm.p (recid(item), v-new-item).
  end.

  STATUS DEFAULT "".

  session:set-wait-state("").

  message trim(c-win:title) + " Process Complete..." view-as alert-box.

  apply "close" to this-procedure.
end.

return no-apply.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-begin_rm-i-no C-Win 
PROCEDURE valid-begin_rm-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    begin_rm-i-no:SCREEN-VALUE = CAPS(begin_rm-i-no:SCREEN-VALUE).

    IF begin_rm-i-no:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE TRIM(begin_rm-i-no:LABEL + " cannot be spaces...")
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_rm-i-no.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

