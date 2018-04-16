&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: est\globquot.w

  Description: Global Quote Price Change

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/02/2002

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
DEF VAR ll-new-file AS LOG NO-UNDO.

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

DEF TEMP-TABLE tt-rowid NO-UNDO FIELD row-id AS ROWID
                                INDEX row-id row-id.
DEFINE TEMP-TABLE tt-quoteqty LIKE quoteqty .

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

DEF STREAM excel.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cust end_cust begin_date ~
end_date begin_part-no end_part-no begin_fg-cat end_fg-cat begin_rm-no ~
end_rm-no percent_chg rd_i-code rd_pur-man rd_round-EA rd_round tb_prmtx ~
tb_undo fi_file btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_date end_date ~
begin_part-no end_part-no begin_fg-cat end_fg-cat begin_rm-no end_rm-no ~
percent_chg rd_i-code lbl_i-code rd_pur-man lbl_pur-man rd_round-EA ~
rd_round tb_prmtx tb_undo fi_file

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
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Product Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_part-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Board Code" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_fg-cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Product Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_part-no AS CHARACTER FORMAT "x(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Board Code" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_i-code AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_pur-man AS CHARACTER FORMAT "X(256)":U INITIAL "Purch/Manuf?" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE percent_chg AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Percent Change" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE rd_i-code AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Stock", "S",
"Custom", "C",
"All", "A"
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE rd_pur-man AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Purchased", yes,
"Manufactured", no,
"All", ?
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE rd_round AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Penny", "P",
"Dime", "D",
"Buck", "B",
"No Round", "N"
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE rd_round-EA AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Penny", "P",
"Dime", "D",
"Buck", "B",
"No Round", "N"
     SIZE 48 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 14.67.

DEFINE VARIABLE tb_prmtx AS LOGICAL INITIAL no 
     LABEL "Update Price Matrix?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_undo AS LOGICAL INITIAL no 
     LABEL "UNDO a prior price change?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-priceitm.csv" 
     LABEL "File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.43 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.43 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_date AT ROW 3.38 COL 29 COLON-ALIGNED
     end_date AT ROW 3.38 COL 73 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_part-no AT ROW 4.33 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Part Number"
     end_part-no AT ROW 4.33 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer Part Number"
     begin_fg-cat AT ROW 5.29 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Product Category"
     end_fg-cat AT ROW 5.29 COL 73 COLON-ALIGNED HELP
          "Enter Ending Product Category"
     begin_rm-no AT ROW 6.24 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Board Code" WIDGET-ID 16
     end_rm-no AT ROW 6.24 COL 73 COLON-ALIGNED HELP
          "Enter Ending Board Code" WIDGET-ID 18
     percent_chg AT ROW 8.05 COL 35 COLON-ALIGNED HELP
          "Enter a Negative or Positive Percentage"
     rd_i-code AT ROW 9.24 COL 38 NO-LABEL
     lbl_i-code AT ROW 9.29 COL 23 COLON-ALIGNED NO-LABEL
     rd_pur-man AT ROW 10.43 COL 38 NO-LABEL
     lbl_pur-man AT ROW 10.48 COL 19 COLON-ALIGNED NO-LABEL
     rd_round-EA AT ROW 11.62 COL 38 NO-LABEL WIDGET-ID 2
     rd_round AT ROW 12.81 COL 38 NO-LABEL
     tb_prmtx AT ROW 14.05 COL 35
     tb_undo AT ROW 15.24 COL 35
     fi_file AT ROW 15.24 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-process AT ROW 17.38 COL 23
     btn-cancel AT ROW 17.38 COL 55
     "For all other UOM, round up to:" VIEW-AS TEXT
          SIZE 30 BY .71 AT ROW 12.95 COL 7 WIDGET-ID 12
     "For UOM=EA, round up to:" VIEW-AS TEXT
          SIZE 26 BY .71 AT ROW 11.76 COL 11 WIDGET-ID 14
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-17 AT ROW 2.05 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.2 BY 18.48.


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
         TITLE              = "Global Quote Price Change"
         HEIGHT             = 18.48
         WIDTH              = 94.2
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
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_part-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_part-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_i-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_i-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_i-code".

/* SETTINGS FOR FILL-IN lbl_pur-man IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_pur-man:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_pur-man".

ASSIGN 
       percent_chg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_i-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_pur-man:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_round:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_round-EA:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prmtx:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_undo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Global Quote Price Change */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Global Quote Price Change */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fg-cat C-Win
ON LEAVE OF begin_fg-cat IN FRAME FRAME-A /* Beginning Product Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Board Code */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  def var v-process as log INIT NO no-undo.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  /*message "Are you sure you want to change the Quote Price(s) within the " +
          "selection parameters?"
          view-as alert-box question button yes-no update v-process.*/

  /*if v-process then*/ run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fg-cat C-Win
ON LEAVE OF end_fg-cat IN FRAME FRAME-A /* Ending Product Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Board Code */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME percent_chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL percent_chg C-Win
ON LEAVE OF percent_chg IN FRAME FRAME-A /* Percent Change */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_pur-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_pur-man C-Win
ON VALUE-CHANGED OF rd_pur-man IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_round
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_round C-Win
ON VALUE-CHANGED OF rd_round IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_round-EA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_round-EA C-Win
ON VALUE-CHANGED OF rd_round-EA IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prmtx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prmtx C-Win
ON VALUE-CHANGED OF tb_prmtx IN FRAME FRAME-A /* Update Price Matrix? */
DO:
  assign {&self-name}.
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

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    tb_undo:HIDDEN  = YES .
    APPLY "entry" TO begin_cust.
  END.

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
  DISPLAY begin_cust end_cust begin_date end_date begin_part-no end_part-no 
          begin_fg-cat end_fg-cat begin_rm-no end_rm-no percent_chg rd_i-code 
          lbl_i-code rd_pur-man lbl_pur-man rd_round-EA rd_round tb_prmtx 
          tb_undo fi_file
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_cust end_cust begin_date end_date begin_part-no 
         end_part-no begin_fg-cat end_fg-cat begin_rm-no end_rm-no percent_chg 
         rd_i-code rd_pur-man rd_round-EA rd_round tb_prmtx tb_undo fi_file  
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-params C-Win 
PROCEDURE get-params :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-fld-list as cha no-undo.

  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.


  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
    if not valid-handle(lv-field-hdl) then leave.

    if lookup(lv-field-hdl:private-data,"parm") > 0 then do:
      if lv-field-hdl:label <> ? then 
        op-fld-list = trim(op-fld-list) + " " +
                      lv-field-hdl:label + ":" +
                      lv-field-hdl:screen-value + ",".

      else do:  /* radio set */
        lv-field2-hdl = lv-group-hdl:first-child.
        repeat:
          if not valid-handle(lv-field2-hdl) then leave.

          if lv-field2-hdl:private-data = lv-field-hdl:name THEN
            op-fld-list = trim(op-fld-list) + " " +
                          lv-field2-hdl:screen-value + ":".

          lv-field2-hdl = lv-field2-hdl:next-sibling.                 
        end. 

        op-fld-list = trim(op-fld-list) +
                      lv-field-hdl:screen-value + ",".      
      end.                 
    end.

    lv-field-hdl = lv-field-hdl:next-sibling.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE round-up C-Win 
PROCEDURE round-up :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipd-new-value AS DEC NO-UNDO.
DEF INPUT PARAMETER ipd-calc-value AS DEC NO-UNDO.
DEF INPUT PARAMETER ipc-type AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opd-out-dec AS DEC NO-UNDO.
/*
1. ROUND
2. IF the truncated old value is same as the new value,
   it was rounded down, so then move it up 
*/

CASE ipc-type:
    WHEN "B" THEN DO:
        IF ipd-new-value - TRUNC(ipd-new-value, 0) GT 0 THEN
            ipd-calc-value = ipd-calc-value + 1.
    END.
    WHEN "D" THEN DO:
        IF ipd-new-value - TRUNC(ipd-new-value, 1) GT 0 THEN
            ipd-calc-value = ipd-calc-value + .1.
    END.
END CASE.
opd-out-dec = ipd-calc-value.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------- cec/quoprchg.p 08/96 FWK */
/* Global Price Change for Quote Sell Price                                   */
/* -------------------------------------------------------------------------- */

def var v-pct               as   dec           format "->>9.99".
def var v-round             as   char          format "!"           init "P".
def var v-round-EA           as   char          format "!"           init "P".
def var v-undo              as   log           format "yes/no"      init no.
def var v                   as   int.
def var v-EA                as   int.
DEF VAR v-orig-price        AS   DEC NO-UNDO.

def var fcust as ch init "" no-undo.
def var tcust like fcust init "zzzzzzzz" no-undo.
def var fdate as date init today no-undo.
def var tdate like fdate init today no-undo.

DEF VAR lv-part-no LIKE quoteitm.part-no NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR ll AS LOG EXTENT 2 NO-UNDO.
DEF VAR lv-reft LIKE reftable.reftable NO-UNDO.
DEF VAR lv-dscr LIKE reftable.dscr NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
def var v-process as log INIT NO no-undo.
DEF BUFFER bf-quoteitm FOR quoteitm.

assign
 fcust   = begin_cust
 tcust   = end_cust
 fdate   = begin_date
 tdate   = end_date
 v-pct   = percent_chg
 v-round = rd_round
 v-round-EA = rd_round-EA
 v-undo  = /*tb_undo*/ NO .

session:set-wait-state("General").

EMPTY TEMP-TABLE tt-rowid.
EMPTY TEMP-TABLE tt-quoteqty .

lv-reft = "est/globquot.w"              + " " +
          STRING(cocode,"x(10)")        + " " +
          STRING(YEAR(TODAY),"9999")    +
          STRING(MONTH(TODAY),"99")     +
          STRING(DAY(TODAY),"99")       + " " +
          STRING(TIME,"99999")          + " " +
          USERID("nosweat")             + " " +
          STRING(cocode,"x(10)")        + " " +
          STRING(locode,"x(10)").

RUN get-params (OUTPUT lv-dscr).

v = index("DP",v-round).
v-EA = index("DP",v-round-EA).

OUTPUT STREAM excel TO VALUE(fi_file).
   excelheader = "Quote,Customer,Estimate,cust part,Item No,Product Category,Qty,Old Price,New Price"
               .

   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

for each quotehd
    where quotehd.company  eq cocode
      and quotehd.loc      eq locode
      and quotehd.cust-no  ge fcust
      and quotehd.cust-no  le tcust
      and quotehd.quo-date ge fdate
      and quotehd.quo-date le tdate
    use-index cust2,

    each quoteitm
    where quoteitm.company eq quotehd.company
      and quoteitm.loc     eq quotehd.loc
      and quoteitm.q-no    eq quotehd.q-no
      and quoteitm.part-no ge begin_part-no
      and quoteitm.part-no le end_part-no
    no-lock

    break by quotehd.q-no:

  IF begin_rm-no NE "" AND NOT END_rm-no BEGINS "zzzzz" AND quotehd.est-no NE "" THEN DO:
      FIND FIRST eb
          WHERE eb.company EQ quotehd.company
            AND eb.est-no EQ quotehd.est-no
            AND eb.part-no EQ  quoteitm.part-no
          NO-LOCK NO-ERROR.
        IF NOT AVAIL eb THEN NEXT.
        IF NOT CAN-FIND(FIRST ef OF eb
            WHERE ef.board GE begin_rm-no
              AND ef.board LE END_rm-no) 
            THEN NEXT.
  END.
  IF FIRST-OF(quotehd.q-no) THEN ll[1] = NO.

  ASSIGN
   ll[2]    = NO
   lv-rowid = ?.

  RELEASE itemfg.

  IF ll-new-file THEN DO:
    lv-part-no = quoteitm.part-no.
    RUN custom/getcpart.p (quotehd.company, quotehd.cust-no,
                           INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
  END.

  IF lv-rowid EQ ? THEN
    FIND FIRST itemfg
        WHERE itemfg.company  EQ quoteitm.company
          AND itemfg.part-no  EQ quoteitm.part-no
          AND itemfg.part-no  NE ""
          AND (itemfg.cust-no EQ quotehd.cust-no OR
               itemfg.i-code  EQ "S")
        NO-LOCK NO-ERROR.
  ELSE
    FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.

  IF NOT AVAIL itemfg                                                 OR
     CAN-FIND(FIRST itemfg
              WHERE ROWID(itemfg)   EQ lv-rowid
                AND itemfg.procat   GE begin_fg-cat
                AND itemfg.procat   LE end_fg-cat
                AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")
                AND (itemfg.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)
                AND lv-rowid        NE ?)                             OR
     CAN-FIND(FIRST itemfg
              WHERE itemfg.company  EQ quoteitm.company
                AND itemfg.part-no  EQ quoteitm.part-no
                AND itemfg.part-no  NE ""
                AND (itemfg.cust-no EQ quotehd.cust-no OR
                     itemfg.i-code  EQ "S")
                AND itemfg.procat   GE begin_fg-cat
                AND itemfg.procat   LE end_fg-cat
                AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")
                AND (itemfg.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)
                AND lv-rowid        EQ ?)                             THEN
  for each quoteqty
      where quoteqty.company eq quoteitm.company
        and quoteqty.loc     eq quoteitm.loc
        and quoteqty.q-no    eq quoteitm.q-no
        and quoteqty.line    eq quoteitm.line
        AND NOT CAN-FIND(FIRST tt-rowid
                         WHERE tt-rowid.row-id EQ ROWID(quoteqty))
      BREAK BY quoteqty.qty:

    ll = YES.
    
     BUFFER-COPY  quoteqty TO  tt-quoteqty.

    v-orig-price = quoteqty.price.
    if v-undo then
      tt-quoteqty.price = tt-quoteqty.price / (1 - (v-pct / 100)).
    else
      tt-quoteqty.price = tt-quoteqty.price + (tt-quoteqty.price * v-pct / 100).
    v-orig-price = tt-quoteqty.price.

    /* Perform rounding */
    IF NOT v-round = "N" AND tt-quoteqty.uom NE "EA" THEN DO:
        tt-quoteqty.price = ROUND(tt-quoteqty.price, v).    
        IF tt-quoteqty.price LT v-orig-price THEN
          RUN round-up (INPUT v-orig-price, INPUT tt-quoteqty.price, INPUT v-round, OUTPUT tt-quoteqty.price).
    END.
    IF NOT v-round-EA = "N" AND tt-quoteqty.uom EQ "EA" THEN DO:
        tt-quoteqty.price = ROUND(tt-quoteqty.price, v-EA).    
        IF tt-quoteqty.price LT v-orig-price THEN
          RUN round-up (INPUT v-orig-price, INPUT tt-quoteqty.price, INPUT v-round-EA, OUTPUT tt-quoteqty.price).
    END.

    PUT STREAM excel UNFORMATTED
             '"' tt-quoteqty.q-no                              '",'
             '"' quotehd.cust-no                                '",'
             '"' quotehd.est-no                                 '",'
             '"' quoteitm.part-no                               '",'
             '"' quoteitm.i-no                                  '",'
             '"' itemfg.procat                                  '",'
             '"' quoteqty.qty                                   '",'
             '"' quoteqty.price                                 '",'
             '"' tt-quoteqty.price                              '",'
             SKIP.
     
  end.
end.

OUTPUT STREAM excel CLOSE.
OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).

 message "Are you sure you want to commit these changes"
     view-as alert-box question button yes-no update v-process.
 
 IF NOT v-process THEN RETURN .

IF v-process THEN
for each quotehd
    where quotehd.company  eq cocode
      and quotehd.loc      eq locode
      and quotehd.cust-no  ge fcust
      and quotehd.cust-no  le tcust
      and quotehd.quo-date ge fdate
      and quotehd.quo-date le tdate
    use-index cust2,

    each quoteitm
    where quoteitm.company eq quotehd.company
      and quoteitm.loc     eq quotehd.loc
      and quoteitm.q-no    eq quotehd.q-no
      and quoteitm.part-no ge begin_part-no
      and quoteitm.part-no le end_part-no
    no-lock

    break by quotehd.q-no:

  IF begin_rm-no NE "" AND NOT END_rm-no BEGINS "zzzzz" AND quotehd.est-no NE "" THEN DO:
      FIND FIRST eb
          WHERE eb.company EQ quotehd.company
            AND eb.est-no EQ quotehd.est-no
            AND eb.part-no EQ  quoteitm.part-no
          NO-LOCK NO-ERROR.
        IF NOT AVAIL eb THEN NEXT.
        IF NOT CAN-FIND(FIRST ef OF eb
            WHERE ef.board GE begin_rm-no
              AND ef.board LE END_rm-no) 
            THEN NEXT.
  END.
  IF FIRST-OF(quotehd.q-no) THEN ll[1] = NO.

  ASSIGN
   ll[2]    = NO
   lv-rowid = ?.

  RELEASE itemfg.

  IF ll-new-file THEN DO:
    lv-part-no = quoteitm.part-no.
    RUN custom/getcpart.p (quotehd.company, quotehd.cust-no,
                           INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).
  END.

  IF lv-rowid EQ ? THEN
    FIND FIRST itemfg
        WHERE itemfg.company  EQ quoteitm.company
          AND itemfg.part-no  EQ quoteitm.part-no
          AND itemfg.part-no  NE ""
          AND (itemfg.cust-no EQ quotehd.cust-no OR
               itemfg.i-code  EQ "S")
        NO-LOCK NO-ERROR.
  ELSE
    FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.

  IF NOT AVAIL itemfg                                                 OR
     CAN-FIND(FIRST itemfg
              WHERE ROWID(itemfg)   EQ lv-rowid
                AND itemfg.procat   GE begin_fg-cat
                AND itemfg.procat   LE end_fg-cat
                AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")
                AND (itemfg.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)
                AND lv-rowid        NE ?)                             OR
     CAN-FIND(FIRST itemfg
              WHERE itemfg.company  EQ quoteitm.company
                AND itemfg.part-no  EQ quoteitm.part-no
                AND itemfg.part-no  NE ""
                AND (itemfg.cust-no EQ quotehd.cust-no OR
                     itemfg.i-code  EQ "S")
                AND itemfg.procat   GE begin_fg-cat
                AND itemfg.procat   LE end_fg-cat
                AND (itemfg.i-code  EQ rd_i-code OR rd_i-code EQ "A")
                AND (itemfg.pur-man EQ rd_pur-man OR rd_pur-man EQ ?)
                AND lv-rowid        EQ ?)                             THEN
  for each quoteqty
      where quoteqty.company eq quoteitm.company
        and quoteqty.loc     eq quoteitm.loc
        and quoteqty.q-no    eq quoteitm.q-no
        and quoteqty.line    eq quoteitm.line
        AND NOT CAN-FIND(FIRST tt-rowid
                         WHERE tt-rowid.row-id EQ ROWID(quoteqty))
      BREAK BY quoteqty.qty:

    ll = YES.

    /* In case they mix buck and penny - task 10151205 */

    CREATE tt-rowid.
    tt-rowid.row-id = ROWID(quoteqty).

    CREATE reftable.
    ASSIGN
     reftable.reftable = lv-reft
     reftable.company  = STRING(quoteqty.q-no,"9999999999")
     reftable.loc      = STRING(quoteqty.line,"9999999999")
     reftable.code     = STRING(quoteqty.qty,"9999999999")
     reftable.code2    = STRING(quoteqty.rels,"9999999999")
     reftable.dscr     = lv-dscr
     reftable.val[1]   = quoteqty.price * 1000
     reftable.val[2]   = v-pct
     reftable.val[3]   = v.

    v-orig-price = quoteqty.price.
    if v-undo then
      quoteqty.price = quoteqty.price / (1 - (v-pct / 100)).
    else
      quoteqty.price = quoteqty.price + (quoteqty.price * v-pct / 100).
    v-orig-price = quoteqty.price.

    /* Perform rounding */
    IF NOT v-round = "N" AND quoteqty.uom NE "EA" THEN DO:
        quoteqty.price = ROUND(quoteqty.price, v).    
        IF quoteqty.price LT v-orig-price THEN
          RUN round-up (INPUT v-orig-price, INPUT quoteqty.price, INPUT v-round, OUTPUT quoteqty.price).
    END.
    IF NOT v-round-EA = "N" AND quoteqty.uom EQ "EA" THEN DO:
        quoteqty.price = ROUND(quoteqty.price, v-EA).    
        IF quoteqty.price LT v-orig-price THEN
          RUN round-up (INPUT v-orig-price, INPUT quoteqty.price, INPUT v-round-EA, OUTPUT quoteqty.price).
    END.


     reftable.val[4]   = quoteqty.price * 1000.
  end.

  for each quoteqty
    where quoteqty.company eq quoteitm.company
      and quoteqty.loc     eq quoteitm.loc
      and quoteqty.q-no    eq quoteitm.q-no
      and quoteqty.line    eq quoteitm.line
    BY quoteqty.qty:

    /* 11021209 - itm price should be first qty price per Joe */
    FIND bf-quoteitm WHERE ROWID(bf-quoteitm) EQ ROWID(quoteitm)
                     EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-quoteitm THEN DO:
       bf-quoteitm.price = quoteqty.price.

        RELEASE bf-quoteitm.
        LEAVE.
    END.

  END.
  IF tb_prmtx AND ll[2] THEN RUN oe/updprmtx.p (ROWID(quoteitm), "", 0, "", 0).

  IF LAST-OF(quotehd.q-no) AND ll[1] THEN quotehd.quo-date = TODAY.
end. 

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

session:set-wait-state("").  

message trim(c-win:title) + " Process Is Completed." view-as alert-box.

apply "close" to this-procedure.

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

