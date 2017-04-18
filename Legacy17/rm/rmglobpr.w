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

DEF TEMP-TABLE tt-rowid NO-UNDO FIELD row-id AS ROWID
                                INDEX row-id row-id.

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
&Scoped-Define ENABLED-OBJECTS begin_upd-code begin_rm-no end_rm-no ~
begin_vend end_vend percent_chg amount_chg tb_undo select-mat btn-process ~
btn-cancel RECT-17 
&Scoped-Define DISPLAYED-OBJECTS begin_upd-code begin_rm-no end_rm-no ~
begin_vend end_vend percent_chg amount_chg tb_undo select-mat 

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

DEFINE VARIABLE amount_chg AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "$Amt Change" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_upd-code AS CHARACTER FORMAT "xx":U 
     LABEL "Updated Code" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE mat-types AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material Types" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE percent_chg AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Percent Change" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 15.48.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 44 BY 5.71 NO-UNDO.

DEFINE VARIABLE tb_undo AS LOGICAL INITIAL no 
     LABEL "UNDO a prior price change?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_upd-code AT ROW 2.91 COL 25 COLON-ALIGNED HELP
          "Enter an Update Code"
     begin_rm-no AT ROW 3.86 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 3.86 COL 66 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_vend AT ROW 4.81 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 4.81 COL 66 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     percent_chg AT ROW 5.76 COL 25 COLON-ALIGNED HELP
          "Enter a Negative or Positive Percentage"
     amount_chg AT ROW 6.71 COL 25 COLON-ALIGNED HELP
          "Enter a Negative or Positive Percentage"
     tb_undo AT ROW 8.14 COL 27
     select-mat AT ROW 10.52 COL 27 HELP
          "Enter description of this Material Type." NO-LABEL
     mat-types AT ROW 13.14 COL 17 COLON-ALIGNED
     btn-process AT ROW 17.67 COL 21
     btn-cancel AT ROW 17.67 COL 53
     RECT-17 AT ROW 1.24 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.71 COL 5
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 38 BY 1 AT ROW 9.57 COL 30
          FONT 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 18.91.


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
         TITLE              = "Global Price Change"
         HEIGHT             = 19.05
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       amount_chg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_upd-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       percent_chg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_undo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Global Price Change */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Global Price Change */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME amount_chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amount_chg C-Win
ON LEAVE OF amount_chg IN FRAME FRAME-A /* $Amt Change */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amount_chg C-Win
ON VALUE-CHANGED OF amount_chg IN FRAME FRAME-A /* $Amt Change */
DO:
  IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
    percent_chg:HIDDEN = NO.
  ELSE
    ASSIGN
     percent_chg:SCREEN-VALUE = "0"
     percent_chg:HIDDEN       = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_upd-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_upd-code C-Win
ON LEAVE OF begin_upd-code IN FRAME FRAME-A /* Updated Code */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:19 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  v-process = NO.

  MESSAGE "Are you sure you want to change the Price Matrix(es) within the " +
          "selection parameters?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:19 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mat-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mat-types C-Win
ON LEAVE OF mat-types IN FRAME FRAME-A /* Material Types */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME percent_chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL percent_chg C-Win
ON LEAVE OF percent_chg IN FRAME FRAME-A /* Percent Change */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL percent_chg C-Win
ON VALUE-CHANGED OF percent_chg IN FRAME FRAME-A /* Percent Change */
DO:
  IF DEC({&self-name}:SCREEN-VALUE) EQ 0 THEN
    amount_chg:HIDDEN = NO.
  ELSE
    ASSIGN
     amount_chg:SCREEN-VALUE = "0"
     amount_chg:HIDDEN       = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
def var v-mat-list as char no-undo.

{sys/inc/f3helpw.i}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:19 am */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  for each mat:
    v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
  end.
  if substr(v-mat-list,length(trim(v-mat-list)),1) eq "," then
    substr(v-mat-list,length(trim(v-mat-list)),1) = "".

  select-mat:list-items = v-mat-list.

  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p on 04.18.2017 @ 11:36:41 am */
    {methods/setButton.i btn-process "Start"} /* added by script _nonAdm1Images1.p on 04.18.2017 @ 11:36:41 am */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:19 am */
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
  DISPLAY begin_upd-code begin_rm-no end_rm-no begin_vend end_vend percent_chg 
          amount_chg tb_undo select-mat 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_upd-code begin_rm-no end_rm-no begin_vend end_vend percent_chg 
         amount_chg tb_undo select-mat btn-process btn-cancel RECT-17 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* --------------------------------------------------- rm/gprcchg.p 05/96 JLF */
/* Global Price Change by Mat-type --- for est mat only                       */
/* -------------------------------------------------------------------------- */

def var v-i-no              like item.i-no          extent 2.
def var v-vend-no           like item.vend-no       extent 2.
def var v-mat-type          like item.mat-type      init "*".
def var v-price-code        like item.price-code.
def var v-pct               as   dec                format "->>9.99".
def var v-mtype             as   char               format "x(47)".

DEF VAR li AS INT NO-UNDO.


DO WITH FRAME {&FRAME-NAME}:
  ASSIGN {&displayed-objects}.
END.

ASSIGN
 v-i-no[1]    = begin_rm-no
 v-i-no[2]    = end_rm-no
 v-vend-no[1] = begin_vend
 v-vend-no[2] = end_vend
 v-price-code = begin_upd-code
 v-pct        = percent_chg.

do with frame {&frame-name}:
  do i = 1 to select-mat:num-items:
    if select-mat:is-selected(i) then
      v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
  end.

  if v-mtype ne "" and substr(v-mtype,length(trim(v-mtype)),1) eq "," then
    substr(v-mtype,length(trim(v-mtype)),1) = "".

  mat-types = v-mtype.

  do i = 1 to length(mat-types):
    if substr(mat-types,i,1) eq "," then substr(mat-types,i,1) = " ".
  end.

  display mat-types.
end.

SESSION:SET-WAIT-STATE("General").

EMPTY TEMP-TABLE tt-rowid.

CREATE reftable.
ASSIGN
 reftable.reftable = "rm/rmglobpr.w"
 reftable.company  = cocode
 reftable.loc      = STRING(YEAR(TODAY),"9999") +
                     STRING(MONTH(TODAY),"99")  +
                     STRING(DAY(TODAY),"99")
 reftable.code     = STRING(TIME,"99999")
 reftable.code2    = USERID("nosweat").

RUN get-params (OUTPUT reftable.dscr).

reftable.dscr = TRIM(reftable.dscr) + " " + "Mat Types:" + v-mtype.

DO li = 1 TO LENGTH(v-mtype):
  FOR EACH item
      {sys/look/itemW.i}   
        AND item.mat-type EQ SUBSTR(v-mtype,li,1)
        AND item.i-no     GE v-i-no[1]
        AND item.i-no     LE v-i-no[2]

    {rm/gprcchg1.i item}

    item.price-code = v-price-code.
  END.
END.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

