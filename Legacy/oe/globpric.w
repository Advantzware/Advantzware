&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oe\globpric.w

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

def var start-cust-no   like oe-prmtx.cust-no NO-UNDO.
def var end-cust-no     like oe-prmtx.cust-no init "zzzzzzzz" NO-UNDO.

def var start-cust-type like oe-prmtx.custype NO-UNDO.
def var end-cust-type   like oe-prmtx.custype init "zzzzzzzz" NO-UNDO.

def var start-item-no   like oe-prmtx.i-no NO-UNDO.
def var end-item-no     like oe-prmtx.i-no init "zzzzzzzzzzzzzzz" NO-UNDO.

def var start-prod-cat  like oe-prmtx.procat NO-UNDO.
def var end-prod-cat    like oe-prmtx.procat init "zzzzz" NO-UNDO.

def var start-level     as integer init 1  format ">9" NO-UNDO.
def var end-level       as integer init 10 format ">9" NO-UNDO.

def var price-basis     as char format "X" init "P" NO-UNDO.
def var pct-divide      as char format "X" init "D" NO-UNDO.

def var percent-change  as decimal format ">>9.99-" NO-UNDO.

DEF VAR li-factor       AS INT NO-UNDO.

def var ctr             as INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cust end_cust begin_cust-type ~
end_cust-type begin_i-no end_i-no begin_cat end_cat begin_level end_level ~
beg_eff_date end_eff_date tg_new_eff_date rd_basis rd_divide percent_chg ~
rd_round btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_cust-type ~
end_cust-type begin_i-no end_i-no begin_cat end_cat begin_level end_level ~
beg_eff_date end_eff_date new_eff_date tg_new_eff_date tg_newmatrix ~
lbl_price rd_basis rd_divide percent_chg lbl_rnd-meth rd_round 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isLatestEffDate C-Win 
FUNCTION isLatestEffDate RETURNS LOGICAL
  ( BUFFER ipbf-oe-prmtx FOR oe-prmtx)  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Type" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_level AS INTEGER FORMAT ">>":U INITIAL 1 
     LABEL "Beginning Level" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE beg_eff_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beg. Effective Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Type" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_eff_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2999 
     LABEL "End. Effective Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_level AS INTEGER FORMAT ">>":U INITIAL 10 
     LABEL "Ending Level" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_price AS CHARACTER FORMAT "X(256)":U INITIAL "Price Basis?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_rnd-meth AS CHARACTER FORMAT "X(256)":U INITIAL "Rounding Method?" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE new_eff_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "New Effective Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE percent_chg AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Percent Change" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE rd_basis AS CHARACTER INITIAL "Price" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Discount", "Discount",
"Price", "Price"
     SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE rd_divide AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "% Profit", "D",
"% Increase", "M"
     SIZE 32 BY 1.19 TOOLTIP "Choose (% Profit) = Price / (100 - %) OR (%Increase) = Price + (Price * %)" NO-UNDO.

DEFINE VARIABLE rd_round AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Dollar", "B",
"Dime", "D",
"2 Decimals", "P",
"4 Decimals", "F"
     SIZE 49 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 15.24.

DEFINE VARIABLE tg_newmatrix AS LOGICAL INITIAL no 
     LABEL "Create New Matrix?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.8 BY .81 NO-UNDO.

DEFINE VARIABLE tg_new_eff_date AS LOGICAL INITIAL no 
     LABEL "Update Effective Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.67 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.67 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-type AT ROW 3.86 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Customer Type"
     end_cust-type AT ROW 3.86 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Type"
     begin_i-no AT ROW 5.05 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 5.05 COL 65 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cat AT ROW 6.24 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 6.24 COL 65 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_level AT ROW 7.43 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Level"
     end_level AT ROW 7.43 COL 65 COLON-ALIGNED HELP
          "Enter Ending Level"
     beg_eff_date AT ROW 8.62 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Effective Date"
     end_eff_date AT ROW 8.62 COL 65 COLON-ALIGNED HELP
          "Enter Ending Effective Date"
     new_eff_date AT ROW 9.95 COL 65 COLON-ALIGNED HELP
          "Enter Beginning Effective Date"
     tg_new_eff_date AT ROW 10.1 COL 19.2
     tg_newmatrix AT ROW 11 COL 19.2 WIDGET-ID 8
     lbl_price AT ROW 12.1 COL 24.8 COLON-ALIGNED NO-LABEL
     rd_basis AT ROW 12.1 COL 40.6 NO-LABEL
     rd_divide AT ROW 13.48 COL 53 HELP
          "% Profit = Price / (100 - %) , %Increase = Price + (Price * %)" NO-LABEL WIDGET-ID 2
     percent_chg AT ROW 13.52 COL 24.6 COLON-ALIGNED HELP
          "Enter a Negative or Positive Percentage"
     lbl_rnd-meth AT ROW 14.95 COL 17 COLON-ALIGNED NO-LABEL
     rd_round AT ROW 14.95 COL 39 NO-LABEL
     btn-process AT ROW 16.71 COL 21
     btn-cancel AT ROW 16.71 COL 53
     "Use" VIEW-AS TEXT
          SIZE 9 BY 1.1 AT ROW 13.48 COL 44 WIDGET-ID 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 1.48 COL 5
     RECT-17 AT ROW 1 COL 1
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
         TITLE              = "Global Price Matrix Change"
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
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN lbl_price IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_rnd-meth IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_rnd-meth:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_rnd-meth".

/* SETTINGS FOR FILL-IN new_eff_date IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_basis:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_divide:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_round:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tg_newmatrix IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Global Price Matrix Change */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Global Price Matrix Change */
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  MESSAGE "Are you sure you want to change the Price Matrix(es) within the " +
          "selection parameters?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_basis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_basis C-Win
ON VALUE-CHANGED OF rd_basis IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_divide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_divide C-Win
ON VALUE-CHANGED OF rd_divide IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tg_newmatrix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_newmatrix C-Win
ON VALUE-CHANGED OF tg_newmatrix IN FRAME FRAME-A /* Create New Matrix? */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN tg_newmatrix.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_new_eff_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_new_eff_date C-Win
ON VALUE-CHANGED OF tg_new_eff_date IN FRAME FRAME-A /* Update Effective Date? */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN tg_new_eff_date
            new_eff_date:SENSITIVE = tg_new_eff_date
            tg_newmatrix:SENSITIVE = tg_new_eff_date
            tg_newmatrix:SCREEN-VALUE = tg_new_eff_date:SCREEN-VALUE.
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.

  new_eff_date = TODAY.

  RUN enable_UI.
  {methods/nowait.i}

  APPLY "ENTRY":U TO begin_cust IN FRAME {&FRAME-NAME}.

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
  DISPLAY begin_cust end_cust begin_cust-type end_cust-type begin_i-no end_i-no 
          begin_cat end_cat begin_level end_level beg_eff_date end_eff_date 
          new_eff_date tg_new_eff_date tg_newmatrix lbl_price rd_basis rd_divide 
          percent_chg lbl_rnd-meth rd_round 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_cust end_cust begin_cust-type end_cust-type begin_i-no 
         end_i-no begin_cat end_cat begin_level end_level beg_eff_date 
         end_eff_date tg_new_eff_date rd_basis rd_divide percent_chg rd_round 
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE markdown C-Win 
PROCEDURE markdown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER ipb-oe-prmtx FOR oe-prmtx.

  do ctr = start-level to min(10,end-level):
    if price-basis eq "D" then 
      ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] +
                               (ipb-oe-prmtx.discount[ctr] * percent-change).
    else
    if price-basis eq "P" then 
      ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] +
                               (ipb-oe-prmtx.price[ctr] * percent-change).

    RUN rounding (INPUT-OUTPUT ipb-oe-prmtx.price[ctr]).
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE markup C-Win 
PROCEDURE markup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipb-oe-prmtx FOR oe-prmtx.

  do ctr = start-level to min(10,end-level):
    if percent-change eq 1 then do:
      if price-basis eq "D" then 
        ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] * 2.
      else
      if price-basis eq "P" then 
        ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] * 2.
    end.

    else do:
      if price-basis eq "D" then DO:
        IF pct-divide = "D" THEN
           ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] / (1 - percent-change).
        ELSE
           ipb-oe-prmtx.discount[ctr] = ipb-oe-prmtx.discount[ctr] * (1 + percent-change).
      END.
      ELSE 
      if price-basis eq "P" then DO:
        IF pct-divide = "D" THEN
           ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] / (1 - percent-change).
        ELSE
           ipb-oe-prmtx.price[ctr]    = ipb-oe-prmtx.price[ctr] * (1 + percent-change).
      END.
    end.
    RUN rounding (INPUT-OUTPUT ipb-oe-prmtx.price[ctr]).
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rounding C-Win 
PROCEDURE rounding :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-price AS DEC DECIMALS 10 NO-UNDO.


  io-price = io-price * li-factor.

  {sys/inc/roundup.i io-price}

  io-price = io-price / li-factor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ---------------------------------------------------oe/oe-pmgpc.p 05/00 DJK */
/* Global Price Matrix Change                                                 */
/* -------------------------------------------------------------------------- */

SESSION:SET-WAIT-STATE("General").

DEF VAR v-date AS DATE NO-UNDO.
DEF VAR v-date-str AS CHAR NO-UNDO.
DEF VAR v-start-i-no AS CHAR FORMAT "X(108)" NO-UNDO.
DEF BUFFER bf-oe-prmtx FOR oe-prmtx.

ASSIGN
 start-cust-no   = begin_cust
 end-cust-no     = end_cust
 start-cust-type = begin_cust-type
 end-cust-type   = end_cust-type
 start-item-no   = begin_i-no
 end-item-no     = end_i-no
 start-prod-cat  = begin_cat
 end-prod-cat    = end_cat
 start-level     = begin_level
 end-level       = end_level
 price-basis     = SUBSTR(rd_basis,1,1)
 pct-divide      = SUBSTR(rd_divide,1,1)
 percent-change  = percent_chg / 100
 li-factor       = 1.

DO ctr = 1 TO INDEX("DPTF",rd_round):
    li-factor = li-factor * 10.    
END.

REPEAT PRESELECT EACH oe-prmtx EXCLUSIVE-LOCK
    WHERE oe-prmtx.company    EQ cocode 
      AND oe-prmtx.cust-no    GE start-cust-no
      AND oe-prmtx.cust-no    LE end-cust-no
      AND oe-prmtx.custype    GE start-cust-type
      AND oe-prmtx.custype    LE end-cust-type
      AND oe-prmtx.procat     GE start-prod-cat
      AND oe-prmtx.procat     LE end-prod-cat
      AND oe-prmtx.i-no       GE start-item-no
      AND oe-prmtx.i-no       LE end-item-no
      AND oe-prmtx.eff-date   GE beg_eff_date
      AND oe-prmtx.eff-date   LE end_eff_date:

    FIND NEXT oe-prmtx.
    RELEASE bf-oe-prmtx.

/*     FIND FIRST reftable WHERE                            */
/*           reftable.rec_key  EQ oe-prmtx.rec_key AND      */
/*           reftable.company  EQ "oe-prmtx"                */
/*           USE-INDEX rec_key                              */
/*           NO-LOCK NO-ERROR.                              */
/*     IF AVAIL reftable THEN v-date = DATE(reftable.CODE). */
/*                                                          */
/*     v-start-i-no = SUBSTR(oe-prmtx.i-no,1,100).          */
/*     IF v-start-i-no GE start-item-no AND                 */
/*        v-start-i-no LE end-item-no AND                   */
/*        v-date GE beg_eff_date AND                        */
/*        v-date LE end_eff_date THEN                       */
/*        DO:                                               */
          IF tg_newmatrix THEN DO:
              IF isLatestEffDate(BUFFER oe-prmtx) THEN DO:
                CREATE bf-oe-prmtx.
                BUFFER-COPY oe-prmtx EXCEPT oe-prmtx.rec_key TO bf-oe-prmtx.
              END.
          END.
          ELSE
              FIND FIRST bf-oe-prmtx WHERE ROWID(bf-oe-prmtx) EQ ROWID(oe-prmtx).
          IF AVAIL bf-oe-prmtx THEN DO:
              IF tg_new_eff_date THEN
                  bf-oe-prmtx.eff-date = new_eff_date.
/*                  ASSIGN                                                    */
/*                     v-date-str = STRING(new_eff_date,"99/99/9999")         */
/*                     lb-oe-prmtx.i-no = STRING(lb-oe-prmtx.i-no,"x(100)") + */
/*                                     SUBSTR(v-date-str,7,4) +               */
/*                                     SUBSTR(v-date-str,1,2) +               */
/*                                     SUBSTR(v-date-str,4,2).                */

              IF percent-change LE 0 THEN
                 RUN markdown (BUFFER bf-oe-prmtx).
              ELSE 
                 RUN markup  (BUFFER bf-oe-prmtx).
          END.
/*        END. */
END.

SESSION:SET-WAIT-STATE("").

MESSAGE trim(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2002  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isLatestEffDate C-Win 
FUNCTION isLatestEffDate RETURNS LOGICAL
  ( BUFFER ipbf-oe-prmtx FOR oe-prmtx) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF BUFFER bf-oe-prmtx FOR oe-prmtx.
/*     DEF BUFFER bf-reftable FOR reftable. */

    FOR EACH bf-oe-prmtx NO-LOCK
        WHERE bf-oe-prmtx.company    EQ cocode 
            AND bf-oe-prmtx.cust-no    EQ ipbf-oe-prmtx.cust-no
            AND bf-oe-prmtx.custype    EQ ipbf-oe-prmtx.custype
            AND bf-oe-prmtx.procat     EQ ipbf-oe-prmtx.procat
            AND SUBSTR(bf-oe-prmtx.i-no,1,100) 
                EQ SUBSTR(ipbf-oe-prmtx.i-no,1,100)
            AND bf-oe-prmtx.rec_key NE ipbf-oe-prmtx.rec_key       
        :
/*         ,                                                 */
/*         FIRST bf-reftable WHERE                           */
/*           lb-reftable.rec_key  EQ lb-oe-prmtx.rec_key AND */
/*           lb-reftable.company  EQ "oe-prmtx"              */
/*           USE-INDEX rec_key                               */
/*           NO-LOCK */

          IF bf-oe-prmtx.eff-date GT ipbf-oe-prmtx.eff-date THEN 
            RETURN NO.
    END. /*each price matrix for same item*/

    RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

