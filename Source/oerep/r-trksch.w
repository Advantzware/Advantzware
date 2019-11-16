&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-trksch.w

  Description: Truck Plan Selection

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
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF VAR v-types AS CHAR NO-UNDO.

{oerep/tt-truck-stop.i NEW}

def var v-qty like oe-rel.qty.
def var v-ship-id like oe-rel.ship-id.
DEF VAR v-ship-text AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR v-del-zone AS CHAR NO-UNDO.
DEF VAR viRelNum AS INT NO-UNDO.
DEF VAR v-no-units AS INT NO-UNDO.
DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-dec AS DEC NO-UNDO.
DEF VAR v-decu AS DEC NO-UNDO.
DEF VAR vcTruckCode AS CHAR NO-UNDO.
DEF VAR vcTruckDscr AS CHAR NO-UNDO.
DEF VAR viStop      AS INT NO-UNDO.
DEF VAR vdShipDate  AS DATE NO-UNDO.
DEF VAR vcCity      AS CHAR NO-UNDO.
DEF VAR vcState     AS CHAR NO-UNDO.
DEF VAR vcZip       AS CHAR NO-UNDO.
DEF VAR viBolNo     AS INT NO-UNDO.
DEF VAR vcLoadNo    AS CHAR NO-UNDO.
DEF VAR char-val    AS CHAR NO-UNDO.
DEF VAR lv-unique-no AS INT NO-UNDO.
DEF VAR v-msf        AS DECI NO-UNDO.
DEF VAR v-weight     AS DECI NO-UNDO.
DEF VAR v-units-per-pallet AS INT NO-UNDO.

DEF TEMP-TABLE tt-report-bol NO-UNDO LIKE tt-report.

DEF BUFFER bf-oe-relh FOR oe-relh.
DEF BUFFER bf-tt-report FOR tt-report.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_date end_date begin_carr end_carr ~
tb_scheduled tb_actual tb_late tb_backordered tb_invoiceable tb_posted ~
btn-ok btn-cancel begin_loc end_loc begin_delv end_delv RECT-7 RECT-40 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_date end_date begin_carr end_carr ~
tb_scheduled tb_actual tb_late tb_backordered tb_invoiceable tb_posted ~
begin_loc end_loc begin_delv end_delv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_carr AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Carrier#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Release Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_delv AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Delivery Zone" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_carr AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Carrier#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Release Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_delv AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Delivery Zone" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 3.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 12.86.

DEFINE VARIABLE tb_actual AS LOGICAL INITIAL yes 
     LABEL "Actual" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE tb_backordered AS LOGICAL INITIAL yes 
     LABEL "Backorder" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE tb_invoiceable AS LOGICAL INITIAL yes 
     LABEL "Past Last Ship Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY .62 NO-UNDO.

DEFINE VARIABLE tb_late AS LOGICAL INITIAL yes 
     LABEL "Late" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "BOL" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.6 BY .62 NO-UNDO.

DEFINE VARIABLE tb_scheduled AS LOGICAL INITIAL yes 
     LABEL "Scheduled" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 1.95 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 1.95 COL 90 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-no AT ROW 2.91 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 2.91 COL 90 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_i-no AT ROW 3.86 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 3.86 COL 90 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_date AT ROW 7.95 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 7.95 COL 90 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_carr AT ROW 5.86 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Carrier Number"
     end_carr AT ROW 5.86 COL 90 COLON-ALIGNED HELP
          "Enter Ending Carrier Number"
     tb_scheduled AT ROW 10.52 COL 33.4
     tb_actual AT ROW 10.52 COL 57.6
     tb_late AT ROW 11.24 COL 33.4
     tb_backordered AT ROW 11.24 COL 57.6
     tb_invoiceable AT ROW 11.95 COL 33.4
     tb_posted AT ROW 10.52 COL 78.6
     btn-ok AT ROW 14.14 COL 31
     btn-cancel AT ROW 14.14 COL 79
     begin_loc AT ROW 4.81 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_loc AT ROW 4.81 COL 90 COLON-ALIGNED HELP
          "Enter Ending Warehouse"
     begin_delv AT ROW 6.95 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Delivery Zone"
     end_delv AT ROW 6.95 COL 90 COLON-ALIGNED HELP
          "Enter Ending Delivery Zone"
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Planned Releases" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.57 COL 33.4
     "Actual Releases" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 9.57 COL 57.4
     "Posted Releases" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 9.57 COL 78.4
     RECT-7 AT ROW 1 COL 1
     RECT-40 AT ROW 9.33 COL 30.4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.4 BY 14.71.


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
         TITLE              = "Truck Plan Selection"
         HEIGHT             = 14.71
         WIDTH              = 123.4
         MAX-HEIGHT         = 15
         MAX-WIDTH          = 123.4
         VIRTUAL-HEIGHT     = 15
         VIRTUAL-WIDTH      = 123.4
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
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_delv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_carr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_delv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_loc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_actual:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_backordered:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_invoiceable:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_late:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_scheduled:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Truck Plan Selection */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Truck Plan Selection */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_carr C-Win
ON LEAVE OF begin_carr IN FRAME FRAME-A /* Beginning Carrier# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Release Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_delv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_delv C-Win
ON HELP OF begin_delv IN FRAME FRAME-A /* Beginning Delivery Zone */
DO:
   run windows/l-zone.w (cocode, output char-val).
   if char-val <> "" THEN
      assign SELF:screen-value = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_delv C-Win
ON LEAVE OF begin_delv IN FRAME FRAME-A /* Beginning Delivery Zone */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* Beginning Warehouse */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  RUN run-report.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_carr C-Win
ON LEAVE OF end_carr IN FRAME FRAME-A /* Ending Carrier# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Release Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_delv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_delv C-Win
ON HELP OF end_delv IN FRAME FRAME-A /* Ending Delivery Zone */
DO:
   run windows/l-zone.w (cocode, output char-val).
   if char-val <> "" THEN
      assign SELF:screen-value = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_delv C-Win
ON LEAVE OF end_delv IN FRAME FRAME-A /* Ending Delivery Zone */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* Ending Warehouse */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_backordered
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_backordered C-Win
ON VALUE-CHANGED OF tb_backordered IN FRAME FRAME-A /* Backorder */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_invoiceable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_invoiceable C-Win
ON VALUE-CHANGED OF tb_invoiceable IN FRAME FRAME-A /* Past Last Ship Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_late
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_late C-Win
ON VALUE-CHANGED OF tb_late IN FRAME FRAME-A /* Late */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* BOL */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_scheduled
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_scheduled C-Win
ON VALUE-CHANGED OF tb_scheduled IN FRAME FRAME-A /* Scheduled */
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  begin_date = today.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-saved-recs C-Win 
PROCEDURE add-saved-recs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-last-unique AS INT.
   DEF VAR v-saved-pallets AS INT.

FOR EACH tt-report:
   FOR EACH truck-run-print WHERE truck-run-print.company EQ tt-report.company AND
         truck-run-print.ord-no  EQ tt-report.order-no AND
         truck-run-print.i-no  EQ tt-report.item-no AND
         truck-run-print.line  EQ tt-report.line-no AND
         truck-run-print.rel-no EQ tt-report.rel-no-internal AND
         truck-run-print.b-ord-no EQ tt-report.b-ord-no AND
         truck-run-print.po-no  EQ tt-report.po-no AND
         truck-run-print.rec_key NE tt-report.truck-print-key
         NO-LOCK.

     IF tt-report.link-no NE 0 
          AND truck-run-print.link-no NE tt-report.link-no THEN
         NEXT.
     FIND FIRST bf-tt-report WHERE bf-tt-report.truck-print-key = truck-run-print.rec_key
         NO-LOCK NO-ERROR.
     IF AVAIL bf-tt-report THEN
         NEXT.
     FIND LAST bf-tt-report USE-INDEX unique-no NO-LOCK NO-ERROR.
     IF AVAIL(bf-tt-report) THEN
         v-last-unique = bf-tt-report.unique-no + 1.

     CREATE bf-tt-report.
     BUFFER-COPY tt-report EXCEPT is-orig truck-print-key TO bf-tt-report .
     ASSIGN
       bf-tt-report.is-orig   = NO
       bf-tt-report.unique-no = v-last-unique.

     ASSIGN tt-report.carrier = truck-run-print.carrier
            vcTruckCode = truck-run-print.truck-code
            vcLoadNo    = truck-run-print.load-no
            viStop      = truck-run-print.stop-no
            v-saved-pallets = truck-run-print.spare-int-1
            vdShipDate  = truck-run-print.ship-date.

     FIND FIRST truck WHERE
          truck.company = cocode AND
          truck.loc     = locode AND
          truck.carrier = tt-report.carrier AND
          truck.truck-code = vcTruckCode
          NO-LOCK NO-ERROR.

     IF AVAIL truck THEN
     DO:
       vcTruckDscr = truck.truck-desc.
       RELEASE truck.
     END.

      ASSIGN
       bf-tt-report.truck-code    = vcTruckCode
       bf-tt-report.truck-dscr    = vcTruckDscr
       bf-tt-report.stop-no       = viStop
       bf-tt-report.load-no       = vcLoadNo
       bf-tt-report.pallets       = v-saved-pallets
       bf-tt-report.truck-print-key   = truck-run-print.rec_key
       bf-tt-report.ship-date     = IF vdShipDate <> ? THEN vdShipDate ELSE bf-tt-report.ship-date
       bf-tt-report.old-truck-code  = bf-tt-report.truck-code
       bf-tt-report.old-stop-no     = bf-tt-report.stop-no
       bf-tt-report.old-load-no     = bf-tt-report.load-no
       bf-tt-report.old-truck-dscr  = bf-tt-report.truck-dscr
       bf-tt-report.old-ship-date   = bf-tt-report.ship-date
       bf-tt-report.truck-print-key = truck-run-print.rec_key.

   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-tt-report-fields C-Win 
PROCEDURE assign-tt-report-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-first-truck AS ROWID.
   DEF VAR v-last-unique AS INT.
   DEF VAR v-truck-run-key AS CHAR.
   DEF VAR v-saved-pallets AS INT. 
   DEF BUFFER bf-tt-report FOR tt-report.
   v-first-truck = ?.
   v-truck-run-key = ?.
   v-saved-pallets = 0.
   ASSIGN
      tt-report.release-type  = tt-report.key-06
      tt-report.order-no      = oe-ord.ord-no
      tt-report.cust-no       = oe-ord.cust-no
      tt-report.ship-no       = v-ship-id
      tt-report.ship-to-text  = v-ship-text
      tt-report.cust-name     = oe-ord.cust-name
      tt-report.city          = vcCity
      tt-report.state         = vcState
      tt-report.zip           = vcZip
      tt-report.deliv-zone    = v-del-zone
      tt-report.rel-no        = viRelNum
      tt-report.no-units      = v-no-units
      tt-report.pallets       = v-pallets
      tt-report.weight        = v-weight
      tt-report.msf           = v-msf
      tt-report.bol-no        = viBolNo.

   IF tt-report.link-no EQ 0 AND tt-report.oe-rel-r-no NE 0 THEN
      FIND FIRST truck-run-print WHERE
           truck-run-print.company EQ tt-report.company AND
           truck-run-print.oe-rel-r-no EQ tt-report.oe-rel-r-no AND
           truck-run-print.spare-char-1 = tt-report.rec_key
           NO-LOCK NO-ERROR.
   ELSE
   IF tt-report.link-no EQ 0 THEN
      FIND FIRST truck-run-print WHERE
           truck-run-print.company EQ tt-report.company AND
           truck-run-print.ord-no  EQ tt-report.order-no AND
           truck-run-print.i-no  EQ tt-report.item-no AND
           truck-run-print.line  EQ tt-report.line-no AND
           truck-run-print.rel-no EQ tt-report.rel-no-internal AND
           truck-run-print.b-ord-no EQ tt-report.b-ord-no AND
           truck-run-print.po-no  EQ tt-report.po-no AND
           truck-run-print.spare-char-1 = tt-report.rec_key
          /* AND
           (IF tt-report.STOP > 0
             THEN truck-run-print.STOP EQ tt-report.STOP ELSE TRUE) AND
           (IF tt-report.LOAD > "" THEN 
               truck-run-print.LOAD = tt-report.LOAD 
               ELSE TRUE) */
           NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST truck-run-print WHERE
           truck-run-print.company EQ tt-report.company AND
           truck-run-print.link-no EQ tt-report.link-no AND
           truck-run-print.ord-no  EQ tt-report.order-no AND
           truck-run-print.rel-no EQ tt-report.rel-no-internal AND
           truck-run-print.b-ord-no EQ tt-report.b-ord-no AND
           truck-run-print.i-no  EQ tt-report.item-no AND
           truck-run-print.line  EQ tt-report.line-no AND
           truck-run-print.po-no  EQ tt-report.po-no AND
          truck-run-print.spare-char-1 = tt-report.rec_key /* AND
           (IF tt-report.STOP > 0
             THEN truck-run-print.STOP EQ tt-report.STOP ELSE TRUE) AND
           (IF tt-report.LOAD GE "" THEN 
               truck-run-print.LOAD = tt-report.LOAD
               ELSE TRUE)*/

           NO-LOCK NO-ERROR.

   IF AVAIL truck-run-print THEN
   DO:
      v-first-truck = ROWID(truck-run-print).

      ASSIGN tt-report.carrier = truck-run-print.carrier
             vcTruckCode = truck-run-print.truck-code
             vcLoadNo    = truck-run-print.load-no
             viStop      = truck-run-print.stop-no
             vdShipDate  = truck-run-print.ship-date
             v-saved-pallets = truck-run-print.spare-int-1
             v-truck-run-key = truck-run-print.rec_key.

      FIND FIRST truck WHERE
           truck.company = cocode AND
           truck.loc     = locode AND
           truck.carrier = tt-report.carrier AND
           truck.truck-code = vcTruckCode
           NO-LOCK NO-ERROR.

      IF AVAIL truck THEN
      DO:
        vcTruckDscr = truck.truck-desc.
        RELEASE truck.
      END.

      RELEASE truck-run-print.
   END.
                            /*OT1 release*/
   IF vcTruckCode EQ "" AND INDEX("AB",tt-report.key-06) GT 0 THEN DO:
      FIND FIRST bf-oe-relh WHERE
           bf-oe-relh.company  EQ cocode AND
           bf-oe-relh.release# EQ tt-report.rel-no
           NO-LOCK NO-ERROR.

      RELEASE truck.

      IF AVAIL bf-oe-relh THEN
         FIND FIRST truck WHERE
              truck.company = cocode AND
              truck.loc     = locode AND
              truck.carrier = tt-report.carrier AND
              truck.truck-code = bf-oe-relh.trailer
              NO-LOCK NO-ERROR.

      IF AVAIL truck THEN
         ASSIGN vcTruckCode = truck.truck-code
                vcTruckDscr = truck.truck-desc.
   END.

   ASSIGN
      tt-report.truck-code    = vcTruckCode
      tt-report.truck-dscr    = vcTruckDscr
      tt-report.stop-no       = viStop
      tt-report.load-no       = vcLoadNo
      tt-report.truck-print-key = v-truck-run-key
      tt-report.ship-date     = IF vdShipDate <> ? THEN vdShipDate ELSE tt-report.ship-date
      tt-report.old-truck-code = tt-report.truck-code
      tt-report.old-stop-no = tt-report.stop-no
      tt-report.old-load-no = tt-report.load-no
      tt-report.old-truck-dscr = tt-report.truck-dscr
      tt-report.old-ship-date = tt-report.ship-date
      tt-report.is-orig = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-vars-proc C-Win 
PROCEDURE clear-vars-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN 
     v-qty       = 0
     v-ship-id   = ""
     v-ship-text = ""
     v-del-zone  = ""
     viRelNum    = 0
     v-no-units  = 0
     v-pallets   = 0
     v-dec       = 0
     vcTruckCode = ""
     vcTruckDscr = ""
     vcLoadno    = ""
     viStop      = 0 
     vcCity      = "" 
     vcState     = "" 
     vcZip       = "" 
     viBolNo     = 0
     vdShipDate  = ?
     v-msf       = 0
     v-weight    = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
          begin_date end_date begin_carr end_carr tb_scheduled tb_actual tb_late 
          tb_backordered tb_invoiceable tb_posted begin_loc end_loc begin_delv 
          end_delv 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
         begin_date end_date begin_carr end_carr tb_scheduled tb_actual tb_late 
         tb_backordered tb_invoiceable tb_posted btn-ok btn-cancel begin_loc 
         end_loc begin_delv end_delv RECT-7 RECT-40 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generate-data C-Win 
PROCEDURE generate-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-cum-no-units AS INT NO-UNDO.
  DEF VAR v-cum-pallets  AS INT NO-UNDO.
  DEF VAR v-tot-qty      AS INT NO-UNDO.
  DEF VAR v-tot-pallets  AS INT NO-UNDO.
  DEF VAR v-tot-msf        AS DECI NO-UNDO.
  DEF VAR v-tot-weight     AS DECI NO-UNDO.
  DEF BUFFER bf-oe-rell FOR oe-rell.

  for each tt-report
      break by tt-report.key-01
            by tt-report.key-04
            by tt-report.key-05:

      release oe-rel.
      release oe-rell.
      release oe-relh.
      RELEASE oe-boll.

      RUN clear-vars-proc.

      find first oe-rel 
          where oe-rel.rec_key eq tt-report.rec_key
          no-lock no-error.

      if avail oe-rel then do:
         FOR EACH oe-rell
             WHERE oe-rell.company  EQ cocode
               AND oe-rell.ord-no   EQ oe-rel.ord-no
               AND oe-rell.rel-no   EQ oe-rel.rel-no
               AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
               AND oe-rell.i-no     EQ oe-rel.i-no
               AND oe-rell.line     EQ oe-rel.line
               AND CAN-FIND(FIRST oe-relh
                            WHERE oe-relh.r-no    EQ oe-rell.r-no
                              AND oe-relh.posted  EQ NO
                              AND oe-relh.deleted EQ NO
                             USE-INDEX r-no)
             USE-INDEX ord-no NO-LOCK:
           tt-report.rec_key = oe-rell.rec_key.
           LEAVE.
         END.

         find first oe-ordl
             where oe-ordl.company eq cocode
               and oe-ordl.ord-no  eq oe-rel.ord-no
               and oe-ordl.i-no    eq oe-rel.i-no
               and oe-ordl.line    eq oe-rel.line
             NO-LOCK NO-ERROR.

         IF NOT AVAIL oe-ordl THEN
         DO:
            DELETE tt-report.
            NEXT.
         END.

         find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-ordl.i-no
             NO-LOCK NO-ERROR.

         FIND FIRST oe-boll WHERE
              oe-boll.company = cocode AND
              oe-boll.ord-no  = oe-ordl.ord-no AND
              oe-boll.LINE    = oe-ordl.LINE AND
              oe-boll.rel-no  = oe-rel.rel-no
              NO-LOCK NO-ERROR.

         IF AVAIL oe-boll THEN DO:
            FIND FIRST oe-bolh WHERE
                 oe-bolh.company EQ cocode AND
                 oe-boll.b-no EQ oe-boll.b-no
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-bolh THEN
            DO:
               tt-report.ship-date = oe-bolh.ship-date.
               RELEASE oe-bolh.
            END.
            RELEASE oe-boll.
         END.
         ELSE   
         IF AVAIL oe-rel THEN
            tt-report.ship-date = oe-rel.rel-date.
      end.

      find first oe-rell
           where oe-rell.rec_key eq tt-report.rec_key
           no-lock no-error.
      if avail oe-rell then do:    
         find first oe-relh WHERE
              oe-relh.r-no eq oe-rell.r-no
              NO-LOCK NO-ERROR.

         IF NOT AVAIL oe-relh THEN
         DO:
            DELETE tt-report.
            NEXT.
         END.

         tt-report.ship-date = oe-relh.rel-date.

         find first oe-ordl
             where oe-ordl.company eq cocode
               and oe-ordl.ord-no  eq oe-rell.ord-no
               and oe-ordl.i-no    eq oe-rell.i-no
               and oe-ordl.line    eq oe-rell.line
             NO-LOCK NO-ERROR.

         IF NOT AVAIL oe-ordl THEN
         DO:
            DELETE tt-report.
            NEXT.
         END.

         find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq oe-ordl.i-no
            NO-LOCK NO-ERROR.
      end.
      ELSE
      DO:
         find first oe-boll WHERE
              oe-boll.rec_key eq tt-report.rec_key
              no-lock no-error.

         IF AVAIL oe-boll THEN
         DO:
            find first oe-ordl
                where oe-ordl.company eq cocode
                  and oe-ordl.ord-no  eq oe-boll.ord-no
                  and oe-ordl.i-no    eq oe-boll.i-no
                  and oe-ordl.line    eq tt-report.line
                NO-LOCK NO-ERROR.

            find first itemfg
                 where itemfg.company eq cocode
                   and itemfg.i-no    eq oe-boll.i-no
               NO-LOCK NO-ERROR.
         END.
      END.

      find first oe-ord of oe-ordl NO-LOCK NO-ERROR.

      IF NOT AVAIL oe-ord THEN
      DO:
         DELETE tt-report.
         NEXT.
      END.

      find first cust
           where cust.company eq cocode
             and cust.cust-no eq oe-ord.cust-no
        NO-LOCK NO-ERROR.

      IF NOT AVAIL cust THEN
      DO:
         DELETE tt-report.
         NEXT.
      END.

      IF tt-report.key-06 EQ "P" THEN
      DO:
         IF NOT AVAIL oe-boll THEN
         DO:
            IF AVAIL oe-rell THEN
               FIND FIRST oe-boll WHERE
                    oe-boll.company = cocode AND
                    oe-boll.ord-no  = oe-ord.ord-no AND
                    oe-boll.LINE    = oe-ordl.LINE AND
                    oe-boll.rel-no  = oe-rell.rel-no
                    NO-LOCK NO-ERROR.
            ELSE 
               IF AVAIL oe-rel THEN
                  FIND FIRST oe-boll WHERE
                       oe-boll.company = cocode AND
                       oe-boll.ord-no  = oe-ord.ord-no AND
                       oe-boll.LINE    = oe-ordl.LINE AND
                       oe-boll.rel-no  = oe-rel.rel-no
                       NO-LOCK NO-ERROR.
         END.

         IF AVAIL oe-boll THEN
         DO:

            v-qty = oe-boll.qty.

            FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.

            IF AVAIL oe-bolh THEN
            DO:
               ASSIGN
                  v-ship-id   = oe-bolh.ship-id
                  viBolNo     = oe-bolh.bol-no
                  viRelNum    = oe-bolh.release#.
               RELEASE oe-bolh.
            END.

            FIND FIRST fg-bin WHERE fg-bin.i-no = oe-boll.i-no
                                AND fg-bin.loc  = oe-boll.loc
                                AND fg-bin.loc-bin = oe-boll.loc-bin
                                AND fg-bin.tag = oe-boll.tag
                              NO-LOCK NO-ERROR.
            IF AVAIL fg-bin THEN
                v-units-per-pallet = fg-bin.cases-unit.
            ELSE DO:
              find first oe-ordl
                   where oe-ordl.company eq cocode
                     and oe-ordl.ord-no  eq oe-boll.ord-no
                     and oe-ordl.i-no    eq oe-boll.i-no
                     and oe-ordl.line    eq oe-boll.line
                   NO-LOCK NO-ERROR.
              IF AVAIL oe-ordl THEN
                  v-units-per-pallet = oe-ordl.units-pallet.
            END.
            IF v-units-per-pallet = 0 THEN
              v-units-per-pallet =  IF AVAIL itemfg THEN itemfg.case-pal ELSE 0.
            IF v-units-per-pallet = 0 THEN
              v-units-per-pallet = 1.
            IF oe-boll.qty-case NE 0 THEN
               ASSIGN v-weight =  oe-boll.weight
                      v-msf    = (oe-boll.qty * (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0) / 1000)
                      v-decu   = (oe-boll.qty / oe-boll.qty-case)
                      v-dec    = (oe-boll.qty / (oe-boll.qty-case * v-units-per-pallet)).
            ELSE
               ASSIGN v-weight = 0
                      v-msf    = 0
                      v-dec    = 0.
            IF v-dec NE 0 AND v-units-per-pallet = 1 THEN DO:            
               {sys/inc/roundup.i v-dec}.
               IF oe-boll.partial > 0 THEN
                   v-dec = v-dec + 1.
            END.
            ELSE
               v-dec = TRUNCATE(v-dec, 0).
            IF v-decu > 0 AND v-dec = 0 THEN
                v-dec = 1.
           {sys/inc/roundup.i v-decu}.

            ASSIGN v-no-units = v-decu
                   v-pallets  = v-pallets + v-dec.

            RELEASE oe-boll.
         END.
         ELSE
         DO:
            DELETE tt-report.
            NEXT.
         END.
       END.
       ELSE if avail oe-rell then
       DO:
           FIND FIRST fg-bin WHERE fg-bin.i-no = oe-rell.i-no
                               AND fg-bin.loc  = oe-rell.loc
                               AND fg-bin.loc-bin = oe-rell.loc-bin
                               AND fg-bin.tag = oe-rell.tag
                             NO-LOCK NO-ERROR.
           IF AVAIL fg-bin THEN
               v-units-per-pallet = fg-bin.cases-unit.
           ELSE DO:
             find first oe-ordl
                  where oe-ordl.company eq cocode
                    and oe-ordl.ord-no  eq oe-rell.ord-no
                    and oe-ordl.i-no    eq oe-rell.i-no
                    and oe-ordl.line    eq oe-rell.line
                  NO-LOCK NO-ERROR.
             IF AVAIL oe-ordl THEN
                 v-units-per-pallet = oe-ordl.units-pallet.
           END.
           IF v-units-per-pallet = 0 THEN
             v-units-per-pallet = IF AVAIL itemfg THEN  itemfg.case-pal ELSE 0.
           IF v-units-per-pallet = 0 THEN
             v-units-per-pallet = 1.
           ASSIGN v-tot-qty = 0
                  v-tot-msf = 0
                  v-tot-weight = 0
                  v-tot-pallets = 0.
           FOR EACH bf-oe-rell
               WHERE bf-oe-rell.company  EQ cocode
                 AND bf-oe-rell.ord-no   EQ oe-rell.ord-no
                 AND bf-oe-rell.rel-no   EQ oe-rell.rel-no
                 AND bf-oe-rell.b-ord-no EQ oe-rell.b-ord-no
                 AND bf-oe-rell.i-no     EQ oe-rell.i-no
                 AND bf-oe-rell.line     EQ oe-rell.line
                 AND CAN-FIND(FIRST oe-relh
                              WHERE oe-relh.r-no    EQ bf-oe-rell.r-no
                                AND oe-relh.posted  EQ NO
                                AND oe-relh.deleted EQ NO
                               USE-INDEX r-no)
               USE-INDEX ord-no NO-LOCK:

               IF bf-oe-rell.qty-case NE 0 THEN
                 ASSIGN v-weight = ((IF AVAIL itemfg THEN itemfg.weight-100 ELSE 0) *  bf-oe-rell.qty / 100)
                        v-msf    = (bf-oe-rell.qty * (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0) / 1000)
                        v-decu   = (bf-oe-rell.qty / bf-oe-rell.qty-case)
                        v-dec    = (bf-oe-rell.qty / (bf-oe-rell.qty-case * v-units-per-pallet)).
               ELSE
                 ASSIGN v-weight =  0
                        v-msf    =  0
                        v-dec = 0
                        v-decu = 0.
               v-tot-weight = v-tot-weight + v-weight.
               v-tot-msf    = v-tot-msf    + v-msf.
               {sys/inc/roundup.i v-decu}.
               v-tot-qty = v-tot-qty + v-decu.
               v-dec    = (bf-oe-rell.qty / (bf-oe-rell.qty-case * v-units-per-pallet)).
               IF v-dec NE 0 AND v-units-per-pallet = 1 THEN DO:
                  {sys/inc/roundup.i v-dec}.
                  IF oe-rell.partial > 0 THEN
                     v-dec = v-dec + 1.
               END.         
               ELSE 
                  v-dec = TRUNC(v-dec, 0).
               IF v-dec = 0 AND bf-oe-rell.qty > 0 THEN
                   v-dec = 1.
               v-tot-pallets = v-tot-pallets + v-dec.
           END.


          assign
            v-qty       = oe-rell.qty
            v-ship-id   = oe-relh.ship-id
            viRelNum    = oe-relh.release#.

          IF tt-report.no-units NE 0 THEN
             ASSIGN v-no-units = tt-report.no-units
                    v-weight   = tt-report.weight
                    v-msf      = tt-report.msf .
          ELSE
          DO:
              /*
              FIND FIRST fg-bin WHERE fg-bin.i-no = oe-rell.i-no
                                  AND fg-bin.loc  = oe-rell.loc
                                  AND fg-bin.loc-bin = oe-rell.loc-bin
                                  AND fg-bin.tag = oe-rell.tag
                                NO-LOCK NO-ERROR.
              IF AVAIL fg-bin THEN
                  v-units-per-pallet = fg-bin.cases-unit.
              ELSE DO:
                find first oe-ordl
                     where oe-ordl.company eq cocode
                       and oe-ordl.ord-no  eq oe-rell.ord-no
                       and oe-ordl.i-no    eq oe-rell.i-no
                       and oe-ordl.line    eq oe-rell.line
                     NO-LOCK NO-ERROR.
                IF AVAIL oe-ordl THEN
                    v-units-per-pallet = oe-ordl.units-pallet.
              END.
              IF v-units-per-pallet = 0 THEN
                v-units-per-pallet = itemfg.case-pal.
              IF v-units-per-pallet = 0 THEN
                v-units-per-pallet = 1.
             */
             IF oe-rell.qty-case NE 0 THEN
                ASSIGN v-weight = ((IF AVAIL itemfg THEN itemfg.weight-100 ELSE 0) *  oe-rell.qty / 100)
                       v-msf    = (oe-rell.qty * (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0) / 1000)
                       v-decu   = (oe-rell.qty / oe-rell.qty-case)
                       v-dec    = (oe-rell.qty / (oe-rell.qty-case * v-units-per-pallet)).
             ELSE
                ASSIGN v-weight =  0
                       v-msf    =  0
                       v-dec = 0
                       v-decu = 0.

             {sys/inc/roundup.i v-decu}.
             /*
             IF v-dec NE 0 AND v-units-per-pallet = 1 THEN DO:
                {sys/inc/roundup.i v-dec}.
                IF oe-rell.partial > 0 THEN
                   v-dec = v-dec + 1.
             END.         
             ELSE 
                v-dec = TRUNC(v-dec, 0).
             IF v-decu > 0 AND v-dec = 0 THEN
                v-dec = 1.
             */
             ASSIGN v-no-units = v-tot-qty
                    v-pallets  = v-pallets + v-tot-pallets
                    v-msf      = v-tot-msf
                    v-weight   = v-tot-weight.
          END.
       END.
       else
       DO:
          assign
             v-qty     = IF oe-rel.qty = 0 THEN oe-rel.tot-qty ELSE oe-rel.qty
             v-ship-id = oe-rel.ship-id
             viRelNum  = oe-rel.rel-no.

          IF AVAIL oe-rell THEN
            FIND FIRST fg-bin WHERE fg-bin.i-no = oe-rell.i-no
                                AND fg-bin.loc  = oe-rell.loc
                                AND fg-bin.loc-bin = oe-rell.loc-bin
                                AND fg-bin.tag = oe-rell.tag
                              NO-LOCK NO-ERROR.
          IF AVAIL fg-bin THEN
             v-units-per-pallet = fg-bin.cases-unit.
          ELSE IF AVAIL oe-rell THEN DO:
            find first oe-ordl
                 where oe-ordl.company eq cocode
                   and oe-ordl.ord-no  eq oe-rell.ord-no
                   and oe-ordl.i-no    eq oe-rell.i-no
                   and oe-ordl.line    eq oe-rell.line
               NO-LOCK NO-ERROR.
               IF AVAIL oe-ordl THEN
                 v-units-per-pallet = oe-ordl.units-pallet.
          END.
          IF v-units-per-pallet = 0 THEN
                v-units-per-pallet = IF AVAIL itemfg THEN itemfg.case-pal ELSE 0.
          IF v-units-per-pallet = 0 THEN
                v-units-per-pallet = 1.
          v-units-per-pallet = IF AVAIL itemfg THEN itemfg.case-pal ELSE 0.
          IF v-units-per-pallet = 0 THEN
            v-units-per-pallet = 1.

          IF oe-ordl.cas-cnt NE 0 THEN
             ASSIGN v-weight =  ((IF AVAIL itemfg THEN itemfg.weight-100 ELSE 0) *  v-qty / 100)
                    v-msf    =  (v-qty * (IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0) / 1000)
                    v-decu    = (v-qty / oe-ordl.cas-cnt)
                    v-dec    = (v-qty / (oe-ordl.cas-cnt * v-units-per-pallet)).
          ELSE
             ASSIGN v-weight =  0
                    v-msf    =  0
                    v-dec = 0.

          {sys/inc/roundup.i v-decu}.
          IF v-dec NE 0 AND v-units-per-pallet = 1 THEN DO:
              {sys/inc/roundup.i v-dec}.
              IF oe-rel.partial > 0 THEN
                v-dec = v-dec + 1.
          END.
          ELSE
              v-dec = TRUNC(v-dec, 0).
          IF v-decu > 0 AND v-dec = 0 THEN
              v-dec = 1.

          ASSIGN v-no-units = v-decu
                 v-pallets  = v-pallets + v-dec.
       END.

       find first shipto WHERE
            shipto.company eq cocode AND
            shipto.cust-no eq oe-ordl.cust-no AND
            shipto.ship-id eq v-ship-id
            no-lock no-error.

       if avail shipto then
       DO:
         assign
           vcCity  = shipto.ship-city
           vcState = shipto.ship-state
           vcZip   = shipto.ship-zip
           v-ship-text = SUBSTRING(shipto.ship-addr[1]
                       + "," + vcCity + " " + vcState + " " + vcZip,1,60)
           v-del-zone  = shipto.dest-code.

         IF shipto.dest-code NE "" AND
            NOT(shipto.dest-code GE begin_delv AND
            shipto.dest-code LE END_delv)  THEN
            DO:
               DELETE tt-report.
               NEXT.
            END.
       END.
       else
         assign
           vcCity  = cust.city
           vcState = cust.state
           vcZip   = cust.zip
           v-ship-text = SUBSTRING(cust.addr[1]
                        + "," + vcCity + " " + vcState + " " + vcZip,1,60).
       RUN assign-tt-report-fields.
  end. /* each tt-report */

  IF v-types EQ "P" THEN /*only posted BOLs*/
  DO:
     FOR EACH tt-report
         BREAK BY tt-report.bol-no:

         IF FIRST-OF(tt-report.bol-no) THEN
            ASSIGN v-cum-no-units = 0
                   v-cum-pallets = 0.

         ASSIGN v-cum-no-units = v-cum-no-units + tt-report.no-units
                v-cum-pallets = v-cum-pallets + tt-report.pallets.

         IF LAST-OF(tt-report.bol-no) THEN
         DO:
            CREATE tt-report-bol.
            BUFFER-COPY tt-report EXCEPT no-units pallets TO tt-report-bol
               ASSIGN tt-report-bol.no-units = v-cum-no-units
                      tt-report-bol.pallets  = v-cum-pallets.
            RELEASE tt-report-bol.
         END.
     END.

     EMPTY TEMP-TABLE tt-report.

     FOR EACH tt-report-bol:
         CREATE tt-report.
         BUFFER-COPY tt-report-bol TO tt-report.
         RELEASE tt-report.
     END.
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------------oerep/r-trksch.w          */
/* Truck Plan Selection                                                       */
/* -------------------------------------------------------------------------- */
  def var v-type as CHAR NO-UNDO.
  DEF VAR lv-qty LIKE oe-rell.qty NO-UNDO.
  DEF VAR lv-no-units AS INT NO-UNDO.
  DEF VAR v-dec AS DEC NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

     SESSION:SET-WAIT-STATE ("GENERAL").

     EMPTY TEMP-TABLE tt-report.
     EMPTY TEMP-TABLE tt-report-bol.

     ASSIGN
     {&displayed-objects}
     v-types  = string(tb_posted,"P/")      + string(tb_actual,"A/")      +
                string(tb_late,"L/")        + string(tb_scheduled,"S/")   +
                string(tb_backordered,"B/") + string(tb_invoiceable,"I/")
     lv-unique-no = 1.

     IF INDEX(v-types,"S") GT 0 or INDEX(v-types,"L") GT 0 OR
        INDEX(v-types,"I") GT 0 THEN
     FOR EACH oe-ordl WHERE
         oe-ordl.company EQ cocode AND
         oe-ordl.opened  EQ YES AND
         oe-ordl.ord-no  GE begin_ord-no AND
         oe-ordl.ord-no  LE end_ord-no AND
         oe-ordl.i-no    GE begin_i-no AND
         oe-ordl.i-no    LE end_i-no AND
         NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
         USE-INDEX opened NO-LOCK,
         FIRST oe-ord FIELDS(ord-no loc) WHERE
               oe-ord.company EQ oe-ordl.company AND
               oe-ord.ord-no  EQ oe-ordl.ord-no AND
               oe-ord.cust-no GE begin_cust-no AND
               oe-ord.cust-no LE end_cust-no
               NO-LOCK:

         FOR EACH oe-rel where
             oe-rel.company   eq cocode AND
             oe-rel.ord-no    eq oe-ordl.ord-no AND
             oe-rel.i-no      eq oe-ordl.i-no AND
             oe-rel.line      eq oe-ordl.line AND
             oe-rel.rel-date  ge begin_date AND
             oe-rel.rel-date  le end_date AND
             oe-rel.carrier   GE begin_carr AND
             oe-rel.carrier   LE end_carr
             NO-LOCK:

             RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

             if index("ABP",v-type) gt 0 then next.

             IF INDEX(v-types,v-type) GT 0 THEN
             DO:
                create tt-report.
                assign
                   tt-report.key-01  = string(year(oe-rel.rel-date),"9999") +
                                       string(month(oe-rel.rel-date),"99")  +
                                       string(day(oe-rel.rel-date),"99")
                   tt-report.key-04  = oe-ord.ord-no
                   tt-report.key-05  = string(index(v-types,v-type),"99")
                   tt-report.key-06  = v-type
                   tt-report.rec_key  = oe-rel.rec_key
                   tt-report.unique-no = lv-unique-no
                   tt-report.company = oe-rel.company
                   tt-report.line-no = oe-rel.LINE 
                   tt-report.item-no = oe-rel.i-no
                   tt-report.rel-no-internal = oe-rel.rel-no
                   tt-report.b-ord-no = oe-rel.b-ord-no
                   tt-report.po-no    = oe-rel.po-no
                   tt-report.oe-rel-r-no = oe-rel.r-no
                   tt-report.loc      = oe-ord.loc
                   tt-report.carrier  = oe-ordl.carrier
                   tt-report.old-carrier = tt-report.carrier
                   lv-unique-no = lv-unique-no + 1.
             END.
         END.
     end.

     IF INDEX(v-types,"A") GT 0 or INDEX(v-types,"B") GT 0 THEN
        FOR EACH oe-relh WHERE
            oe-relh.company EQ cocode AND
            oe-relh.posted EQ NO AND
            oe-relh.deleted  EQ NO AND
            oe-relh.cust-no GE begin_cust-no AND
            oe-relh.cust-no LE end_cust-no AND
            oe-relh.rel-date GE begin_date AND
            oe-relh.rel-date LE end_date AND
            oe-relh.carrier  GE begin_carr AND
            oe-relh.carrier  LE end_carr
            NO-LOCK,
            EACH oe-rell WHERE
                 oe-rell.r-no EQ oe-relh.r-no AND
                 oe-rell.company EQ oe-relh.company AND
                 oe-rell.loc GE begin_loc AND
                 oe-rell.loc LE end_loc AND
                 oe-rell.ord-no GE begin_ord-no AND
                 oe-rell.ord-no LE end_ord-no AND
                 oe-rell.i-no GE begin_i-no AND
                 oe-rell.i-no LE end_i-no AND
                 ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
                 (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
                 USE-INDEX r-no
                 NO-LOCK
             BREAK BY oe-rell.r-no
                   BY oe-rell.ord-no
                   BY oe-rell.i-no
                   BY oe-rell.line
                   BY oe-rell.rel-no
                   BY oe-rell.b-ord-no
                   BY oe-rell.po-no:

         IF LAST-OF(oe-rell.po-no) THEN
             DO:
                create tt-report.
                assign
                 tt-report.key-01  = string(year(oe-relh.rel-date),"9999") +
                                     string(month(oe-relh.rel-date),"99")  +
                                     string(day(oe-relh.rel-date),"99")
                 tt-report.key-04  = oe-rell.ord-no
                 tt-report.key-05  = string(index(v-types,v-type),"99")
                 tt-report.key-06  = if oe-rell.b-ord-no eq 0 then "A" else "B"
                 tt-report.rec_key  = oe-rell.rec_key
                 tt-report.unique-no = lv-unique-no
                 tt-report.company = oe-rell.company
                 tt-report.line-no = oe-rell.LINE 
                 tt-report.item-no = oe-rell.i-no
                 tt-report.rel-no-internal = oe-rell.rel-no
                 tt-report.b-ord-no = oe-rell.b-ord-no
                 tt-report.po-no    = oe-rell.po-no
                 tt-report.link-no  = oe-rell.r-no
                 tt-report.loc      = oe-rell.loc
                 tt-report.carrier  = oe-relh.carrier
                 tt-report.old-carrier = tt-report.carrier
                 lv-unique-no = lv-unique-no + 1.
             END.
        END.

     IF INDEX(v-types,"P") GT 0 THEN
        FOR EACH oe-bolh WHERE
            oe-bolh.company EQ cocode AND
            oe-bolh.deleted EQ NO AND
            oe-bolh.posted EQ NO AND
            oe-bolh.cust-no GE begin_cust-no AND
            oe-bolh.cust-no LE end_cust-no AND
            oe-bolh.carrier GE begin_carr AND
            oe-bolh.carrier LE end_carr
            NO-LOCK,
            FIRST oe-relh WHERE
                  oe-relh.company EQ oe-bolh.company AND
                  oe-relh.release# EQ oe-bolh.release# AND
                  oe-relh.rel-date GE begin_date AND
                  oe-relh.rel-date LE end_date
                  NO-LOCK,
            EACH oe-boll WHERE
                 oe-boll.company eq oe-bolh.company AND
                 oe-boll.b-no eq oe-bolh.b-no AND
                 oe-boll.ord-no GE begin_ord-no AND
                 oe-boll.ord-no LE end_ord-no AND
                 oe-boll.i-no GE begin_i-no AND
                 oe-boll.i-no LE end_i-no AND
                 oe-boll.loc GE begin_loc AND
                 oe-boll.loc LE end_loc
                 NO-LOCK:

            create tt-report.
            assign
               tt-report.key-01  = string(year(oe-relh.rel-date),"9999") +
                                   string(month(oe-relh.rel-date),"99")  +
                                   string(day(oe-relh.rel-date),"99")
               tt-report.key-04  = oe-boll.ord-no
               tt-report.key-05  = string(index(v-types,"P"),"99")
               tt-report.key-06  = "P"
               tt-report.rec_key  = oe-boll.rec_key
               tt-report.unique-no = lv-unique-no
               tt-report.company = oe-boll.company
               tt-report.line-no = oe-boll.LINE
               tt-report.item-no = oe-boll.i-no
               tt-report.rel-no-internal = oe-boll.rel-no
               tt-report.b-ord-no = oe-boll.b-ord-no
               tt-report.po-no    = oe-boll.po-no
               tt-report.loc      = oe-boll.loc
               tt-report.carrier  = oe-bolh.carrier
               tt-report.old-carrier = tt-report.carrier
               lv-unique-no = lv-unique-no + 1.
        END.

     RUN generate-data. 

     RUN tot-wgt-proc.



     RUN add-saved-recs.

     SESSION:SET-WAIT-STATE ("").

     FOR EACH tt-report.
                 FIND FIRST truck-run-print WHERE
           truck-run-print.company EQ tt-report.company AND
           truck-run-print.ord-no  EQ tt-report.order-no AND
           truck-run-print.i-no  EQ tt-report.item-no AND
           truck-run-print.line  EQ tt-report.line-no AND
           truck-run-print.rel-no EQ tt-report.rel-no-internal AND
           truck-run-print.b-ord-no EQ tt-report.b-ord-no AND
           truck-run-print.po-no  EQ tt-report.po-no AND
           truck-run-print.spare-char-1 = tt-report.rec_key AND
           truck-run-print.rec_key = tt-report.truck-print-key
           NO-LOCK NO-ERROR.
        IF AVAIL truck-run-print THEN
            tt-report.pallets = truck-run-print.spare-int-1.
     END.
     IF CAN-FIND(FIRST tt-report) THEN
        RUN oerep\b-trksch.w.
     ELSE
        MESSAGE "No Releases Found."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  END.



/* end ---------------------------------- copr. 2007 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-wgt-proc C-Win 
PROCEDURE tot-wgt-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tt-report WHERE
      tt-report.carrier NE "" AND
      tt-report.truck-code NE "":

      ASSIGN
         tt-report.tot-msf = 0
         tt-report.tot-weight = 0
         tt-report.tot-units = 0.

      FOR EACH bf-tt-report WHERE
          bf-tt-report.carrier = tt-report.carrier AND
          bf-tt-report.truck-code = tt-report.truck-code AND
          bf-tt-report.load-no = tt-report.load-no AND
          bf-tt-report.ship-date = tt-report.ship-date:

          ASSIGN tt-report.tot-msf = tt-report.tot-msf + bf-tt-report.msf
                 tt-report.tot-weight = tt-report.tot-weight + bf-tt-report.weight
                 tt-report.tot-units = tt-report.tot-units + bf-tt-report.pallets.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

