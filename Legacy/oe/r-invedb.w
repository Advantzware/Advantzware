&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oe\r-invedb.w
  
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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

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

def TEMP-TABLE work-rel NO-UNDO
  field company like oe-relh.company
  field loc like oe-rell.loc
  field r-no like oe-relh.r-no
  field bol-no like oe-bolh.bol-no
  field carrier like oe-relh.carrier
  field cust-no like oe-relh.cust-no
  field ord-no like oe-relh.ord-no
  field po-no like oe-relh.po-no
  field rel-date like oe-relh.rel-date
  field ship-id like oe-relh.ship-id
  field ship-i like oe-relh.ship-i
  field i-no like oe-rell.i-no
  field line like oe-rell.line
  field qty like oe-rell.qty
  field tot-qty like oe-rell.qty
  field posted like oe-rell.posted
  field printed like oe-relh.printed
  field ship-addr as char format "x(20)"
  field ship-city as char format "x(10)"
  field ship-state as char format "x(2)"
  field ship-zip as char format "x(10)"
  field completed as character format "x(1)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 tb_detailed rd_sort lv-ornt ~
lines-per-page rd-dest lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_detailed tb_detailed lbl_sort rd_sort ~
lv-ornt lines-per-page rd-dest lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE lbl_detailed AS CHARACTER FORMAT "X(256)":U INITIAL "Detailed?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 21 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"BOL", "BOL"
     SIZE 24 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.38.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_detailed AT ROW 3.86 COL 28 COLON-ALIGNED NO-LABEL
     tb_detailed AT ROW 3.86 COL 41
     lbl_sort AT ROW 5.29 COL 29 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 5.29 COL 41 NO-LABEL
     lv-ornt AT ROW 11.24 COL 29 NO-LABEL
     lines-per-page AT ROW 11.24 COL 82 COLON-ALIGNED
     rd-dest AT ROW 11.48 COL 5 NO-LABEL
     lv-font-no AT ROW 13.62 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 14.57 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.52 COL 5
     btn-ok AT ROW 18.86 COL 21
     btn-cancel AT ROW 18.86 COL 59
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.05 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 9.57 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "Invoice Edit Listing"
         HEIGHT             = 21.86
         WIDTH              = 96.4
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
IF NOT C-Win:LOAD-ICON("Graphics\xRemove.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\xRemove.ico"
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
/* SETTINGS FOR FILL-IN lbl_detailed IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Invoice Edit Listing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Invoice Edit Listing */
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  assign rd-dest.
       
  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
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
  DISPLAY lbl_detailed tb_detailed lbl_sort rd_sort lv-ornt lines-per-page 
          rd-dest lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 tb_detailed rd_sort lv-ornt lines-per-page rd-dest 
         lv-font-no td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
          
     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME
   
         UPDATE OKpressed.
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  
/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
  */
     RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------- oe/rep/newinv.p  3/94 RM */
/* Invoicing  - new Invoice report                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-detail as log format "Detail/Summary" init no.
def var v-sort   as log format "Customer/BOL"   init yes.

def var save_id as recid.
def var time_stamp as ch.
def var qfirst as l.
def var v-trnum as int.
def var v-ext-price like inv-line.t-price.
def var v-tot-cas as dec format "->>>9.9999".
def var v-cases as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-postable as log init no.
def var v-tot-cost like inv-head.t-inv-cost.     /* total cost invoiced */
def var v-tot-weight like inv-head.t-inv-weight.  /* total weight shipped */
def var v-line-price like inv-line.price.
def var v-line-cost like inv-line.cost.
def var v-line-freight like inv-line.t-freight.
def var v-set-qty as int.
def var v-part-qty as dec format "999.9999".
def var v-bol-cases like oe-boll.cases.
def var v-line-tot  like inv-line.t-price.
def var v-misc-tot like inv-misc.amt.
def var v-tmp as decimal no-undo.

def buffer xinv-line for inv-line.

form header
  " Customer"
      "Weight" to 47 "Pallets" to 58 "Cases" to 65 "Freight" to 75
      "Tax" to 85 "Misc" to 100 "Items" to 115 " total" to 130
  fill("=",130) format "x(130)"
  with FRAME r-top.

form
  inv-head.cust-no "-"
  inv-head.cust-name format "x(25)"
  inv-head.t-inv-weight v-tot-pallets v-tot-cas format "->>>>9"
  inv-head.t-inv-freight format "->,>>9.99"
  inv-head.t-inv-tax format "->,>>9.99"
  v-misc-tot to 100 format "->>>,>>9.99"
  v-line-tot to 115
  inv-head.t-inv-rev to 130
with down STREAM-IO width 132 no-labels no-box no-underline frame ord.

form
  inv-line.ord-no at 5 label "Order#"
  oe-ordl.po-no label "Order PO Number"
  inv-line.i-no label "Item"
  inv-line.i-name format "x(20)" label "Description"
  inv-line.qty format "->>,>>>,>>9" label "Order"
  inv-line.inv-qty format "->>,>>>,>>9" column-label "Quantities!Invoiced "
  inv-line.ship-qty format "->>,>>>,>>9" label "Shipped"
  inv-line.price format "->>>,>>9.99" label "Price"
  inv-line.pr-uom label "UOM" inv-line.t-price column-label "Extended! Price"
  skip
  with down no-box STREAM-IO width 132 frame ordl.

form
  inv-misc.charge at 10 label "Charge"
  inv-misc.dscr label "Description"
  inv-misc.amt format "->>>,>>9.99" to 71 label "Price"
  skip
  with down STREAM-IO no-box frame ordm.

form
  work-rel.i-no at 10 column-label "RELEASE!Items"
  work-rel.po-no label "PO Number"
  work-rel.loc label "Location"
  work-rel.rel-date label "Date"
  work-rel.bol-no label "BOL#"
  work-rel.completed column-label "P/C"
  work-rel.r-no   label "REL#"
  work-rel.carrier label "Carrier"
  work-rel.ship-id label "Ship To"
  work-rel.qty   label "Quantity" skip
  with down no-box STREAM-IO width 132 frame rel.


{sa/sa-sls01.i}
    
find first oe-ctrl where oe-ctrl.company EQ cocode no-lock no-error.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}
    
 v-detail = tb_detailed
 v-sort   = rd_sort BEGINS "Cust".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

  for each inv-head
      where inv-head.company eq cocode
        and inv-head.inv-no  eq 0
        and inv-head.printed eq no
        and inv-head.posted  eq no
      use-index prnt no-lock:

    create report.
    assign
     report.term-id = v-term
     report.key-01  = if v-sort then inv-head.cust-no else ""
     report.key-02  = string(inv-head.bol-no,"9999999999")
     report.rec-id  = recid(inv-head).
  end.

  for each report where report.term-id eq v-term,
      first inv-head where recid(inv-head) eq report.rec-id no-lock
      by report.key-01
      by report.key-02:

    assign
     v-postable = yes
     v-tot-cas = 0
     v-tot-pallets = 0
     v-bol-cases = 0.

    /** CALCULATE toTAL PALLETS **/
    for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no:

      for each oe-bolh no-lock where oe-bolh.b-no = xinv-line.b-no and
          oe-bolh.ord-no = xinv-line.ord-no:
        for each oe-boll no-lock where oe-boll.company = oe-bolh.company and
            oe-boll.b-no = oe-bolh.b-no and
            oe-boll.i-no = xinv-line.i-no:

                                      /** Bill Of Lading toTAL CASES **/
          assign v-bol-cases = v-bol-cases + oe-boll.cases.
        end. /* each oe-boll */
      end. /* each oe-bolh */


        if xinv-line.est-no ne "" then do:
          find first eb where eb.company = xinv-line.company and
            eb.est-no = xinv-line.est-no and
            eb.e-num = xinv-line.e-num and
            eb.form-no = xinv-line.form-no and
            eb.blank-no = xinv-line.blank-no no-lock no-error.

          if xinv-line.form-no   eq 0                             and
             (xinv-line.est-type eq 2 or xinv-line.est-type eq 6) then do:
            for each fg-set no-lock where fg-set.company = xinv-line.company
                and fg-set.set-no = xinv-line.i-no:
              assign v-set-qty = v-set-qty + fg-set.part-qty.
            end.
            if v-set-qty = 0 then
            assign v-set-qty = 1.
            for each eb no-lock where eb.company = xinv-line.company and
                eb.est-no = xinv-line.est-no and
                eb.e-num = xinv-line.e-num and
                eb.form-no ne 0:
              find fg-set where fg-set.company = xinv-line.company and
                fg-set.set-no = xinv-line.i-no  and
                fg-set.part-no = eb.stock-no no-lock no-error.

              if avail fg-set and fg-set.part-qty ne 0 then
              assign v-part-qty = fg-set.part-qty / v-set-qty.
              else
              assign v-part-qty = 1 / v-set-qty.

              IF eb.cas-cnt = 0 then
              assign v-cases = round((xinv-line.t-weight * v-part-qty) /
                eb.cas-wt, 2).
              else
              assign v-cases = round((xinv-line.ship-qty * v-part-qty) /
                eb.cas-cnt, 2).
              if v-bol-cases ne 0 then
               assign v-cases = v-bol-cases.
              assign v-tot-pallets = v-tot-pallets +
                round((v-cases  / eb.cas-pal) + .49, 0).
            end. /* each eb */
          end. /* do */
          else
          if avail eb then
          do:
            IF eb.cas-cnt = 0 then
            assign v-cases = round(xinv-line.t-weight / eb.cas-wt, 2).
            else
            assign v-cases = round(xinv-line.ship-qty / eb.cas-cnt, 2).
              if v-bol-cases ne 0 then
               assign v-cases = v-bol-cases.
            assign v-tot-pallets = v-tot-pallets +
              round((v-cases  / eb.cas-pal) + .49, 0).
          end. /* do */
        end. /* est-no ne "" */
        assign v-tot-weight = v-tot-weight + xinv-line.t-weight
               v-tot-cas = v-tot-cas + v-cases.
    end. /* each xoe-ordl */

    assign v-line-tot = 0
           v-misc-tot = 0.

    /* Added by FWK 8/11/97 */
    for each inv-line where inv-line.r-no = inv-head.r-no no-lock:
      assign v-line-tot = v-line-tot + inv-line.t-price.
    end.

    for each inv-misc where inv-misc.r-no = inv-head.r-no and
            inv-misc.bill ne "I" no-lock:
      assign v-misc-tot = v-misc-tot + inv-misc.amt.
    end.
    /* Added by FWK 8/11/97 */

                                  /** Write header Line **/
    display
      inv-head.cust-no inv-head.cust-name
      inv-head.t-inv-weight v-tot-pallets v-tot-cas
      inv-head.t-inv-freight
      inv-head.t-inv-tax
      v-misc-tot
      v-line-tot
      inv-head.t-inv-rev
    with frame ord.
    down with frame ord.

    if inv-head.stat eq "H" then put "*** THIS INVOICE IS IN HOLD ***" skip.

    /************ LINE ITEMS ************************************************/
    for each inv-line where inv-line.r-no = inv-head.r-no
        break by inv-line.r-no by inv-line.ord-no:
      assign v-ext-price = 0
        v-line-cost = 0
        v-line-freight = 0.

      if v-detail then do:                           /** Write Detail Limes **/
        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq inv-line.ord-no
              and oe-ordl.i-no    eq inv-line.i-no
              and oe-ordl.line    eq inv-line.line
            no-lock no-error.
        display inv-line.ord-no
                oe-ordl.po-no when avail oe-ordl
                  inv-line.po-no when not avail oe-ordl @ oe-ordl.po-no
                inv-line.i-no
                inv-line.i-name
                inv-line.qty
                inv-line.inv-qty
                inv-line.ship-qty
                inv-line.price
                inv-line.pr-uom
                inv-line.t-price
            with frame ordl.
        down with frame ordl.

        if inv-line.stat = "I" or inv-line.stat = "B" then
        for each oe-relh no-lock where oe-relh.company = inv-line.company and
            oe-relh.ord-no = inv-line.ord-no and
            oe-relh.posted use-index relh:
          for each oe-rell no-lock where
              oe-rell.company = oe-relh.company and /* CTS added for speed */
              oe-rell.r-no = oe-relh.r-no and
              oe-rell.i-no = inv-line.i-no and
              oe-rell.line = inv-line.line and
              oe-rell.s-code = "I"
              USE-INDEX r-no:
            create work-rel.
            assign
              work-rel.company = oe-relh.company
              work-rel.loc = oe-rell.loc
              work-rel.r-no = oe-relh.r-no
              work-rel.carrier = oe-relh.carrier
              work-rel.cust-no = oe-relh.cust-no
              work-rel.ord-no = oe-relh.ord-no
              work-rel.rel-date = oe-relh.rel-date
              work-rel.ship-id = oe-relh.ship-id
              work-rel.ship-i[1] = oe-relh.ship-i[1]
              work-rel.ship-i[2] = oe-relh.ship-i[2]
              work-rel.ship-i[3] = oe-relh.ship-i[3]
              work-rel.ship-i[4] = oe-relh.ship-i[4]
              work-rel.i-no = oe-rell.i-no
              work-rel.line = oe-rell.line
              work-rel.po-no = oe-rell.po-no
              work-rel.qty = oe-rell.qty
              work-rel.posted = oe-rell.posted
              work-rel.printed = oe-relh.printed.

            find first shipto where shipto.company = inv-line.company and
              shipto.cust-no = oe-relh.cust-no and
              shipto.ship-no = oe-relh.ship-no no-lock no-error.
            if avail shipto then
            assign
              work-rel.ship-addr = shipto.ship-addr[1]
              work-rel.ship-city = shipto.ship-city
              work-rel.ship-state = shipto.ship-state
              work-rel.ship-zip = shipto.ship-zip.
          end. /* each oe-rell */
        end. /* each oe-relh */
        
        if inv-line.stat = "S" or inv-line.stat = "B" then
        for each oe-bolh no-lock where oe-bolh.b-no = inv-line.b-no:

          v-tmp = 0.
          for each oe-boll
            where oe-boll.company = oe-bolh.company
              and oe-boll.bol-no = oe-bolh.bol-no
              and oe-boll.i-no = inv-line.i-no
              and oe-boll.ord-no = inv-line.ord-no
          no-lock:
            v-tmp = v-tmp + oe-boll.qty.
          end.

          for each oe-boll no-lock where oe-boll.company = oe-bolh.company and
              oe-boll.b-no = oe-bolh.b-no and
              oe-boll.i-no = inv-line.i-no and
              oe-boll.ord-no = inv-line.ord-no:
            find first work-rel where work-rel.company = oe-bolh.company and
              work-rel.i-no = oe-boll.i-no and
              work-rel.line = oe-boll.line and
              work-rel.loc = oe-boll.loc no-error.
            if not avail work-rel then
            do:
              create work-rel.
              assign  work-rel.qty = oe-boll.qty.
            end.
            else
            assign  work-rel.qty = work-rel.qty + oe-boll.qty.

            assign
              work-rel.company = oe-bolh.company
              work-rel.loc = oe-bolh.loc
              work-rel.r-no = oe-bolh.r-no
              work-rel.bol-no = oe-bolh.bol-no
              work-rel.carrier = oe-bolh.carrier
              work-rel.cust-no = oe-bolh.cust-no
              work-rel.ord-no = oe-bolh.ord-no
              work-rel.rel-date = oe-bolh.bol-date
              work-rel.ship-id = oe-bolh.ship-id
              work-rel.ship-i[1] = oe-bolh.ship-i[1]
              work-rel.ship-i[2] = oe-bolh.ship-i[2]
              work-rel.ship-i[3] = oe-bolh.ship-i[3]
              work-rel.ship-i[4] = oe-bolh.ship-i[4]
              work-rel.i-no = oe-boll.i-no
              work-rel.line = oe-boll.line
              work-rel.po-no = oe-boll.po-no
              work-rel.posted = oe-boll.posted
              work-rel.printed = oe-bolh.printed.

              find first oe-ordl
                where oe-ordl.company = oe-boll.company
                  and oe-ordl.ord-no = oe-bolh.ord-no
                  and oe-ordl.i-no = oe-boll.i-no
                  and oe-ordl.line = oe-boll.line
              no-lock no-error.
              find oe-ord where oe-ord.company = oe-boll.company
                and oe-ord.ord-no = oe-bolh.ord-no
              no-lock no-error.
              if available oe-ordl and available oe-ord then
              work-rel.completed = if oe-ordl.ship-qty + v-tmp >=
                oe-ordl.qty * (1 - oe-ordl.under-pct * .01) then "C" else "P".

            find first shipto where shipto.company = inv-line.company and
              shipto.cust-no = oe-bolh.cust-no and
              shipto.ship-no = oe-bolh.ship-no no-lock no-error.
            if avail shipto then
            assign
              work-rel.ship-addr = shipto.ship-addr[1]
              work-rel.ship-city = shipto.ship-city
              work-rel.ship-state = shipto.ship-state
              work-rel.ship-zip = shipto.ship-zip.
          end. /* each oe-boll */
        end. /* each oe-bolh */
      end. /* v-detail */

 /******************* MISCELLANEOUS ITEMS ***********************************/
      IF last-of(inv-line.r-no) then
      do:
        for each inv-misc where inv-misc.r-no = inv-head.r-no and
            inv-misc.bill ne "I"
            break by inv-misc.ord-no:

          if first(inv-misc.ord-no) and v-detail then
          put skip(1) "Miscellaneous" at 10 skip.

          if v-detail then
          do:
            display inv-misc.charge inv-misc.dscr inv-misc.amt
              with frame ordm.
            if inv-misc.bill = "N" then
            display "       N/C" @ inv-misc.amt with frame ordm.
          end. /* v-detail */

          down with frame ordm.
        end. /* each inv-misc */

        if v-detail then
        for each work-rel break by work-rel.line by work-rel.i-no:
          if first(work-rel.line) then
          put skip(1).
          display
            work-rel.i-no work-rel.po-no work-rel.loc work-rel.rel-date             work-rel.bol-no work-rel.r-no work-rel.carrier work-rel.ship-id
            work-rel.qty work-rel.completed
            with frame rel.
          down with frame rel.
          delete work-rel.
        end. /* each work-rel */
        put skip(1).
      end. /* last-of(inv-line.r-no) */
    end. /* each oe-ordl */

    delete report.
  end.

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.
  
  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .
  
  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).
  
  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:
       
      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".
                 
      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.
 
  put fill("-",80) format "x(80)" skip.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

