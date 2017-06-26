&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

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

/* Local Variable Definitions ---  
                                     */

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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR ll-do-deactivate AS LOG NO-UNDO.
DEF VAR hStatus AS HANDLE NO-UNDO.
DEF VAR llCancel AS LOG NO-UNDO.
DEF TEMP-TABLE tt-inactive-list 
    FIELD tt-i-no AS CHAR.
DEF VAR gviCnt AS INT NO-UNDO.
DEF STREAM excel.

def var v-i-no     like itemfg.i-no     extent 2  init ["","zzzzzzzzzzzzzzz"].
def var v-cust     like itemfg.cust-no  extent 2  init ["","zzzzzzzz"].
def var v-procat   like itemfg.procat   extent 2  init ["","zzzzz"].
def var v-break    as   char format "!" init "N".
def var v-prt-cost as   log format "Cost/Value" init no.
def var v-custown  as   log format "Y/N" init no.
def var v-sort-by  as   log format "Y/N" init no.
def var v-zero     as   log format "Y/N" init no.
def var v-sho-cost as   log format "Y/N" init no.

def var v-first       as log extent 2 init yes.
def var v-page-break  as char.
def var v-label1      as char format "x(14)" extent 3.
def var v-label2      as char format "x(14)".
def var v-price       as dec.
def var v-cost        as dec.
def var v-tq-onh      as dec extent 2.
def var v-tq-ono      like v-tq-onh.
def var v-tq-alloc    like v-tq-onh.
def var v-tq-avail    like v-tq-onh.
def var v-tprice      like v-tq-onh.
def var v-qty-onh     like itemfg.q-onh.
DEF VAR iCnt          AS INT  NO-UNDO.
DEF VAR jCnt          AS INT  NO-UNDO.
DEF VAR v-status      AS CHAR NO-UNDO FORMAT "x(1)".
DEF VAR lvdCutOffDate AS DATE NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR cMasterItemfg AS CHAR NO-UNDO.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, 
                       INPUT "FGMASTER",
                       INPUT "C",
                       INPUT NO, 
                       INPUT NO, 
                       INPUT "",
                       INPUT "", 
                       OUTPUT cReturn, 
                       OUTPUT lFound).
IF lFound THEN
    cMasterItemfg = cReturn.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiCutOffDate begin_i-no end_i-no ~
begin_cust-no end_cust-no begin_cat end_cat rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiCutOffDate begin_i-no end_i-no ~
begin_cust-no end_cust-no begin_cat end_cat rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCanDel C-Win 
FUNCTION getCanDel RETURNS LOGICAL
  ( ipcItem AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiCutOffDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Set FG Inactive Flag if no Shipments or Receipt History Exists after this Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-alphls.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_ext-cst AS CHARACTER FORMAT "X(256)":U INITIAL "Print Extended Cost or Value?" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_pg-brk AS CHARACTER FORMAT "X(256)":U INITIAL "Page Break?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_active AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Active", "A",
"Inactive", "I",
"All", "All"
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE rd_ext-cst AS CHARACTER INITIAL "Value" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cost", "Cost",
"Value", "Value"
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE rd_pg-brk AS CHARACTER INITIAL "None" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "None", "None",
"Customer#", "Customer#",
"Product Category", "Product Category"
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.81.

DEFINE VARIABLE tb_cost AS LOGICAL INITIAL no 
     LABEL "Print Unit Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-whse AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Sort By Item#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zero AS LOGICAL INITIAL yes 
     LABEL "Show Only Items with Quantity Greater than Zero?" 
     VIEW-AS TOGGLE-BOX
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiCutOffDate AT ROW 2 COL 75 COLON-ALIGNED WIDGET-ID 6
     begin_i-no AT ROW 3.86 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 3.86 COL 66 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cust-no AT ROW 4.81 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 4.81 COL 66 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cat AT ROW 5.76 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 5.76 COL 66 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     tb_cust-whse AT ROW 7.43 COL 3
     tb_cost AT ROW 7.43 COL 8
     tb_sort AT ROW 7.43 COL 15
     rd-dest AT ROW 7.57 COL 5 NO-LABEL
     lv-ornt AT ROW 7.57 COL 29 NO-LABEL
     lines-per-page AT ROW 7.57 COL 84 COLON-ALIGNED
     rd_active AT ROW 8.14 COL 4 NO-LABEL WIDGET-ID 2
     lbl_ext-cst AT ROW 8.14 COL 16 COLON-ALIGNED NO-LABEL
     rd_pg-brk AT ROW 8.14 COL 43 NO-LABEL
     tb_zero AT ROW 8.38 COL 5
     lbl_pg-brk AT ROW 8.38 COL 41 COLON-ALIGNED NO-LABEL
     rd_ext-cst AT ROW 8.62 COL 69 NO-LABEL
     lv-font-no AT ROW 9 COL 28 COLON-ALIGNED
     lv-font-name AT ROW 10.19 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 11.38 COL 27
     tb_excel AT ROW 12.33 COL 69 RIGHT-ALIGNED
     tb_runExcel AT ROW 12.33 COL 91 RIGHT-ALIGNED
     fi_file AT ROW 13.29 COL 47 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 16.71 COL 13
     btn-cancel AT ROW 16.81 COL 53
     "(Allocated Qty must = 0,  Qty On Order Qty must = 0 and Qty On Hand Qty = 0)" VIEW-AS TEXT
          SIZE 77 BY .62 AT ROW 2.95 COL 3 WIDGET-ID 8
     RECT-1 AT ROW 7.43 COL 2 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.4 BY 17.52.


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
         TITLE              = "Finished Goods Set Inactive Utility"
         HEIGHT             = 17.86
         WIDTH              = 96.8
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_ext-cst IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lbl_ext-cst:HIDDEN IN FRAME FRAME-A           = TRUE
       lbl_ext-cst:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ext-cst".

/* SETTINGS FOR FILL-IN lbl_pg-brk IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lbl_pg-brk:HIDDEN IN FRAME FRAME-A           = TRUE
       lbl_pg-brk:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_pg-brk".

/* SETTINGS FOR RADIO-SET rd_active IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_active:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET rd_ext-cst IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_ext-cst:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_ext-cst:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RADIO-SET rd_pg-brk IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_pg-brk:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_pg-brk:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cost IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_cost:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_cost:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cust-whse IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_cust-whse:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_cust-whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sort IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_sort:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_zero IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_zero:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Set Inactive Utility */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Set Inactive Utility */
DO:
  IF VALID-HANDLE(hStatus) THEN
      DELETE OBJECT hStatus.

  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
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


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  rd_active = "A".

  IF fiCutOffDate EQ ? THEN DO:
      MESSAGE "Cut Off Date cannot be blank - please re-enter"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "entry" TO fiCutOffDate .
      RETURN NO-APPLY.
  END.

  RUN windows/w-message.w PERSISTENT SET hStatus.
  RUN setWindowTitle IN hStatus (INPUT "Searching for Items").


  run run-report. 

  IF VALID-HANDLE(hStatus) THEN
      DELETE OBJECT hStatus.
 case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject=c-win:TITLE 
                            &fax-body=c-win:TITLE 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject=c-win:TITLE 
                             &mail-body=c-win:title 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust-no
                                  &END_cust=end_cust-no
                                  &mail-subject=c-win:TITLE 
                                  &mail-body=c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 


/*   case rd-dest:                                                                     */
/*        when 1 then run output-to-printer.                                           */
/*        when 2 then run output-to-screen.                                            */
/*        when 3 then run output-to-file.                                              */
/*        when 4 then do:                                                              */
/*            /*run output-to-fax.*/                                                   */
/*            {custom/asifax.i &begin_cust=begin_cust-no                               */
/*                             &END_cust=END_cust-no                                   */
/*                             &fax-subject=c-win:TITLE                                */
/*                             &fax-body=c-win:TITLE                                   */
/*                             &fax-file=list-name }                                   */
/*        END.                                                                         */
/*        when 5 then do:                                                              */
/*            IF is-xprint-form THEN DO:                                               */
/*               RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22"). */
/*               {custom/asimail.i &TYPE = "CUSTOMER"                                  */
/*                              &begin_cust= begin_cust-no                             */
/*                              &END_cust=end_cust-no                                  */
/*                              &mail-subject=c-win:TITLE                              */
/*                              &mail-body=c-win:title                                 */
/*                              &mail-file=list-name }                                 */
/*            END.                                                                     */
/*            ELSE DO:                                                                 */
/*                {custom/asimailr.i &TYPE = "CUSTOMER"                                */
/*                                   &begin_cust= begin_cust-no                        */
/*                                   &END_cust=end_cust-no                             */
/*                                   &mail-subject=c-win:TITLE                         */
/*                                   &mail-body=c-win:TITLE                            */
/*                                   &mail-file=list-name }                            */
/*                                                                                     */
/*            END.                                                                     */
/*                                                                                     */
/*        END.                                                                         */
/*        WHEN 6 THEN run output-to-port.                                              */
/*   end case.                                                                         */
   FIND FIRST tt-inactive-list NO-ERROR.
   IF AVAIL tt-inactive-list THEN DO:

      /* Prompt to inactivate items based on list created */
       MESSAGE "Deactivate the " gviCnt " items?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll-do-deactivate.
       IF ll-do-deactivate THEN DO:
          RUN windows/w-message.w PERSISTENT SET hStatus.
          RUN setWindowTitle IN hStatus (INPUT "Deactivating Items").

          RUN deactivate-items.
          IF VALID-HANDLE(hStatus) THEN
               DELETE OBJECT hStatus.
          MESSAGE "Done!"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.


    END. /* something was found to delete */
    ELSE 
        MESSAGE "Nothing found to deactivate!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

     IF VALID-HANDLE(hStatus) THEN
          DELETE OBJECT hStatus.


    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
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


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME rd_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_active C-Win
ON VALUE-CHANGED OF rd_active IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ext-cst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ext-cst C-Win
ON VALUE-CHANGED OF rd_ext-cst IN FRAME FRAME-A
DO:
  assign {&self-name}.
  IF {&self-name} EQ "Cost" THEN tb_cost:SCREEN-VALUE = "yes".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_pg-brk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_pg-brk C-Win
ON VALUE-CHANGED OF rd_pg-brk IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost C-Win
ON VALUE-CHANGED OF tb_cost IN FRAME FRAME-A /* Print Unit Cost? */
DO:
  assign {&self-name}.
  APPLY "value-changed" TO rd_ext-cst.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-whse C-Win
ON VALUE-CHANGED OF tb_cust-whse IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort By Item#? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Show Only Items with Quantity Greater than Zero? */
DO:
  assign {&self-name}.
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
APPLY "entry" TO fiCutOffDate .

  RUN enable_UI.

SUBSCRIBE TO "CancelIt" ANYWHERE.
SUBSCRIBE TO "NumDel" ANYWHERE.
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    /* {custom/usrprint.i} */
    APPLY "entry" TO fiCutOffDate .
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
PROCEDURE LockWindowUpdate EXTERNAL "user32.dll": 
DEFINE INPUT PARAMETER hWndLock AS LONG NO-UNDO. 
END PROCEDURE. /* LockWindowUpdate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelIt C-Win 
PROCEDURE cancelIt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* respond to cancel event */
llCancel = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deactivate-items C-Win 
PROCEDURE deactivate-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iCnt          AS INT  NO-UNDO.
DEF VAR jCnt          AS INT  NO-UNDO.


FOR EACH tt-inactive-list:


    FIND itemfg WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ tt-inactive-list.tt-i-no
       EXCLUSIVE-LOCK NO-ERROR.


    IF AVAIL itemfg THEN DO:

        iCnt = iCnt + 1. 
        IF iCnt GT 999 THEN DO:  
          iCnt = 0. PROCESS EVENTS. 
          jCnt = jCnt + 1000. 
          PUBLISH "NUMDEL" (itemfg.i-no, jCnt). 
        END. 
        IF llCancel THEN 
            LEAVE.

        ASSIGN itemfg.stat = "I".
/*         FIND FIRST reftable     WHERE reftable.reftable EQ "FGSTATUS" */
/*           AND reftable.company  EQ cocode                             */
/*           AND reftable.loc      EQ ""                                 */
/*           AND reftable.code     EQ itemfg.i-no NO-ERROR.              */
/*                                                                       */
/*         IF NOT AVAIL reftable THEN  DO:                               */
/*             CREATE reftable.                                          */
/*             ASSIGN                                                    */
/*                reftable.reftable = "FGSTATUS"                         */
/*                reftable.company  = cocode                             */
/*                reftable.loc      = ""                                 */
/*                reftable.code     = itemfg.i-no.                       */
/*         END. /* if not avail reftable */                              */
/*                                                                       */
/*         IF AVAIL reftable THEN                                        */
/*            reftable.code2 = "I".                                      */


    END. /* avail itemfg */


  END. /* each tt-inactive-list */

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
  DISPLAY fiCutOffDate begin_i-no end_i-no begin_cust-no end_cust-no begin_cat 
          end_cat rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-1 fiCutOffDate begin_i-no end_i-no begin_cust-no end_cust-no 
         begin_cat end_cat rd-dest lv-ornt lines-per-page lv-font-no 
         lv-font-name td-show-parm tb_excel tb_runExcel fi_file btn-ok 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE numDel C-Win 
PROCEDURE numDel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiCnt AS INT NO-UNDO.
Run LockWindowUpdate(input CURRENT-WINDOW:HWND). 
IF VALID-HANDLE(hStatus) THEN
  RUN process-message IN hStatus (INPUT ipcTable + ": " + STRING(ipiCnt)).
Run LockWindowUpdate(input 0).

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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
 */
 {CUstom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN custom/d-print.w (list-name).

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
/* ------------------------------------------------ fg/rep/fg-aitem.p 9/91 cd */
/* finished goods - alphabetic item report.                                   */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-i-no     like itemfg.i-no     extent 2  init ["","zzzzzzzzzzzzzzz"].
def var v-cust     like itemfg.cust-no  extent 2  init ["","zzzzzzzz"].
def var v-procat   like itemfg.procat   extent 2  init ["","zzzzz"].
def var v-break    as   char format "!" init "N".
def var v-prt-cost as   log format "Cost/Value" init no.
def var v-custown  as   log format "Y/N" init no.
def var v-sort-by  as   log format "Y/N" init no.
def var v-zero     as   log format "Y/N" init no.
def var v-sho-cost as   log format "Y/N" init no.

def var v-first       as log extent 2 init yes.
def var v-page-break  as char.
def var v-label1      as char format "x(14)" extent 3.
def var v-label2      as char format "x(14)".
def var v-price       as dec.
def var v-cost        as dec.
def var v-tq-onh      as dec extent 2.
def var v-tq-ono      like v-tq-onh.
def var v-tq-alloc    like v-tq-onh.
def var v-tq-avail    like v-tq-onh.
def var v-tprice      like v-tq-onh.
def var v-qty-onh     like itemfg.q-onh.
DEF VAR iCnt          AS INT  NO-UNDO.
DEF VAR jCnt          AS INT  NO-UNDO.
DEF VAR v-status      AS CHAR NO-UNDO FORMAT "x(1)".

DEF VAR excelheader AS CHAR NO-UNDO.

form header skip(1)
            v-page-break format "x(132)"

    with frame r-top-2 STREAM-IO width 140 no-labels no-box no-underline page-top.

form header skip(1)
            "               "
            "             "
            "PROD "
            "   "
            v-label1[1]
            "     SELLING"
            "           "
            "           "
            "   CUSTOMER"
            "           "
            "         TOTAL"
            skip

            "ITEM           "
            "DESCRIPTION  "
            "CAT  "
            "UOM"
            v-label1[2]
            "       PRICE"
            "    ON HAND"
            "   ON ORDER"
            "     ORDERS"
            "  QTY AVAIL"
            v-label2
            "STAT"
            skip

            "---------------"
            "-------------"
            "-----"
            "---"
            v-label1[3]
            "------------"
            "-----------"
            "-----------"
            "-----------"
            "-----------"
            "--------------"
            "-----"
          skip
    with frame r-top-3 STREAM-IO width 140 no-labels no-box no-underline page-top.

form
    itemfg.i-no                                 
    itemfg.i-name           format "x(13)"          
    itemfg.procat                               
    itemfg.sell-uom 
    itemfg.total-std-cost   format "->>,>>>,>>9.99" 
    itemfg.sell-price       format ">,>>>,>>9.99"
    v-qty-onh               format "->>,>>>,>>9"    
    itemfg.q-ono            format "->>,>>>,>>9"    
    itemfg.q-alloc          format "->>,>>>,>>9"    
    itemfg.q-avail          format "->>,>>>,>>9"    
    v-price                 format "->>,>>>,>>9.99" 
    v-status                FORMAT "x(1)"
    SKIP  
   with frame itemx no-labels no-box down STREAM-IO WIDTH 140.

ASSIGN
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-i-no[1]    = begin_i-no
 v-i-no[2]    = end_i-no
 v-cust[1]    = begin_cust-no
 v-cust[2]    = end_cust-no
 v-procat[1]  = begin_cat
 v-procat[2]  = END_cat
 v-break      = SUBSTR(rd_pg-brk,1,1)
 v-sho-cost   = tb_cost
 v-prt-cost   = rd_ext-cst EQ "Cost"
 v-custown    = tb_cust-whse
 v-sort-by    = tb_sort
 v-zero       = tb_zero
 lvdCutOffDate = DATE(fiCutOffDate).
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

/* if td-show-parm then run show-param. */

SESSION:SET-WAIT-STATE ("general").

  find first fg-ctrl where fg-ctrl.company eq cocode no-lock.

  assign
   v-label1[1] = if fg-ctrl.inv-meth eq "A" then
                   "       AVERAGE" else "          LAST"
   v-label1[2] = "          COST"
   v-label1[3] = "--------------"
   v-label2    = if v-prt-cost then
                   "          COST" else "         VALUE".

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "FG ITEM#,DESCRIPTION,PROD CAT,UOM," + TRIM(v-label1[1]) +
                " " + TRIM(v-label1[2]) + ",SELLING PRICE,ON HAND,ON ORDER," +
                "CUSTOMER ORDERS,QTY AVAIL,TOTAL " + TRIM(v-label2).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

  if not v-sho-cost then v-label1 = "".
/* For utility, only look at items le 0 */
v-zero = NO.
gviCnt = 0.
EMPTY TEMP-TABLE tt-inactive-list.
main-loop:
  for each itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    ge v-i-no[1]
        and itemfg.i-no    le v-i-no[2]
        and itemfg.cust-no ge v-cust[1]
        and itemfg.cust-no le v-cust[2]
        and itemfg.procat  ge v-procat[1]
        and itemfg.procat  le v-procat[2]
        AND itemfg.q-onh   LE 0  /* Only want to inactivate items with no inventory */
      no-lock

      break by (if v-break eq "C" then itemfg.cust-no else
                if v-break eq "P" then itemfg.procat  else "1")
            by (if v-sort-by then itemfg.i-no else itemfg.i-name)

      with frame itemx.



/*       /* Check active status. */                                            */
/*      FIND FIRST reftable NO-LOCK WHERE                                      */
/*          reftable.reftable = "FGSTATUS" AND                                 */
/*          reftable.company  = itemfg.company AND                             */
/*          reftable.loc      = "" AND                                         */
/*          reftable.code     = itemfg.i-no NO-ERROR.                          */
/*                                                                             */
/*      /* If no status record found and user selected Active or Inactive,     */
/*         then skip.  (If all selected, then print it) */                     */
/*      IF AVAILABLE reftable AND reftable.code2 EQ "I" THEN NEXT.             */
/*                                                                             */
/*                                                                             */
/*     IF AVAIL reftable THEN                                                  */
/*         ASSIGN v-status = reftable.code2.                                   */
/*     ELSE                                                                    */
/*         ASSIGN v-status = "".                                               */
/*                                                                             */
/*     /* Make sure last activity for this item is before cutoff date */       */
/*     FIND LAST fg-rcpth WHERE fg-rcpth.company EQ itemfg.company             */
/*          AND fg-rcpth.i-no  EQ itemfg.i-no                                  */
/*          AND fg-rcpth.trans-date GT lvdCutOffDate                           */
/*          AND fg-rcpth.rita-code EQ "R"                                      */
/*        USE-INDEX tran NO-LOCK NO-ERROR.                                     */
/*     IF AVAIL(fg-rcpth) THEN                                                 */
/*         NEXT.                                                               */
/*                                                                             */
/*     FIND LAST fg-rcpth WHERE fg-rcpth.company EQ itemfg.company             */
/*          AND fg-rcpth.i-no  EQ itemfg.i-no                                  */
/*          AND fg-rcpth.trans-date GT lvdCutOffDate                           */
/*          AND fg-rcpth.rita-code EQ "S"                                      */
/*        USE-INDEX tran NO-LOCK NO-ERROR.                                     */
/*     IF AVAIL(fg-rcpth) THEN                                                 */
/*         NEXT.                                                               */
/*                                                                             */
/*     FIND LAST fg-rcpth WHERE fg-rcpth.company EQ itemfg.company             */
/*          AND fg-rcpth.i-no  EQ itemfg.i-no                                  */
/*          AND fg-rcpth.trans-date GT lvdCutOffDate                           */
/*          AND fg-rcpth.rita-code EQ "T"                                      */
/*        USE-INDEX tran NO-LOCK NO-ERROR.                                     */
/*     IF AVAIL(fg-rcpth) THEN                                                 */
/*         NEXT.                                                               */
/*                                                                             */
/*     v-qty-onh  = 0.                                                         */
/*     for each fg-bin                                                         */
/*         where fg-bin.company eq cocode                                      */
/*           and fg-bin.i-no    eq itemfg.i-no                                 */
/*         use-index i-no no-lock:                                             */
/*                                                                             */
/*       if v-custown or (fg-bin.loc ne "CUST" and fg-bin.cust-no eq "") then  */
/*         v-qty-onh = v-qty-onh + fg-bin.qty.                                 */
/*     end. /* each bin */                                                     */
/*                                                                             */
/*     /* Has on hand, then don't include */                                   */
/*     IF v-qty-onh GT 0 THEN                                                  */
/*         NEXT.                                                               */
/*                                                                             */
/*     /* Has on order, then don't include */                                  */
/*     IF itemfg.q-ono GT 0 THEN                                               */
/*         NEXT.                                                               */
/*                                                                             */
/*     /* Has allocated, then don't include */                                 */
/*     IF itemfg.q-alloc GT 0 THEN                                             */
/*         NEXT.                                                               */

    IF NOT getCanDel(itemfg.i-no) THEN
        NEXT.
    IF itemfg.isaset THEN DO:
        FOR EACH fg-set WHERE fg-set.company EQ cocode
            AND fg-set.set-no EQ itemfg.i-no NO-LOCK.
            IF NOT getCanDel(fg-set.part-no) THEN
                NEXT main-loop.
        END.
    END.

    /* Never deactivate the master item */
    IF itemfg.i-no EQ cMasterItemfg THEN
      NEXT main-loop.

    CREATE tt-inactive-list .
    ASSIGN tt-inactive-list.tt-i-no = itemfg.i-no.
    gviCnt = gviCnt + 1.

    iCnt = iCnt + 1. 
    IF iCnt GT 999 THEN DO:  
        iCnt = 0. PROCESS EVENTS. 
        jCnt = jCnt + 1000. 
        PUBLISH "NUMDEL" (itemfg.i-no, jCnt). 
    END. 


    /* If it's an adjustment or a count, I'm assuming they purposely */
    /* closed the item out so that is allowed after cutoff date      */


    if first-of(if v-break eq "C" then itemfg.cust-no else
                if v-break eq "P" then itemfg.procat  else "1") then
      v-first[2] = yes.



    if v-qty-onh ne 0 or not v-zero then do:
      if v-first[2] then do:
        assign
         v-first[2]   = no
         v-page-break = if v-break eq "C" then
                          ("Customer: " + trim(itemfg.cust-no))
                        else
                        if v-break eq "P" then
                          ("Product Category: " + trim(itemfg.procat))
                        else "".  

        IF v-first[1] THEN DO:
          v-first[1] = NO.
          DISPLAY WITH frame r-top.
          IF v-break NE "N" THEN DISPLAY WITH FRAME r-top-2.
          DISPLAY WITH frame r-top-3.
        END.

        ELSE PAGE.
      end.

      if v-prt-cost then do:
        v-cost = itemfg.total-std-cost.

        if itemfg.prod-uom ne "EA" then
          run sys/ref/convcuom.p (itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                  v-cost, output v-cost).

        v-price = v-cost * v-qty-onh.  
      end.

      else do:
        find first uom
            where uom.uom  eq itemfg.sell-uom
              and uom.mult ne 0
            no-lock no-error.
        v-price = v-qty-onh * itemfg.sell-price /
                  (if avail uom then uom.mult else 1000).

        if itemfg.sell-uom eq "CS" then
          v-price = v-qty-onh / itemfg.case-count * itemfg.sell-price.

        else
        /* gdm - 04130901 */
        IF itemfg.sell-uom EQ "L" AND v-qty-onh > 0
          THEN v-price =  itemfg.sell-price.
        IF itemfg.sell-uom EQ "L" AND v-qty-onh  < 1
          THEN v-price = (v-qty-onh * itemfg.sell-price).

      end.

      if v-price eq ? then v-price = 0.
      IF llCancel THEN
          LEAVE.
      display itemfg.i-no
              itemfg.i-name
              itemfg.procat
              itemfg.sell-uom
              itemfg.prod-uom when v-prt-cost @ itemfg.sell-uom
              itemfg.total-std-cost when v-sho-cost
              itemfg.sell-price
              v-qty-onh
              itemfg.q-ono
              itemfg.q-alloc
              (v-qty-onh + itemfg.q-ono - itemfg.q-alloc) @ itemfg.q-avail
              v-price
              v-status
          with frame itemx.
      down with frame itemx.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            '"' itemfg.i-no                                               '",'
            '"' itemfg.i-name                                             '",'
            '"' itemfg.procat                                             '",'
            '"' (IF v-prt-cost THEN itemfg.prod-uom ELSE itemfg.sell-uom) '",'
            '"' (IF v-sho-cost THEN itemfg.total-std-cost ELSE 0)         '",'
            '"' itemfg.sell-price                                         '",'
            '"' v-qty-onh                                                 '",'
            '"' itemfg.q-ono                                              '",'
            '"' itemfg.q-alloc                                            '",'
            '"' (v-qty-onh + itemfg.q-ono - itemfg.q-alloc)               '",'
            '"' v-price                                                   '",'
            '"' v-status                                            '",'
            SKIP.

      assign
       v-tq-onh[1]   = v-tq-onh[1]   + v-qty-onh
       v-tq-ono[1]   = v-tq-ono[1]   + itemfg.q-ono
       v-tq-alloc[1] = v-tq-alloc[1] + itemfg.q-alloc
       v-tq-avail[1] = v-tq-avail[1] + 
                       (v-qty-onh + itemfg.q-ono - itemfg.q-alloc)
       v-tprice[1]   = v-tprice[1]   + v-price.
    end.

    if not v-first[2]                                          and
       last-of(if v-break eq "C" then itemfg.cust-no else
               if v-break eq "P" then itemfg.procat  else "1") then do:

      if v-break ne "N" then do:
        put skip(1).

        display "Cust Total"                          @ itemfg.i-name
                  "ProdCat Total" when v-break eq "P" @ itemfg.i-name
                v-tq-onh[1]                           @ v-qty-onh
                v-tq-ono[1]                           @ itemfg.q-ono
                v-tq-alloc[1]                         @ itemfg.q-alloc
                v-tq-avail[1]                         @ itemfg.q-avail
                v-tprice[1]                           @ v-price
            with frame itemx.
        down with frame itemx.

        put skip(1).
      end.

      assign
       v-tq-onh[2]   = v-tq-onh[2]   + v-tq-onh[1]
       v-tq-ono[2]   = v-tq-ono[2]   + v-tq-ono[1]
       v-tq-alloc[2] = v-tq-alloc[2] + v-tq-alloc[1]
       v-tq-avail[2] = v-tq-avail[2] + v-tq-avail[1]
       v-tprice[2]   = v-tprice[2]   + v-tprice[1]

       v-tq-onh[1]   = 0
       v-tq-ono[1]   = 0
       v-tq-alloc[1] = 0
       v-tq-avail[1] = 0
       v-tprice[1]   = 0.
    end.

    if not v-first[1]                                       and
       last(if v-break eq "C" then itemfg.cust-no else
            if v-break eq "P" then itemfg.procat  else "1") then do:

      hide frame r-top-2 no-pause.

      if v-break ne "N" then page.
      else put skip(2).

      display "Grand Total"                         @ itemfg.i-name
              v-tq-onh[2]                           @ v-qty-onh
              v-tq-ono[2]                           @ itemfg.q-ono
              v-tq-alloc[2]                         @ itemfg.q-alloc
              v-tq-avail[2]                         @ itemfg.q-avail
              v-tprice[2]                           @ v-price
          with frame itemx.
      down with frame itemx.
    end.      
  end. /* each itemfg */

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

/* Taking this out as it is a utility */
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCanDel C-Win 
FUNCTION getCanDel RETURNS LOGICAL
  ( ipcItem AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bf-itemfg FOR itemfg.
DEF VAR lCanDel AS LOG NO-UNDO.

FIND bf-itemfg WHERE bf-itemfg.company = cocode
    AND bf-itemfg.i-no = ipcItem NO-LOCK NO-ERROR.
IF NOT AVAIL bf-itemfg THEN
    RETURN FALSE.

lCanDel = TRUE.
     /* Check active status. */
/*      FIND FIRST reftable NO-LOCK WHERE                */
/*          reftable.reftable = "FGSTATUS" AND           */
/*          reftable.company  = bf-itemfg.company AND    */
/*          reftable.loc      = "" AND                   */
/*          reftable.code     = bf-itemfg.i-no NO-ERROR. */

     /* If no status record found and user selected Active or Inactive,
        then skip.  (If all selected, then print it) */
    IF bf-itemfg.stat = "I" THEN lCanDel = FALSE.
    v-status = bf-itemfg.stat.
/*      IF AVAILABLE reftable AND reftable.code2 EQ "I" THEN lCanDel = FALSE. */
/*                                                                            */
/*                                                                            */
/*     IF AVAIL reftable THEN                                                 */
/*         ASSIGN v-status = reftable.code2.                                  */
/*     ELSE                                                                   */
/*         ASSIGN v-status = "".                                              */

    /* Make sure last activity for this item is before cutoff date */
    FIND LAST fg-rcpth WHERE fg-rcpth.company EQ bf-itemfg.company
         AND fg-rcpth.i-no  EQ bf-itemfg.i-no
         AND fg-rcpth.trans-date GT lvdCutOffDate
         AND fg-rcpth.rita-code EQ "R"
       USE-INDEX tran NO-LOCK NO-ERROR.
    IF AVAIL(fg-rcpth) THEN
        lCanDel = FALSE.

    FIND LAST fg-rcpth WHERE fg-rcpth.company EQ bf-itemfg.company
         AND fg-rcpth.i-no  EQ bf-itemfg.i-no
         AND fg-rcpth.trans-date GT lvdCutOffDate
         AND fg-rcpth.rita-code EQ "S"
       USE-INDEX tran NO-LOCK NO-ERROR.
    IF AVAIL(fg-rcpth) THEN
        lCanDel = FALSE.

    FIND LAST fg-rcpth WHERE fg-rcpth.company EQ bf-itemfg.company
         AND fg-rcpth.i-no  EQ bf-itemfg.i-no
         AND fg-rcpth.trans-date GT lvdCutOffDate
         AND fg-rcpth.rita-code EQ "T"
       USE-INDEX tran NO-LOCK NO-ERROR.
    IF AVAIL(fg-rcpth) THEN
        lCanDel = FALSE.

    v-qty-onh  = 0.     
    for each fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq bf-itemfg.i-no
        use-index i-no no-lock:

      if v-custown or (fg-bin.loc ne "CUST" and fg-bin.cust-no eq "") then
        v-qty-onh = v-qty-onh + fg-bin.qty.
    end. /* each bin */

    /* Has on hand, then don't include */
    IF v-qty-onh GT 0 THEN
        lCanDel = FALSE.

    /* Has on order, then don't include */
    IF bf-itemfg.q-ono GT 0 THEN
        lCanDel = FALSE.

    /* Has allocated, then don't include */
    IF bf-itemfg.q-alloc GT 0 THEN
        lCanDel = FALSE.

    RETURN lCanDel.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

