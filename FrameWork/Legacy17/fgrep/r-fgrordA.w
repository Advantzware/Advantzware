&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-fgord.w

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
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/prgsecdt.i} 
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

DO TRANSACTION:
    {sys/inc/custlistform.i ""IR1"" }
END.

 {sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR v-whse-bin-found AS LOG NO-UNDO.
DEF VAR v-sales-rep AS CHAR NO-UNDO.
DEF VAR v-whse-printed AS LOG NO-UNDO.

DEFINE STREAM excel.

DO TRANSACTION:
   {sys/inc/selrptcol.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_cust-list btnCustList begin_cust end_cust ~
begin_i-no end_i-no begin_cat end_cat begin_whse end_whse begin_class ~
end_class begin_group end_group tb_inc-qoh tb_part tb_inc-cust tb_history ~
tb_below tb_dash rd_qoh begin_as-of rd_stocked rd_pur-man rd_lot-reo ~
rd_qav-ven rd_pri-ven-max lv-ornt lines-per-page rd-dest lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel ~
tb_reord-by-whse RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust end_cust ~
begin_i-no end_i-no begin_cat end_cat begin_whse end_whse begin_class ~
end_class begin_group end_group tb_inc-qoh tb_part tb_inc-cust tb_history ~
tb_below tb_dash lbl_qoh1 rd_qoh begin_as-of lbl_stocked rd_stocked ~
lbl_pur-man rd_pur-man lbl_lot-reo rd_lot-reo lbl_qav-ven rd_qav-ven ~
lbl_pri-ven-max rd_pri-ven-max lv-ornt lines-per-page rd-dest lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file tb_reord-by-whse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY*/
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_as-of AS DATE FORMAT "99/99/9999":U INITIAL 01/01/02 
     LABEL "As Of" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_class AS CHARACTER FORMAT "X(1)":U 
     LABEL "Beginning Class" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_group AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Group" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_class AS CHARACTER FORMAT "X(1)":U INITIAL "z" 
     LABEL "Ending Class" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_group AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Group" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fgord.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_lot-reo AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91 NO-UNDO.

DEFINE VARIABLE lbl_pri-ven-max AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91 NO-UNDO.

DEFINE VARIABLE lbl_pur-man AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91 NO-UNDO.

DEFINE VARIABLE lbl_qav-ven AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91 NO-UNDO.

DEFINE VARIABLE lbl_qoh1 AS CHARACTER FORMAT "X(256)":U INITIAL "Calc QOH?" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_stocked AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91 NO-UNDO.

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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 21 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_lot-reo AS CHARACTER INITIAL "Reorder" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lot Controlled", "Lot Controlled",
"Reorder", "Reorder",
"All", "All"
     SIZE 39 BY .95 NO-UNDO.

DEFINE VARIABLE rd_pri-ven-max AS CHARACTER INITIAL "Vendor" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Price", "Price",
"Vendor Item #", "Vendor",
"Max Qty", "Max Qty"
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE rd_pur-man AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Purchased", "Purchased",
"Manufactured", "Manufactured",
"All", "All"
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE rd_qav-ven AS CHARACTER INITIAL "Vendor" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Qty Avail", "Qty Avail",
"Vendor", "Vendor"
     SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE rd_qoh AS CHARACTER INITIAL "Total Allocated" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Allocated", "Total Allocated",
"Scheduled", "Scheduled Releases",
"Actual", "Actual Releases",
"All Releases", "All Releases"
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE rd_stocked AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Stocked", "Stocked",
"Not Stocked", "Not Stocked",
"All", "All"
     SIZE 38 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 8.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 17.38.

DEFINE VARIABLE tb_below AS LOGICAL INITIAL YES 
     LABEL "Print Items Below Reorder Point Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 49 BY .91 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL NO 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_dash AS LOGICAL INITIAL NO 
     LABEL "Print Dashes Between Each Line?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .91 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_history AS LOGICAL INITIAL NO 
     LABEL "Print 6 Month History?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .91 NO-UNDO.

DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL NO 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .91 NO-UNDO.

DEFINE VARIABLE tb_inc-qoh AS LOGICAL INITIAL YES 
     LABEL "Include Quantity On Order with Quantity On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .91 NO-UNDO.

DEFINE VARIABLE tb_part AS LOGICAL INITIAL NO 
     LABEL "Print Customer Part#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .91 NO-UNDO.

DEFINE VARIABLE tb_reord-by-whse AS LOGICAL INITIAL YES 
     LABEL "ReOrder Level by Whse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .91 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL YES 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.19 COL 31.4 WIDGET-ID 6
     btnCustList AT ROW 1.24 COL 63.4 WIDGET-ID 8
     begin_cust AT ROW 2.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.24 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_i-no AT ROW 3.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning FG Item Number"
     end_i-no AT ROW 3.19 COL 70 COLON-ALIGNED HELP
          "Enter Ending FG Item Number"
     begin_cat AT ROW 4.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 4.14 COL 70 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_whse AT ROW 5.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_whse AT ROW 5.1 COL 70 COLON-ALIGNED HELP
          "Enter Ending Warehouse Number"
     begin_class AT ROW 6.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Class" WIDGET-ID 2
     end_class AT ROW 6.14 COL 70 COLON-ALIGNED HELP
          "Enter Ending Class" WIDGET-ID 4
     begin_group AT ROW 7.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Group" WIDGET-ID 6
     end_group AT ROW 7.19 COL 70 COLON-ALIGNED HELP
          "Enter Ending Group" WIDGET-ID 8
     tb_inc-qoh AT ROW 8.38 COL 2
     tb_part AT ROW 8.38 COL 57
     tb_inc-cust AT ROW 9.33 COL 45 RIGHT-ALIGNED
     tb_history AT ROW 9.33 COL 57
     tb_below AT ROW 10.29 COL 50 RIGHT-ALIGNED
     tb_dash AT ROW 10.29 COL 57
     lbl_qoh1 AT ROW 12.19 COL 2 NO-LABEL
     rd_qoh AT ROW 12.19 COL 14 NO-LABEL
     begin_as-of AT ROW 12.19 COL 75 COLON-ALIGNED
     lbl_stocked AT ROW 13.38 COL 18 COLON-ALIGNED NO-LABEL
     rd_stocked AT ROW 13.38 COL 29 NO-LABEL
     lbl_pur-man AT ROW 14.33 COL 18 COLON-ALIGNED NO-LABEL
     rd_pur-man AT ROW 14.33 COL 29 NO-LABEL
     lbl_lot-reo AT ROW 15.29 COL 18 COLON-ALIGNED NO-LABEL
     rd_lot-reo AT ROW 15.29 COL 29 NO-LABEL
     lbl_qav-ven AT ROW 16.24 COL 18 COLON-ALIGNED NO-LABEL
     rd_qav-ven AT ROW 16.24 COL 29 NO-LABEL
     lbl_pri-ven-max AT ROW 17.19 COL 18 COLON-ALIGNED NO-LABEL
     rd_pri-ven-max AT ROW 17.19 COL 29 NO-LABEL
     lv-ornt AT ROW 19.24 COL 30 NO-LABEL
     lines-per-page AT ROW 19.24 COL 83 COLON-ALIGNED
     rd-dest AT ROW 19.76 COL 6 NO-LABEL
     lv-font-no AT ROW 20.19 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 21.19 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.62 COL 29
     tb_excel AT ROW 23.86 COL 66 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.86 COL 89.8 RIGHT-ALIGNED
     fi_file AT ROW 24.81 COL 44 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27 COL 19
     btn-cancel AT ROW 27 COL 59
     tb_reord-by-whse AT ROW 11.24 COL 36 RIGHT-ALIGNED WIDGET-ID 10
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 19.05 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 18.38 COL 1.4
     RECT-7 AT ROW 1 COL 1.4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 96 BY 27.91.


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
         TITLE              = "FG Reordering Advice Report"
         HEIGHT             = 27.95
         WIDTH              = 96.2
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_as-of:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_class:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_group:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_class:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_group:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_lot-reo IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_lot-reo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_lot-reo".

/* SETTINGS FOR FILL-IN lbl_pri-ven-max IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_pri-ven-max:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_pri-ven-max".

/* SETTINGS FOR FILL-IN lbl_pur-man IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_pur-man:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_pur-man".

/* SETTINGS FOR FILL-IN lbl_qav-ven IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_qav-ven:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qav-ven".

/* SETTINGS FOR FILL-IN lbl_qoh1 IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lbl_qoh1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qoh".

/* SETTINGS FOR FILL-IN lbl_stocked IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_stocked:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_stocked".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_lot-reo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_pri-ven-max:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_pur-man:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_qav-ven:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_qoh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_stocked:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_below IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_below:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_dash:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_history:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_inc-cust IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_inc-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inc-qoh:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_part:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_reord-by-whse IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_reord-by-whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Reordering Advice Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG Reordering Advice Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_as-of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_as-of C-Win
ON LEAVE OF begin_as-of IN FRAME FRAME-A /* As Of */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_class
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_class C-Win
ON HELP OF begin_class IN FRAME FRAME-A /* Beginning Class */
DO:
    DEF VAR char-val AS CHAR NO-UNDO INIT "".
    RUN windows/l-usrgrp.w (INPUT "FG CLASS", OUTPUT char-val).
     IF char-val <> "" THEN
        ASSIGN SELF:SCREEN-VALUE = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_class C-Win
ON LEAVE OF begin_class IN FRAME FRAME-A /* Beginning Class */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_group C-Win
ON HELP OF begin_group IN FRAME FRAME-A /* Beginning Group */
DO:
    DEF VAR char-val AS CHAR NO-UNDO INIT "".
    RUN windows/l-usrgrp.w (INPUT "SALES GROUPS", OUTPUT char-val).
     IF char-val <> "" THEN
        ASSIGN SELF:SCREEN-VALUE = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_group C-Win
ON LEAVE OF begin_group IN FRAME FRAME-A /* Beginning Group */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* Beginning Warehouse */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

  SESSION:SET-WAIT-STATE("general").
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN DO:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT end_cust).
  END. 
  IF tb_reord-by-whse THEN
      RUN run-report-whse.
  ELSE
      RUN run-report. 
  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=END_cust
                            &END_cust=END_cust
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= END_cust
                             &END_cust=END_cust
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust
                                  &END_cust=END_cust
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN RUN output-to-port.
  END CASE. 
  SESSION:SET-WAIT-STATE("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_class
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_class C-Win
ON HELP OF end_class IN FRAME FRAME-A /* Ending Class */
DO:
    DEF VAR char-val AS CHAR NO-UNDO INIT "".
    RUN windows/l-usrgrp.w (INPUT "FG CLASS", OUTPUT char-val).
     IF char-val <> "" THEN
        ASSIGN SELF:SCREEN-VALUE = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_class C-Win
ON LEAVE OF end_class IN FRAME FRAME-A /* Ending Class */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_group
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_group C-Win
ON HELP OF end_group IN FRAME FRAME-A /* Ending Group */
DO:
    DEF VAR char-val AS CHAR NO-UNDO INIT "".
    RUN windows/l-usrgrp.w (INPUT "SALES GROUPS", OUTPUT char-val).
     IF char-val <> "" THEN
        ASSIGN SELF:SCREEN-VALUE = char-val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_group C-Win
ON LEAVE OF end_group IN FRAME FRAME-A /* Ending Group */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* Ending Warehouse */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_lot-reo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_lot-reo C-Win
ON VALUE-CHANGED OF rd_lot-reo IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_pri-ven-max
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_pri-ven-max C-Win
ON VALUE-CHANGED OF rd_pri-ven-max IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_pur-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_pur-man C-Win
ON VALUE-CHANGED OF rd_pur-man IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qav-ven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qav-ven C-Win
ON VALUE-CHANGED OF rd_qav-ven IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qoh C-Win
ON VALUE-CHANGED OF rd_qoh IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_stocked
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_stocked C-Win
ON VALUE-CHANGED OF rd_stocked IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_below
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_below C-Win
ON VALUE-CHANGED OF tb_below IN FRAME FRAME-A /* Print Items Below Reorder Point Only? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  ASSIGN {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_dash
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dash C-Win
ON VALUE-CHANGED OF tb_dash IN FRAME FRAME-A /* Print Dashes Between Each Line? */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&self-name}.

    IF {&self-name} THEN DO:
      ASSIGN
       lv-ornt:SCREEN-VALUE        = "L"
       tb_part:SCREEN-VALUE        = "no"
       rd_qav-ven:SCREEN-VALUE     = "Qty Avail"
       rd_pri-ven-max:SCREEN-VALUE = "Vendor".
     IF int(lv-font-no:SCREEN-VALUE) > 12  THEN
          lv-font-no:SCREEN-VALUE = "12".

      DISABLE tb_part
              rd_qav-ven
              rd_pri-ven-max.
    END.

    ELSE DO:
      ASSIGN
       lv-ornt:SCREEN-VALUE        = "P".
      IF int(lv-font-no:SCREEN-VALUE) > 11  THEN
          lv-font-no:SCREEN-VALUE = "11".

      ENABLE tb_part
             rd_qav-ven
             rd_pri-ven-max.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_history
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_history C-Win
ON VALUE-CHANGED OF tb_history IN FRAME FRAME-A /* Print 6 Month History? */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&self-name}.

    IF {&self-name} THEN DO:
      ASSIGN
       lv-ornt:SCREEN-VALUE        = "L"
       tb_part:SCREEN-VALUE        = "no"
       rd_qav-ven:SCREEN-VALUE     = "Qty Avail"
       rd_pri-ven-max:SCREEN-VALUE = "Vendor".
      IF int(lv-font-no:SCREEN-VALUE) > 12  THEN
          lv-font-no:SCREEN-VALUE = "12".

      DISABLE tb_part
              rd_qav-ven
              rd_pri-ven-max.
    END.

    ELSE DO:
      ASSIGN
       lv-ornt:SCREEN-VALUE        = "P".
      IF int(lv-font-no:SCREEN-VALUE) > 11  THEN
          lv-font-no:SCREEN-VALUE = "11".

      ENABLE tb_part
             rd_qav-ven
             rd_pri-ven-max.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-cust C-Win
ON VALUE-CHANGED OF tb_inc-cust IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-qoh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-qoh C-Win
ON VALUE-CHANGED OF tb_inc-qoh IN FRAME FRAME-A /* Include Quantity On Order with Quantity On Hand? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_part C-Win
ON VALUE-CHANGED OF tb_part IN FRAME FRAME-A /* Print Customer Part#? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reord-by-whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reord-by-whse C-Win
ON VALUE-CHANGED OF tb_reord-by-whse IN FRAME FRAME-A /* ReOrder Level by Whse? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
  ASSIGN {&self-name}.
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

  begin_as-of = TODAY.

  RUN enable_UI.

  DO WITH FRAME {&frame-name}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i} 
  END.

  {methods/nowait.i}
  APPLY "entry" TO begin_cust IN FRAME {&FRAME-NAME}.
  APPLY "value-changed" TO tb_history IN FRAME {&FRAME-NAME}.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IR1',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IR1""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

   IF ou-log AND ou-cust-int = 0 THEN DO:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'IR1',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'IR1').


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
  DISPLAY tb_cust-list begin_cust end_cust begin_i-no end_i-no begin_cat end_cat 
          begin_whse end_whse begin_class end_class begin_group end_group 
          tb_inc-qoh tb_part tb_inc-cust tb_history tb_below tb_dash lbl_qoh1 
          rd_qoh begin_as-of lbl_stocked rd_stocked lbl_pur-man rd_pur-man 
          lbl_lot-reo rd_lot-reo lbl_qav-ven rd_qav-ven lbl_pri-ven-max 
          rd_pri-ven-max lv-ornt lines-per-page rd-dest lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file tb_reord-by-whse 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tb_cust-list btnCustList begin_cust end_cust begin_i-no end_i-no 
         begin_cat end_cat begin_whse end_whse begin_class end_class 
         begin_group end_group tb_inc-qoh tb_part tb_inc-cust tb_history 
         tb_below tb_dash rd_qoh begin_as-of rd_stocked rd_pur-man rd_lot-reo 
         rd_qav-ven rd_pri-ven-max lv-ornt lines-per-page rd-dest lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
         tb_reord-by-whse RECT-6 RECT-7 
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
{custom/out2file.i}  

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
  RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-reord.p 9/91 cd */
/* reorder report                                                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

DEF VAR v-cust      LIKE itemfg.cust-no EXTENT 2 INIT ["","zzzzzzzz"] NO-UNDO.
DEF VAR v-cat       LIKE itemfg.procat EXTENT 2 INIT ["","zzzzzz"] NO-UNDO.
DEF VAR v-item      LIKE itemfg.i-no EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"] NO-UNDO.
DEF VAR v-class     LIKE itemfg.CLASS EXTENT 2 INIT ["","z"] NO-UNDO.
DEF VAR v-group     AS CHAR FORMAT "x(15)" EXTENT 2 INIT  ["","zzzzzzzzzzzzzzz"] NO-UNDO.
DEF VAR v-inconh    AS   LOG FORMAT "Y/N" INIT "Y" NO-UNDO.
DEF VAR v-totrel    AS   LOG FORMAT "Tot All/Release" INIT "Y" NO-UNDO.
DEF VAR v-date      AS   DATE FORMAT "99/99/9999" INIT TODAY NO-UNDO.
DEF VAR v-custown   AS   LOG FORMAT "Y/N" INIT "N" NO-UNDO.
DEF VAR v-prt-all   AS   LOG FORMAT "All/Below" INIT NO NO-UNDO.
DEF VAR v-stocked   AS   CHAR FORMAT "!" INIT "A" NO-UNDO.
DEF VAR v-pur-man   AS   CHAR FORMAT "!" INIT "A" NO-UNDO.
DEF VAR v-lot-reo   AS   CHAR FORMAT "!" INIT "R" NO-UNDO.
DEF VAR v-prt-cpn   AS   LOG FORMAT "Y/N" INIT NO NO-UNDO.
DEF VAR v-prt-qty   AS   LOG FORMAT "Qty/Vendor" INIT NO NO-UNDO.
DEF VAR v-prt-prc   AS   CHAR FORMAT "!" INIT "V" NO-UNDO.

DEF VAR v-qty-onh   AS   INT FORMAT "->>>>>>>>9" NO-UNDO.
DEF VAR v-cust-qty  AS   INT FORMAT "->>>>>>>>9" NO-UNDO.
DEF VAR v-reord-qty AS   INT FORMAT ">>>>>>>>>" NO-UNDO.
DEF VAR v-qty-avail AS   INT NO-UNDO.
DEF VAR v-alloc-qty AS   INT NO-UNDO.
DEF VAR v-stat      AS   CHAR NO-UNDO.

DEF VAR li-hist AS INT FORMAT "->>>>>9" EXTENT 6 NO-UNDO.
DEF VAR ls-hlbl AS CHAR FORMAT "x(48)" NO-UNDO.
DEF VAR ls-hlbl2 AS CHAR FORMAT "x(48)" NO-UNDO.
DEF VAR ld-fr AS DATE NO-UNDO.
DEF VAR ld-to AS DATE NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO. /* 02/05/07 01100718 */

FORMAT HEADER "               "
              "                    "
              "PROD "
              "   "
              "REORDER" 
              "    QTY"
              "    QTY"
              "    QTY"
              "   MINIMUM"
              "     QTY"
              "VENDOR         "
              "  SUGGESTED"
              SPACE(15)
              "LAST 6 MONTHS USAGE"
              SKIP

              "ITEM #         "
              "DESCRIPTION         "
              "CAT  "
              "UOM"
              "  LEVEL"
              "ON HAND"
              "  ALLOC"
              " ON ORD"
              " ORDER QTY"
              "   AVAIL"
              "ITEM NUMBER    "
              "    REORDER"
              SPACE(0)
              ls-hlbl
              SKIP

              "---------------"
              "--------------------"
              "-----"
              "---"
              "-------"
              "-------"
              "-------"
              "-------"
              "----------"
              "-------"
              "--------------"
              "-----------"
              "-------"
              "-------"
              "-------"
              "-------"
              "-------"
              "-------"
              SKIP

    WITH FRAME itemhist-top NO-BOX PAGE-TOP STREAM-IO WIDTH 180.


ASSIGN
 li1   = MONTH(TODAY) + 1
 ld-to = TODAY.

DO li = 1 TO 6:
  li1 = li1 - 1.
  IF li1 LT 1 THEN li1 = li1 + 12.

  ASSIGN
   ld-fr   = DATE(li1,1,YEAR(TODAY) - IF li1 GT MONTH(TODAY) THEN 1 ELSE 0)
   ls-hlbl = ls-hlbl + "   " + STRING(MONTH(ld-fr),"99") + "/" +
                               SUBSTR(STRING(YEAR(ld-fr),"9999"),3,2) 
   ls-hlbl2 = ls-hlbl2 + "   " + STRING(MONTH(ld-fr),"99") + "/" +
                               SUBSTR(STRING(YEAR(ld-fr),"9999"),3,2) + ","  .
END.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 v-cust[1]   = begin_cust
 v-cust[2]   = end_cust
 v-cat[1]    = begin_cat
 v-cat[2]    = end_cat
 v-item[1]   = begin_i-no
 v-item[2]   = end_i-no
 v-class[1]  = begin_class
 v-class[2]  = END_class
 v-group[1]  = begin_group
 v-group[2]  = END_group
 v-inconh    = tb_inc-qoh
 v-totrel    = rd_qoh BEGINS "Tot"
 v-date      = begin_as-of
 v-custown   = tb_inc-cust
 v-prt-all   = NOT tb_below
 v-stocked   = SUBSTR(rd_stocked,1,1)
 v-pur-man   = SUBSTR(rd_pur-man,1,1)
 v-lot-reo   = SUBSTR(rd_lot-reo,1,1)
 v-prt-cpn   = tb_part
 v-prt-qty   = rd_qav-ven BEGINS "Qty"
 v-prt-prc   = SUBSTR(rd_pri-ven-max,1,1)
 lSelected      = tb_cust-list .

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel TO VALUE(fi_file).

excelheader = "".
 IF tb_history THEN 
   excelheader = "ITEM#,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR ITEM#,SUGT REORD," + 
              TRIM(TRIM(ls-hlbl2),",").
 ELSE DO:
     IF v-prt-cpn THEN
         IF v-prt-qty THEN
             IF v-prt-prc EQ "P" THEN 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL,SELL PRC,SUGT REORD".
             ELSE IF v-prt-prc EQ "V" THEN 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR ITEM#,SUGT REORD".
             ELSE
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL," + 
               "MAX ORDER QTY,SUGT REORD".
         ELSE IF v-prt-prc EQ "P" THEN
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR,SELL PRC,SUGGT REORD".
         ELSE IF v-prt-prc EQ "V" THEN 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR,VENDOR ITEM#,SUGT REORD".
         ELSE
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR," + 
               "MAX ORD QTY,SUGT REORD".
         ELSE IF v-prt-qty THEN
           IF v-prt-prc EQ "P" THEN 
 excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
               "QTY ON ORD,MIN ORD QTY,QTY AVAIL,SELL PRC,SUGGT REORD".
           ELSE IF v-prt-prc EQ "V" THEN 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
               "QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR ITEM#,SUGT REORD".
           ELSE
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,QTY AVAIL,MAX ORD QTY,SUGT REORD".
         ELSE IF v-prt-prc EQ "P" THEN 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,VENDOR,SELL PRC,SUGT REORD".
         ELSE IF v-prt-prc EQ "V" THEN 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "MIN ORD QTY,VENDOR,VENDOR ITEM#,SUGT REORD".
         ELSE
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,VENDOR,MAX ORD QTY,SUGT REORD".
 END.
 excelheader = excelheader + ", CUSTOMER #, SALES REP, COST, UOM". /*Premier Mod*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN  v-cust[1] = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN  v-cust[2] = ttCustList.cust-no .
END.

DISPLAY WITH FRAME r-top.

IF tb_history THEN DISPLAY WITH FRAME itemhist-top.

{fgrep/r-fgrord.i}

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-whse C-Win 
PROCEDURE run-report-whse :
/* ------------------------------------------------ fg/rep/fg-reord.p 9/91 cd */
/* reorder report                                                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

DEF VAR v-cust      LIKE itemfg.cust-no EXTENT 2 INIT ["","zzzzzzzz"] NO-UNDO.
DEF VAR v-cat       LIKE itemfg.procat EXTENT 2 INIT ["","zzzzzz"] NO-UNDO.
DEF VAR v-item      LIKE itemfg.i-no EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"] NO-UNDO.
DEF VAR v-class     LIKE itemfg.CLASS EXTENT 2 INIT ["","z"] NO-UNDO.
DEF VAR v-group     AS CHAR FORMAT "x(15)" EXTENT 2 INIT  ["","zzzzzzzzzzzzzzz"] NO-UNDO.
DEF VAR v-inconh    AS   LOG FORMAT "Y/N" INIT "Y" NO-UNDO.
DEF VAR v-totrel    AS   LOG FORMAT "Tot All/Release" INIT "Y" NO-UNDO.
DEF VAR v-date      AS   DATE FORMAT "99/99/9999" INIT TODAY NO-UNDO.
DEF VAR v-custown   AS   LOG FORMAT "Y/N" INIT "N" NO-UNDO.
DEF VAR v-prt-all   AS   LOG FORMAT "All/Below" INIT NO NO-UNDO.
DEF VAR v-stocked   AS   CHAR FORMAT "!" INIT "A" NO-UNDO.
DEF VAR v-pur-man   AS   CHAR FORMAT "!" INIT "A" NO-UNDO.
DEF VAR v-lot-reo   AS   CHAR FORMAT "!" INIT "R" NO-UNDO.
DEF VAR v-prt-cpn   AS   LOG FORMAT "Y/N" INIT NO NO-UNDO.
DEF VAR v-prt-qty   AS   LOG FORMAT "Qty/Vendor" INIT NO NO-UNDO.
DEF VAR v-prt-prc   AS   CHAR FORMAT "!" INIT "V" NO-UNDO.

DEF VAR v-qty-onh   AS   INT FORMAT "->>>>>>>>9" NO-UNDO.
DEF VAR v-cust-qty  AS   INT FORMAT "->>>>>>>>9" NO-UNDO.
DEF VAR v-reord-qty AS   INT FORMAT ">>>>>>>>>" NO-UNDO.
DEF VAR v-qty-avail AS   INT NO-UNDO.
DEF VAR v-alloc-qty AS   INT NO-UNDO.
DEF VAR v-stat      AS   CHAR NO-UNDO.

DEF VAR li-hist AS INT FORMAT "->>>>>9" EXTENT 6 NO-UNDO.
DEF VAR ls-hlbl AS CHAR FORMAT "x(48)" NO-UNDO.
DEF VAR ls-hlbl2 AS CHAR FORMAT "x(48)" NO-UNDO.
DEF VAR ld-fr AS DATE NO-UNDO.
DEF VAR ld-to AS DATE NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO. /* 02/05/07 01100718 */

FORMAT HEADER "               "
              "                    "
              "PROD "
              "   "
              "REORDER" 
              "    QTY"
              "    QTY"
              "    QTY"
              "   MINIMUM"
              "     QTY"
              "VENDOR         "
              "  SUGGESTED"
              SPACE(15)
              "LAST 6 MONTHS USAGE"
              SKIP

              "ITEM #         "
              "DESCRIPTION         "
              "CAT  "
              "UOM"
              "  LEVEL"
              "ON HAND"
              "  ALLOC"
              " ON ORD"
              " ORDER QTY"
              "   AVAIL"
              "ITEM NUMBER    "
              "    REORDER"
              SPACE(0)
              ls-hlbl
              SKIP

              "---------------"
              "--------------------"
              "-----"
              "---"
              "-------"
              "-------"
              "-------"
              "-------"
              "----------"
              "-------"
              "--------------"
              "-----------"
              "-------"
              "-------"
              "-------"
              "-------"
              "-------"
              "-------"
              SKIP

    WITH FRAME itemhist-top NO-BOX PAGE-TOP STREAM-IO WIDTH 180.


ASSIGN
 li1   = MONTH(TODAY) + 1
 ld-to = TODAY.

DO li = 1 TO 6:
  li1 = li1 - 1.
  IF li1 LT 1 THEN li1 = li1 + 12.

  ASSIGN
   ld-fr   = DATE(li1,1,YEAR(TODAY) - IF li1 GT MONTH(TODAY) THEN 1 ELSE 0)
   ls-hlbl = ls-hlbl + "   " + STRING(MONTH(ld-fr),"99") + "/" +
                               SUBSTR(STRING(YEAR(ld-fr),"9999"),3,2) 
   ls-hlbl2 = ls-hlbl2 + "   " + STRING(MONTH(ld-fr),"99") + "/" +
                               SUBSTR(STRING(YEAR(ld-fr),"9999"),3,2) + ","  .
END.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 v-cust[1]   = begin_cust
 v-cust[2]   = end_cust
 v-cat[1]    = begin_cat
 v-cat[2]    = end_cat
 v-item[1]   = begin_i-no
 v-item[2]   = end_i-no
 v-class[1]  = begin_class
 v-class[2]  = END_class
 v-group[1]  = begin_group
 v-group[2]  = END_group
 v-inconh    = tb_inc-qoh
 v-totrel    = rd_qoh BEGINS "Tot"
 v-date      = begin_as-of
 v-custown   = tb_inc-cust
 v-prt-all   = NOT tb_below
 v-stocked   = SUBSTR(rd_stocked,1,1)
 v-pur-man   = SUBSTR(rd_pur-man,1,1)
 v-lot-reo   = SUBSTR(rd_lot-reo,1,1)
 v-prt-cpn   = tb_part
 v-prt-qty   = rd_qav-ven BEGINS "Qty"
 v-prt-prc   = SUBSTR(rd_pri-ven-max,1,1)
 lSelected      = tb_cust-list.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel TO VALUE(fi_file).

excelheader = "".
 IF tb_history THEN 
   excelheader = "ITEM#,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR ITEM#,SUGT REORD," + 
              TRIM(TRIM(ls-hlbl2),",").
 ELSE DO:
     IF v-prt-cpn THEN
         IF v-prt-qty THEN
             IF v-prt-prc EQ "P" THEN 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL,SELL PRC,SUGT REORD".
             ELSE IF v-prt-prc EQ "V" THEN 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR ITEM#,SUGT REORD".
             ELSE
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,QTY AVAIL," + 
               "MAX ORDER QTY,SUGT REORD".
         ELSE IF v-prt-prc EQ "P" THEN
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," + 
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR,SELL PRC,SUGGT REORD".
         ELSE IF v-prt-prc EQ "V" THEN 
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR,VENDOR ITEM#,SUGT REORD".
         ELSE
 excelheader = "ITEM #,CUST PART #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND," +
               "QTY ALLOC,QTY ON ORD,MIN ORD QTY,VENDOR," + 
               "MAX ORD QTY,SUGT REORD".
         ELSE IF v-prt-qty THEN
           IF v-prt-prc EQ "P" THEN 
 excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
               "QTY ON ORD,MIN ORD QTY,QTY AVAIL,SELL PRC,SUGGT REORD".
           ELSE IF v-prt-prc EQ "V" THEN 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
               "QTY ON ORD,MIN ORD QTY,QTY AVAIL,VENDOR ITEM#,SUGT REORD".
           ELSE
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,QTY AVAIL,MAX ORD QTY,SUGT REORD".
         ELSE IF v-prt-prc EQ "P" THEN 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,VENDOR,SELL PRC,SUGT REORD".
         ELSE IF v-prt-prc EQ "V" THEN 
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "MIN ORD QTY,VENDOR,VENDOR ITEM#,SUGT REORD".
         ELSE
excelheader = "ITEM #,DESC,PROD CAT,UOM,REORD LVL,QTY ON HAND,QTY ALLOC," + 
              "QTY ON ORD,MIN ORD QTY,VENDOR,MAX ORD QTY,SUGT REORD".
 END.
 excelheader = excelheader + ", CUSTOMER #, SALES REP, COST, UOM". /*Premier Mod*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN  v-cust[1] = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN  v-cust[2] = ttCustList.cust-no .
END.

DISPLAY WITH FRAME r-top.

IF tb_history THEN DISPLAY WITH FRAME itemhist-top.

{fgrep/r-fgrordw.i}

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust:SENSITIVE = NOT iplChecked
        end_cust:SENSITIVE = NOT iplChecked
        begin_cust:VISIBLE = NOT iplChecked
        end_cust:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

