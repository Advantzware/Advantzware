&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-relprt.w

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
DEF VAR tfile AS CHAR INIT "c:\tmp\rptfile.txt" NO-UNDO.
DEF VAR v-1st-page AS LOG NO-UNDO.
DEF VAR v-multi-cust AS LOG NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR tmp-dir AS CHAR NO-UNDO. 
DEF VAR cRptFile AS CHAR NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{oe/rep/oe-pick1.i new}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEF VAR v-ddate AS DATE NO-UNDO.

DEF VAR lv-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{custom/xprint.i}
DEF NEW SHARED VAR vPrinted AS LOG NO-UNDO.
DEF NEW SHARED VAR v-exc-bin AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-components AS LOG NO-UNDO.
DEF NEW SHARED VAR s-print-part-no AS LOG NO-UNDO.
DEF NEW SHARED VAR s-print-what-item AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-loc-from AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-loc-to AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-bin-from AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-bin-to AS cha NO-UNDO.
DEF NEW SHARED VAR s-print-pricing AS LOG NO-UNDO.
DEF NEW SHARED VAR lv-spec-list AS CHAR NO-UNDO.
DEF NEW SHARED VAR s-print-spec AS LOG NO-UNDO .
DEFINE NEW SHARED VARIABLE lPrintQtyUom AS LOGICAL NO-UNDO .
DEFINE NEW SHARED VARIABLE lSortRelSeq AS LOGICAL NO-UNDO .
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF BUFFER b-oe-relh FOR oe-relh.
{oe/oe-relp1.i NEW}

{sys/ref/relpost.i}

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

/* gdm - 02020902 */
DEF VAR v-hldflg AS LOG NO-UNDO.
DEF VAR v-chkflg AS LOG NO-UNDO.
DEF VAR lActive AS LOG NO-UNDO.
DEFINE VARIABLE  ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.
DO TRANSACTION:
     {sys/ref/CustList.i NEW}
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-9 begin_cust-no ~
end_cust-no tb_excl_cust begin_relnum end_relnum begin_ord-no end_ord-no ~
begin_date end_date begin_del-zone end_del-zone begin_whse end_whse ~
tb_printed tb_print-qty-uom tb_print-spec tb_sort-rel fi_specs tb_posted ~
tb_more rd-print-what tb_p-bin tb_zone-s tb_zone-p tb_print-component ~
begin_loc end_loc begin_loc-bin end_loc-bin tb_whs-bin-sort rd-dest lv-ornt ~
lines-per-page lv-font-no tgMultipleReleases td-show-parm tb_post-rel ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no tb_excl_cust ~
begin_relnum end_relnum begin_ord-no end_ord-no begin_date end_date ~
begin_del-zone end_del-zone begin_whse end_whse tb_printed tb_exl-tg-bin ~
tb_print-qty-uom tb_print-spec tb_sort-rel fi_specs tb_posted tb_more ~
rd-print-what tb_p-bin tb_zone-s tb_zone-p tb_print-component begin_loc ~
end_loc tb_prt-part-no tb_pricing begin_loc-bin end_loc-bin tb_whs-bin-sort ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name tgMultipleReleases ~
td-show-parm tb_post-rel 

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_del-zone AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Delivery Zone" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_relnum AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Release#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Ship From Whse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_del-zone AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Delivery Zone" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc-bin AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_relnum AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Release#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "Ending Ship From Whse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_specs AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd-print-what AS CHARACTER INITIAL "I" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Print All Tags in Item Bin Locations", "I",
"Print Only Tags on Release", "R"
     SIZE 39 BY 2.86 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 15.95.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 6.67.

DEFINE VARIABLE tb_excl_cust AS LOGICAL INITIAL no 
     LABEL "Exclude Specific Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .57 NO-UNDO.

DEFINE VARIABLE tb_exl-tg-bin AS LOGICAL INITIAL no 
     LABEL "Exclude Tags and Bins?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_more AS LOGICAL INITIAL no 
     LABEL "Print Multiple Releases Per Form?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_p-bin AS LOGICAL INITIAL yes 
     LABEL "Print Bin Locations?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_post-rel AS LOGICAL INITIAL no 
     LABEL "Post Release?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted Release?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_pricing AS LOGICAL INITIAL no 
     LABEL "Print Pricing?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-component AS LOGICAL INITIAL no 
     LABEL "Print Assembled Components?" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-qty-uom AS LOGICAL INITIAL no 
     LABEL "Print Order Qty/UOM?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-spec AS LOGICAL INITIAL no 
     LABEL "Print Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_printed AS LOGICAL INITIAL no 
     LABEL "Reprint Release Tickets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-part-no AS LOGICAL INITIAL no 
     LABEL "Print Customer Part#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sort-rel AS LOGICAL INITIAL no 
     LABEL "Sort Rel Seq?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_whs-bin-sort AS LOGICAL INITIAL no 
     LABEL "Sort Bin Locations" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zone-p AS LOGICAL INITIAL yes 
     LABEL "Print Delivery Zone?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zone-s AS LOGICAL INITIAL no 
     LABEL "Sort By Delivery Zone?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tgMultipleReleases AS LOGICAL INITIAL yes 
     LABEL "Multiple Releases" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 1.76 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 1.76 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_excl_cust AT ROW 2.86 COL 29 WIDGET-ID 8
     begin_relnum AT ROW 3.57 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Release Number"
     end_relnum AT ROW 3.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending Release Number"
     begin_ord-no AT ROW 4.52 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 4.52 COL 70 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_date AT ROW 5.48 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.48 COL 70 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_del-zone AT ROW 6.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Delivery Zone"
     end_del-zone AT ROW 6.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Delivery zone"
     begin_whse AT ROW 7.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Warehouse" WIDGET-ID 4
     end_whse AT ROW 7.38 COL 70 COLON-ALIGNED HELP
          "Enter Ending Warehouse Number" WIDGET-ID 6
     tb_printed AT ROW 8.33 COL 6
     tb_exl-tg-bin AT ROW 8.33 COL 53
     tb_print-qty-uom AT ROW 8.50 COL 48.8 WIDGET-ID 16
     tb_print-spec AT ROW 8.86 COL 46.6 WIDGET-ID 10
     tb_sort-rel AT ROW 8.86 COL 52.8 WIDGET-ID 14
     fi_specs AT ROW 8.86 COL 70 COLON-ALIGNED HELP
          "Enter Spec Code separated by commas" NO-LABEL WIDGET-ID 12
     tb_posted AT ROW 9.19 COL 35 RIGHT-ALIGNED
     tb_more AT ROW 10.05 COL 6
     rd-print-what AT ROW 10.38 COL 53 NO-LABEL
     tb_p-bin AT ROW 10.91 COL 6
     tb_zone-s AT ROW 11.76 COL 6
     tb_zone-p AT ROW 12.62 COL 6
     tb_print-component AT ROW 13.48 COL 6
     begin_loc AT ROW 14.19 COL 60 COLON-ALIGNED
     end_loc AT ROW 14.19 COL 78 COLON-ALIGNED NO-LABEL
     tb_prt-part-no AT ROW 14.33 COL 6
     tb_pricing AT ROW 15.19 COL 6
     begin_loc-bin AT ROW 15.38 COL 60 COLON-ALIGNED
     end_loc-bin AT ROW 15.38 COL 78 COLON-ALIGNED NO-LABEL
     tb_whs-bin-sort AT ROW 16.05 COL 6
     rd-dest AT ROW 17.91 COL 5 NO-LABEL
     lv-ornt AT ROW 18.38 COL 30 NO-LABEL
     lines-per-page AT ROW 18.38 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 19.81 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 20.76 COL 27 COLON-ALIGNED NO-LABEL
     tgMultipleReleases AT ROW 22.43 COL 64 HELP
          "parm" WIDGET-ID 2
     td-show-parm AT ROW 23.14 COL 30
     tb_post-rel AT ROW 23.14 COL 64
     btn-ok AT ROW 25.29 COL 21
     btn-cancel AT ROW 25.29 COL 59
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.33 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 4.2
          BGCOLOR 2 
     "From" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 13.48 COL 67
     "To" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 13.48 COL 85
     RECT-6 AT ROW 17.19 COL 1
     RECT-7 AT ROW 1.19 COL 1
     RECT-9 AT ROW 10.14 COL 48
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 97.4 BY 25.71.


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
         TITLE              = "Release Ticket"
         HEIGHT             = 25.95
         WIDTH              = 98.4
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_del-zone:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_relnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_del-zone:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_relnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_specs:HIDDEN IN FRAME FRAME-A           = TRUE
       fi_specs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd-print-what:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excl_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_exl-tg-bin IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_exl-tg-bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_more:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_p-bin:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_post-rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_posted IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_pricing IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_print-component:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-qty-uom:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_print-qty-uom:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-spec:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_print-spec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_printed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-part-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_sort-rel:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_sort-rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zone-p:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zone-s:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Release Ticket */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Release Ticket */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

   RUN windows/l-cust2.w (INPUT cocode, INPUT begin_cust-no:SCREEN-VALUE,"", OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN begin_cust-no:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  IF DATE(end_date:SCREEN-VALUE) EQ end_date AND v-ddate EQ ? THEN
    end_date:SCREEN-VALUE = begin_date:SCREEN-VALUE.

  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_del-zone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_del-zone C-Win
ON LEAVE OF begin_del-zone IN FRAME FRAME-A /* Beginning Delivery Zone */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_relnum C-Win
ON VALUE-CHANGED OF begin_relnum IN FRAME FRAME-A /* Beginning Release# */
DO:
  RUN new-relnum.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* Beginning Ship From Whse */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:

  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR v-num-custs AS INT NO-UNDO.
  ASSIGN v-1st-page = YES
         v-multi-cust = NO
         lExclCust    = tb_excl_cust.

    IF tmp-dir = "" THEN
    DO:
        FIND FIRST users WHERE
            users.user_id EQ USERID("NOSWEAT")
            NO-LOCK NO-ERROR.

        IF AVAIL users AND users.user_program[2] NE "" THEN
            tmp-dir = users.user_program[2].
        ELSE
            tmp-dir = "c:\tmp".
    END. 

    /* Used if multiple customers in range */
    cRptFile = "RPT" + STRING(TIME).

  /* Will later merge these files into listfile for printing */          
  tfile = tmp-dir + "\" + cRptFile + ".txt".

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN
    ASSIGN END_relnum = begin_relnum.

  IF begin_date:SCREEN-VALUE EQ "" OR end_date:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Release date may not be left blank..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_date.
      RETURN NO-APPLY.
  END.
  /* gdm - 02020902 - CHECK FOR RELEASE HOLD*/        
  IF v-hldflg THEN
    DO TRANSACTION:
      IF (begin_relnum:SCREEN-VALUE EQ end_relnum:SCREEN-VALUE) THEN DO:

        FIND FIRST oe-relh NO-LOCK
            WHERE oe-relh.company  EQ cocode
              AND oe-relh.release# EQ INT(begin_relnum:SCREEN-VALUE) NO-ERROR.
        IF AVAIL oe-relh THEN DO:

            RUN check-4-hold (OUTPUT v-chkflg).                       

            IF v-chkflg THEN DO:

                MESSAGE 
                    "Release # " begin_relnum:SCREEN-VALUE " is on CREDIT HOLD." 
                    "Please contact your supervisor." 
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN.
            END.
        END.
      END.
      ELSE 
        IF NOT tb_posted THEN /* gdm - 03200904 */
          MESSAGE 
             "Please be advised,all releases that are on credit hold will not be printed."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

      ASSIGN v-chkflg = NO.
  END.

  v-print-components = tb_print-component.
  s-print-part-no = tb_prt-part-no.
  v-num-custs = 0.
  FOR EACH b-oe-relh FIELDS(company cust-no ship-id r-no release#) WHERE
               b-oe-relh.company EQ cocode AND
               b-oe-relh.release# GE begin_relnum AND
               b-oe-relh.release# LE end_relnum AND
               b-oe-relh.cust-no GE begin_cust-no AND
               b-oe-relh.cust-no LE end_cust-no AND
               b-oe-relh.rel-date GE begin_date AND
               b-oe-relh.rel-date LE end_date AND
               b-oe-relh.posted   EQ tb_posted  AND    /* 09131307*/
               b-oe-relh.printed  EQ tb_printed AND
               b-oe-relh.stat     NE "W" 
               AND CAN-FIND(FIRST oe-rell
                       WHERE oe-rell.company EQ cocode
                         AND oe-rell.r-no    EQ b-oe-relh.r-no
                         AND oe-rell.ord-no  GE begin_ord-no
                         AND oe-rell.ord-no  LE end_ord-no
                         AND oe-rell.loc     GE begin_whse
                         AND oe-rell.loc     LE END_whse
                       USE-INDEX r-no) NO-LOCK  USE-INDEX post
                BREAK BY b-oe-relh.company
                      BY b-oe-relh.cust-no:
               IF lExclCust AND LOOKUP(b-oe-relh.cust-no,cCustList) GT 0 THEN  
                    NEXT.
               IF FIRST-OF(b-oe-relh.cust-no) THEN
                   v-num-custs = v-num-custs + 1.
  END.

  IF v-num-custs > 1 THEN DO:
      v-multi-cust = YES.
      DOS SILENT DEL VALUE(tmp-dir + "\" + cRptFile + ".txt*").
  END.
  FOR EACH b-oe-relh FIELDS(company cust-no ship-id r-no release#) WHERE
               b-oe-relh.company EQ cocode AND
               b-oe-relh.release# GE begin_relnum AND
               b-oe-relh.release# LE end_relnum AND
               b-oe-relh.cust-no GE begin_cust-no AND
               b-oe-relh.cust-no LE end_cust-no AND
               b-oe-relh.rel-date GE begin_date AND
               b-oe-relh.rel-date LE end_date AND
               b-oe-relh.posted   EQ tb_posted AND   /* 09131307*/
               b-oe-relh.printed  EQ tb_printed AND
               b-oe-relh.stat     NE "W" 
               AND CAN-FIND(FIRST oe-rell
                       WHERE oe-rell.company EQ cocode
                         AND oe-rell.r-no    EQ b-oe-relh.r-no
                         AND oe-rell.ord-no  GE begin_ord-no
                         AND oe-rell.ord-no  LE end_ord-no
                         AND oe-rell.loc     GE begin_whse
                         AND oe-rell.loc     LE END_whse
                       USE-INDEX r-no) NO-LOCK  USE-INDEX post
                BREAK BY b-oe-relh.company
                      BY b-oe-relh.cust-no:
               IF lExclCust AND LOOKUP(b-oe-relh.cust-no,cCustList) GT 0 THEN  
                    NEXT.
               IF FIRST-OF(b-oe-relh.cust-no) THEN
               DO:
                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "RELPRINT" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-oe-relh.cust-no AND
                       sys-ctrl-shipto.ship-id = b-oe-relh.ship-id AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF NOT AVAIL sys-ctrl-shipto THEN
                     FIND FIRST sys-ctrl-shipto WHERE
                          sys-ctrl-shipto.company = cocode AND
                          sys-ctrl-shipto.NAME = "RELPRINT" AND
                          sys-ctrl-shipto.cust-vend = YES AND
                          sys-ctrl-shipto.cust-vend-no = b-oe-relh.cust-no AND
                          sys-ctrl-shipto.char-fld > ''
                          NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     v-relprint = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE DO:
                      v-relprint = vcDefaultForm .
                  END.

                  RUN set-report.
                  RUN run-report.
                  IF v-multi-cust THEN DO:

                    tfile = tfile + "1".
                    PAUSE 1.
                    OUTPUT CLOSE.
                    DOS SILENT COPY VALUE(list-name) VALUE(tfile).

                  END.
          END.

 END.
 IF v-multi-cust THEN DO:
   PAGE.
   OUTPUT CLOSE.
   DOS SILENT COPY VALUE(tmp-dir + "\" + cRptFile + ".txt*") VALUE(list-name).
   DOS SILENT DEL VALUE(tmp-dir + "\" + cRptFile + ".txt*").
 END.

 CASE rd-dest:
  WHEN 1 THEN RUN output-to-printer.
  WHEN 2 THEN RUN output-to-screen.
  WHEN 3 THEN RUN output-to-file.
  WHEN 4 THEN DO:
    /*run output-to-fax.*/
    {custom/asifax.i &type="Customer"
        &begin_cust=begin_cust-no
        &END_cust= end_cust-no
        &fax-subject=c-win:title
        &fax-body=c-win:title
        &fax-file=list-name }
  END. 
  WHEN 5 THEN DO:
    IF is-xprint-form THEN DO:
       RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
       {custom/asimail2.i &TYPE = "Customer"
           &group-title='r-relprt.' /* v-prgmname */
           &begin_cust=begin_cust-no
           &END_cust=end_cust-no
           &mail-subject=c-win:title
           &mail-body=c-win:title
           &mail-file=list-name }
    END.
    ELSE DO:
       {custom/asimailr2.i &TYPE = "Customer"
           &group-title='r-relprt.' /* v-prgmname */
           &begin_cust=begin_cust-no
           &END_cust=end_cust-no
           &mail-subject=c-win:title
           &mail-body=c-win:title
           &mail-file=list-name }
    END.
  END. 
  WHEN 6 THEN RUN output-to-port.
END CASE.

  ll = tb_post-rel AND NOT tb_posted.

  IF ll THEN DO:
    FOR EACH tt-except:
      DELETE tt-except.
    END.

    FOR EACH tt-fg-bin:
      DELETE tt-fg-bin.
    END.

    {oe/rep/foreachr.i}:
      RUN oe/relcheck.p (ROWID(oe-relh), OUTPUT ll).
      ll = NOT ll.
      IF NOT ll THEN DO:
        FIND FIRST tt-except NO-ERROR.
        MESSAGE "Release Ticket " +
                TRIM(IF oe-relh.deleted THEN "has been DELETED" ELSE
                     IF tt-except.reason EQ 1 THEN "is in use"  ELSE
                                                "has insufficient inventory") +
                " and cannot be posted..."
            VIEW-AS ALERT-BOX ERROR.
        LEAVE.
      END.
    END.
  END.

  IF ll THEN DO:
    ll = NO.

    MESSAGE "Post Releases?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.
  END.

  IF ll THEN DO:
    RUN post-releases.

    MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

   RUN windows/l-cust2.w (INPUT cocode, INPUT end_cust-no:SCREEN-VALUE,"", OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN end_cust-no:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF end_date:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Ending Date may not be left blank..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO {&self-name}.
      RETURN NO-APPLY.
    END.

    ASSIGN {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_del-zone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_del-zone C-Win
ON LEAVE OF end_del-zone IN FRAME FRAME-A /* Ending Delivery Zone */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_relnum C-Win
ON VALUE-CHANGED OF end_relnum IN FRAME FRAME-A /* Ending Release# */
DO:
  RUN new-relnum.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* Ending Ship From Whse */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_specs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_specs C-Win
ON LEAVE OF fi_specs IN FRAME FRAME-A
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


&Scoped-define SELF-NAME rd-print-what
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-print-what C-Win
ON VALUE-CHANGED OF rd-print-what IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
  IF LOOKUP(rd-print-what,"I,S") > 0 THEN
     ASSIGN begin_loc:SENSITIVE = YES
            END_loc:SENSITIVE = YES
            begin_loc-bin:SENSITIVE = YES
            END_loc-bin:SENSITIVE = YES
            tb_p-bin:SCREEN-VALUE = "Yes".
  ELSE
     ASSIGN begin_loc:SENSITIVE = NO 
            END_loc:SENSITIVE = NO 
            begin_loc-bin:SENSITIVE = NO 
            END_loc-bin:SENSITIVE = NO
            tb_p-bin:SCREEN-VALUE = "No".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excl_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excl_cust C-Win
ON VALUE-CHANGED OF tb_excl_cust IN FRAME FRAME-A /* Exclude Specific Customers */
DO:
  ASSIGN {&self-name}.
  IF tb_excl_cust THEN do:
       IF custcount NE "" AND ou-log THEN
           cCustList =  custcount .

      RUN windows/d-custlist.w(INPUT cocode, INPUT-OUTPUT cCustList).
  END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_exl-tg-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_exl-tg-bin C-Win
ON VALUE-CHANGED OF tb_exl-tg-bin IN FRAME FRAME-A /* Exclude Tags and Bins? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_more
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_more C-Win
ON VALUE-CHANGED OF tb_more IN FRAME FRAME-A /* Print Multiple Releases Per Form? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_p-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_p-bin C-Win
ON VALUE-CHANGED OF tb_p-bin IN FRAME FRAME-A /* Print Bin Locations? */
DO:
  ASSIGN {&self-name}.
  IF LOOKUP(v-relprint,"HOPX,ACPI,Fibrex,Accord,Carded,Loylang,PremierX,Relprint 10,Lakeside,Distributor,Frank,CSC-GA,Protagon,CardedX,Peachtree,Multicell,CCC,Soule,StClair,Midwest") > 0 THEN DO:
     IF tb_p-bin THEN
     DO:
        IF v-relprint = "PremierX" OR v-relprint = "Relprint 10" OR v-relprint = "Lakeside" OR v-relprint = "Distributor" OR v-relprint = "Frank" OR v-relprint = "NSTOCK" OR v-relprint = "Axis"
            OR v-relprint = "Protagon" OR v-relprint = "Soule" 
                OR v-relprint = "NStock"  /*OR v-relprint = "Prystup"*/ OR v-relprint = "StClair" OR v-relprint = "Midwest" THEN
           rd-print-what:SCREEN-VALUE = "S".
        ELSE
           rd-print-what:SCREEN-VALUE = "I".
     END.
     ELSE
        rd-print-what:SCREEN-VALUE = "R".

     APPLY "value-changed" TO rd-print-what.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_post-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_post-rel C-Win
ON VALUE-CHANGED OF tb_post-rel IN FRAME FRAME-A /* Post Release? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted Release? */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF tb_posted:SCREEN-VALUE NE ""                         AND
       NOT CAN-FIND(FIRST oe-relh
                    WHERE oe-relh.company  EQ cocode
                      AND oe-relh.posted   EQ YES
                      AND oe-relh.release# EQ begin_relnum) THEN DO:
      MESSAGE "Posted Release does not exist, please re-enter..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_relnum.
      RETURN NO-APPLY.
    END.
    ASSIGN {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-qty-uom C-Win
ON VALUE-CHANGED OF tb_print-qty-uom IN FRAME FRAME-A /* Print Order Qty/UOM? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-spec C-Win
ON VALUE-CHANGED OF tb_print-spec IN FRAME FRAME-A /* Print Spec Notes? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printed C-Win
ON VALUE-CHANGED OF tb_printed IN FRAME FRAME-A /* Reprint Release Tickets? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort-rel C-Win
ON VALUE-CHANGED OF tb_sort-rel IN FRAME FRAME-A /* Sort Rel Seq? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zone-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zone-p C-Win
ON VALUE-CHANGED OF tb_zone-p IN FRAME FRAME-A /* Print Delivery Zone? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zone-s
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zone-s C-Win
ON VALUE-CHANGED OF tb_zone-s IN FRAME FRAME-A /* Sort By Delivery Zone? */
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


&Scoped-define SELF-NAME tgMultipleReleases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgMultipleReleases C-Win
ON VALUE-CHANGED OF tgMultipleReleases IN FRAME FRAME-A /* Multiple Releases */
DO:
   ASSIGN {&SELF}.
  IF tgMultipleReleases:SCREEN-VALUE EQ "YES" THEN DO:
      ASSIGN END_relnum:VISIBLE = TRUE begin_relnum:LABEL = "Beginning Release#".
      ENABLE END_relnum.
      END_relnum:SENSITIVE = YES.
  END.
  ELSE DO:
      ASSIGN END_relnum:VISIBLE = FALSE begin_relnum:LABEL = "Release#".
      DISABLE END_relnum.
      END_relnum:SENSITIVE = NO.
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN sys/inc/custlistform.p (INPUT "OT1" , INPUT cocode , OUTPUT ou-log , OUTPUT ou-cust-int) .
  custcount = "".
  
  RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OT1',
                            INPUT YES,
                            OUTPUT lActive).
 {sys/inc/chblankcust.i ""OT1""}
  
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "RELPACK"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "RELPACK"
     sys-ctrl.descrip  = "The heading to be printed for releases/packing slips"
     sys-ctrl.char-fld = "RELEASE".
    MESSAGE "System control record NOT found - enter heading :"
    UPDATE sys-ctrl.char-fld FORMAT "x(14)".
  END.
  rel-pack = sys-ctrl.char-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "RELPRINT"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "RELPRINT"
     sys-ctrl.date-fld = 12/31/99
     sys-ctrl.descrip  = "Print Release headers on Release/Packing List Form?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  END.

  ASSIGN
   v-relprint    = sys-ctrl.char-fld
   vcDefaultForm = sys-ctrl.char-fld
   tb_more    = v-relprint EQ "Century"
   v-ddate    = sys-ctrl.date-fld
   v-headers  = sys-ctrl.log-fld
   tb_zone-p  = LOOKUP(v-relprint,"Argrov,Fibre") GT 0
   tb_zone-s  = v-relprint EQ "Argrov"
   begin_date = TODAY
   end_date   = IF v-ddate EQ ? THEN begin_date ELSE v-ddate
   v-more = tb_more.                        

  DO TRANSACTION:
     {sys/inc/xmlorder.i} 
  END.

  RUN enable_UI.

  IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN DO:
      ASSIGN END_relnum:VISIBLE = FALSE begin_relnum:LABEL = "Release#".
      DISABLE END_relnum.
      END_relnum:SENSITIVE = NO.
  END.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:

    IF v-relprint EQ "PremierX" OR v-relprint EQ "NSTOCK" 
        OR v-relprint EQ "Lakeside"
        OR v-relprint EQ "Distributor"
        OR v-relprint EQ "Axis"
        OR v-relprint EQ "Frank" 
        OR v-relprint EQ "Protagon" 
        OR v-relprint EQ "Soule" /*OR v-relprint EQ "NStock"*/ 
        /*OR v-relprint EQ "Prystup" */
        OR v-relprint EQ "StClair"
        OR v-relprint EQ "Midwest"
        OR v-relprint EQ "Relprint 10" THEN
       rd-print-what:ADD-LAST("Summary of Bins On Hand","S").

    {custom/usrprint.i}

      IF tb_excl_cust:SCREEN-VALUE EQ "Yes" THEN do:
       IF custcount NE "" AND ou-log THEN
           cCustList =  custcount .
      END.

     IF v-relprint EQ "Prystup" OR v-relprint EQ "NStock" THEN
         ASSIGN  rd-print-what:SCREEN-VALUE = "R"
            rd-print-what = "R".

    IF tgMultipleReleases:SCREEN-VALUE NE "YES" THEN DO:
        ASSIGN END_relnum:VISIBLE = FALSE begin_relnum:LABEL = "Release#".
        DISABLE END_relnum.
        END_relnum:SENSITIVE = NO.
    END.

    /* gdm - 02020902 - CHECK FOR RELEASE HOLD*/        
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
    IF AVAIL sys-ctrl THEN  ASSIGN v-hldflg = sys-ctrl.log-fld.
    /* gdm - 02020902 - CHECK FOR RELEASE HOLD end */        

    APPLY "entry" TO begin_cust-no.

    ASSIGN
     lines-per-page:SCREEN-VALUE = STRING(v-lines-per-page)
     tb_posted:SCREEN-VALUE = "NO"   .

   /* gdm - 09090908*/
/*    IF LOOKUP(v-relprint,"Carded,PremierX,Frank,NStock,CSC-GA,Protagon,Soule,Prystup,StClair") EQ 0 THEN
       rd-print-what:SCREEN-VALUE  = rd-print-what.   */    /* Task# 03041404 */

    IF v-relprint EQ "Xprint" OR v-relprint EQ "relprint 1" THEN
       tb_prt-part-no:SENSITIVE = YES.

    lines-per-page:SENSITIVE = NO.

    IF v-relprint EQ "Hughes" THEN
       tb_exl-tg-bin:HIDDEN = NO.
    ELSE
       tb_exl-tg-bin:HIDDEN = YES.

     IF v-relprint NE "PremierX" THEN
         tb_sort-rel:HIDDEN = YES .
     ELSE tb_sort-rel:HIDDEN = NO .

    IF v-relprint NE "Argrov" THEN
      ASSIGN
       tb_zone-s:SENSITIVE = NO
       tb_zone-p:SENSITIVE = NO.
    IF v-relprint = "NStock" OR v-relprint = "Axis" THEN
        ASSIGN
        tb_p-bin:SENSITIVE = NO .

    IF v-relprint = "Axis" THEN
      ASSIGN 
        tb_print-spec:HIDDEN = NO
        fi_specs:HIDDEN = NO.
    ELSE
      ASSIGN 
        tb_print-spec:HIDDEN = YES
        fi_specs:HIDDEN = YES.


    IF v-relprint EQ "Sonoco" 
       /* gdm - 10080912 */
       OR v-relprint EQ "Rosmar" 
        THEN
       tb_whs-bin-sort:SENSITIVE = YES.
    ELSE
       ASSIGN
          tb_whs-bin-sort:SENSITIVE = NO
          tb_whs-bin-sort:SCREEN-VALUE = "NO".

    IF NOT PROGRAM-NAME(1) BEGINS "listobjs/oe-relh_." THEN
      ASSIGN
       tb_post-rel:SCREEN-VALUE = "no"
       tb_post-rel:HIDDEN       = YES.

    IF v-relprint EQ "StClair" THEN
        ASSIGN 
            rd-print-what:SENSITIVE = NO
            rd-print-what:SCREEN-VALUE = "I"
            rd-print-what = "I" 
            tb_p-bin:SCREEN-VALUE = "Yes"
            tb_p-bin:SENSITIVE = NO 
            tb_p-bin = YES.

    IF v-relprint EQ "Midwest" THEN
        ASSIGN 
            rd-print-what:SENSITIVE = NO
            rd-print-what:SCREEN-VALUE = "I"
            rd-print-what = "I" 
            tb_p-bin:SCREEN-VALUE = "Yes"
            tb_p-bin:SENSITIVE = NO 
            tb_p-bin = YES.

    IF LOOKUP(v-relprint,"Carded") > 0 THEN
       ASSIGN rd-print-what:sensitive = YES
              begin_loc:SENSITIVE = IF rd-print-what:SCREEN-VALUE = "I" THEN YES ELSE NO
              END_loc:SENSITIVE = begin_loc:SENSITIVE
              begin_loc-bin:SENSITIVE = begin_loc:SENSITIVE
              END_loc-bin:SENSITIVE = begin_loc:SENSITIVE.
    ELSE
    IF LOOKUP(v-relprint,"HOPX,ACPI,Fibrex,Accord,Loylang,PremierX,Relprint 10,Lakeside,Distributor,Frank,Axis,CSC-GA,Protagon,CardedX,Peachtree,Multicell,CCC,Soule,StClair,Midwest") > 0 THEN   /* NSTOCK,*/
       ASSIGN rd-print-what:sensitive = YES
              begin_loc:SENSITIVE = IF LOOKUP(rd-print-what:SCREEN-VALUE,"I,S") > 0 THEN YES ELSE NO
              END_loc:SENSITIVE = begin_loc:SENSITIVE
              begin_loc-bin:SENSITIVE = begin_loc:SENSITIVE
              END_loc-bin:SENSITIVE = begin_loc:SENSITIVE.
    ELSE 
       ASSIGN rd-print-what:SENSITIVE = NO
              begin_loc:SENSITIVE = NO
              END_loc:SENSITIVE = NO
              begin_loc-bin:SENSITIVE = NO
              END_loc-bin:SENSITIVE = NO.

    tb_print-component:LABEL = IF v-relprint EQ "Fibrex" OR 
                                  v-relprint EQ "Accord" OR
                                  v-relprint EQ "Metro" OR
                                  v-relprint EQ "CentBox" OR
                                  v-relprint EQ "Loylang" OR
                                  v-relprint EQ "CCC" 
                                 THEN "Print components of unassembled sets?"
                                 ELSE "Print Assembled Components?"  .
    IF v-relprint EQ "Metro" THEN
        ASSIGN 
            tb_p-bin:SENSITIVE = NO
            rd-print-what:SCREEN-VALUE = "R"
            rd-print-what = "R".

    IF v-relprint EQ "StClair" THEN
        ASSIGN 
            rd-print-what:SENSITIVE = NO .
    IF v-relprint EQ "Midwest" THEN
        ASSIGN 
            rd-print-what:SENSITIVE = NO .

    IF v-relprint EQ "Indiana" THEN
       tb_pricing:SENSITIVE = YES.

    IF v-relprint = "Relprint 10" THEN
      ASSIGN 
        tb_print-qty-uom:HIDDEN = NO
        tb_print-qty-uom:SCREEN-VALUE = "YES".
    ELSE
      ASSIGN 
        tb_print-qty-uom:HIDDEN = YES.
    

    RUN new-relnum. 

   IF v-ddate NE ?  THEN DO:
    ASSIGN begin_date:SCREEN-VALUE =  STRING("01/01/" + substring(STRING(v-ddate),7,4)) 
        end_date:SCREEN-VALUE = STRING(v-ddate) .       /*task# 05291409*/

   END.


  END.    

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-4-hold C-Win 
PROCEDURE check-4-hold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM op-flg AS LOG NO-UNDO.

/* gdm - 03200904 */
IF tb_posted THEN DO:

    ASSIGN op-flg = NO.

    RETURN.
END.

/* gdm - 02020902 */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld AND oe-relh.w-ord THEN
   op-flg = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createXMLRelease C-Win 
PROCEDURE createXMLRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cXMLFieldList AS cha NO-UNDO.
DEF VAR cXMLFieldLabel AS cha NO-UNDO.
DEF VAR cXMLOutput AS cha NO-UNDO.
DEF VAR cFldName AS cha NO-UNDO.

IF xmlorder-chr = "" THEN xmlorder-chr = "c:\tmp\".
IF SUBSTRING(xmlorder-chr,LENGTH(xmlorder-chr),1) <> "/" AND
     substring(xmlorder-chr,LENGTH(xmlorder-chr),1) <> "\" THEN
     xmlorder-chr = xmlorder-chr + "\".

ASSIGN
cXMLOutput = xmlorder-chr + "XMLOrderRelease" + string(YEAR(TODAY)) + string(MONTH(TODAY)) +
                    string(DAY(TODAY)) + string(TIME) + ".xml"

cXMLFieldList = "ord-no,po-no,i-no,part-no,qty"
cXMLFieldLabel = "Order_No,Customer_PO,Item_No,Part_No,Actual_Release_Qty".

DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE hRow AS HANDLE NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE hText AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.

DEFINE VARIABLE hDBFld AS HANDLE NO-UNDO.
DEFINE VARIABLE icnt AS INTEGER NO-UNDO.

CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hRow.
CREATE X-NODEREF hField.
CREATE X-NODEREF hText.

/*set up a root node*/
hDoc:CREATE-NODE(hRoot,"OrderRelease","ELEMENT").
hDoc:APPEND-CHILD(hRoot).

 {oe/rep/foreachr.i},

        FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ oe-relh.cust-no
        NO-LOCK

        BREAK BY oe-relh.release#:

  hDoc:CREATE-NODE(hRow,"Order","ELEMENT"). /*create a row node*/
  hRoot:APPEND-CHILD(hRow). /*put the row in the tree*/
  hRow:SET-ATTRIBUTE("Release_Number",STRING(oe-relh.release#)).

  RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).


  /*create a tag Customer# */ 
   hDoc:CREATE-NODE(hField, "Customer_Number", "ELEMENT") NO-ERROR.   
   /*put the new field as next child of row*/
   hRow:APPEND-CHILD(hField).
   /*add a node to hold field value*/
   hDoc:CREATE-NODE(hText, "", "TEXT"). 
    /*attach the text to the field*/
    hField:APPEND-CHILD(hText).
    hText:NODE-VALUE = STRING(oe-relh.cust-no) NO-ERROR.


  /*create a tag Ship To */ 
   hDoc:CREATE-NODE(hField, "Ship_To", "ELEMENT").
   /*put the new field as next child of row*/
   hRow:APPEND-CHILD(hField).
   /*add a node to hold field value*/
   hDoc:CREATE-NODE(hText, "", "TEXT"). 
    /*attach the text to the field*/
    hField:APPEND-CHILD(hText).
    hText:NODE-VALUE = STRING(shipto.ship-name).

  hBuf = BUFFER oe-rell:HANDLE.

  FOR EACH oe-rell
      WHERE oe-rell.company EQ cocode
        AND oe-rell.r-no    EQ oe-relh.r-no
      NO-LOCK
      USE-INDEX r-no,
      FIRST oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ oe-rell.ord-no
        AND oe-ordl.i-no    EQ oe-rell.i-no
        AND oe-ordl.line    EQ oe-rell.line
      NO-LOCK:

  /*create a tag with the field name*/ 
    hDoc:CREATE-NODE(hField, "Order_NO", "ELEMENT").
    /*put the new field as next child of row*/
    hRow:APPEND-CHILD(hField).
    /*add a node to hold field value*/
    hDoc:CREATE-NODE(hText, "", "TEXT"). 
     /*attach the text to the field*/
     hField:APPEND-CHILD(hText).
     hText:NODE-VALUE = STRING(oe-rell.ord-no).

     /*create a tag with the field name*/ 
    hDoc:CREATE-NODE(hField, "Item_NO", "ELEMENT").
    /*put the new field as next child of row*/
    hRow:APPEND-CHILD(hField).
    /*add a node to hold field value*/
    hDoc:CREATE-NODE(hText, "", "TEXT"). 
     /*attach the text to the field*/
     hField:APPEND-CHILD(hText).
     hText:NODE-VALUE = STRING(oe-rell.i-no).

   /*create a tag with the field name*/ 
    hDoc:CREATE-NODE(hField, "Part_NO", "ELEMENT").
    /*put the new field as next child of row*/
    hRow:APPEND-CHILD(hField).
    /*add a node to hold field value*/
    hDoc:CREATE-NODE(hText, "", "TEXT"). 
     /*attach the text to the field*/
     hField:APPEND-CHILD(hText).
     hText:NODE-VALUE = STRING(oe-ordl.part-no).

   /*create a tag with the field name*/ 
    hDoc:CREATE-NODE(hField, "Customer_PO_NO", "ELEMENT").
    /*put the new field as next child of row*/
    hRow:APPEND-CHILD(hField).
    /*add a node to hold field value*/
    hDoc:CREATE-NODE(hText, "", "TEXT"). 
     /*attach the text to the field*/
     hField:APPEND-CHILD(hText).
     hText:NODE-VALUE = STRING(oe-rell.po-no).

   /*create a tag with the field name*/ 
    hDoc:CREATE-NODE(hField, "Actual_Release_Qty", "ELEMENT").
    /*put the new field as next child of row*/
    hRow:APPEND-CHILD(hField).
    /*add a node to hold field value*/
    hDoc:CREATE-NODE(hText, "", "TEXT"). 
     /*attach the text to the field*/
     hField:APPEND-CHILD(hText).
     hText:NODE-VALUE = STRING(oe-rell.qty).


   /*create a tag with the field name*/ 
    hDoc:CREATE-NODE(hField, "Order_Qty", "ELEMENT").
    /*put the new field as next child of row*/
    hRow:APPEND-CHILD(hField).
    /*add a node to hold field value*/
    hDoc:CREATE-NODE(hText, "", "TEXT"). 
     /*attach the text to the field*/
     hField:APPEND-CHILD(hText).
     hText:NODE-VALUE = STRING(oe-ordl.qty).


    /*create a tag with the field name*/ 
   hDoc:CREATE-NODE(hField, "Due_Date", "ELEMENT").
   /*put the new field as next child of row*/
   hRow:APPEND-CHILD(hField).
   /*add a node to hold field value*/
   hDoc:CREATE-NODE(hText, "", "TEXT"). 
    /*attach the text to the field*/
    hField:APPEND-CHILD(hText).
    hText:NODE-VALUE = STRING(oe-ordl.req-date) .


 END.  /* each oe-rell, oe-ordl */

END.


/*write the XML node tree to an xml file*/
hDoc:SAVE("file",cXMLOutput).

DELETE OBJECT hDoc.
DELETE OBJECT hRoot.
DELETE OBJECT hRow.
DELETE OBJECT hField.
DELETE OBJECT hText.
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
  DISPLAY begin_cust-no end_cust-no tb_excl_cust begin_relnum end_relnum 
          begin_ord-no end_ord-no begin_date end_date begin_del-zone 
          end_del-zone begin_whse end_whse tb_printed tb_exl-tg-bin 
          tb_print-qty-uom tb_print-spec tb_sort-rel fi_specs tb_posted tb_more 
          rd-print-what tb_p-bin tb_zone-s tb_zone-p tb_print-component 
          begin_loc end_loc tb_prt-part-no tb_pricing begin_loc-bin end_loc-bin 
          tb_whs-bin-sort rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          tgMultipleReleases td-show-parm tb_post-rel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-9 begin_cust-no end_cust-no tb_excl_cust 
         begin_relnum end_relnum begin_ord-no end_ord-no begin_date end_date 
         begin_del-zone end_del-zone begin_whse end_whse tb_printed 
         tb_print-qty-uom tb_print-spec tb_sort-rel fi_specs tb_posted tb_more 
         rd-print-what tb_p-bin tb_zone-s tb_zone-p tb_print-component 
         begin_loc end_loc begin_loc-bin end_loc-bin tb_whs-bin-sort rd-dest 
         lv-ornt lines-per-page lv-font-no tgMultipleReleases td-show-parm 
         tb_post-rel btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-relnum C-Win 
PROCEDURE new-relnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(begin_relnum:SCREEN-VALUE) NE 0                            AND
       INT(begin_relnum:SCREEN-VALUE) EQ INT(end_relnum:SCREEN-VALUE) THEN DO:
      FIND FIRST oe-relh NO-LOCK
          WHERE oe-relh.company  EQ cocode
            AND oe-relh.release# EQ INT(begin_relnum:SCREEN-VALUE)
          NO-ERROR.
      IF AVAIL oe-relh THEN
        ASSIGN
         tb_printed:SCREEN-VALUE = STRING(oe-relh.printed)
         tb_posted:SCREEN-VALUE  = STRING(oe-relh.posted).
    END.
  END.

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
  RUN custom\d-print.w (list-name).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

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
IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-releases C-Win 
PROCEDURE post-releases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR ll-exception AS LOG NO-UNDO.
DEF VAR v-first-release AS LOG NO-UNDO.
DEF VAR v-royal AS LOG NO-UNDO.

DEF BUFFER upd-oe-relh FOR oe-relh.

{sa/sa-sls01.i}

DISABLE TRIGGERS FOR LOAD OF itemfg.

IF relpost-chr EQ "Nothing" THEN DO:
  {oe/rep/foreachr3.i}

  headblok:
  {oe/rep/foreachr2.i}.

    {oe/oe-relp.i}
  END. /* each oe-relh */
END.

ASSIGN
 v-frel     = begin_relnum
 v-trel     = end_relnum
 v-fdat     = begin_date
 v-tdat     = end_date
 v-fcus     = begin_cust-no
 v-tcus     = end_cust-no
 v-ford     = begin_ord-no
 v-tord     = end_ord-no.

RUN oe/oe-relp2.p (v-term, v-royal).

FOR EACH report WHERE report.term-id EQ v-term,
    FIRST oe-boll
    WHERE RECID(oe-boll) EQ report.rec-id
      AND CAN-FIND(FIRST oe-bolh
                   WHERE oe-bolh.b-no    EQ oe-boll.b-no
                     AND oe-bolh.printed EQ NO)
    NO-LOCK:
  DELETE report.
END.

RUN oe/oe-bolp3.p (v-term).

FIND CURRENT oe-relh NO-LOCK NO-ERROR.
FIND CURRENT oe-rell NO-LOCK NO-ERROR.
FIND CURRENT itemfg NO-LOCK NO-ERROR.
FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
FIND CURRENT upd-oe-relh NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------- oe/rep/oe-pick.p 6/94 rd ---- */
/* print oe Release/Picking tickets                                           */
/* -------------------------------------------------------------------------- */

{sys/FORM/r-top.i}

ASSIGN
 v-s-cust-no  = b-oe-relh.cust-no
 v-e-cust-no  = b-oe-relh.cust-no
 v-s-rel      = begin_relnum
 v-e-rel      = end_relnum
 v-s-ord      = begin_ord-no
 v-e-ord      = end_ord-no
 v-fdate      = begin_date
 v-tdate      = end_date
 v-s-ter      = begin_del-zone
 v-e-ter      = end_del-zone
 v-printed    = tb_printed
 v-posted     = tb_posted
 v-more       = tb_more
 v-p-bin      = tb_p-bin
 v-posted     = tb_posted
 v-zone-s     = tb_zone-s
 v-zone-p     = tb_zone-p
 v-sort-loc-bin = tb_whs-bin-sort
 s-print-what-item = rd-print-what
 s-print-pricing = tb_pricing
 vPrinted        = tb_printed
 v-exc-bin    = tb_exl-tg-bin
 lExclCust    = tb_excl_cust
 cLocStart    = begin_whse
 cLocEnd      = end_whse
 lv-spec-list  = fi_specs
 s-print-spec  = tb_print-spec 
 lSortRelSeq   = tb_sort-rel
 lPrintQtyUom  = tb_print-qty-uom.

IF LOOKUP(v-relprint,"Hopx,ACPI,Fibrex,Accord,Metro,Carded,Loylang,PremierX,Relprint 10,Lakeside,Distributor,Frank,NSTOCK,Axis,CSC-GA,Protagon,CardedX,Peachtree,Multicell,CCC,Soule,StClair,Midwest") > 0 AND
   LOOKUP(s-print-what-item,"I,S") > 0 THEN 
   ASSIGN s-print-loc-from = begin_loc
          s-print-loc-to = END_loc
          s-print-bin-from = begin_loc-bin
          s-print-bin-to = END_loc-bin.

IF v-posted THEN DO WITH FRAME {&FRAME-NAME} TRANSACTION:
  FIND FIRST oe-relh
      WHERE oe-relh.company  EQ cocode
        AND oe-relh.posted   EQ YES
        AND oe-relh.release# EQ b-oe-relh.release#  /* task 08141403 */
      NO-ERROR.

  IF AVAIL oe-relh THEN 
     ASSIGN oe-relh.posted = NO
            v-date         = oe-relh.rel-date.
  tb_printed     = YES.

  DISPLAY tb_printed.

  IF AVAIL oe-relh THEN
  FIND FIRST oe-bolh
      WHERE oe-bolh.company  EQ cocode
        AND oe-bolh.release# EQ oe-relh.release#
      USE-INDEX release# NO-LOCK NO-ERROR.
  IF AVAIL oe-relh AND NOT AVAIL oe-bolh THEN
    MESSAGE "Please enter new Release Date:"
            UPDATE oe-relh.rel-date FORMAT "99/99/9999".

  IF AVAIL oe-relh AND oe-relh.rel-date NE v-date THEN DO:
    FOR EACH oe-rell
        WHERE oe-rell.company EQ cocode
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no,

        FIRST oe-rel
        WHERE oe-rel.link-no EQ oe-rell.r-no
        USE-INDEX seq-no:

      FIND FIRST oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ oe-rel.ord-no
            AND oe-ordl.i-no    EQ oe-rel.i-no
            AND oe-ordl.LINE    EQ oe-rell.LINE
          NO-ERROR.
      IF AVAIL oe-ordl THEN
        oe-ordl.t-rel-qty = oe-ordl.t-rel-qty - oe-rel.qty.

      ASSIGN
       oe-rell.posted  = NO
       oe-rel.rel-date = oe-relh.rel-date
       oe-rel.link-no  = 0.
    END.
  END.
END.

FIND CURRENT oe-rell NO-LOCK NO-ERROR.
FIND CURRENT oe-rel NO-LOCK NO-ERROR.

{sys/inc/print1.i}
v-count = v-count + 1.
list-name = list-name + STRING(v-count).
{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

v-lines-per-page = lines-per-page.

IF IS-xprint-form AND (v-1st-page OR v-multi-cust = NO) THEN DO:
    v-1st-page = NO.
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?></PROGRESS>".
        WHEN 2 THEN DO:
            IF NOT lBussFormModle THEN DO:            
              IF v-relprint = "StClair" OR v-relprint = "Midwest" THEN  PUT "<PREVIEW><MODAL=NO><OLANDSCAPE></PROGRESS>".
              ELSE PUT "<PREVIEW><MODAL=NO></PROGRESS>". 
            END. 
            ELSE DO:
              IF v-relprint = "StClair" OR v-relprint = "Midwest" THEN  PUT "<PREVIEW><OLANDSCAPE></PROGRESS>".
              ELSE PUT "<PREVIEW></PROGRESS>".
            END.      
        END.

        WHEN 4 THEN DO:
           ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
           PUT UNFORMATTED "</PROGRESS><PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN DO:
            IF LOOKUP(v-relprint,"PremierX,Lakeside,Distributor") > 0 THEN
            PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=-3mm><PDF-TOP=5mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
            ELSE
             PUT "<PREVIEW><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
        END.
    END CASE.
END.

RUN value(lv-program).
IF xmlorder-log THEN RUN createxmlrelease.

IF AVAIL oe-relh AND v-posted AND oe-relh.rel-date EQ v-date THEN DO TRANSACTION:
  oe-relh.posted = YES.
END.

FIND CURRENT oe-relh NO-LOCK NO-ERROR.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-report C-Win 
PROCEDURE set-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF v-relprint EQ "MultiWll" THEN
    ASSIGN
     lv-program     = "oe/rep/relmulti.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "HOP" THEN 
    ASSIGN
     lv-program     = "oe/rep/relhop.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "TriState" THEN
    ASSIGN
     lv-program     = "oe/rep/reltrist.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "Fibre" THEN
    ASSIGN
     lv-program     = "oe/rep/relfibre.p"
     lines-per-page = 59. /* 60*/

  ELSE
  IF v-relprint EQ "Premier" THEN
    ASSIGN
     lv-program     = "oe/rep/relprem.p"
     lines-per-page = 60.
  ELSE
  IF v-relprint EQ "Carded" THEN
    ASSIGN
     lv-program     = "oe/rep/relcard.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "Pacific" THEN
    ASSIGN
     lv-program     = "oe/rep/relpacif.p"
     lines-per-page = 75
     is-xprint-form = YES.

  ELSE
  IF v-relprint EQ "Xprint" OR v-relprint EQ "relprint 1" THEN
   ASSIGN
    lv-program     = "oe/rep/relxprnt.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/
  ELSE

  IF v-relprint EQ "relprint 10" THEN
   ASSIGN
    lv-program     = "oe/rep/relxprnt10.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE
  IF v-relprint EQ "APC" THEN
   ASSIGN
    lv-program     = "oe/rep/relxapc.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE
  IF v-relprint EQ "HPB" THEN
   ASSIGN
    lv-program     = "oe/rep/relxhpb.p"
    lines-per-page = 75
    is-xprint-form = YES  .

  ELSE
  IF v-relprint EQ "PPI" THEN
   ASSIGN
    lv-program     = "oe/rep/relppi.p"
    lines-per-page = 75
    is-xprint-form = YES  .

  ELSE IF v-relprint EQ "Xprint2" OR  v-relprint EQ "relprint 2" THEN
    ASSIGN lv-program     = "oe/rep/relxprn2.p"
           lines-per-page = 75
           is-xprint-form = YES  . 

  ELSE IF v-relprint EQ "Sonoco" THEN DO:
   ASSIGN 
    lines-per-page = 75
    is-xprint-form = YES
    lv-program     = "oe/rep/relsonoc.p". 
  END.
  ELSE IF v-relprint EQ "Indiana" THEN
   ASSIGN
    lv-program     = "oe/rep/relindc.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Hopx" THEN
   ASSIGN
    lv-program     = "oe/rep/relhopx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "ACPI" THEN
   ASSIGN
    lv-program     = "oe/rep/relacpi.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "CardedX" THEN
   ASSIGN
    lv-program     = "oe/rep/relcardx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Peachtree" THEN
   ASSIGN
    lv-program     = "oe/rep/relpchtr.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Multicell" THEN
   ASSIGN
    lv-program     = "oe/rep/relmcell.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "PremierX" THEN
   ASSIGN
    lv-program     = "oe/rep/relpremx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/
  
  ELSE IF v-relprint EQ "Lakeside" THEN
   ASSIGN
    lv-program     = "oe/rep/relkside.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

   ELSE IF v-relprint EQ "Distributor" THEN
   ASSIGN
    lv-program     = "oe/rep/reldistb.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Frank" THEN
   ASSIGN
    lv-program     = "oe/rep/relfrnkx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Prystup" THEN
   ASSIGN
    lv-program     = "oe/rep/relprysx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "NStock" THEN
   ASSIGN
    lv-program     = "oe/rep/relnstok.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Axis" THEN
   ASSIGN
    lv-program     = "oe/rep/relaxis.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Soule" THEN
   ASSIGN
    lv-program     = "oe/rep/relsoule.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "StClair" THEN
   ASSIGN
    lv-program     = "oe/rep/relStClair.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Midwest" THEN
   ASSIGN
    lv-program     = "oe/rep/relmidwest.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "CSC-GA" THEN
   ASSIGN
    lv-program     = "oe/rep/relcsc.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Protagon" THEN
   ASSIGN
    lv-program     = "oe/rep/relprogn.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Fibrex" THEN
   ASSIGN
    lv-program     = "oe/rep/relfibx.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Accord" THEN
   ASSIGN
    lv-program     = "oe/rep/relacrd.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/

  ELSE IF v-relprint EQ "Metro" THEN
  ASSIGN
   lv-program     = "oe/rep/relmetro.p"
   lines-per-page = 75
   is-xprint-form = YES  . /*60*/

  ELSE
  IF v-relprint EQ "CentBox" THEN
   ASSIGN
    lv-program     = "oe/rep/relcntbx.p"
    lines-per-page = 59.

  ELSE
  IF v-relprint EQ "Keystone" THEN
   ASSIGN
    lv-program     = "oe/rep/relkeyst.p"
    lines-per-page = 59.

  ELSE
  IF v-relprint EQ "Frankstn" THEN
   ASSIGN
    lv-program     = "oe/rep/relfrank.p"
    lines-per-page = 57. /*was 59*/  

  ELSE
  IF v-zone-s THEN
    ASSIGN
     lv-program     = "oe/rep/oe-pick1.p"
     lines-per-page = 60.

  ELSE
  IF v-relprint EQ "Hughes" THEN
    ASSIGN
     lv-program     = "oe/rep/relhughs.p"
     lines-per-page = 59. /* 60*/

  ELSE IF v-relprint EQ "Allwest" THEN
    ASSIGN lv-program     = "oe/rep/relallws.p"
           lines-per-page = 75
           is-xprint-form = YES  . 
  ELSE IF v-relprint EQ "CCC" THEN
    ASSIGN lv-program     = "oe/rep/relccc.p"
           lines-per-page = 75
           is-xprint-form = YES  . 
  /* gdm - 10080912*/
  ELSE IF v-relprint EQ "Rosmar" THEN DO:
   ASSIGN 
    lines-per-page = 75
    is-xprint-form = YES
    lv-program     = "oe/rep/relrosmr.p".    
  END.
  /* gdm - 09220907 */
  ELSE IF v-relprint EQ "Loylang" THEN DO:  
   ASSIGN
    lv-program     = "oe/rep/relloyl.p"
    lines-per-page = 75
    is-xprint-form = YES  . /*60*/
  END.
  ELSE
  IF v-more THEN
    ASSIGN
     lv-program     = "oe/rep/rel-more.p"
     lines-per-page = 55.
  ELSE
    ASSIGN
     lv-program     = "oe/rep/oe-pick2.p"
     lines-per-page = 60.

  v-lines-per-page = lines-per-page.

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
  DEF VAR lv-label AS cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = FRAME {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD
  lv-field-hdl = lv-group-hdl:FIRST-CHILD.

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
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
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

