&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg\fgpstall.w

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
DEF INPUT PARAMETER ip-post-eom-date AS DATE NO-UNDO.
DEF INPUT PARAMETER ip-run-what AS CHAR NO-UNDO. /* "SETUP" from initial setup (addon/sshoot/sssetups.w), 
                                                   else "" */

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

DEF SHARED VAR choice AS LOG NO-UNDO.

DEF VAR v-fgpostgl AS CHAR NO-UNDO.
def var v-fg-value as dec format "->,>>>,>>9.99".
def var v-msf as dec format ">,>>9.999" extent 6.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
DEF VAR ip-rowid AS ROWID NO-UNDO.
DEF VAR t-setup AS LOG NO-UNDO.

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO
                                    FIELD old-tag AS CHAR
                                    FIELD ret-loc AS CHAR
                                    FIELD ret-loc-bin AS CHAR.

DEF TEMP-TABLE tt-email NO-UNDO FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .

{fg/fullset.i NEW}

{jc/jcgl-sh.i NEW}

{fg/fg-post3.i NEW}

{fg/invrecpt.i NEW}

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DEF STREAM st-email.
DEF STREAM logFile.
DEF STREAM before.
DEF STREAM after.
/* AJ 06/24/2008  Added two variables for excel report */
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b2-fg-rctd FOR fg-rctd.

{sys/inc/ssfgretc.i}

DO TRANSACTION:
  {sys/inc/fgpost.i}   
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-30 RECT-31 RECT-32 ~
v-post-date begin_fg-r-no end_fg-r-no begin_userid end_userid ldt-from ~
ldt-to begin_job-no end_job-no begin_i-no end_i-no end_whs begin_whs ~
rd_print t-receipt rd-Itm#Cst# rd-ItmPo t-ship t-trans rd-UOMJob t-adj ~
tb_glnum t-ret tb_totCstVal tb_grndtotal td-show-parm rd-dest lv-ornt ~
lv-font-no lines-per-page tb_excel tb_runExcel fi_file Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-post-date begin_fg-r-no end_fg-r-no ~
begin_userid end_userid ldt-from ldt-to begin_job-no end_job-no begin_i-no ~
end_i-no end_whs begin_whs v-trans-lbl rd_print tgl-itemCD t-receipt ~
rd-Itm#Cst# rd-ItmPo t-ship t-trans rd-UOMJob t-adj tb_glnum t-ret ~
tb_totCstVal tb_grndtotal td-show-parm rd-dest lv-ornt lv-font-no ~
lines-per-page lv-font-name tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_fg-r-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Seq#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_userid AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning User ID" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "From Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_fg-r-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Seq#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_userid AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending User ID" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "To Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fgpstall.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE ldt-from AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE ldt-to AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

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

DEFINE VARIABLE v-post-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE v-trans-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Transaction Types" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .91
     FONT 6 NO-UNDO.

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
     SIZE 20 BY 5.71 NO-UNDO.

DEFINE VARIABLE rd-Itm#Cst# AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG Item #", 1,
"Customer Part # ", 2
     SIZE 37.2 BY .91 NO-UNDO.

DEFINE VARIABLE rd-ItmPo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item Name", 1,
"P.O. # / Vendor", 2
     SIZE 36.2 BY .91 NO-UNDO.

DEFINE VARIABLE rd-UOMJob AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "UOM", 1,
"Job #", 2
     SIZE 31.6 BY .91 NO-UNDO.

DEFINE VARIABLE rd_print AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cost", "C",
"Sell Value", "S"
     SIZE 36.4 BY .91 NO-UNDO.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.4 BY 6.67.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.6 BY 6.67.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.6 BY 6.67.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 7.38.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 15.48.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ret AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ship AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_glnum AS LOGICAL INITIAL no 
     LABEL "Print GL Account Numbers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tb_grndtotal AS LOGICAL INITIAL no 
     LABEL "Grand Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_totCstVal AS LOGICAL INITIAL no 
     LABEL "Total Cost/Value" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-itemCD AS LOGICAL INITIAL no 
     LABEL "Item Code" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     v-post-date AT ROW 2.91 COL 26 COLON-ALIGNED
     begin_fg-r-no AT ROW 3.86 COL 26 COLON-ALIGNED HELP
          "Enter the Beginning Sequence Number"
     end_fg-r-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter the Ending Sequence Number"
     begin_userid AT ROW 4.81 COL 26 COLON-ALIGNED HELP
          "Enter the Beginning User ID"
     end_userid AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter the Ending User ID"
     ldt-from AT ROW 5.76 COL 26 COLON-ALIGNED HELP
          "Enter the Beginning Date"
     ldt-to AT ROW 5.76 COL 69 COLON-ALIGNED HELP
          "Enter the Ending Date"
     begin_job-no AT ROW 6.71 COL 26 COLON-ALIGNED HELP
          "Enter the Beginning Job Number"
     end_job-no AT ROW 6.71 COL 69 COLON-ALIGNED HELP
          "Enter the Ending Job Number"
     begin_i-no AT ROW 7.67 COL 26 COLON-ALIGNED HELP
          "Enter the Beginning FG Item Number"
     end_i-no AT ROW 7.67 COL 69 COLON-ALIGNED HELP
          "Enter the Ending FG Item Number"
     end_whs AT ROW 8.62 COL 69 COLON-ALIGNED HELP
          "Enter the To Warehouse" WIDGET-ID 4
     begin_whs AT ROW 8.67 COL 26 COLON-ALIGNED HELP
          "Enter the from Warehouse" WIDGET-ID 2
     v-trans-lbl AT ROW 10.81 COL 2 NO-LABEL
     rd_print AT ROW 10.91 COL 57 NO-LABEL
     tgl-itemCD AT ROW 11.19 COL 37.8 WIDGET-ID 16
     t-receipt AT ROW 11.76 COL 2
     rd-Itm#Cst# AT ROW 11.76 COL 57 NO-LABEL WIDGET-ID 18
     rd-ItmPo AT ROW 12.57 COL 57 NO-LABEL WIDGET-ID 26
     t-ship AT ROW 12.62 COL 2
     t-trans AT ROW 13.48 COL 2
     rd-UOMJob AT ROW 13.52 COL 57 NO-LABEL WIDGET-ID 22
     t-adj AT ROW 14.38 COL 2
     tb_glnum AT ROW 14.57 COL 57
     t-ret AT ROW 15.33 COL 2
     tb_totCstVal AT ROW 15.52 COL 57 WIDGET-ID 30
     tb_grndtotal AT ROW 15.52 COL 78.6
     td-show-parm AT ROW 17.19 COL 29
     rd-dest AT ROW 17.67 COL 2 NO-LABEL
     lv-ornt AT ROW 18.14 COL 29 NO-LABEL
     lv-font-no AT ROW 19.33 COL 32 COLON-ALIGNED
     lines-per-page AT ROW 19.33 COL 60 COLON-ALIGNED
     lv-font-name AT ROW 20.52 COL 32 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 21.71 COL 53.2
     tb_runExcel AT ROW 21.71 COL 94.8 RIGHT-ALIGNED
     fi_file AT ROW 22.67 COL 51 COLON-ALIGNED HELP
          "Enter File Name"
     Btn_OK AT ROW 23.95 COL 22
     Btn_Cancel AT ROW 24 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 1.8
          BGCOLOR 2 
     "Trans Typ :" VIEW-AS TEXT
          SIZE 12 BY .95 AT ROW 9.95 COL 2 WIDGET-ID 6
     "Sort Options :" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 9.95 COL 36.6 WIDGET-ID 14
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.62 COL 1
     "Print Options :" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 9.95 COL 54.8
     "This Procedure Will Post All Finished Goods Transactions" VIEW-AS TEXT
          SIZE 65 BY .95 AT ROW 1.71 COL 23.8
          FONT 6
     RECT-6 AT ROW 16.48 COL 1
     RECT-7 AT ROW 1 COL 1
     RECT-30 AT ROW 9.81 COL 1.6 WIDGET-ID 8
     RECT-31 AT ROW 9.81 COL 35.6 WIDGET-ID 10
     RECT-32 AT ROW 9.81 COL 54 WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 96.4 BY 24.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Finished Goods Posting"
         HEIGHT             = 24.62
         WIDTH              = 97
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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
       Btn_Cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".



ASSIGN 
    v-post-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    begin_fg-r-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    end_fg-r-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    begin_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    end_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    ldt-from:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    ldt-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-receipt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-trans:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-adj:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_glnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ret:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_totCstVal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_grndtotal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
/*
ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd-Itm#Cst#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd-ItmPo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd-UOMJob:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
ASSIGN 
       rd-dest:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
*/


/* SETTINGS FOR TOGGLE-BOX tgl-itemCD IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-trans-lbl IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       v-trans-lbl:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Posting */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK NO-ERROR.
  IF NOT AVAIL period THEN DO WITH FRAME {&FRAME-NAME}:
    MESSAGE "No period exists for this date..."
            VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO v-post-date.
    RETURN NO-APPLY.
  END.

  RUN print-and-post.
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


&Scoped-define SELF-NAME rd-Itm#Cst#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-Itm#Cst# C-Win
ON VALUE-CHANGED OF rd-Itm#Cst# IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-ItmPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-ItmPo C-Win
ON VALUE-CHANGED OF rd-ItmPo IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-UOMJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-UOMJob C-Win
ON VALUE-CHANGED OF rd-UOMJob IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-A /* Adjustments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-A /* Receipts */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-ship C-Win
ON VALUE-CHANGED OF t-ship IN FRAME FRAME-A /* Shipments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-A /* Transfers */
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


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-post-date C-Win
ON LEAVE OF v-post-date IN FRAME FRAME-A /* Post Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


IF ip-run-what EQ "" THEN DO:
  PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
  END.

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
END.

ELSE DELETE WIDGET {&WINDOW-NAME}.

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

  DO TRANSACTION:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}   
    {sys/inc/adjustgl.i}
    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
  END.

  ASSIGN
   v-fgpostgl  = fgpostgl
   tb_glnum    = v-fgpostgl NE "None" OR v-adjustgl
   tgl-itemCD  = YES   .

  IF ip-run-what EQ "" THEN DO WITH FRAME {&FRAME-NAME}:
    RUN enable_UI.

    {custom/usrprint.i}

    ASSIGN
      begin_userid:SCREEN-VALUE = USERID("nosweat")
      end_userid:SCREEN-VALUE   = USERID("nosweat")
      v-fgpostgl                = fgpostgl
      tb_glnum:SCREEN-VALUE     = STRING(v-fgpostgl NE "None" OR v-adjustgl).

    IF NOT LOGICAL(tb_glnum:SCREEN-VALUE) THEN DISABLE tb_glnum.

    IF postdate-log THEN
    DO:
      v-post-date:SCREEN-VALUE = STRING(TODAY).
      APPLY "ENTRY" TO begin_fg-r-no.
    END.
    ELSE
    DO:
      v-post-date:SCREEN-VALUE = "".
      APPLY "entry" TO v-post-date.
    END.

    RUN init-values.

    {methods/nowait.i}

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.

  ELSE DO:
    /*FIND fg-rctd NO-LOCK WHERE RECID(fg-rctd) EQ INT(ip-run-what) NO-ERROR.

    IF AVAIL fg-rctd THEN
      ASSIGN
       ip-rowid    = ROWID(fg-rctd)
       ip-run-what = fg-rctd.rita-code.

    ELSE ip-rowid = ?.*/

    ASSIGN
     v-post-date  = TODAY
     t-receipt    = ip-run-what EQ "R"
     t-ship       = ip-run-what EQ "S"
     t-trans      = ip-run-what EQ "T"
     t-adj        = NO
     t-ret        = NO
     t-setup      = ip-run-what EQ "SETUP"
     begin_fg-r-no = 0
     end_fg-r-no   = 2147483647
     begin_i-no   = ""
     end_i-no     = "zzzzzzzzzzzzzzzzzzzzzzzz"
     ldt-from     = 01/01/0001
     ldt-to       = 12/31/9999
     begin_job-no = ""
     end_job-no   = "zzzzzzzzzzzzzzzzzzzzzzzz"
     begin_userid = USERID("nosweat")
     end_userid   = USERID("nosweat").

    /*IF ip-run-what EQ "SETUP" THEN DO :
      ASSIGN
       t-receipt = NO
       t-ship   = NO
       t-trans   = NO
       t-adj     = NO
       t-ret     = NO
       tb_glnum  = NO

       t-receipt:HIDDEN   = YES
       t-ship:HIDDEN     = YES
       t-trans:HIDDEN     = YES
       t-adj:HIDDEN       = YES
       t-ret:HIDDEN       = YES
       tb_glnum:HIDDEN    = YES
       v-trans-lbl:HIDDEN = YES.
    END.*/

    RUN print-and-post.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tables C-Win 
PROCEDURE build-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
DEF VAR li-max-qty AS INT NO-UNDO.
def var v-part-qty as dec no-undo.
def var v-set-qty as dec no-undo.
DEF VAR v-cost AS DEC NO-UNDO.

DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b-itemfg FOR itemfg.
/*DEF BUFFER use-job FOR reftable.*/

FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ fg-rctd.i-no
    NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO TRANSACTION:
  li-max-qty = fg-rctd.t-qty.

  IF li-max-qty GE fg-rctd.t-qty THEN DO:
    CREATE w-fg-rctd.
    BUFFER-COPY fg-rctd TO w-fg-rctd
    ASSIGN
     w-fg-rctd.row-id  = ROWID(fg-rctd)
     w-fg-rctd.has-rec = YES.

    IF ip-run-what EQ "SETUP" THEN
       ASSIGN
       w-fg-rctd.old-tag = fg-rctd.tag
       w-fg-rctd.ret-loc = fg-rctd.loc
       w-fg-rctd.ret-loc-bin = fg-rctd.loc-bin.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-partial C-Win 
PROCEDURE calc-partial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.

    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.

      if itemfg.sell-uom begins "L" then
        v-fg-value = 0.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = 0.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * w-fg-rctd.partial / uom.mult.

      else
        v-fg-value = itemfg.sell-price * w-fg-rctd.partial / 1000.

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.partial * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.partial * itemfg.t-sqft)).

        v-msf[1] = w-fg-rctd.partial * itemfg.t-sqft.
      end.
    end. /* avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total C-Win 
PROCEDURE calc-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.

    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.

      if itemfg.sell-uom begins "L" then
        v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / uom.mult).

      else
        v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / 1000).

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.t-qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - ((w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft)).

        v-msf[1] = (w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft.
      end.
    end. /* avail itemfg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-phy-count-proc C-Win 
PROCEDURE create-phy-count-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rno AS INT NO-UNDO.

   DEF BUFFER b-fg-bin FOR fg-bin.

   CREATE b2-fg-rctd.

   FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
   IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN
      lv-rno = b-fg-rctd.r-no.

   FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN
      lv-rno = fg-rcpth.r-no.

   DO WHILE TRUE:
      lv-rno = lv-rno + 1.
      IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
         CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
         NEXT.
      LEAVE.
   END.

   /*task 06101005*/
   IF w-fg-rctd.rita-code EQ "I" THEN
      ASSIGN
         w-fg-rctd.job-no = ""
         w-fg-rctd.job-no2 = 0
         w-fg-rctd.cost = 0
         w-fg-rctd.std-cost = 0
         w-fg-rctd.ext-cost = 0.

   assign
      b2-fg-rctd.company = cocode
      b2-fg-rctd.r-no    = lv-rno
      b2-fg-rctd.rita-code = "C"
      b2-fg-rctd.s-num = 0
      b2-fg-rctd.rct-date = today
      b2-fg-rctd.trans-time = TIME 
      b2-fg-rctd.tag = w-fg-rctd.old-tag
      b2-fg-rctd.loc = w-fg-rctd.ret-loc
      b2-fg-rctd.loc-bin = w-fg-rctd.ret-loc-bin
      b2-fg-rctd.i-no = w-fg-rctd.i-no
      b2-fg-rctd.i-name = w-fg-rctd.i-name
      b2-fg-rctd.job-no = w-fg-rctd.job-no
      b2-fg-rctd.job-no2 = w-fg-rctd.job-no2
      b2-fg-rctd.t-qty = w-fg-rctd.inv-no
      b2-fg-rctd.cases = w-fg-rctd.cases
      b2-fg-rctd.cases-unit = w-fg-rctd.cases-unit
      b2-fg-rctd.qty-case = w-fg-rctd.qty-case
      b2-fg-rctd.std-cost = w-fg-rctd.std-cost
      b2-fg-rctd.cost     = w-fg-rctd.cost
      b2-fg-rctd.cost-uom = w-fg-rctd.cost-uom
      b2-fg-rctd.ext-cost = w-fg-rctd.ext-cost
      b2-fg-rctd.cust-no  = w-fg-rctd.cust-no.

   IF b2-fg-rctd.t-qty NE w-fg-rctd.t-qty AND
      b2-fg-rctd.qty-case NE 0 THEN
      ASSIGN
         b2-fg-rctd.cases = TRUNC(b2-fg-rctd.t-qty / b2-fg-rctd.qty-case,0)
         b2-fg-rctd.partial = b2-fg-rctd.t-qty - (b2-fg-rctd.cases * b2-fg-rctd.qty-case).

   FIND FIRST b-fg-bin 
      WHERE b-fg-bin.company EQ b2-fg-rctd.company
        AND b-fg-bin.i-no    EQ b2-fg-rctd.i-no
        AND b-fg-bin.job-no  EQ b2-fg-rctd.job-no
        AND b-fg-bin.job-no2 EQ b2-fg-rctd.job-no2
        AND b-fg-bin.loc     EQ b2-fg-rctd.loc
        AND b-fg-bin.loc-bin EQ b2-fg-rctd.loc-bin
        AND b-fg-bin.tag     EQ b2-fg-rctd.tag
        AND b-fg-bin.cust-no EQ b2-fg-rctd.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL b-fg-bin THEN
     ASSIGN
        b2-fg-rctd.ext-cost = b2-fg-rctd.t-qty /
                           (IF b-fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                           b-fg-bin.std-tot-cost
        b2-fg-rctd.cost     = b2-fg-rctd.ext-cost / b2-fg-rctd.t-qty
        b2-fg-rctd.cost-uom = b-fg-bin.pur-uom.

  IF b2-fg-rctd.ext-cost EQ ? THEN b2-fg-rctd.ext-cost = 0.
  IF b2-fg-rctd.cost     EQ ? THEN b2-fg-rctd.cost = 0.

   FIND FIRST loadtag WHERE
        loadtag.company = g_company AND
        loadtag.item-type = NO AND
        loadtag.tag-no = b2-fg-rctd.tag
        NO-LOCK NO-ERROR.

   IF AVAIL loadtag AND
      CAN-FIND(FIRST fg-bin WHERE
      fg-bin.company EQ cocode AND
      fg-bin.i-no    EQ b2-fg-rctd.i-no AND
      fg-bin.tag     EQ b2-fg-rctd.tag AND
      fg-bin.job-no  EQ b2-fg-rctd.job-no AND
      fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
      (fg-bin.loc    NE b2-fg-rctd.loc OR
       fg-bin.loc-bin NE b2-fg-rctd.loc-bin)
       USE-INDEX tag) AND
       (loadtag.loc <> b2-fg-rctd.loc OR 
        loadtag.loc-bin <> b2-fg-rctd.loc-bin) THEN 
        RUN crt-transfer.

   RELEASE b2-fg-rctd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-transfer C-Win 
PROCEDURE crt-transfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-rno AS INT NO-UNDO.
  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF VAR lv-rctd-rowid AS ROWID NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
       CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
       NEXT.
    LEAVE.
  END.

  /*FOR EACH b-fg-rctd WHERE
      recid(b-fg-rctd) <> RECID(b2-fg-rctd) AND
      b-fg-rctd.i-no = b2-fg-rctd.i-no AND
      b-fg-rctd.tag = b2-fg-rctd.tag:
      DELETE b-fg-rctd.
  END.*/

  FOR EACH fg-bin WHERE
      fg-bin.company EQ cocode AND
      fg-bin.i-no    EQ b2-fg-rctd.i-no AND
      fg-bin.job-no  EQ b2-fg-rctd.job-no AND
      fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
      fg-bin.tag     EQ b2-fg-rctd.tag
      NO-LOCK:

     IF fg-bin.loc NE b2-fg-rctd.loc OR
        fg-bin.loc-bin NE b2-fg-rctd.loc-bin THEN DO:
        CREATE b-fg-rctd.
        BUFFER-COPY b2-fg-rctd EXCEPT b2-fg-rctd.r-no TO b-fg-rctd
        ASSIGN b-fg-rctd.r-no = lv-rno
               b-fg-rctd.loc = fg-bin.loc
               b-fg-rctd.loc-bin = fg-bin.loc-bin
               b-fg-rctd.cases = 0
               b-fg-rctd.qty-case = 0
               b-fg-rctd.cases-unit = 0
               b-fg-rctd.partial = 0
               b-fg-rctd.t-qty = 0
               lv-rno = lv-rno + 1.
        RELEASE b-fg-rctd.
     END.
  END.  /* for each fg-bin*/
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
  DISPLAY v-post-date begin_fg-r-no end_fg-r-no begin_userid end_userid ldt-from 
          ldt-to begin_job-no end_job-no begin_i-no end_i-no end_whs begin_whs 
          v-trans-lbl rd_print tgl-itemCD t-receipt rd-Itm#Cst# rd-ItmPo t-ship 
          t-trans rd-UOMJob t-adj tb_glnum t-ret tb_totCstVal tb_grndtotal 
          td-show-parm rd-dest lv-ornt lv-font-no lines-per-page lv-font-name 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-30 RECT-31 RECT-32 v-post-date begin_fg-r-no 
         end_fg-r-no begin_userid end_userid ldt-from ldt-to begin_job-no 
         end_job-no begin_i-no end_i-no end_whs begin_whs rd_print t-receipt 
         rd-Itm#Cst# rd-ItmPo t-ship t-trans rd-UOMJob t-adj tb_glnum t-ret 
         tb_totCstVal tb_grndtotal td-show-parm rd-dest lv-ornt lv-font-no 
         lines-per-page tb_excel tb_runExcel fi_file Btn_OK Btn_Cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fg-post C-Win 
PROCEDURE fg-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-fg-rcpts for fg-rcpts.
  def buffer b-fg-rdtl for fg-rdtl.
  def buffer b-fg-bin for fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  def buffer b-itemfg1 for itemfg.
  def buffer ps-rctd for fg-rctd .
  def buffer b-po-ordl for po-ordl.
  def buffer b-oe-ordl for oe-ordl.

  def var v-one-item as log.
  def var v-dec as dec decimals 10.
  def var v-po-no like rm-rcpt.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like fg-rctd.qty no-undo.
  def var v-i-qty like fg-rctd.qty no-undo.
  def var v-t-qty like fg-rctd.qty no-undo.
  def var v-overrun-qty like fg-rctd.qty no-undo.
  def var v-underrun-qty like fg-rctd.qty no-undo.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  def var v-recid as recid no-undo.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec DECIMALS 10 no-undo.
  def var v-autobin  as cha no-undo.
  def var v-newhdr as log no-undo. 
  def var v-fin-qty as dec no-undo.
  def var choice as log no-undo.
  def var v-trnum like gl-ctrl.trnum no-undo.
  def var uperiod as int no-undo.
  def var sysdate as date init today no-undo.    
  def var v-date like sysdate no-undo.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.

  DEFINE VARIABLE fgPostLog AS LOGICAL NO-UNDO.

  fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
  IF fgPostLog THEN
  OUTPUT STREAM logFile TO VALUE('logs/fgpstall.' +
         STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

  SESSION:SET-WAIT-STATE ("general").
  IF fgPostLog THEN RUN fgPostLog ('Started').
  FIND FIRST period NO-LOCK
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

  find first sys-ctrl  where sys-ctrl.company eq gcompany
                         and sys-ctrl.name    eq "AUTOPOST"
       no-lock no-error.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  FOR EACH w-fg-rctd
      BY w-fg-rctd.tag
      BY w-fg-rctd.rct-date
      BY w-fg-rctd.trans-time
      BY w-fg-rctd.r-no:

    IF NOT CAN-FIND(FIRST itemfg WHERE
       itemfg.company EQ cocode AND
       itemfg.i-no    EQ w-fg-rctd.i-no) THEN
       NEXT.

    loop1:
    REPEAT:

       FIND FIRST itemfg WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no    EQ w-fg-rctd.i-no
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

       IF AVAIL itemfg THEN
       DO:
          IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
          {fg/fg-post.i w-fg-rctd w-fg-rctd}

          FIND CURRENT po-ordl NO-LOCK NO-ERROR.
          FIND CURRENT fg-bin NO-LOCK NO-ERROR.

          FIND CURRENT itemfg NO-LOCK NO-ERROR.
          FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
          LEAVE loop1.
       END.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
    IF w-fg-rctd.rita-code = "R" THEN DO:
       {fg/fgemails.i}
    END.

    IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

    FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

    IF AVAIL fg-rctd THEN DO:
      ASSIGN
       fg-rctd.rita-code = "P"  /* posted */
       fg-rctd.post-date = v-post-date
       fg-rctd.trans-time = TIME
       fg-rctd.tag2      = w-fg-rctd.tag2.

      FOR EACH fg-rcpts
          WHERE fg-rcpts.company EQ fg-rctd.company
            AND fg-rcpts.r-no    EQ fg-rctd.r-no:
        fg-rcpts.rita-code = fg-rctd.rita-code.
      END.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End loop'). 
  END.  /* for each w-fg-rctd */

  IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
  FOR EACH w-fg-rctd
      BREAK BY w-fg-rctd.i-no
            BY w-fg-rctd.job-no
            BY w-fg-rctd.job-no2
            BY w-fg-rctd.loc
            BY w-fg-rctd.loc-bin
            BY w-fg-rctd.tag:

    IF LAST-OF(w-fg-rctd.tag) THEN DO:
      IF TRIM(w-fg-rctd.tag) NE "" THEN 
      /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
      FOR EACH fg-bin NO-LOCK
          WHERE fg-bin.company EQ g_company
            AND fg-bin.i-no    EQ w-fg-rctd.i-no
            AND fg-bin.tag     EQ w-fg-rctd.tag
          USE-INDEX tag:
        RUN fg/calcbinq.p (ROWID(fg-bin)).
      END.

      /* IF w-fg-rctd.tag <> "" then*/
      FIND FIRST loadtag
          WHERE loadtag.company   EQ g_company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ w-fg-rctd.tag
            AND loadtag.i-no      EQ w-fg-rctd.i-no
            AND loadtag.job-no    EQ w-fg-rctd.job-no
          USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.
      IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

      IF AVAIL loadtag THEN DO:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ g_company
              AND fg-bin.i-no    EQ loadtag.i-no
              AND fg-bin.tag     EQ loadtag.tag-no
            /*AND fg-bin.job-no = loadtag.job-no
              AND fg-bin.job-no2 = loadtag.job-no2*/
              AND fg-bin.qty     GT 0
            USE-INDEX tag NO-LOCK NO-ERROR.
        IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/
           TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/ 
          ASSIGN
           loadtag.loc          = w-fg-rctd.loc2   
           loadtag.loc-bin      = w-fg-rctd.loc-bin2
           loadtag.qty          = fg-bin.qty
           loadtag.pallet-count = fg-bin.qty
           loadtag.partial      = fg-bin.partial-count
           loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.
        ELSE /*partial transfer */
          ASSIGN
           loadtag.loc     = w-fg-rctd.loc
           loadtag.loc-bin = w-fg-rctd.loc-bin.     
      END.
    END.

    IF ip-run-what EQ "SETUP" AND ssfgretc-log AND
       ( (w-fg-rctd.rita-code EQ "T" AND w-fg-rctd.inv-no NE 0) OR
          w-fg-rctd.rita-code EQ "I" ) THEN
       RUN create-phy-count-proc.
  END.

  FOR EACH w-inv:
    DELETE w-inv.
  END.

  IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
  FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK:

    CREATE w-inv.
    w-inv.row-id = w-fg-rctd.row-id.
  END.
  IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

  IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
  RUN fg/invrecpt.p (?, 2).
  IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

  IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').
  FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK
      BREAK BY w-fg-rctd.i-no:

    IF LAST-OF(w-fg-rctd.i-no) THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
      RUN fg/updfgcs1.p (RECID(itemfg), NO).
      IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.opened  EQ YES
            AND oe-ordl.i-no    EQ w-fg-rctd.i-no
            AND oe-ordl.job-no  EQ ""
            AND oe-ordl.cost    EQ 0
          USE-INDEX opened NO-LOCK
          BREAK BY oe-ordl.ord-no
          TRANSACTION:

        DO i = 1 TO 1000:
          FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
          IF AVAIL b-oe-ordl THEN DO:
            IF itemfg.prod-uom EQ "M" THEN
              b-oe-ordl.cost = itemfg.total-std-cost.
            ELSE
              RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                      THEN "EA" ELSE itemfg.prod-uom),
                                     "M", 0, 0, 0, 0,
                                     itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
            LEAVE.
          END.
        END.
      END.
    END.
  END.
  IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

  IF v-fgpostgl NE "None" THEN DO TRANSACTION:
    /* gdm - 11050906 */
    loop2:
    REPEAT:
     FIND FIRST gl-ctrl EXCLUSIVE-LOCK
       WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
     IF AVAIL gl-ctrl THEN DO:
       ASSIGN v-trnum       = gl-ctrl.trnum + 1
              gl-ctrl.trnum = v-trnum.

       FIND CURRENT gl-ctrl NO-LOCK.
       IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').
       RUN gl-from-work (1, v-trnum).
       IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
       RUN gl-from-work (2, v-trnum).
       IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
       LEAVE loop2.
     END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.
  find first w-job no-error.
  if avail w-job THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
    run jc/d-jclose.w.
    IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
  END.

  if v-adjustgl then do TRANSACTION:
    /** GET next G/L TRANS. POSTING # **/
    find first gl-ctrl where gl-ctrl.company eq cocode exclusive-lock.
    assign
     v-trnum       = gl-ctrl.trnum + 1
     gl-ctrl.trnum = v-trnum.
    FIND CURRENT gl-ctrl NO-LOCK.
    IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
    for each work-job break by work-job.actnum:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-job.actnum
       gltrans.jrnl    = "ADJUST"
       gltrans.tr-date = v-post-date
       gltrans.period  = period.pnum
       gltrans.trnum   = v-trnum.

      if work-job.fg then
        assign
         gltrans.tr-amt  = - work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT FG".
      else
        assign
         gltrans.tr-amt  = work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT COGS".
    end. /* each work-job */
    IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
  end.
  IF v-got-fgemail THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
    RUN send-fgemail (v-fgemail-file).
    IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
  END.
  IF fgPostLog THEN RUN fgPostLog ('End').
  IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgPostLog C-Win 
PROCEDURE fgPostLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.

  PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
    STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix C-Win 
PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-first-disp as log no-undo.
  DEF OUTPUT PARAMETER ext-cost AS DEC NO-UNDO.
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.

  if not avail fg-rctd then return.  /* no records */

  cocode = fg-rctd.company.

 FOR EACH tt-email:
     DELETE tt-email.
 END.

if ip-first-disp  and avail fg-rctd and fg-rctd.i-no <> "" then do: /* for row-display */
  find itemfg  where itemfg.company eq cocode                           /* no screen-value used */
                     and itemfg.i-no  eq fg-rctd.i-no /*:screen-value in browse {&browse-name}*/
                     use-index i-no no-lock no-error.

  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = int(fg-rctd.po-no)
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = (fg-rctd.job-no)
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.

  if not avail po-ordl AND fg-rctd.po-no <> "" then return.

  lv-out-qty = fg-rctd.t-qty . /* fg-rctd.qty-case. ??? */
  /* convert cost pr-uom*/
  run rm/convcuom.p(fg-rctd.cost-uom, IF AVAIL po-ordl THEN po-ordl.cons-uom ELSE "EA",
                    0,0,0,0,fg-rctd.std-cost, output lv-out-cost).
  ext-cost = lv-out-qty * lv-out-cost.
 /* disp ext-cost with browse {&browse-name}. /*it's displayed automatically */ */

 /* message "after calc:" po-ordl.cons-uom fg-rctd.cost-uom lv-out-cost ext-cost.
  */
end. /* avail fg-rctd */
/* ======================================================================= */
else if avail fg-rctd and fg-rctd.i-no <> "" then do: /* in update mode - use screen-value */
  find itemfg  where itemfg.company eq cocode
                and itemfg.i-no  eq fg-rctd.i-no
                use-index i-no no-lock no-error.
/*  if avail itemfg then v-dep = itemfg.s-dep.    */
  find first po-ordl where po-ordl.company = fg-rctd.company
                   /*    and po-ordl.po-no = integer(fg-rctd.po-no:screen-value in browse {&browse-name}) */
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = fg-rctd.job-no
                       and po-ordl.job-no2 = fg-rctd.job-no2
                       and po-ordl.item-type = no
                       no-lock no-error.

  if not avail po-ordl AND fg-rctd.po-no <> "" then return.

/*
  /* convert qty */
  run rm/convquom.p(fg-rctd.pur-uom:screen-value in browse {&browse-name} ,
                         po-ordl.cons-uom,
                         v-bwt,
                         v-len,
                         input v-wid,
                         input v-dep,
                         input fg-rctd.qty:screen-value in browse {&browse-name},
                         output lv-out-qty).
*/
  lv-out-qty = fg-rctd.t-qty  .
  /* convert cost */
  if avail po-ordl then assign v-len = po-ordl.s-len
                               v-wid = po-ordl.s-wid.
  else assign v-len = 0
              v-wid = 0.

  run rm/convcuom.p( fg-rctd.cost-uom,
                     IF AVAIL po-ordl THEN po-ordl.cons-uom ELSE "EA" ,
                             0,v-len,v-wid,0,
                             fg-rctd.std-cost, output lv-out-cost).

/*  message "new qty: " lv-out-qty "PO-uom:" po-ordl.pr-uom skip
          " cost:" lv-out-cost po-ordl.cons-uom fg-rctd.cost-uom:screen-value in browse {&browse-name}
         "ext: " ext-cost
         .
*/   
  ext-cost = lv-out-qty * lv-out-cost.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work C-Win 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.

  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 


  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:

    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum.

      assign
       debits  = 0
       credits = 0.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-values C-Win 
PROCEDURE init-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*FOR EACH fg-rctd NO-LOCK
      WHERE fg-rctd.company  EQ gcompany
        AND fg-rctd.rct-date GE TODAY - 15
      USE-INDEX rct-detail:

    IF fg-rctd.rita-code EQ "R" THEN t-receipt = YES.
    ELSE
    IF fg-rctd.rita-code EQ "I" THEN t-ship   = YES.
    ELSE
    IF fg-rctd.rita-code EQ "T" THEN t-trans   = YES.
    ELSE
    IF fg-rctd.rita-code EQ "A" THEN t-adj     = YES.
    ELSE
    IF fg-rctd.rita-code EQ "E" THEN t-ret     = YES.

    IF t-receipt AND t-ship AND t-trans AND t-adj AND t-ret THEN LEAVE.
  END. */

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     t-receipt:SCREEN-VALUE = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "R"))
     t-ship:SCREEN-VALUE    = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "S"))
     t-trans:SCREEN-VALUE   = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "T"))
     t-adj:SCREEN-VALUE     = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "A"))
     t-ret:SCREEN-VALUE     = STRING(CAN-FIND(FIRST fg-rctd
                                       WHERE fg-rctd.company   EQ gcompany
                                         AND fg-rctd.rita-code EQ "E")).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE orig C-Win 
PROCEDURE orig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq w-fg-rctd.i-no
        use-index i-no no-lock no-error.

    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.

      if itemfg.sell-uom begins "L" then
        v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

      else
      if itemfg.sell-uom eq "CS" then
        v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

      else
      if avail uom then
        v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / uom.mult.

      else
        v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / 1000.

      if w-fg-rctd.rita-code eq "R" then do:
        if v-msf[1] gt w-fg-rctd.t-qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.t-qty * itemfg.t-sqft)).

        v-msf[1] = w-fg-rctd.t-qty * itemfg.t-sqft.
      end.
    end. /* avail itemfg */

    assign
     v-msf[1] = v-msf[1] / 1000
     v-msf[2] = v-msf[2] / 1000.

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
{custom\out2file.i}  

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

  run scr-rpt.w (list-name,frame {&frame-name}:title,int(lv-font-no),lv-ornt). /* open file-name, title */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-and-post C-Win 
PROCEDURE print-and-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.


  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  FOR EACH work-gl:
    DELETE work-gl.
  END.

  FOR EACH work-job:
    DELETE work-job.
  END.

  RUN run-report.

  IF fgpost-cha EQ "Before" OR fgpost-cha EQ "Both" THEN RUN show-report (1).

  choice = CAN-FIND(FIRST w-fg-rctd WHERE w-fg-rctd.has-rec).

  IF choice THEN
  FOR EACH w-fg-rctd
      WHERE w-fg-rctd.has-rec
        AND NOT CAN-FIND(FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id):
    choice = NO.
    LEAVE.
  END.

  IF choice THEN DO:
    choice = NO.
    MESSAGE "Are you ready to post to finished goods?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.
  END.

  ELSE MESSAGE "Sorry, nothing is available for posting..."
           VIEW-AS ALERT-BOX.

  IF choice THEN DO:
    FOR EACH w-fg-rctd
        WHERE w-fg-rctd.has-rec
          AND CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ w-fg-rctd.r-no),
        FIRST fg-rctd
        WHERE ROWID(fg-rctd)    EQ w-fg-rctd.row-id
          AND fg-rctd.rita-code NE "P":
      lv-r-no = fg-rctd.r-no.
      DO TRANSACTION:
        fg-rctd.r-no = 0.
      END.
      DO TRANSACTION:
        fg-rctd.r-no = lv-r-no.
      END.
      w-fg-rctd.r-no = fg-rctd.r-no.
    END.

    FOR EACH w-fg-rctd WHERE w-fg-rctd.has-rec,
        FIRST fg-rctd NO-LOCK WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id,
        FIRST fg-rcpth NO-LOCK WHERE fg-rcpth.r-no EQ fg-rctd.r-no:

      MESSAGE "Sorry, these FG Transactions cannot be processed because 1 or " +
              "more have already been posted by UserID: " +
              TRIM(fg-rcpth.user-id) + "..."
          VIEW-AS ALERT-BOX ERROR.

      choice = NO.
      LEAVE.
    END.
  END.

  IF choice THEN DO: 
    RUN fg-post. 
    MESSAGE "Posting completed..." VIEW-AS ALERT-BOX.
    IF fgpost-cha EQ "After" OR fgpost-cha EQ "Both" THEN RUN show-report (2).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 112}.

{sys/form/r-top3w1.f "Before"}

{sys/form/r-top3w1.f "After"}

DEF VAR ext-cost AS DEC NO-UNDO.
def var type as ch format "X" initial "R".
def var type-prt as ch format "X(11)" init "".
def var v-fg-qty like fg-rctd.t-qty.
def var v-fg-cost as dec format "->,>>>,>>9.99<<".
def var v-tot-qty as int format "->>>,>>>,>>9".
def var v-tot-cost as dec format "->>>,>>9.99<<".
def var v-grd-tot-qty as int format "->>>,>>>,>>9".
def var v-grd-tot-cost as dec format "->>,>>>,>>9.99<<".                     
def var v-grd-tot-value as dec format "->>,>>>,>>9.99<<".                     
def var v-tot-value as dec format "->>,>>>,>>9.99".
def var v-cum-tot as de.                                   
def var v-tran-type as char format "x(1)".      
def var v-entrytype as char initial "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
def var v-on like eb.num-up.
def var v-qty-pallet as decimal format "->>,>>>,>>9" no-undo.
def var v-whse like fg-rctd.loc.            
def var v-one as integer format "->>,>>9" init 1.
def var v-ftime as logical init no.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".
def var v-hdr as char format "x(12)".
def var v-postlst  as cha no-undo.
DEF VAR ll-wip AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li-loop AS INT NO-UNDO.
DEF VAR v-time AS CHAR FORMAT "X(5)" NO-UNDO.

DEF VAR v-itm-lbl  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-itm-dsh  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-desc-lbl AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-lbl   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-lbl AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-desc-dsh AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-dsh   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-dsh AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-uom-lbl  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-uom-dsh  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-cstprt   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-pr-tots2 LIKE v-pr-tots         NO-UNDO.

IF rd-Itm#Cst# EQ 1 
  THEN ASSIGN v-itm-lbl = "ITEM"
              v-itm-dsh = "---------------".
  ELSE ASSIGN v-itm-lbl = "CUSTOMER PART #"
              v-itm-dsh = "---------------".

IF rd-ItmPo EQ 1   
  THEN ASSIGN v-desc-lbl = "DESCRIPTION                           "
              v-Po-lbl   = ""
              v-vend-lbl = ""
              v-desc-dsh = "------------------------------".

  ELSE ASSIGN v-desc-lbl = "DESCRIPTION"
              v-Po-lbl   = "P.O. #"
              v-vend-lbl = "VEND"
              v-desc-dsh = "-------------- --------- --------".

IF rd-UOMJob EQ 1 
  THEN ASSIGN v-uom-lbl = "UOM"
              v-uom-dsh = "----".
  ELSE ASSIGN v-uom-lbl = "JOB #"
              v-uom-dsh = "----------".

FORM HEADER
     SPACE(56) "PRE POST AUDIT TRAIL"
    WITH FRAME before STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
     SPACE(57) "POSTED AUDIT TRAIL"
    WITH FRAME after STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
     "WHSE:"
     v-whse
     SKIP    
     "         TOTAL"   at 128    
     "DATE"             at 1
     "TIME"             AT 10
     TRIM(v-itm-lbl)  FORMAT "x(15)"  at 16
     TRIM(v-desc-lbl) FORMAT "x(11)"  at 32
     TRIM(v-Po-lbl)     at 47
     TRIM(v-vend-lbl)  FORMAT "X(4)" at 57
     "T"                at 63
     "TAG #"            at 65
     "UNITS"            at 88  
     "COUNT"            at 97
     "TOTAL"            at 106
     "BIN"              at 112    
     TRIM(v-uom-lbl) FORMAT "x(10)" at 119
     v-hdr                  at 130
     "--------"             at 1                /*date*/
     "----"                 AT 10               /* time */                
     TRIM(v-itm-dsh)  FORMAT "x(15)" at 16       /*item*/
     TRIM(v-desc-dsh) FORMAT "x(30)" at 32      /*description p.o. # vendor*/
     "-"                    at 63               /*t*/
     "--------------------" at 65               /*tag # 8 -> 20*/
     "-------"              at 86               /*units*/
     "--------"             at 94               /*count*/
     "--------"             at 103              /*total 11->8*/
     "------"               at 112              /*bin  8 -> 6*/    
     TRIM(v-uom-dsh) FORMAT "x(10)" at 119              /*uom*/
     "------------"         at 130              /*total value 14 -> 12*/
    with frame r-top1 STREAM-IO width 170 no-labels no-box no-underline page-top.

/*form #1 Print cases / qty case for TOTAL COST*/
form w-fg-rctd.rct-date             format "99/99/99" 
     v-time                                           
     w-fg-rctd.i-no                 format "x(15)"    
     w-fg-rctd.i-name               format "x(14)"    
     w-fg-rctd.po-no                                  
     po-ord.vend-no                 FORMAT "x(5)"                                  
     v-tran-type                                      
     w-fg-rctd.tag                  FORM "x(20)"      
     w-fg-rctd.cases                format "->>,>>9"  
     w-fg-rctd.qty-case             format "->>>,>>9" 
     v-fg-qty                       format "->>>,>>9" 
     w-fg-rctd.loc-bin              FORM "x(6)"       
     w-fg-rctd.pur-uom              FORMAT "x(9)"     
     v-fg-cost
    with frame itemx no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99" AT 1  
     v-time                                           AT 10 
     w-fg-rctd.i-no                 format "x(15)"    AT 16 
     w-fg-rctd.i-name               format "x(27)"    AT 32 
     v-tran-type                                      AT 63 
     w-fg-rctd.tag                  FORM "x(20)"      AT 65 
     w-fg-rctd.cases                format "->>,>>9"  AT 86 
     w-fg-rctd.qty-case             format "->>>,>>9" AT 94 
     v-fg-qty                       format "->>>,>>9" AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"       AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"     AT 119
     v-fg-cost 
    with frame itemxA no-box down STREAM-IO width 170 no-labels.

/*form #2 Print 1 / partial for TOTAL COST*/
form w-fg-rctd.rct-date             format "99/99/99"
     v-time                                          
     w-fg-rctd.i-no                 format "x(15)"   
     w-fg-rctd.i-name               format "x(14)"   
     w-fg-rctd.po-no                                 
     po-ord.vend-no                 FORMAT "x(5)"                   
     v-tran-type                                     
     w-fg-rctd.tag                FORM "x(20)"       
     v-one                          format "->>,>>9" 
     w-fg-rctd.partial              format "->>>,>>9"
     v-fg-qty                       format "->>>,>>9"
     w-fg-rctd.loc-bin              FORM "x(6)"      
     w-fg-rctd.pur-uom              FORMAT "x(9)"    
     v-fg-cost  
    with frame itempx no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"   AT 1  
     v-time                                             AT 10 
     w-fg-rctd.i-no                 format "x(15)"      AT 16 
     w-fg-rctd.i-name               format "x(27)"      AT 32 
     v-tran-type                                        AT 63 
     w-fg-rctd.tag                  FORM "x(20)"        AT 65 
     v-one                          format "->>,>>9"    AT 86 
     w-fg-rctd.partial              format "->>>,>>9"   AT 94 
     v-fg-qty                       format "->>>,>>9"   AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"         AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"       AT 119
     v-fg-cost                                       
    with frame itempxA no-box down STREAM-IO width 170 no-labels.

/*form #3 Print cases / qty case for TOTAL VALUE*/
form w-fg-rctd.rct-date             format "99/99/99"   
     v-time                                             
     w-fg-rctd.i-no                 format "x(15)"      
     w-fg-rctd.i-name               format "x(14)"      
     w-fg-rctd.po-no                                    
     po-ord.vend-no                 FORMAT "x(5)"                                   
     v-tran-type                                        
     w-fg-rctd.tag                  FORM "x(20)"        
     w-fg-rctd.cases                format "->>,>>9"    
     w-fg-rctd.qty-case             format "->>>,>>9"   
     v-fg-qty                       format "->>>,>>9"   
     w-fg-rctd.loc-bin              FORM "x(6)"         
     w-fg-rctd.pur-uom              FORMAT "x(9)"       
     v-fg-value 
    with frame itemy no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"  AT 1  
     v-time                                            AT 10 
     w-fg-rctd.i-no                 format "x(15)"     AT 16 
     w-fg-rctd.i-name               format "x(27)"     AT 32 
     v-tran-type                                       AT 63 
     w-fg-rctd.tag                  FORM "x(20)"       AT 65 
     w-fg-rctd.cases                format "->>,>>9"   AT 86 
     w-fg-rctd.qty-case             format "->>>,>>9"  AT 94 
     v-fg-qty                       format "->>>,>>9"  AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"        AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"      AT 119
     v-fg-value                                      
    with frame itemyA no-box down STREAM-IO width 170 no-labels.

/*form #4 Print 1 / partial for TOTAL VALUE*/
form w-fg-rctd.rct-date             format "99/99/99"    
     v-time                                              
     w-fg-rctd.i-no                 format "x(15)"       
     w-fg-rctd.i-name               format "x(14)"       
     w-fg-rctd.po-no                                     
     po-ord.vend-no                 FORMAT "x(5)"                                  
     v-tran-type                                         
     w-fg-rctd.tag                                       
     v-one                          format "->>,>>9"     
     w-fg-rctd.partial              format "->>>,>>9"    
     v-fg-qty                       format "->>,>>>,>>9" 
     w-fg-rctd.loc-bin              FORM "x(6)"          
     w-fg-rctd.pur-uom              FORMAT "x(9)"        
     v-fg-value 
    with frame itempy no-box down STREAM-IO width 170 no-labels.

form w-fg-rctd.rct-date             format "99/99/99"    AT 1  
     v-time                                              AT 10 
     w-fg-rctd.i-no                 format "x(15)"       AT 16 
     w-fg-rctd.i-name               format "x(27)"       AT 32 
     v-tran-type                                         AT 63 
     w-fg-rctd.tag                                       AT 65 
     v-one                          format "->>,>>9"     AT 86 
     w-fg-rctd.partial              format "->>>,>>9"    AT 94 
     v-fg-qty                       format "->>,>>>,>>9" AT 103
     w-fg-rctd.loc-bin              FORM "x(6)"          AT 112
     w-fg-rctd.pur-uom              FORMAT "x(9)"        AT 119
     v-fg-value                                       
    with frame itempyA no-box down STREAM-IO width 170 no-labels.


form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" SKIP
    with down STREAM-IO width 130 frame gldetail.    

{ce/msfcalc.i}

SESSION:SET-WAIT-STATE ("general").

IF length(begin_job-no) < 6 THEN
   begin_job-no = FILL(" ",6 - LENGTH(trim(begin_job-no))) + TRIM(begin_job-no).
IF length(end_job-no) < 6 THEN
   end_job-no = FILL(" ",6 - LENGTH(trim(end_job-no))) + TRIM(end_job-no).

/* Only save user selections if the UI was enabled */
IF ip-run-what EQ "" THEN
  DISPLAY begin_job-no END_job-no WITH FRAME {&FRAME-NAME}.

ASSIGN
 str-tit2 = CURRENT-WINDOW:TITLE
 {sys/inc/ctrtext.i str-tit2 112}
 str-tit3 = "Period Date: " + string(v-post-date,"99/99/9999") + "             Posted by: " + USERID('nosweat') + "  As of " + string(TODAY,"99/99/9999")
 {sys/inc/ctrtext.i str-tit3 132}

 v-postlst   = (IF t-receipt THEN "R," ELSE "") +
               (IF t-setup THEN "I," ELSE "") +
               (IF t-ship THEN "S," ELSE "") +
               (IF t-trans THEN "T," ELSE "") +
               (IF t-adj THEN "A," ELSE "") +
               (IF t-ret THEN "E," ELSE "")
 v-cost-sell = rd_print EQ "C"
 v-pr-tots2  = tb_totCstVal
 v-pr-tots   = tb_grndtotal.

IF LENGTH(v-postlst) GT 0 AND
   SUBSTR(v-postlst,LENGTH(v-postlst),1) EQ "," THEN
   SUBSTR(v-postlst,LENGTH(v-postlst),1) = "".

DO li = 1 TO 2:
  {sys/inc/print1.i}
  lv-list-name[li] = list-name.
  PAUSE 1 NO-MESSAGE.
END.

OUTPUT STREAM before TO VALUE(lv-list-name[1]) PAGE-SIZE VALUE(lines-per-page).
OUTPUT STREAM after  TO VALUE(lv-list-name[2]) PAGE-SIZE VALUE(lines-per-page).

 if td-show-parm then run show-param.

FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid NO-ERROR.

IF AVAIL fg-rctd THEN RUN build-tables.

ELSE
DO li-loop = 1 TO NUM-ENTRIES(v-postlst):
  FOR EACH fg-rctd
      WHERE fg-rctd.company   EQ gcompany
        AND fg-rctd.rita-code EQ ENTRY(li-loop,v-postlst)
        AND fg-rctd.r-no      GE begin_fg-r-no
        AND fg-rctd.r-no      LE end_fg-r-no
        AND fg-rctd.i-no      GE begin_i-no
        AND fg-rctd.i-no      LE end_i-no
        AND fg-rctd.rct-date  GE ldt-from
        AND fg-rctd.rct-date  LE ldt-to
        AND fg-rctd.job-no    GE begin_job-no
        AND fg-rctd.job-no    LE end_job-no
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.loc       GE begin_whs
        AND fg-rctd.loc       LE end_whs
        AND begin_userid      LE "" 
        AND end_userid      GE "" 
        AND fg-rctd.created-by GE begin_userid
        AND fg-rctd.created-by LE end_userid     

      USE-INDEX rita-code:

    RUN build-tables.
  END.
END.

if v-cost-sell then do:
  v-hdr = "        COST".

  IF tb_excel THEN 
  DO:
    OUTPUT STREAM excel TO VALUE(fi_file).

    IF rd-ItmPo EQ 1
      THEN ASSIGN excelheader = "Date,Time,Item,Description,".
      ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

    ASSIGN excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".
    IF rd-UOMJob EQ 1 
      THEN ASSIGN excelheader = excelheader + "UOM,Total Cost".
      ELSE ASSIGN excelheader = excelheader + "Job #,Total Cost".

    PUT STREAM excel UNFORMATTED excelheader SKIP.

  END.

   IF rd-ItmPo EQ 1 THEN DO:
     {fg/rep/fg-post.i "itemxA" "v-fg-cost" "itempxA" "v-tot-cost"}
   END.
   ELSE DO:
     {fg/rep/fg-post.i "itemx" "v-fg-cost" "itempx" "v-tot-cost"}
   END.
end.
else do:
  v-hdr = "       VALUE".

  IF tb_excel THEN 
  DO:
    OUTPUT STREAM excel TO VALUE(fi_file).

    IF rd-ItmPo EQ 1
      THEN ASSIGN excelheader = "Date,Time,Item,Description,".
      ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

    ASSIGN excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".

    IF rd-UOMJob EQ 1 
      THEN ASSIGN excelheader = excelheader + "UOM,Total Value".
      ELSE ASSIGN excelheader = excelheader + "Job #,Total Value".

    PUT STREAM excel UNFORMATTED excelheader SKIP.
  END.

  IF rd-ItmPo EQ 1 THEN DO:
   {fg/rep/fg-post.i "itemyA" "v-fg-value" "itempyA" "v-tot-value"}
  END.
  ELSE DO:
   {fg/rep/fg-post.i "itemy" "v-fg-value" "itempy" "v-tot-value"}
  END.
end.

if v-pr-tots then do:
  if v-cost-sell then DO:                   
    PUT STREAM before
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],"->>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],"->>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
                    format "x(63)" at 15
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-cost to 141 skip. 

    PUT STREAM after
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],"->>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],"->>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
                    format "x(63)" at 15 
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-cost to 141 skip.     
  END.
  ELSE DO:
    PUT STREAM before
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],"->>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],"->>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
                    format "x(63)" at 15 
        "GRAND TOTALS:" to 100
        v-grd-tot-qty to 113 v-grd-tot-value to 144 skip.

    PUT STREAM after
        " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],"->>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],"->>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
                    format "x(63)" at 15 
        "GRAND TOTALS:" to 97
        v-grd-tot-qty to 110 v-grd-tot-value to 141 skip.
  END.
end. /* if v-pr-tots */

HIDE FRAME r-top1.

if tb_glnum THEN DO:
  PAGE STREAM before.
  PAGE STREAM after.

  for each work-gl break by work-gl.actnum:

    find first account
        where account.company eq cocode
          and account.actnum  eq work-gl.actnum
        no-lock no-error.

    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-gl.actnum
     v-disp-actnum = work-gl.actnum
     v-disp-amt    = work-gl.debits - work-gl.credits.

    display STREAM before
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM before with frame gldetail.

    display STREAM after
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM after with frame gldetail.
  end. /* each work-job */

  for each work-job break by work-job.actnum:

    find first account
        where account.company eq cocode
          and account.actnum  eq work-job.actnum
        no-lock no-error.

    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-job.actnum
     v-disp-actnum = work-job.actnum.

    if work-job.fg then
      v-disp-amt = - work-job.amt.
    else
      v-disp-amt = work-job.amt.

    display STREAM before
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM before with frame gldetail.

    display STREAM after
            v-disp-actnum v-dscr udate v-disp-amt
          with frame gldetail.
    down STREAM after with frame gldetail.
  end. /* each work-job */
END.

OUTPUT STREAM before CLOSE.
OUTPUT STREAM after  CLOSE.

IF tb_excel THEN 
  DO:
      OUTPUT STREAM excel CLOSE.
      IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

IF ip-run-what EQ "" THEN
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-fgemail C-Win 
PROCEDURE send-fgemail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-fgemail-file AS cha .

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     v-dir = users.user_program[2] + "\".
  ELSE
     v-dir = "c:\tmp\".

   FOR EACH tt-email,
       FIRST cust NO-LOCK WHERE cust.company = g_company
                           AND cust.cust-no = tt-email.cust-no
                           AND cust.active = "E" BREAK BY tt-email.cust-no:
       IF FIRST-OF(tt-email.cust-no) THEN DO:
          v-fgemail-file = v-dir + trim(tt-email.cust-no) + ".txt".
          OUTPUT STREAM st-email TO VALUE(v-fgemail-file).
          PUT STREAM st-email "JOB#       FG Item#              Qty    " SKIP
                              "========== =============== ============ " SKIP.
       END.
       PUT STREAM st-email UNFORMATTED
                 tt-email.job-no + "-" + string(tt-email.job-no2,"99") FORM "x(10)"
                 " " tt-email.i-no " " tt-email.qty FORM "->>>,>>>,>>9" 
                 SKIP.
       IF LAST-OF(tt-email.cust-no) THEN do:
           OUTPUT STREAM st-email CLOSE.
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
           IF ls-to-list NE '' THEN DO:
             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Finished Goods Receipts have been posted"
                    lv-mailbody = "Finished Goods Receipts have been posted"
                    lv-mailattach = v-fgemail-file.
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,lv-mailattach,1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
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

  PUT STREAM before space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      PUT STREAM before lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  PUT STREAM before fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-report C-Win 
PROCEDURE show-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  list-name = lv-list-name[ip-int].

  IF ip-run-what EQ "" THEN
  DO WITH FRAME {&FRAME-NAME}:
      case rd-dest :
          when 1 then run output-to-printer.
          when 2 then run output-to-screen.
          when 3 then run output-to-file.
          when 4 then do:
              /*run output-to-fax.*/
              {custom/asifax.i &type= "Customer"
                               &begin_cust=v-trans-lbl
                               &END_cust= v-trans-lbl
                               &fax-subject="FRAME {&FRAME-NAME}:TITLE"
                               &fax-body="FRAME {&FRAME-NAME}:TITLE"
                               &fax-file=list-name }
          END. 
          when 5 then do:
              IF is-xprint-form THEN DO:
                 {custom/asimail.i &TYPE = "Customer"
                                &begin_cust= v-trans-lbl
                                &END_cust=v-trans-lbl
                                &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                &mail-file=list-name }
              END.
              ELSE DO:
                  {custom/asimailr.i &TYPE = "Customer"
                                     &begin_cust= v-trans-lbl
                                     &END_cust=v-trans-lbl
                                     &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-file=list-name }

              END.
          END. 
         WHEN 6 THEN RUN output-to-port.
      end case. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

