&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-ariprt.w

  Description: Print Invoices

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

{ar/rep/invoice.i "new"}

DEF VAR v-program AS CHAR NO-UNDO.
{custom/xprint.i}
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR lv-prt-bypass AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF VAR lv-multi-faxout AS LOG NO-UNDO.  /*for faxing to multiple receipents */
DEF VAR lv-fax-image AS cha NO-UNDO.  /* fax imge file */

DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF VAR v-org-lines-per-page AS INT NO-UNDO.
DEF VAR vcInvNums AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS CHAR NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.

DEF VAR vcBOLFiles   AS CHAR NO-UNDO.
DEF VAR vcBOLSignDir AS CHAR NO-UNDO.
DEF VAR v-rec-found  AS LOG  NO-UNDO.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

DEF BUFFER b-ar-inv FOR ar-inv.
DEF BUFFER b-broker-bol FOR reftable.

/* Build a Table to keep sequence of pdf files */
DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.

DEF NEW SHARED VAR nsv_setcomp AS LOGICAL NO-UNDO.
DEF NEW SHARED VAR s-print-zero-qty AS LOG NO-UNDO.

/* br Task 12081002 - to pass which item to print on invoice */
DEFINE NEW SHARED VARIABLE svi-print-item AS INTEGER INITIAL 2 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_inv end_inv begin_cust ~
end_cust begin_date end_date tb_reprint tb_posted tb_export rd-dest ~
td-show-parm tb_email-orig tb_AttachBOL btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_inv end_inv begin_cust end_cust ~
begin_date end_date tb_reprint tb_posted tb_export lv-ornt lines-per-page ~
rd-dest lv-font-no lv-font-name td-show-parm tb_email-orig tb_AttachBOL 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_broker-bol AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Broker BOL#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE fi_depts AS CHARACTER FORMAT "X(100)" 
     VIEW-AS FILL-IN 
     SIZE 35.6 BY 1.

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

DEFINE VARIABLE lv-scr-num-copies AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# of Copies" 
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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rs_no_PN AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item #", 1,
"Customer PN", 2
     SIZE 55.4 BY .86 TOOLTIP "Select to print Item # or Customer PN on invoice." NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.86.

DEFINE VARIABLE tb_AttachBOL AS LOGICAL INITIAL no 
     LABEL "Attach Signed BOL" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_collate AS LOGICAL INITIAL no 
     LABEL "Collate?" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-copy AS LOGICAL INITIAL no 
     LABEL "Customer Copy?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_email-orig AS LOGICAL INITIAL no 
     LABEL "Email as Original?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_export AS LOGICAL INITIAL no 
     LABEL "Export/FTP  Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_office-copy AS LOGICAL INITIAL no 
     LABEL "Office Copy?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-dept AS LOGICAL INITIAL no 
     LABEL "Print Dept Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-zero-qty AS LOGICAL INITIAL yes 
     LABEL "Print if Inv/Ship Qty = 0?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sman-copy AS LOGICAL INITIAL no 
     LABEL "SalesRep Copy?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_inv AT ROW 2.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 2.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_cust AT ROW 3.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 3.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_date AT ROW 4.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning BOL Date"
     end_date AT ROW 4.81 COL 70 COLON-ALIGNED HELP
          "Enter Ending BOL Date"
     tb_reprint AT ROW 6.24 COL 36
     lv-scr-num-copies AT ROW 6.24 COL 74.8 COLON-ALIGNED
     tb_posted AT ROW 7.19 COL 63 RIGHT-ALIGNED
     tb_collate AT ROW 7.19 COL 64.6
     tb_export AT ROW 8.14 COL 36
     fi_depts AT ROW 9.05 COL 56 COLON-ALIGNED HELP
          "Enter Deprtments separated by commas" NO-LABEL
     tb_print-dept AT ROW 9.1 COL 56 RIGHT-ALIGNED
     tb_prt-zero-qty AT ROW 10.19 COL 42.8 RIGHT-ALIGNED WIDGET-ID 16
     fi_broker-bol AT ROW 10.19 COL 56 COLON-ALIGNED HELP
          "Enter Broker BOL#" WIDGET-ID 6
     rs_no_PN AT ROW 11.52 COL 35.6 NO-LABEL WIDGET-ID 12
     tb_cust-copy AT ROW 12.67 COL 8
     tb_office-copy AT ROW 12.67 COL 36
     tb_sman-copy AT ROW 12.67 COL 64
     lv-ornt AT ROW 14.29 COL 31 NO-LABEL
     lines-per-page AT ROW 14.29 COL 84 COLON-ALIGNED
     rd-dest AT ROW 15.38 COL 7 NO-LABEL
     lv-font-no AT ROW 15.71 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 16.67 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.57 COL 30
     tb_email-orig AT ROW 19.91 COL 30 WIDGET-ID 18
     tb_AttachBOL AT ROW 20.05 COL 57 WIDGET-ID 20
     btn-ok AT ROW 22.71 COL 24
     btn-cancel AT ROW 22.71 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.43 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     RECT-6 AT ROW 14.24 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 23.57.


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
         TITLE              = "Invoicing"
         HEIGHT             = 23.81
         WIDTH              = 95.8
         MAX-HEIGHT         = 34.62
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.62
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_broker-bol IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_broker-bol:HIDDEN IN FRAME FRAME-A           = TRUE
       fi_broker-bol:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_depts IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_depts:HIDDEN IN FRAME FRAME-A           = TRUE
       fi_depts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-scr-num-copies IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-scr-num-copies:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET rs_no_PN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rs_no_PN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_collate IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_collate:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_collate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_cust-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_cust-copy:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_email-orig:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_export:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_export:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_office-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_office-copy:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_posted IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_print-dept IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       tb_print-dept:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_print-dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-zero-qty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       tb_prt-zero-qty:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_prt-zero-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sman-copy IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_sman-copy:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Invoicing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Invoicing */
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice# */
DO:
     assign {&self-name}.

     RUN set-broker-bol-proc.
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
  DEF VAR lv-fax-type AS cha NO-UNDO.
  DEF VAR ll-ans AS LOG INIT NO NO-UNDO.
  DEF VAR vcSubject AS CHAR  NO-UNDO.
  DEF VAR vcMailBody AS CHAR NO-UNDO.
  DEF VAR vcErrorMsg AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}
           tb_collate lv-scr-num-copies
           tb_cust-copy tb_office-copy tb_sman-copy fi_broker-bol.
  END.

  IF rs_no_PN:HIDDEN = NO THEN
     ASSIGN rs_no_PN
            svi-print-item = rs_no_PN.

  IF tb_prt-zero-qty:HIDDEN = NO THEN
     ASSIGN tb_prt-zero-qty
            s-print-zero-qty = tb_prt-zero-qty.

  IF fi_broker-bol:HIDDEN = NO AND
     fi_broker-bol:SCREEN-VALUE NE "" AND
     begin_inv NE end_inv THEN
     DO:
        MESSAGE "For Broker BOL# to be used, Beginning and Ending Invoice# must be the same."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        LEAVE.
     END.

  IF rd-dest EQ 1 AND v-print-fmt EQ "Harwell" THEN
    MESSAGE "Is this a laser printer?:" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-ans.
  IF ll-ans THEN lines-per-page = lines-per-page - 4.

  lv-multi-faxout = IF rd-dest = 4 AND begin_cust <> END_cust THEN YES 
                    ELSE NO.
  IF is-xprint-form AND rd-dest = 4 THEN lv-multi-faxout = YES.

  lv-fax-type = IF lv-multi-faxout THEN "MULTI" ELSE "CUSTOMER".

  CASE rd-dest:
      WHEN 1 THEN ASSIGN LvOutputSelection = "Printer".
      WHEN 2 THEN ASSIGN LvOutputSelection = "Screen". 
      WHEN 3 THEN ASSIGN LvOutputSelection = "File".
      WHEN 4 THEN ASSIGN LvOutputSelection = "Fax".
      WHEN 5 THEN ASSIGN LvOutputSelection = "Email"
                         lv-pdf-file = init-dir + "\Inv".
      WHEN 6 THEN ASSIGN LvOutputSelection = "Port".
  END CASE.

  IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "INVPRINT") THEN
     DO:
        FOR EACH b-ar-inv FIELDS(cust-no ship-id) WHERE
            b-ar-inv.company  EQ cocode AND
            b-ar-inv.inv-no   GE begin_inv AND
            b-ar-inv.inv-no   LE end_inv AND
            b-ar-inv.cust-no  GE begin_cust AND
            b-ar-inv.cust-no  LE end_cust AND
            (b-ar-inv.posted  EQ NO OR
             b-ar-inv.posted  EQ tb_posted) AND
            b-ar-inv.printed  EQ tb_reprint AND
            b-ar-inv.inv-date GE begin_date AND
            b-ar-inv.inv-date LE end_date AND
            CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no EQ b-ar-inv.x-no)
            NO-LOCK
            BREAK BY b-ar-inv.cust-no 
                  BY b-ar-inv.ship-id:

            IF FIRST-OF(b-ar-inv.ship-id) THEN
            DO:
               FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                    sys-ctrl-shipto.ship-id = b-ar-inv.ship-id AND
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.

               IF NOT AVAIL sys-ctrl-shipto THEN
                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "INVPRINT" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

               IF AVAIL sys-ctrl-shipto THEN
               DO:
                  RUN SetInvForm(sys-ctrl-shipto.char-fld).
                  v-print-fmt = sys-ctrl-shipto.char-fld.

               END.
               ELSE
               DO:
                  RUN SetInvForm(vcDefaultForm).
                  v-print-fmt = vcDefaultForm.

               END.

               RUN run-report(b-ar-inv.cust-no,b-ar-inv.ship-id, TRUE).
               RUN GenerateReport(INPUT lv-fax-type,
                                  INPUT b-ar-inv.cust-no,
                                  INPUT b-ar-inv.cust-no,
                                  INPUT lv-fax-image).
            END.

        END.
     END.
  ELSE /* not find sys-ctrl-shipto*/
  DO:
     v-print-fmt = vcDefaultForm.

     RUN run-report("","", FALSE).
     RUN GenerateReport(INPUT lv-fax-type,
                        INPUT begin_cust,
                        INPUT end_cust,
                        INPUT lv-fax-image).
  END.

  IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.
  OS-DELETE value(init-dir + "\Invoice.pdf").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
DO:
   assign {&self-name}.
   IF begin_inv = END_inv THEN DO:
     FIND FIRST ar-inv WHERE ar-inv.company = g_company
                         AND ar-inv.inv-no = begin_inv NO-LOCK NO-ERROR.
     IF AVAIL ar-inv THEN ASSIGN begin_cust:SCREEN-VALUE = ar-inv.cust-no
                                 end_cust:SCREEN-VALUE = ar-inv.cust-no
                                begin_date:SCREEN-VALUE = string(ar-inv.inv-date)
                                end_date:SCREEN-VALUE = string(ar-inv.inv-date).

     RUN set-broker-bol-proc.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_depts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_depts C-Win
ON LEAVE OF fi_depts IN FRAME FRAME-A
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


&Scoped-define SELF-NAME lv-scr-num-copies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-scr-num-copies C-Win
ON LEAVE OF lv-scr-num-copies IN FRAME FRAME-A /* # of Copies */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
  RUN setEmailBoxes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs_no_PN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs_no_PN C-Win
ON VALUE-CHANGED OF rs_no_PN IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_collate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_collate C-Win
ON VALUE-CHANGED OF tb_collate IN FRAME FRAME-A /* Collate? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_email-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_email-orig C-Win
ON VALUE-CHANGED OF tb_email-orig IN FRAME FRAME-A /* Email as Original? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_export C-Win
ON VALUE-CHANGED OF tb_export IN FRAME FRAME-A /* Export/FTP  Invoices? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted Invoices? */
DO:
  assign {&self-name}.
  IF tb_posted THEN ASSIGN tb_export:SENSITIVE = YES
                           tb_reprint:SCREEN-VALUE = "YES".
  ELSE ASSIGN tb_export:SCREEN-VALUE = "NO"
              tb_export:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-dept C-Win
ON VALUE-CHANGED OF tb_print-dept IN FRAME FRAME-A /* Print Dept Notes? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-zero-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-zero-qty C-Win
ON VALUE-CHANGED OF tb_prt-zero-qty IN FRAME FRAME-A /* Print if Inv/Ship Qty = 0? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Invoices? */
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

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.

  if not avail sys-ctrl then
  do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "INVPRINT"
     sys-ctrl.descrip = "Print Invoice Headers on Invoice Form?".
    message sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  ASSIGN
   v-print-head = sys-ctrl.log-fld
   v-print-fmt  = sys-ctrl.char-fld
   vcDefaultForm = v-print-fmt.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  do transaction:
    {sys/inc/inexport.i}
  end.

  RUN sys/ref/nk1look.p (cocode, "BOLSign", "C", no, no, "", "", 
                         Output vcBOLSignDir, output v-rec-found).

  RUN SetInvForm(v-print-fmt).

  /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */      
  IF lookup(v-print-fmt,"Capitol,APC,AllWest,Bell,Loylang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2") > 0
  THEN DO:

      ASSIGN
        fi_broker-bol:HIDDEN = NO
        fi_broker-bol:SENSITIVE = YES.

  END.

/* /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt Cut free */      
  IF v-print-fmt EQ "APC"   OR v-print-fmt EQ "AllWest"   OR v-print-fmt EQ "Loylang"   OR v-print-fmt EQ "Soule"   OR v-print-fmt EQ "SouleMed" 
  OR v-print-fmt EQ "SoulePO"   OR v-print-fmt EQ "LoylangJIT"   OR v-print-fmt EQ "LoylangBSF"    OR v-print-fmt EQ "Printers"   OR v-print-fmt EQ "Protagon" 
  OR v-print-fmt EQ "Protagon2" THEN
     ASSIGN
        fi_broker-bol:HIDDEN = NO
        fi_broker-bol:SENSITIVE = YES.
*/

  RUN enable_UI.

  IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis,BlueRx,ColoniaX,ABC,knight,knight1,Central,Rosmar,ACPI,ColorX,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree,PremierS") > 0 THEN DO:
     ASSIGN tb_cust-copy:HIDDEN IN FRAME {&FRAME-NAME} = NO
            tb_cust-copy:SENSITIVE = YES
            tb_office-copy:HIDDEN = NO
            tb_office-copy:SENSITIVE = YES
            tb_sman-copy:HIDDEN = NO
            tb_sman-copy:SENSITIVE = YES.
  END.

  IF LOOKUP(v-print-fmt,"Peachtreefgl3,SouleMed,SoulePO,Peachtree") GT 0 THEN
     ASSIGN rs_no_PN:HIDDEN = FALSE
            rs_no_PN:SENSITIVE = TRUE.

  RUN set-broker-bol-proc.

  IF v-print-fmt EQ "Fibrex" THEN
     ASSIGN
        tb_collate:HIDDEN = NO
        tb_collate:SENSITIVE = YES
        lv-scr-num-copies:HIDDEN = NO
        lv-scr-num-copies:SENSITIVE = YES.

  ELSE IF v-print-fmt EQ "XPRINT" OR v-print-fmt EQ "Boss" OR
          v-print-fmt EQ "SIMKINS" OR v-print-fmt EQ "CapCityIN" THEN
     ASSIGN
        tb_print-dept:HIDDEN = NO
        tb_print-dept:SENSITIVE = YES
        fi_depts:HIDDEN = NO
        fi_depts:SENSITIVE = YES.

  IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "Coburn" OR v-print-fmt EQ "PremierS" OR v-print-fmt EQ "Axis" THEN
     ASSIGN
        tb_prt-zero-qty:SENSITIVE = YES
        tb_prt-zero-qty:HIDDEN = NO.

  {methods/nowait.i}
  v-org-lines-per-page = lines-per-page .
    DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_inv.
    RUN setEmailBoxes.
    lines-per-page:SCREEN-VALUE = string(v-org-lines-per-page).
    IF LOOKUP(v-print-fmt,"frankstn,Mirpkg,DEE") > 0 THEN 
       ASSIGN tb_export:HIDDEN = NO
              tb_export = NO
              tb_export:SCREEN-VALUE = "NO"
              tb_export:SENSITIVE = IF tb_posted:SCREEN-VALUE = "yes" THEN YES ELSE NO .
    ELSE ASSIGN tb_export = no
                tb_export:SCREEN-VALUE = "NO"
                tb_export:HIDDEN = YES.
    DISABLE lines-per-page.
    IF tb_posted:SCREEN-VALUE = 'yes' THEN tb_reprint:SCREEN-VALUE = "YES".
    ASSIGN
        tb_email-orig:SCREEN-VALUE = "NO"
        tb_email-orig = NO.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY begin_inv end_inv begin_cust end_cust begin_date end_date tb_reprint 
          tb_posted tb_export lv-ornt lines-per-page rd-dest lv-font-no 
          lv-font-name td-show-parm tb_email-orig tb_AttachBOL 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_inv end_inv begin_cust end_cust begin_date 
         end_date tb_reprint tb_posted tb_export rd-dest td-show-parm 
         tb_email-orig tb_AttachBOL btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER lv-fax-type AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-begin-cust AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-end-cust AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER lv-fax-image AS CHAR NO-UNDO.

   DEF VAR vcSubject AS CHAR NO-UNDO.
   DEF VAR vcMailBody AS CHAR NO-UNDO.
   DEF VAR vcErrorMsg AS CHAR NO-UNDO.

   IF v-print-fmt <> "Southpak-xl" AND v-print-fmt <> "PrystupExcel" THEN
   DO:
      case rd-dest:
          when 1 then run output-to-printer.
          when 2 then run output-to-screen.
          when 3 then run output-to-file.
          when 4 then do:
              IF lv-fax-type = "MULTI" THEN DO:
                 RUN output-to-fax-prt. /* create tif file */              
                 {custom/asifaxm3.i &TYPE="MULTI"
                               &begin_cust=ip-begin-cust
                               &END_cust=ip-end-cust
                               &fax-subject="Invoice"
                               &fax-body="Invoice"
                               &fax-file=lv-fax-image
                               &end-widget=end_cust }      
              END.
              ELSE DO:
                   {custom/asifax3.i &TYPE="CUSTOMER"
                         &begin_cust=ip-begin-cust
                         &END_cust=ip-end-cust
                         &fax-subject="Invoice"
                         &fax-body="Invoice"
                         &fax-file=lv-fax-image
                         &end-widget=end_cust }      
              END.                  
          END. 
          WHEN 6 THEN RUN output-to-port.
       end case. 
   END.

   case rd-dest:
      when 5 then do:

         IF is-xprint-form THEN DO:

            IF v-print-fmt EQ "Southpak-xl" OR v-print-fmt EQ "PrystupExcel" THEN
            DO:
                 OS-DELETE value(init-dir + "\Invoice").
                 OS-COPY value(init-dir + "\Invoice.pdf") value(init-dir + "\Invoice").
                 ASSIGN list-name = init-dir + "\Invoice".
            END.

            IF v-print-fmt NE "Southpak-xl" AND v-print-fmt NE "PrystupExcel" THEN
            DO:
               RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
               ASSIGN
                  lv-pdf-file = lv-pdf-file + vcInvNums + '.pdf'.
                  list-name = lv-pdf-file.
            END.
            ELSE
               list-name = list-name + ".pdf".

            IF tb_attachBOL THEN
              list-name = list-name + "," + TRIM(vcBOLfiles, ",").

            ASSIGN
              vcSubject = "INVOICE:" + vcInvNums + '   ' + STRING (TODAY, '99/99/9999') + ' ' + STRING (TIME, 'HH:MM:SS AM')
              vcSubject   = IF tb_reprint AND NOT tb_email-orig THEN '[REPRINT] ' + vcSubject ELSE vcSubject
              vcMailBody  = "Please review attached Invoice(s) for Invoice #: " + vcInvNums.

            RUN custom/xpmail2.p(input 'Customer' ,
                                 INPUT 'R-INVPRT.' ,
                                 input list-name,
                                 input begin_cust,
                                 input vcSubject,
                                 input vcMailBody,
                                 OUTPUT vcErrorMsg).                

         END.
         ELSE DO:
             {custom/asimailR.i &TYPE="CUSTOMER"
                              &begin_cust=ip-begin-cust
                              &END_cust=ip-end-cust
                              &mail-subject="Invoice"
                              &mail-body="Invoice"
                              &mail-file=list-name }
         END.

      END.
  end case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax-prt C-Win 
PROCEDURE output-to-fax-prt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-file-name AS cha FORM "x(60)" NO-UNDO.
  DEF VAR lv-xpr-file AS cha FORM "x(60)" NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
     REPEAT:
        SET lv-file-name.
        IF lv-file-name <> "." AND lv-file-name <> ".."  AND lv-file-name MATCHES "*xpr*" 
        THEN DO:
             lv-xpr-file = "c:\temp\fax\" + lv-file-name.             
             RUN printfile (lv-xpr-file).
        END.
        lv-file-name = "".   
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
  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN DO:
       RUN custom/d-print.w (list-name).
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
 ELSE 
    run custom/scr-rpt2.w (list-name,c-win:title,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ ar/rep/invoice.p  9/94 RM */
/* PRINT INVOICE - A/R MODULE                                                 */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-ship-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

DEF VAR lv-copy# AS INT NO-UNDO.

{sys/form/r-top.i}

ASSIGN
 finv       = begin_inv
 tinv       = end_inv
 v-print    = tb_reprint
 v-posted   = tb_posted.

 IF ip-sys-ctrl-shipto THEN
    ASSIGN
       fcust = ip-cust-no
       tcust = ip-cust-no.
 ELSE
    ASSIGN
       fcust = begin_cust
       tcust = end_cust.

 IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
    ASSIGN
       v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
       v-depts = fi_depts:SCREEN-VALUE.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

{sa/sa-sls01.i}

v-term-id = v-term.

SESSION:SET-WAIT-STATE ("general").

vcInvNums = "".

FOR EACH ar-inv
    WHERE ar-inv.company  EQ cocode
      AND ar-inv.inv-no   GE finv
      AND ar-inv.inv-no   LE tinv
      AND ar-inv.cust-no  GE fcust
      AND ar-inv.cust-no  LE tcust
      AND (ar-inv.posted  EQ NO OR
           ar-inv.posted  EQ v-posted)
      AND ar-inv.printed  EQ v-print
      AND ar-inv.inv-date GE begin_date
      AND ar-inv.inv-date LE end_date
      AND (ar-inv.ship-id  EQ ip-ship-no OR ip-ship-no = "")
      AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no)
    USE-INDEX inv-no NO-LOCK:

  /* wfk - can be implemented if multi-invoice is know from ar-inv */
  /*
  if multi invoice then ...

  IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string(b-inv-head.bol-no) + ".pdf") NE ?  THEN 
    vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string(b-inv-head.bol-no) + ".pdf").
  */
  vcBOLFiles = "".
  FOR EACH ar-invl NO-LOCK 
    WHERE ar-invl.x-no  eq ar-inv.x-no 
    BREAK BY ar-invl.bol-no:

    IF FIRST-OF(ar-invl.bol-no) THEN DO:    
      IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string(ar-invl.bol-no) + ".pdf") NE ?  THEN 
      vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string(ar-invl.bol-no) + ".pdf").
    END.

  END.

  CREATE report.
  ASSIGN
   report.term-id = v-term-id
   report.key-01  = STRING(ar-inv.inv-no,"9999999999")
   report.rec-id  = RECID(ar-inv)
   vcInvNums      = vcInvNums + '-' + STRING (ar-inv.inv-no)
   vcInvNums      = LEFT-TRIM (vcInvNums, '-').

  IF vcInvNums MATCHES '*-*' THEN
     vcInvNums = RIGHT-TRIM (SUBSTRING (vcInvNums, 1, INDEX (vcInvNums,'-')), '-') + SUBSTRING (vcInvNums, R-INDEX (vcInvNums, '-')).

END.

v-lines-per-page = lines-per-page.

IF v-print-fmt NE "Fibrex" THEN
DO:
   find first sys-ctrl WHERE
        sys-ctrl.company eq cocode AND
        sys-ctrl.name    eq "INVCOPYS"
        NO-LOCK NO-ERROR.

   lv-copy# = IF AVAIL sys-ctrl AND sys-ctrl.int-fld <> 0 THEN sys-ctrl.int-fld ELSE 1.
END.
ELSE
   lv-copy# = lv-scr-num-copies.


  /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */      
  IF lookup(v-print-fmt,("Capitol,APC,AllWest,Bell,Loylang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2")) > 0 
  AND begin_inv EQ end_inv
  THEN DO:

/*
IF fi_broker-bol:HIDDEN = NO AND
   (v-print-fmt = "APC" OR
    v-print-fmt = "AllWest" OR 
    v-print-fmt EQ "Loylang" OR
    v-print-fmt EQ "LoylangBSF" OR
    v-print-fmt EQ "Protagon" OR
    v-print-fmt EQ "Protagon2" OR
    v-print-fmt EQ "Soule" OR
    v-print-fmt EQ "SouleMed" OR
    v-print-fmt EQ "SoulePO" OR
    v-print-fmt EQ "Printers" OR
    v-print-fmt EQ "LoylangJIT") AND
   begin_inv EQ end_inv THEN
   DO:
*/
      FIND FIRST b-broker-bol WHERE
           b-broker-bol.reftable EQ "brokerbol" AND
           b-broker-bol.CODE EQ STRING(begin_inv)
           NO-ERROR.

      IF NOT AVAIL b-broker-bol AND
         fi_broker-bol:SCREEN-VALUE NE "" THEN
         DO:
             CREATE b-broker-bol.
             ASSIGN
                b-broker-bol.reftable = "brokerbol"
                b-broker-bol.CODE = STRING(begin_inv).
         END.

      IF AVAIL b-broker-bol THEN
      DO:
         b-broker-bol.code2 = fi_broker-bol:SCREEN-VALUE.
         RELEASE b-broker-bol.
      END.
   END.   

IF is-xprint-form THEN DO:

   DO WITH FRAME {&FRAME-NAME}:
      IF v-print-fmt EQ "Fibrex" AND
         tb_collate:HIDDEN EQ NO AND tb_collate THEN
         PUT "<COLLATE=YES,ALWAYS>".
   END.

   CASE rd-dest :
        WHEN 1 THEN PUT "<COPIES=" + string(lv-copy#) + "><PRINTER?>" FORM "x(30)".
        WHEN 2 THEN do:
            IF NOT lBussFormModle THEN
              PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW><MODAL=NO>" FORM "x(30)".
            ELSE
              PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
        END.        
        WHEN 4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN DO:

            IF v-print-fmt EQ "CentBox" THEN
               PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
            ELSE IF v-print-fmt EQ "Southpak-XL" OR v-print-fmt EQ "PrystupExcel" THEN
               PUT "<PREVIEW><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
            ELSE IF v-print-fmt EQ "Protagon" OR v-print-fmt = "Protagon2" THEN
              PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-LEFT=0.5mm><PDF-TOP=-0.5mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
            ELSE
               PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
        END.
   END CASE.
   PUT "</PROGRESS>".
END.

IF LOOKUP(v-print-fmt,"SOUTHPAK,Southpak-xl,PrystupExcel,ASIXprnt,Badger,Badger-Emailed,Southpakl") > 0 THEN DO: 

    RUN value(v-program) (lv-multi-faxout,lines-per-page).     

END.
ELSE IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
    PUT CONTROL CHR(27) CHR(67) CHR(44). 
    RUN value(v-program). 
    PUT CONTROL CHR(18).
END.
/* br Task 12081002 */
/* br Task 12081001 */
ELSE IF lookup(v-print-fmt,"BlueRX,ColoniaX,ABC,knight,knight1,Central,Rosmar,ACPI,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree") > 0 THEN do:
    RUN value(v-program) ("").
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
END.
ELSE IF lookup(v-print-fmt,"ColorX") > 0 THEN do:    
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
END.
ELSE IF lookup(v-print-fmt,"PremierX,Coburn,Axis") > 0 THEN do:    
    RUN value(v-program) ("", NO).
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",NO).
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy",NO).
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",NO).
END.
ELSE IF lookup(v-print-fmt,"PremierS") > 0 THEN do:    
    RUN value(v-program) ("", YES).
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",YES).
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy",YES).
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",YES).
END.
ELSE RUN value(v-program). 
OUTPUT CLOSE.

v-ftp-done = NO.
IF tb_export AND inexport-log THEN DO:    
   DEF VAR v-exp-file AS cha NO-UNDO.
   v-exp-file = inexport-desc +  
                "ARINV_" + trim(v-print-fmt) + 
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat".
   OUTPUT TO VALUE(v-exp-file).
   IF inexport-cha EQ "CIT" THEN DO:
      RUN ar/rep/expfrank.p .
      OUTPUT CLOSE.
      OUTPUT TO VALUE(".\ar\ftpcmd2.txt").     /* ftp text file */
      PUT UNFORMATTED 
       "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
       "ftpa1526" SKIP  /* userid*/
       "none" SKIP  /* password */
       "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
       "quit" .
      OUTPUT CLOSE.
      OS-COMMAND SILENT value("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
      v-ftp-done = YES.
   END.
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-broker-bol-proc C-Win 
PROCEDURE set-broker-bol-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:

      IF fi_broker-bol:HIDDEN = NO AND
         begin_inv:SCREEN-VALUE EQ end_inv:SCREEN-VALUE THEN
         DO:
            FIND FIRST b-broker-bol WHERE
                 b-broker-bol.reftable EQ "brokerbol" AND
                 b-broker-bol.CODE EQ STRING(begin_inv:SCREEN-VALUE)
                 NO-LOCK NO-ERROR.

            IF AVAIL b-broker-bol THEN
            DO:
               fi_broker-bol:SCREEN-VALUE = b-broker-bol.code2.
               RELEASE b-broker-bol.
            END.
            ELSE
               fi_broker-bol:SCREEN-VALUE = "".
         END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEmailBoxes C-Win 
PROCEDURE setEmailBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF rd-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '5' THEN
     ASSIGN tb_attachBOL:SENSITIVE = TRUE.

  ELSE ASSIGN tb_attachBOL:SENSITIVE = FALSE
              tb_attachBOL:CHECKED   = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetInvForm C-Win 
PROCEDURE SetInvForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.

   ASSIGN
      lv-prt-bypass = NO
      is-xprint-form = NO.

   CASE icFormName:
       WHEN "Allpkg" THEN
          ASSIGN
             v-program = "ar/rep/invallpk.p"
             lines-per-page = 62.
       WHEN "1/2 page" THEN
          ASSIGN
             v-program = "ar/rep/invhalfp.p"
             lines-per-page = 44.
       WHEN "Livngstn" THEN
          ASSIGN
             v-program = "ar/rep/invhalfp.p"
             lines-per-page = 66.
       WHEN "TriState" THEN
          ASSIGN
             v-program = "ar/rep/invhalfp.p"
             lines-per-page = 41.
       WHEN "Clev 1/2" THEN
          ASSIGN
             v-program = "ar/rep/invhalfp.p"
             lines-per-page = 42.
       WHEN "Phoenix" THEN
          ASSIGN
             v-program = "ar/rep/invphx.p"
             lines-per-page = 62.
       WHEN "Color" THEN
          ASSIGN
             v-program = "ar/rep/color.p"
             lines-per-page = 60.
       WHEN "Interpac" THEN
          ASSIGN
             v-program = "ar/rep/invinter.p"
             lines-per-page = 60.
       WHEN "Brick" THEN
          ASSIGN
             v-program = "ar/rep/invbrick.p"
             lines-per-page = 62.
       WHEN "Rudd" THEN
          ASSIGN
             v-program = "ar/rep/invrudd.p"
             lines-per-page = 66.
       WHEN "Premier" THEN
          ASSIGN
             v-program = "ar/rep/invprem.p"
             lines-per-page = 66.
       WHEN "PremierX" THEN
          ASSIGN
             v-program      =  "ar/rep/invpremx.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Coburn" THEN
          ASSIGN
             v-program      =  "ar/rep/invcobrn.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Axis" THEN
          ASSIGN
             v-program      =  "ar/rep/invaxis.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "PremierS" THEN
          ASSIGN
             v-program      =  "ar/rep/invpremx.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "ColoniaX" THEN
          ASSIGN
             v-program =  "ar/rep/invcolnx.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "CCCFGLot" THEN
          ASSIGN
             v-program =  "ar/rep/invcccfg.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Carded" THEN
          ASSIGN
             v-program =  "ar/rep/invcardx.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "ABC" THEN
          ASSIGN
             v-program =  "ar/rep/invabcx.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "BlueRX" THEN
          ASSIGN
             v-program =  "ar/rep/invbluex.p"
             lines-per-page = 66
             is-xprint-form = YES.

       WHEN "PAC 1/2" THEN
          ASSIGN
             v-program = "ar/rep/invpack.p"
             lines-per-page = 44.
       WHEN "Triad" THEN
          ASSIGN
             v-program = "ar/rep/invtriad.p"
             lines-per-page = 52.
       WHEN "Danbury" THEN
          ASSIGN
             v-program = "ar/rep/invdnbry.p"
             lines-per-page = 41.
       WHEN "Sonoco" THEN
          ASSIGN
             v-program = "ar/rep/invsono.p"
             lines-per-page = 62.
       WHEN "Empire" THEN
          ASSIGN
             v-program = "ar/rep/invempir.p"
             lines-per-page = 62.
       WHEN "HOP" THEN
          ASSIGN
             v-program = "ar/rep/invhop.p"
             lines-per-page = 42.
       WHEN "MaxPak" THEN
          ASSIGN
             v-program = "ar/rep/invmaxpk.p"
             lines-per-page = 42.
       WHEN "Fibre" THEN
          ASSIGN
             v-program      = "ar/rep/invfibre.p"
             lines-per-page = 50.
       WHEN "Abox" THEN
          ASSIGN
             v-program      = "ar/rep/invabox.p"
             lines-per-page = 60.
       WHEN "ABOX-Xp" THEN
          ASSIGN
             v-program      = "ar/rep/invxabox.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Harwell" THEN
          ASSIGN
            v-program = "ar/rep/invharwl.p"
            lines-per-page = 63.
       WHEN "Chillic" THEN
          ASSIGN
             v-program = "ar/rep/invchill.p"
             lines-per-page = 45.
       WHEN "Pacific" THEN
          ASSIGN
             v-program      = "ar/rep/invpacif.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Xprint" OR WHEN "invprint 1" OR WHEN "invprint 2" THEN
          ASSIGN
             v-program = "ar/rep/invxprnt.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Boss" THEN
          ASSIGN
             v-program = "ar/rep/invboss.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Keystone" THEN
          ASSIGN
             v-program = "ar/rep/invkeystone.p"  /*Xprint format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Fibrex" THEN
          ASSIGN
             v-program = "ar/rep/invfibrex.p"   /*Xprint format*/
             lines-per-page = 69
             is-xprint-form = YES.
       WHEN "ImperiaX" THEN
          ASSIGN
             v-program      = "ar/rep/invximp.p"   /*Xprint format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "ConsBox" THEN
          ASSIGN
             v-program = "ar/rep/invconsb.p"   
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "APC" THEN
          ASSIGN
             v-program = "ar/rep/invxapc.p"   /*APC format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "CSCIN" THEN
          ASSIGN
             v-program      = "ar/rep/invcscin.p"   /*CSCIN  format*/
             lines-per-page = 64
             is-xprint-form = YES.
       WHEN "CSCINStamp" THEN
          ASSIGN
             v-program      = "ar/rep/invcstmp.p"   /*CSCINSTAMP  format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "RUDDX" THEN
          ASSIGN
             v-program = "ar/rep/invruddx.p"   /*Rudd Xprint format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Sonocox" THEN
          ASSIGN
             v-program = "ar/rep/invsonox.p"   /*Sonoco Xprint format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "ASIXprnt" THEN
          ASSIGN
             v-program = "ar/rep/invxasi.p"   /*ASIXprint format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "midwest" THEN
          ASSIGN
             v-program      = "ar/rep/invmidws.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Accord" THEN
          ASSIGN
             v-program      = "ar/rep/invaccrd.p"
             lines-per-page = 72
             is-xprint-form = YES.
       WHEN "mwbox" THEN
          ASSIGN
             v-program = "ar/rep/invmwbox.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Southpak" THEN
          ASSIGN
             v-program = "ar/rep/invsthpk.p" /*Southpak format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Southpak-xl" THEN
          ASSIGN
             v-program = "ar/rep/invsthpk-xl.p" /*Southpak excel format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "PrystupExcel" THEN
          ASSIGN
             v-program = "ar/rep/invpryst-xl.p" /*PrystupExcel format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Hughes" THEN
          ASSIGN
             v-program = "ar/rep/invhughs.p"  /*Hughes format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "NStock" THEN
          ASSIGN
             v-program = "ar/rep/invnstok.p"  /*NStock format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Hughes2" THEN
          ASSIGN
             v-program = "ar/rep/invhugh2.p"  /*Hughes format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Concepts" THEN
          ASSIGN
             v-program      = "ar/rep/invxcorc.p"  /*Corrugate Concepts format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "CSC" THEN
          ASSIGN
             v-program      = "ar/rep/invxcsc.p"  /*Container Service format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Elite" THEN
          ASSIGN
            v-program      = "ar/rep/invelite.p"  /*Elite format*/
            lines-per-page = 66
            is-xprint-form = YES.
       WHEN "Adapt" THEN
          ASSIGN
            v-program      = "ar/rep/invadapt.p"  /*Adapt format*/
            lines-per-page = 66
            is-xprint-form = YES.
       WHEN "CSC-GA" THEN
          ASSIGN
            v-program      = "ar/rep/invcscga.p"  /*CSC GA format*/
            lines-per-page = 71
            is-xprint-form = YES.
       WHEN "CSC-GASummary" THEN
          ASSIGN
            v-program      = "ar/rep/invcscgsm.p"  /*CSC GA format*/
            lines-per-page = 71
            is-xprint-form = YES.
       WHEN "ARGROVX" THEN
          ASSIGN
            v-program      = "ar/rep/invxargv.p"  /*ArgrovX format*/
            lines-per-page = 66
            is-xprint-form = YES.
       WHEN "Indiana" THEN
          ASSIGN
             v-program = "ar/rep/invindc.p"  /*Indiana <= Elite format*/
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Imperial" THEN
          ASSIGN
             v-program = "ar/rep/invimper.p"
             lines-per-page = 62.
       WHEN "RFC" OR WHEN "AgMach" THEN
          ASSIGN
             v-program = "ar/rep/invrfc.p"
             lines-per-page = IF v-print-fmt EQ "RFC" THEN 62 ELSE 66.
       WHEN "Herman" THEN
          ASSIGN
             v-program      = "ar/rep/invhermn.p"
             lines-per-page = 62.
       WHEN "CENTBOX" THEN
          ASSIGN
             v-program      = "ar/rep/invcentx.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Oracle" THEN
          ASSIGN
             v-program = "ar/rep/invoracl.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "TriLakes" THEN
          ASSIGN
             v-program = "ar/rep/invtri.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "TriLakesBroker" THEN
          ASSIGN
             v-program = "ar/rep/invtribrk.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "frankstn" OR WHEN "Mirpkg" THEN
          ASSIGN
             v-program = "ar/rep/invfrank.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "DEE"  THEN
          ASSIGN
             v-program = "ar/rep/invdee.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "PPI" THEN
          ASSIGN
             v-program = "ar/rep/invppi.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Dayton" THEN
          ASSIGN
             v-program      = "ar/rep/invdaytn.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Unipak" THEN
          ASSIGN
             v-program = "ar/rep/invunipk.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "HPB" THEN
          ASSIGN
             v-program = "ar/rep/invhpb.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Allpkgx" THEN
          ASSIGN
             v-program = "ar/rep/invalpkx.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Knight" THEN
          ASSIGN
             v-program      =  "ar/rep/invknight.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Knight1" THEN
          ASSIGN
             v-program      =  "ar/rep/invknight1.p"
             lines-per-page = 66
             is-xprint-form = YES.
       WHEN "Central" THEN                                  /*task# 12041303*/
          ASSIGN
             v-program      =  "ar/rep/invcentral.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Southpakl" THEN
          ASSIGN
             v-program = "ar/rep/invsthpklg.p" /*Southpak logistic format*/
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Androp" THEN
          ASSIGN
             v-program = "ar/rep/invandrop.p"   
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Packrite" THEN
          ASSIGN
             v-program = "ar/rep/invpkrt.p"  
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Rosmar" THEN
          ASSIGN
             v-program =  "ar/rep/invrosmr.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Badger" THEN
          ASSIGN
             v-program = "ar/rep/invBadger.p"  
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Badger-Emailed" THEN
          ASSIGN
             v-program = "ar/rep/invbadgereml.p"  
             lines-per-page = 66
             is-xprint-form = YES.

       /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */ 
      WHEN "CAPITOL" THEN
          ASSIGN
             v-program      = "ar/rep/invcapitol.p"
             lines-per-page = 71
             is-xprint-form = YES.

      WHEN "allwest" THEN
          ASSIGN
             v-program      = "ar/rep/invallws.p"
             lines-per-page = 71
             is-xprint-form = YES.

      WHEN "Bell" THEN
          ASSIGN
             v-program      = "ar/rep/invbell.p"
             lines-per-page = 71
             is-xprint-form = YES.

      WHEN "Simkins" THEN
          ASSIGN
             v-program = "ar/rep/invsmkct.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "CapCityIN" THEN
          ASSIGN
             v-program = "ar/rep/invcapcin.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "ACPI" THEN
          ASSIGN
             v-program =  "ar/rep/invacpi.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "ColorX" THEN
          ASSIGN
             v-program      =  "ar/rep/invcolrx.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Loylang" THEN
          ASSIGN
             v-program      = "ar/rep/invloyln.p"
             lines-per-page = 71
             is-xprint-form = YES.
      WHEN "PrestigeLLB" THEN /* Task# 08271402*/
          ASSIGN
             v-program      = "ar/rep/invprstl.p"
             lines-per-page = 71
             is-xprint-form = YES.
      WHEN "RFCX" THEN
          ASSIGN
             v-program      = "ar/rep/invrfcx.p"
             lines-per-page = 71
             is-xprint-form = YES.
      WHEN "LoylangBSF" THEN
          ASSIGN
             v-program      = "ar/rep/invloyln.p"
             lines-per-page = 71
             is-xprint-form = YES.
      WHEN "Protagon" THEN
          ASSIGN
             v-program      = "ar/rep/invprot.p"
             lines-per-page = 71
             is-xprint-form = YES.
      WHEN "Protagon2" THEN
          ASSIGN
             v-program      = "ar/rep/invprot2.p"
             lines-per-page = 71
             is-xprint-form = YES.
      WHEN "Soule" THEN /* LOYLANG Format */
          ASSIGN
             v-program      = "ar/rep/invsoule.p"
             lines-per-page = 71             
             is-xprint-form = YES.
      WHEN "Printers" THEN /* LOYLANG Format */
          ASSIGN
             v-program      = "ar/rep/invprnts.p"
             lines-per-page = 71             
             is-xprint-form = YES.
       WHEN "SouleMed" THEN /* LOYLANG Format */
          ASSIGN
             v-program      = "ar/rep/invsoulemed.p"
             lines-per-page = 71             
             is-xprint-form = YES.
       WHEN "SoulePO" THEN /* LOYLANG Format */
          ASSIGN
             v-program      = "ar/rep/invsoulepo.p"
             lines-per-page = 71             
             is-xprint-form = YES.
      WHEN "LoylangJIT" THEN
          ASSIGN
             v-program      = "ar/rep/invloyjit.p"
             lines-per-page = 76
             is-xprint-form = YES.
      WHEN "ColonialLot#" THEN
          ASSIGN
             v-program =  "ar/rep/invcolnx2.p"
             lines-per-page = 71            /*Task# 11131304*/
             is-xprint-form = YES.
      WHEN "CCCFGL3" THEN
         ASSIGN
             v-program =  "ar/rep/invcfgl3.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Peachtreefgl3" THEN
         ASSIGN
             v-program =  "ar/rep/invptreefgl3.p"
             lines-per-page = 66
             is-xprint-form = YES.
      WHEN "Peachtree" THEN
         ASSIGN
             v-program =  "ar/rep/invptreelot.p"
             lines-per-page = 66
             is-xprint-form = YES.
      OTHERWISE
          ASSIGN
             v-program      = "ar/rep/invasi.p"
             is-xprint-form = YES
             lines-per-page = 66.

   END CASE.

   IF icFormName = "BOXTECH" THEN
      lv-prt-bypass = YES.

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

  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

