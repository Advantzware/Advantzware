&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-cust.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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


/*{sys/inc/custlistform.i ""AR1"" }*/

{sys/ref/CustList.i NEW}

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR v-seq AS INT NO-UNDO.
DEF VAR v-page-break AS LOG NO-UNDO.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF STREAM excel.
DEF TEMP-TABLE tt-cust NO-UNDO
    FIELD cust-name AS CHAR FORMAT "X(30)"
    FIELD cust-no AS CHAR
    FIELD seq AS INT.

DEF TEMP-TABLE tt-contact NO-UNDO
    FIELD cust-no AS CHAR
    FIELD attention AS CHAR FORMAT "X(30)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-10 RECT-11 RECT-12 ~
tb_cust-list btnCustList begin_cust-no end_cust-no begin_cust-type ~
end_cust-type begin_slsmn end_slsmn begin_date end_date tb_active ~
tb_contacts tb_inactive scr-title price-level tb_contact-sort rd_sort ~
tb_detailed tb_invoice-date tb_disc-only rd-dest lv-ornt lines-per-page ~
lv-font-no tb_show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_cust-type end_cust-type begin_slsmn end_slsmn begin_date end_date ~
tb_active tb_contacts tb_inactive scr-title price-level lbl_sort ~
tb_contact-sort rd_sort tb_detailed tb_invoice-date tb_disc-only rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name tb_show-parm tb_excel ~
tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY */
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Since Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Since Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-cust.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE price-level AS INTEGER FORMAT ">9":U INITIAL 99 
     LABEL "Price Level" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE scr-title AS CHARACTER FORMAT "X(8)" 
     LABEL "Title Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "C" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "C",
"Name", "N"
     SIZE 27 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 5.86.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 3.71.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 2.14.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.67.

DEFINE VARIABLE tb_active AS LOGICAL INITIAL yes 
     LABEL "Show Active?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_contact-sort AS LOGICAL INITIAL no 
     LABEL "Sort by Contact?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_contacts AS LOGICAL INITIAL no 
     LABEL "Print Contacts by Title?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_disc-only AS LOGICAL INITIAL no 
     LABEL "Print Only Customers with Discount?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_inactive AS LOGICAL INITIAL yes 
     LABEL "Show Inactive?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE tb_invoice-date AS LOGICAL INITIAL no 
     LABEL "Print First Invoice Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.29 COL 29.4 WIDGET-ID 6
     btnCustList AT ROW 1.38 COL 63 WIDGET-ID 8
     begin_cust-no AT ROW 2.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.43 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-type AT ROW 3.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Type"
     end_cust-type AT ROW 3.38 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Type"
     begin_slsmn AT ROW 4.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 4.33 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     begin_date AT ROW 5.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Since Date"
     end_date AT ROW 5.29 COL 70 COLON-ALIGNED HELP
          "Enter Ending Since Date"
     tb_active AT ROW 6.38 COL 70 RIGHT-ALIGNED
     tb_contacts AT ROW 6.57 COL 6 WIDGET-ID 2
     tb_inactive AT ROW 7.29 COL 71 RIGHT-ALIGNED
     scr-title AT ROW 7.71 COL 14.2 COLON-ALIGNED HELP
          "Enter Title Code" WIDGET-ID 8
     price-level AT ROW 8.38 COL 50 COLON-ALIGNED
     lbl_sort AT ROW 8.43 COL 43 COLON-ALIGNED NO-LABEL
     tb_contact-sort AT ROW 8.86 COL 6 WIDGET-ID 10
     rd_sort AT ROW 9.38 COL 52 NO-LABEL
     tb_detailed AT ROW 10.24 COL 69 RIGHT-ALIGNED
     tb_invoice-date AT ROW 10.52 COL 6 WIDGET-ID 14
     tb_disc-only AT ROW 11.1 COL 89 RIGHT-ALIGNED
     rd-dest AT ROW 13.62 COL 8 NO-LABEL
     lv-ornt AT ROW 14.1 COL 32 NO-LABEL
     lines-per-page AT ROW 14.1 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 15.52 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 29 COLON-ALIGNED NO-LABEL
     tb_show-parm AT ROW 17.62 COL 31
     tb_excel AT ROW 19.43 COL 51 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.43 COL 72 RIGHT-ALIGNED
     fi_file AT ROW 20.24 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.95 COL 24
     btn-cancel AT ROW 21.95 COL 56
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.67 COL 4
     "(99 for all)" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 8.38 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 4
          BGCOLOR 2 
     RECT-6 AT ROW 12.67 COL 2
     RECT-7 AT ROW 1 COL 1
     RECT-10 AT ROW 6.33 COL 38 WIDGET-ID 4
     RECT-11 AT ROW 6.33 COL 2 WIDGET-ID 6
     RECT-12 AT ROW 10.05 COL 2 WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.57.


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
         TITLE              = "Customer List"
         HEIGHT             = 22.81
         WIDTH              = 96.2
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       price-level:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       scr-title:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_active IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_active:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_contact-sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_contacts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_detailed IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_disc-only IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_disc-only:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_inactive IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_inactive:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_invoice-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-type C-Win
ON LEAVE OF begin_cust-type IN FRAME FRAME-A /* Beginning Customer Type */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Since Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning Sales Rep# */
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

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
       EMPTY TEMP-TABLE ttCustList.
       RUN BuildCustList(INPUT cocode,
                         INPUT tb_cust-list AND glCustListActive,
                         INPUT begin_cust-no,
                         INPUT END_cust-no).
 END.

  IF tb_contacts THEN
     RUN run-report-contact.
  ELSE
     IF tb_invoice-date THEN
        RUN run-report-invoice.
  ELSE
     RUN run-report.

  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_slsmn
                            &END_cust=END_slsmn
                            &fax-subject="Customer List"
                            &fax-body="Customer List"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_slsmn
                             &END_cust=end_slsmn
                             &mail-subject="Customer List"
                             &mail-body="Customer List"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_slsmn
                                  &END_cust=end_slsmn
                                  &mail-subject='"Customer List"'
                                  &mail-body='"Customer List"'
                                  &mail-file=list-name }
           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-type C-Win
ON LEAVE OF end_cust-type IN FRAME FRAME-A /* Ending Customer Type */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Since Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending Sales Rep# */
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

    RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

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


&Scoped-define SELF-NAME price-level
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL price-level C-Win
ON LEAVE OF price-level IN FRAME FRAME-A /* Price Level */
DO:
  assign {&self-name}.
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-title
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-title C-Win
ON HELP OF scr-title IN FRAME FRAME-A /* Title Code */
DO:
   DEF VAR v-title AS cha NO-UNDO.

   RUN windows/l-ttlcod.w (scr-title:SCREEN-VALUE, OUTPUT v-title).
   IF v-title <> "" THEN scr-title:SCREEN-VALUE = entry(1,v-title).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-title C-Win
ON LEAVE OF scr-title IN FRAME FRAME-A /* Title Code */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_active C-Win
ON VALUE-CHANGED OF tb_active IN FRAME FRAME-A /* Show Active? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_disc-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disc-only C-Win
ON VALUE-CHANGED OF tb_disc-only IN FRAME FRAME-A /* Print Only Customers with Discount? */
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


&Scoped-define SELF-NAME tb_inactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inactive C-Win
ON VALUE-CHANGED OF tb_inactive IN FRAME FRAME-A /* Show Inactive? */
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


&Scoped-define SELF-NAME tb_show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-parm C-Win
ON VALUE-CHANGED OF tb_show-parm IN FRAME FRAME-A /* Show Parameters? */
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

  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "AR1",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'AR1',
                          INPUT NO,
                          OUTPUT glCustListActive).

 {sys/inc/chblankcust.i ""AR1""}

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
  IF ou-log AND ou-cust-int = 0 THEN do:
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
                            INPUT 'AR1',
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
                                  INPUT 'AR1').


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
  DISPLAY tb_cust-list begin_cust-no end_cust-no begin_cust-type end_cust-type 
          begin_slsmn end_slsmn begin_date end_date tb_active tb_contacts 
          tb_inactive scr-title price-level lbl_sort tb_contact-sort rd_sort 
          tb_detailed tb_invoice-date tb_disc-only rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name tb_show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-10 RECT-11 RECT-12 tb_cust-list btnCustList 
         begin_cust-no end_cust-no begin_cust-type end_cust-type begin_slsmn 
         end_slsmn begin_date end_date tb_active tb_contacts tb_inactive 
         scr-title price-level tb_contact-sort rd_sort tb_detailed 
         tb_invoice-date tb_disc-only rd-dest lv-ornt lines-per-page lv-font-no 
         tb_show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
PROCEDURE run-report PRIVATE :
/* -------------------------------------------------- sys/rep/cust.p 9/91 cd */
/*                                                                            */
/* cust list printout                                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}

def var fco as ch.
def var tco like fco.
def var floc as ch.
def var tloc like floc.
def var fcust as ch init "".
def var tcust like fcust init "zzzzzzzz".
def var ftype as ch init "".
def var ttype like ftype init "zzzzzzzz".
def var fsman as ch init "".
def var tsman like fsman init "zzzzzzzz".
def var v-level like cust.cust-level init 99.
def var v-sort as log format "Yes/No" init no.
def var detailed as log format "Yes/No" init no.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.

form skip(1)
     cust.cust-no
     cust.name at 12
     "Type:" to 51
     cust.type
     "Active?" to 67
     cust.active skip
     cust.addr[1] at 12
     "Contact:" to 51
     cust.contact    skip
     cust.addr[2] at 12
     "Sales Rep:" to 51
     cust.sman
     sman.sname
     skip
     cust.city at 12
     "Tel. #:" to 51
     cust.area-code
     cust.phone
     "Disc%:"  AT 12
     cust.disc FORMAT ">>9.99%"
     "Price Level:" TO 51
     cust.cust-level
     skip

  with frame cust2 no-labels stream-io down.

{sys/ref/cust.f}

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 56}

 fco      = cocode
 tco      = cocode
 floc     = locode
 tloc     = locode
 fcust    = begin_cust-no
 tcust    = end_cust-no
 ftype    = begin_cust-type
 ttype    = end_cust-type
 fsman    = begin_slsmn
 tsman    = end_slsmn
 v-level  = price-level
 v-sort   = rd_sort eq "N"
 detailed = tb_detailed
 lSelected  = tb_cust-list.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  IF NOT detailed THEN
     excelheader = "Cust No,Name,Type,Active,Address 1,Address 2,City,State,Zip,"
                 + "Tel. #,Contact,SalesRep Code,SalesRep Name,Disc%,Price Level,Tax Resale ID#,Exp.".
  ELSE
     excelheader = "Cust.#,Name,Address 1,Address 2,City,State,Zip,Tel. #,FAX #,Type,Active,"
                 + "Contact,SalesRep Code,SalesRep,AutoReprice,Customer Price Level,"
                 + "EDI,Bal Method,Mfg/Whse Days,Pallet,Case/Bundle,Stmnt Freq,"
                 + "Territory,Cr. Acct #,Ord. Loc.,Carrier,Delivery Zone,Cr. Rating,Hold Days/Inv.,Credit Lim,"
                 + "Partial Ship,Fr Pay Meth,Order Lim,Taxable,FOB Orig/Dest,Cred. Hold,"
                 + "TAX Code,Fin. Chrgs,Invoice Per PO,TAX Resale ID#,Exp.,"
                 + "Terms Code,Credit Hold Term,Date Added,Discount,MarkUp,"
                 + "Underrun,Overrun,E-Mail/Web Address,Load Tags".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
END.

if tb_show-parm then run show-param.

display str-tit with frame r-top.

SESSION:SET-WAIT-STATE ("general").

if rd_sort EQ "C" then {ar/rep/cust.i "cust-no" "use-index cust"}
else
if rd_sort EQ "N" then {ar/rep/cust.i "name" "use-index name"}
else                   {ar/rep/cust.i "date-field[1]"}

STATUS DEFAULT "".

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-contact C-Win 
PROCEDURE run-report-contact :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-sort AS LOG NO-UNDO.
   DEF VAR excelheader AS CHAR NO-UNDO.
   DEF VAR lSelected AS LOG INIT YES NO-UNDO.
   def var fcust as ch init "".
   def var tcust like fcust init "zzzzzzzz".
 ASSIGN  
   lSelected  = tb_cust-list
   fcust    = begin_cust-no
   tcust    = end_cust-no .
   form tt-contact.attention LABEL "Contact"
       with frame contact-top stream-io down.

   {sys/form/r-top.f}

   assign
      str-tit2 = c-win:title
      {sys/inc/ctrtext.i str-tit2 56}
      v-sort   = rd_sort eq "N".
   IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
   END.
   {sys/inc/print1.i}

   {sys/inc/outprint.i value(lines-per-page)}

   if tb_show-parm then run show-param.

   display str-tit with frame r-top.

   SESSION:SET-WAIT-STATE ("general").

   if rd_sort EQ "C" then
   DO: 
      {ar/rep/custcontact.i "cust-no" "use-index cust"}
   END.
   else
      if rd_sort EQ "N" then
      DO:
         {ar/rep/custcontact.i "name" "use-index name"}
      END.
   else
   DO:
      {ar/rep/custcontact.i "date-field[1]"}
   END.

   STATUS DEFAULT "".

   IF tb_excel THEN DO:
      OUTPUT STREAM excel CLOSE.
      IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
   END.

   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

   SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-invoice C-Win 
PROCEDURE run-report-invoice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-sort AS LOG NO-UNDO.
   DEF VAR excelheader AS CHAR NO-UNDO.
   DEF VAR lSelected AS LOG INIT YES NO-UNDO.
   def var fcust as ch init "".
   def var tcust like fcust init "zzzzzzzz".
 ASSIGN  
   lSelected  = tb_cust-list
   fcust    = begin_cust-no
   tcust    = end_cust-no .

   {sys/form/r-top.f}

   assign
      str-tit2 = c-win:title
      {sys/inc/ctrtext.i str-tit2 56}
      v-sort   = rd_sort eq "N"
      lSelected = tb_cust-list.
   IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
   END.

   {sys/inc/print1.i}

   {sys/inc/outprint.i value(lines-per-page)}

   if tb_show-parm then run show-param.

   display str-tit with frame r-top.

   SESSION:SET-WAIT-STATE ("general").

   if rd_sort EQ "C" then
   DO: 
      {ar/rep/custinvoice.i "cust-no" "use-index cust"}
   END.
   else
      if rd_sort EQ "N" then
      DO:
         {ar/rep/custinvoice.i "name" "use-index name"}
      END.
   else
   DO:
      {ar/rep/custinvoice.i "date-field[1]"}
   END.

   STATUS DEFAULT "".

   IF tb_excel THEN DO:
      OUTPUT STREAM excel CLOSE.
      IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
   END.

   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

   SESSION:SET-WAIT-STATE ("").
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
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
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

