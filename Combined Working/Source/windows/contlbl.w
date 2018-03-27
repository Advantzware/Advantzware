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

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
def stream s-mail.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

DEF STREAM st-excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 ls-mail-list begin_cust-no ~
end_cust-no begin_contact_sman end_contact_sman begin_contact_zip ~
end_contact_zip tb_excel tbLastOrder rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS ls-mail-list selected-company ~
begin_name_description begin_cust-no end_name_description end_cust-no ~
begin_contact_sman-dscr begin_contact_sman end_contact_sman-dscr ~
end_contact_sman begin_contact_zip end_contact_zip tb_excel v-export-name ~
tbLastOrder rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
rd-printer-type td-show-parm 

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

DEFINE VARIABLE begin_contact_sman AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning SalesRep" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_contact_sman-dscr AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE begin_contact_zip AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Zip" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Customer Number" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_name_description AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE end_contact_sman AS CHARACTER FORMAT "X(10)" INITIAL "zzzzz" 
     LABEL "Ending SalesRep" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_contact_sman-dscr AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE end_contact_zip AS CHARACTER FORMAT "X(10)" INITIAL "zzzzz" 
     LABEL "Ending Zip" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(10)" INITIAL "zzzzz" 
     LABEL "Ending Customer Number" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_name_description AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE ls-mail-list AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mail List" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(10)" INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE selected-printer AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Printer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 TOOLTIP "Enter Printer Number Only when Printing to Printer".

DEFINE VARIABLE v-export-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Excel File" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.19 NO-UNDO.

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

DEFINE VARIABLE rd-printer-type AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Laser", 1,
"Ink/Desk or Laser Jet", 2
     SIZE 29 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 111 BY 8.57.

DEFINE VARIABLE t-sortby AS LOGICAL INITIAL yes 
     LABEL "Sort By Zip Code" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tbLastOrder AS LOGICAL INITIAL no 
     LABEL "Export Last Order Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 TOOLTIP "Print Last Order Date" NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export to Excel File(comma delimited)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     ls-mail-list AT ROW 1.95 COL 29 COLON-ALIGNED
     selected-company AT ROW 3.38 COL 29 COLON-ALIGNED
     begin_name_description AT ROW 4.33 COL 49.4 COLON-ALIGNED HELP
          "Enter Beginning Description" NO-LABEL
     begin_cust-no AT ROW 4.38 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_name_description AT ROW 5.29 COL 49.4 COLON-ALIGNED HELP
          "Enter Ending Description" NO-LABEL
     end_cust-no AT ROW 5.33 COL 29 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_contact_sman-dscr AT ROW 6.24 COL 49.4 COLON-ALIGNED HELP
          "Enter Beginning Description" NO-LABEL
     begin_contact_sman AT ROW 6.29 COL 29 COLON-ALIGNED HELP
          "Enter Beginning SalesRep"
     end_contact_sman-dscr AT ROW 7.19 COL 49.4 COLON-ALIGNED HELP
          "Enter Ending Description" NO-LABEL
     end_contact_sman AT ROW 7.24 COL 29 COLON-ALIGNED HELP
          "Enter Ending SalesRep"
     begin_contact_zip AT ROW 8.19 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Zip"
     t-sortby AT ROW 8.86 COL 52
     end_contact_zip AT ROW 9.14 COL 29 COLON-ALIGNED HELP
          "Enter Ending Zip"
     selected-printer AT ROW 10.1 COL 29 COLON-ALIGNED HELP
          "Enter Printer for Control Codes"
     tb_excel AT ROW 11.24 COL 17
     v-export-name AT ROW 11.24 COL 68 COLON-ALIGNED
     tbLastOrder AT ROW 11.95 COL 17
     rd-dest AT ROW 13.86 COL 4 NO-LABEL
     lv-ornt AT ROW 14.1 COL 35 NO-LABEL
     lines-per-page AT ROW 14.1 COL 90 COLON-ALIGNED
     lv-font-no AT ROW 15.52 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 28 COLON-ALIGNED NO-LABEL
     rd-printer-type AT ROW 17.91 COL 80 NO-LABEL
     td-show-parm AT ROW 20.76 COL 4
     btn-ok AT ROW 22.19 COL 22
     btn-cancel AT ROW 22.19 COL 73
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.14 COL 4
     "Laser Label  1~" x 2-5/8~"  (3 across, 10 down)" VIEW-AS TEXT
          SIZE 45 BY .95 AT ROW 18.14 COL 32
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
     RECT-6 AT ROW 13.14 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 116.2 BY 22.67.


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
         TITLE              = "Mail List Label"
         HEIGHT             = 22.91
         WIDTH              = 117.2
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_contact_sman:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN begin_contact_sman-dscr IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_contact_zip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN begin_name_description IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_contact_sman:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN end_contact_sman-dscr IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_contact_zip:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN end_name_description IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd-printer-type IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN selected-company IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       selected-company:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN selected-printer IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       selected-printer:HIDDEN IN FRAME FRAME-A           = TRUE
       selected-printer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR TOGGLE-BOX t-sortby IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       t-sortby:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tbLastOrder:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN v-export-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mail List Label */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mail List Label */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_contact_sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_contact_sman C-Win
ON LEAVE OF begin_contact_sman IN FRAME FRAME-A /* Beginning Salesman */
DO:
    def var ll-got-it as log no-undo.
    if lastkey <> -1 then do:

       find first maillist where maillist.list-name = ls-mail-list:screen-value no-lock no-error.
       ll-got-it = no.
       for each mailcont of maillist no-lock, 
           first contact where recid(contact) = mailcont.contact-rec and
                                contact.sman >= self:screen-value
                                no-lock:
           ll-got-it = yes.
           if ll-got-it then leave.                     
       end.                               
       if not ll-got-it then do:
          message "Invalid Sales Rep. " view-as alert-box error.
          return no-apply.
       end.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer Number */
DO:

    def var ll-got-it as log no-undo.
    if lastkey <> -1 then do:

       find first maillist where maillist.list-name = ls-mail-list:screen-value no-lock no-error.
       ll-got-it = no.
       for each mailcont of maillist no-lock where mailcont.cust-no >= self:screen-value : 
           ll-got-it = yes.
           if ll-got-it then leave.                     
       end.                               
       if not ll-got-it then do:
          message "Invalid Customer Number. " view-as alert-box error.
          return no-apply.
       end.
   end.

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
    if not can-find(first maillist where maillist.list-name = ls-mail-list:screen-value)
    then do:
        message "Invalid Mail List. Try Help." view-as alert-box error.
        apply "entry" to ls-mail-list in frame {&frame-name}.
        return no-apply.
    end.



  session:set-wait-state("general").
  assign rd-dest tb_excel tblastorder v-export-name.

/*  run run-report.  for laser */
  run run-report-jet.

 case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
           /* output-to-label */
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject="Mail List Label"
                            &fax-body="Mail List Label"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE="CUSTOMER"
                             &begin_cust=begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject="Mail List Label"
                             &mail-body="Mail List Label"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE="CUSTOMER"
                                  &begin_cust=begin_cust-no
                                  &END_cust=end_cust-no
                                  &mail-subject="Acknowledgement"
                                  &mail-body="Acknowledgement"
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 

  session:set-wait-state("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_contact_sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_contact_sman C-Win
ON LEAVE OF end_contact_sman IN FRAME FRAME-A /* Ending Salesman */
DO:
    def var ll-got-it as log no-undo.
    if lastkey <> -1 then do:

       find first maillist where maillist.list-name = ls-mail-list:screen-value no-lock no-error.
       ll-got-it = no.
       for each mailcont of maillist no-lock, 
           first contact where recid(contact) = mailcont.contact-rec and
                                contact.sman <= self:screen-value
                                no-lock:
           ll-got-it = yes.
           if ll-got-it then leave.                     
       end.                               
       if not ll-got-it then do:
          message "Invalid Sales Rep. " view-as alert-box error.
          return no-apply.
       end.
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer Number */
DO:
    def var ll-got-it as log no-undo.
    if lastkey <> -1 then do:

       find first maillist where maillist.list-name = ls-mail-list:screen-value no-lock no-error.
       ll-got-it = no.
       for each mailcont of maillist no-lock where mailcont.cust-no <= self:screen-value : 
           ll-got-it = yes.
           if ll-got-it then leave.                     
       end.                               
       if not ll-got-it then do:
          message "Invalid Customer Number. " view-as alert-box error.
          return no-apply.
       end.
   end.
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


&Scoped-define SELF-NAME ls-mail-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-mail-list C-Win
ON HELP OF ls-mail-list IN FRAME FRAME-A /* Mail List */
DO:
    def var char-val as cha no-undo.
    run windows/l-maillst.w (self:screen-value, output char-val).
    if char-val <> "" then self:screen-value = entry(1,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-mail-list C-Win
ON LEAVE OF ls-mail-list IN FRAME FRAME-A /* Mail List */
DO:
    if lastkey <> -1 and
       not can-find(first maillist where maillist.list-name = self:screen-value)
    then do:
        message "Invalid Mail List. Try Help." view-as alert-box error.
        return no-apply.
    end.
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export to Excel File(comma delimited) */
DO:
   IF SELF:SCREEN-VALUE = "Yes" THEN ENABLE v-export-name WITH FRAME {&FRAME-NAME}.
   ELSE  DISABle v-export-name WITH FRAME {&FRAME-NAME}. 
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
{sys/inc/f3help.i}
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

  FIND FIRST users WHERE
      users.user_id EQ USERID("NOSWEAT")
      NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     v-dir = users.user_program[2].
  ELSE
     v-dir = "c:\temp".

  ASSIGN
    v-export-name = v-dir + "\maillist.csv"
    cocode = g_company.

  RUN enable_UI.

  {methods/nowait.i}
  apply "entry" to ls-mail-list in frame {&frame-name}.

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
  DISPLAY ls-mail-list selected-company begin_name_description begin_cust-no 
          end_name_description end_cust-no begin_contact_sman-dscr 
          begin_contact_sman end_contact_sman-dscr end_contact_sman 
          begin_contact_zip end_contact_zip tb_excel v-export-name tbLastOrder 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name rd-printer-type 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 ls-mail-list begin_cust-no end_cust-no begin_contact_sman 
         end_contact_sman begin_contact_zip end_contact_zip tb_excel 
         tbLastOrder rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-label C-Win 
PROCEDURE output-to-label :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 0, INPUT 0, INPUT 0, OUTPUT result).


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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.
/*  
     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/
  /* Use Progress Print. Always use Font#9 in Registry (set above) */
  if t-sortby then 
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).    /* portrait */
  else RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,  
                            INPUT 3, INPUT 2, INPUT 0, INPUT 0, OUTPUT result).  /* landscape */



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
  run rfq/scr-rpt2.w (list-name,"Contact LIST"). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  assign list-name = v-dir + "\contlbl.rpt"
         init-dir = v-dir.

 do with frame {&frame-name}:
    assign selected-company begin_cust-no end_cust-no
           begin_contact_sman end_contact_sman
           begin_contact_zip end_contact_zip
           t-sortby /*tblastorder
           selected-printer */
           ls-mail-list.
 end.


def var i as int no-undo. 
def var v-start-compress as char init "" no-undo.
def var v-end-compress as char init "" no-undo.
def var v-delim as char format "x(1)" init '~~' no-undo.
def var print-head as log init yes no-undo.
def var v-last-date as date format "99/99/9999" label "Last Ord Date" no-undo.
def var v-cust-name like cust.name no-undo.
def var v-sname like sman.sname no-undo.
def var v-address as cha form "x(120)" no-undo.
def var v-lbl-1 as cha form "x(35)" extent 5 no-undo.
def var v-lbl-2 as cha form "x(35)" extent 5 no-undo.
def var v-lbl-3 as cha form "x(35)" extent 5 no-undo.
def var v-lbl-cnt as int no-undo.
def var v-addr1 as cha no-undo.
def var v-addr2 as cha no-undo.
def var v-city as cha no-undo.
def var v-state as cha no-undo.
def var v-zip as cha no-undo.
def var v-country as cha no-undo.
def var li-cnt as int no-undo.
form 
      skip(2)
      v-lbl-1[1] at 3 v-lbl-2[1] at 45 v-lbl-3[1] at 88 skip
      v-lbl-1[2] at 3 v-lbl-2[2] at 45 v-lbl-3[2] at 88 skip
      v-lbl-1[3] at 3 v-lbl-2[3] at 45 v-lbl-3[3] at 88 skip
      v-lbl-1[4] at 3 v-lbl-2[4] at 45 v-lbl-3[4] at 88 skip
      v-lbl-1[5] at 3 v-lbl-2[5] at 45 v-lbl-3[5] at 88 skip
      with width 130 no-box no-label stream-io down frame lbl.

assign v-start-compress = ""
       v-end-compress = "".

if t-sortby then do:  /* yes all the time */

    output to value(list-name) page-size 80.

    find first asi.printer where printer.company eq gcompany
                     and asi.printer.loc     eq gloc
                     and asi.printer.pr-no   eq selected-printer no-lock no-error.
    if avail printer then
    do:
      if asi.printer.pr-cmd ne "" then
      do i = 1 to num-entries(printer.pr-cmd):
        if entry(i,asi.printer.pr-cmd) ne ? then
          v-start-compress = v-start-compress +
                          chr(int(entry(i,asi.printer.pr-cmd))).
      end.
    end.

    put control v-start-compress.


/*
for each cust where cust.company eq gcompany
                and cust.cust-no ge begin_cust-no
                and cust.cust-no le end_cust-no,
*/

    find first maillist where maillist.list-name = ls-mail-list no-lock no-error.
    if not avail maillist then return.

    for each mailcont of maillist no-lock where mailcont.maillist,
        each contact where recid(contact) = mailcont.contact-rec
                       and contact.company eq selected-company
                   and contact.cust-no ge begin_cust-no
                   and contact.cust-no le end_cust-no
                   and contact.sman    ge begin_contact_sman
                   and contact.sman    le end_contact_sman
                   and contact.zip     ge begin_contact_zip
                   and contact.zip     le end_contact_zip
                NO-LOCK 
                BREAK BY contact.zip by contact.cust-no by contact.first-name:
        find first cust where cust.company eq selected-company
                        and cust.cust-no eq contact.cust-no
                      no-lock no-error.
        find sman where sman.company eq selected-company
                  and sman.sman eq contact.sman
              no-lock no-error.
   /* ======              
      if tbMailMrge /*and contact.maillist*/ then
      do:
        if print-head then
        do:
          put stream s-mail unformatted
              "sirname" v-delim
              "firstname" v-delim
              "lastname" v-delim
              "companyname" v-delim
              "address1" v-delim
              "address2" v-delim
              "city" v-delim
              "state" v-delim
              "zip" v-delim
              "country" v-delim skip.
          assign print-head = no.
        end.

        v-cust-name = if avail cust then cust.name else "".

        put stream s-mail unformatted
            trim(contact.sirname) v-delim
            trim(contact.first-name) v-delim
            trim(contact.last-name) v-delim 
/*          trim(cust.name) v-delim */
            trim(v-cust-name) v-delim 
            trim(contact.addr1) v-delim
            trim(contact.addr2) v-delim
            trim(contact.city) v-delim
            trim(contact.state) v-delim
            trim(contact.zip) v-delim
            trim(contact.country) v-delim skip.
        create nosweat.note.
        assign nosweat.note.rec_key = contact.rec_key
               nosweat.note.note_date = TODAY
               nosweat.note.note_time = TIME
               nosweat.note.user_id = USERID("NOSWEAT")
               nosweat.note.note_title = FILL-IN-Title
               nosweat.note.note_text = "Automatic Note Generation from Mail Merge Report. " +
                                        string(note.note_date,"99/99/9999") + " " +
                                        string(note.note_time,"HH:MM:SS AM").
      end.  
      ===========*/
      /*v-sname = if avail sman then sman.sname else "".  */
      v-sname = contact.first-name + " " + contact.middle-initial + " " + contact.last-name.
     /* ========== address display    ==================== */
     v-lbl-cnt = v-lbl-cnt + 1.
     if v-lbl-cnt = 1 then assign v-lbl-1[1] = v-sname
                                  v-lbl-1[2] = contact.cust-name
                                  v-lbl-1[3] = contact.addr1
                                  v-lbl-1[4] = contact.addr2
                                  v-lbl-1[5] = contact.city + ", " + contact.state + " " + contact.zip
                                               + " " + contact.country
                                               .
     else if v-lbl-cnt = 2 then assign v-lbl-2[1] = v-sname
                                  v-lbl-2[2] = contact.cust-name
                                  v-lbl-2[3] = contact.addr1
                                  v-lbl-2[4] = contact.addr2
                                  v-lbl-2[5] = contact.city + ", " + contact.state + " " + contact.zip
                                               + " " + contact.country.

     else if v-lbl-cnt = 3 then assign v-lbl-3[1] = v-sname
                                       v-lbl-3[2] = contact.cust-name
                                       v-lbl-3[3] = contact.addr1
                                       v-lbl-3[4] = contact.addr2
                                       v-lbl-3[5] = contact.city + ", " + contact.state + " " + contact.zip
                                               + " " + contact.country
                                       .

     if v-lbl-cnt >= 3 then do:
        if v-lbl-1[4] = "" then assign v-lbl-1[4] = v-lbl-1[5]
                                       v-lbl-1[5] = "".
        if v-lbl-2[4] = "" then assign v-lbl-2[4] = v-lbl-2[5]
                                       v-lbl-2[5] = "".
        if v-lbl-3[4] = "" then assign v-lbl-3[4] = v-lbl-3[5]
                                       v-lbl-3[5] = "".
        li-cnt = li-cnt + 1. 
        if li-cnt <> 8 then   put skip(1).  /* one line up - 8th line */ 
        if li-cnt > 10 then do:
           put skip(1).
           li-cnt = 0.  /* 10 line per page */
        end.
        display v-lbl-1[1 for 5]
                v-lbl-2[1 for 5]
                v-lbl-3[1 for 5]
                with frame lbl .
        down with frame lbl.        
        assign v-lbl-cnt = 0
               v-lbl-1 = ""
               v-lbl-2 = ""
               v-lbl-3 = "". 
     end.
   end.  /* for each */
   if v-lbl-cnt < 3 and (v-lbl-1[1] <> ""  or v-lbl-2[1] <> "") then do:
      if v-lbl-1[4] = "" then assign v-lbl-1[4] = v-lbl-1[5]
                                     v-lbl-1[5] = "".
      if v-lbl-2[4] = "" then assign v-lbl-2[4] = v-lbl-2[5]
                                     v-lbl-2[5] = "".
      put skip(1).
      display v-lbl-1[1 for 5]
              v-lbl-2[1 for 5]
             with frame lbl .
   end.
/* ==========  no merge here moved to contmail.w    
   if tbMailMrge /*and contact.maillist*/ then
   do:
      find first maillist where maillist.list-name = ls-mail-list no-lock no-error.
      if not avail maillist then return.

      for each mailcont of maillist no-lock,
          each contact no-lock where recid(contact) = mailcont.contact-rec 
                                 and contact.company eq selected-company
                   and contact.cust-no ge begin_cust-no
                   and contact.cust-no le end_cust-no
                   and contact.sman    ge begin_contact_sman
                   and contact.sman    le end_contact_sman
                   and contact.zip     ge begin_contact_zip
                   and contact.zip     le end_contact_zip
                BREAK BY contact.zip by contact.cust-no by contact.first-name:

          if print-head then    do:
              put stream s-mail unformatted
                  "sirname" v-delim
                  "firstname" v-delim
                  "lastname" v-delim
                  "companyname" v-delim
                  "address1" v-delim
                  "address2" v-delim
                  "city" v-delim
                  "state" v-delim
                  "zip" v-delim
                  "country" v-delim skip.
              assign print-head = no.
          end.

          v-cust-name = if avail cust then cust.name else "".

          put stream s-mail unformatted
              trim(contact.sirname) v-delim
              trim(mailcont.first-name) v-delim
              trim(mailcont.last-name) v-delim 
/*            trim(cust.name) v-delim */
              trim(v-cust-name) v-delim 
              trim(contact.addr1) v-delim
              trim(contact.addr2) v-delim
              trim(contact.city) v-delim
              trim(contact.state) v-delim
              trim(contact.zip) v-delim
              trim(contact.country) v-delim skip.
          create nosweat.note.
          assign nosweat.note.rec_key = contact.rec_key
               nosweat.note.note_date = TODAY
               nosweat.note.note_time = TIME
               nosweat.note.user_id = USERID("NOSWEAT")
               nosweat.note.note_title = FILL-IN-Title
               nosweat.note.note_text = "Automatic Note Generation from Mail Merge Report. " +
                                        string(note.note_date,"99/99/9999") + " " +
                                        string(note.note_time,"HH:MM:SS AM").
      end.  /* for each */

   end.  /* tbmailmrge */

   output stream s-mail close.
 =====================================*/

   output close.
   return.
end.
/* ==== end of label print ===============*/





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-jet C-Win 
PROCEDURE run-report-jet :
/*------------------------------------------------------------------------------
  Purpose:     frame for ink jet printer
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR v-delimit AS cha INIT "," FORM "x" NO-UNDO.

  assign list-name = v-dir + "\contlbl.rpt"
         init-dir = v-dir.

 do with frame {&frame-name}:
    assign selected-company begin_cust-no end_cust-no
           begin_contact_sman end_contact_sman
           begin_contact_zip end_contact_zip
           t-sortby /*tblastorder
           selected-printer */
           ls-mail-list.
 end.


def var i as int no-undo. 
def var v-start-compress as char init "" no-undo.
def var v-end-compress as char init "" no-undo.
def var v-delim as char format "x(1)" init '~~' no-undo.
def var print-head as log init yes no-undo.
def var v-last-date as date format "99/99/9999" label "Last Ord Date" no-undo.
def var v-cust-name like cust.name no-undo.
def var v-sname like sman.sname no-undo.
def var v-address as cha form "x(120)" no-undo.
def var v-lbl-1 as cha form "x(35)" extent 5 no-undo.
def var v-lbl-2 as cha form "x(35)" extent 5 no-undo.
def var v-lbl-3 as cha form "x(35)" extent 5 no-undo.
def var v-lbl-cnt as int no-undo.
def var v-addr1 as cha no-undo.
def var v-addr2 as cha no-undo.
def var v-city as cha no-undo.
def var v-state as cha no-undo.
def var v-zip as cha no-undo.
def var v-country as cha no-undo.
def var li-cnt as int no-undo.
form 
      skip(2)
      v-lbl-1[1] at 3 v-lbl-2[1] at 45 v-lbl-3[1] at 88 skip
      v-lbl-1[2] at 3 v-lbl-2[2] at 45 v-lbl-3[2] at 88 skip
      v-lbl-1[3] at 3 v-lbl-2[3] at 45 v-lbl-3[3] at 88 skip
      v-lbl-1[4] at 3 v-lbl-2[4] at 45 v-lbl-3[4] at 88 skip
      v-lbl-1[5] at 3 v-lbl-2[5] at 45 v-lbl-3[5] at 88 skip(1)
      with width 130 no-box no-label stream-io down frame lbl.

assign v-start-compress = ""
       v-end-compress = "".

IF tb_excel THEN DO:
   OUTPUT  STREAM st-excel TO VALUE(v-export-name).
   PUT STREAM st-excel "Customer#,Name,Address,,City,State,Zip,Last Order Date" skip.
END.

if t-sortby then do:  /* yes all the time */

    output to value(list-name) page-size 0.
    put skip(2).  /* for jet */

    find first asi.printer where asi.printer.company eq gcompany
                     and asi.printer.loc     eq gloc
                     and asi.printer.pr-no   eq selected-printer no-lock no-error.
    if avail asi.printer then
    do:
      if asi.printer.pr-cmd ne "" then
      do i = 1 to num-entries(asi.printer.pr-cmd):
        if entry(i,asi.printer.pr-cmd) ne ? then
          v-start-compress = v-start-compress +
                          chr(int(entry(i,asi.printer.pr-cmd))).
      end.
    end.

/*    put control v-start-compress. */


/*
for each cust where cust.company eq gcompany
                and cust.cust-no ge begin_cust-no
                and cust.cust-no le end_cust-no,
*/

    find first maillist where maillist.list-name = ls-mail-list no-lock no-error.
    if not avail maillist then return.

    for each mailcont of maillist no-lock where mailcont.maillist,
        each contact where recid(contact) = mailcont.contact-rec
                       and contact.company eq selected-company
                   and contact.cust-no ge begin_cust-no
                   and contact.cust-no le end_cust-no
                   and contact.sman    ge begin_contact_sman
                   and contact.sman    le end_contact_sman
                   and contact.zip     ge begin_contact_zip
                   and contact.zip     le end_contact_zip
                NO-LOCK 
                BREAK BY contact.zip by contact.cust-no by contact.first-name:
        find first cust where cust.company eq selected-company
                        and cust.cust-no eq contact.cust-no
                      no-lock no-error.
        find sman where sman.company eq selected-company
                  and sman.sman eq contact.sman
              no-lock no-error.


       /*v-sname = if avail sman then sman.sname else "".  */
       v-sname = contact.first-name + " " + contact.middle-initial + " " + contact.last-name.
       /* ========== address display    ==================== */
       v-lbl-cnt = v-lbl-cnt + 1.
       if v-lbl-cnt = 1 then assign v-lbl-1[1] = v-sname
                                  v-lbl-1[2] = contact.cust-name
                                  v-lbl-1[3] = contact.addr1
                                  v-lbl-1[4] = contact.addr2
                                  v-lbl-1[5] = contact.city + ", " + contact.state + " " + contact.zip
                                               + " " + contact.country
                                               .
       else if v-lbl-cnt = 2 then assign v-lbl-2[1] = v-sname
                                  v-lbl-2[2] = contact.cust-name
                                  v-lbl-2[3] = contact.addr1
                                  v-lbl-2[4] = contact.addr2
                                  v-lbl-2[5] = contact.city + ", " + contact.state + " " + contact.zip
                                               + " " + contact.country.

     else if v-lbl-cnt = 3 then assign v-lbl-3[1] = v-sname
                                       v-lbl-3[2] = contact.cust-name
                                       v-lbl-3[3] = contact.addr1
                                       v-lbl-3[4] = contact.addr2
                                       v-lbl-3[5] = contact.city + ", " + contact.state + " " + contact.zip
                                               + " " + contact.country
                                       .
     if tbLastOrder THEN do:
           assign v-last-date = ?.
           for each oe-ord where oe-ord.company eq gcompany
                      and oe-ord.cust-no eq contact.cust-no
                      no-lock break by oe-ord.ord-date desc:
               assign v-last-date = oe-ord.ord-date.
               leave.
           end.
     end.
     IF tb_excel THEN DO:
          PUT STREAM st-excel contact.cust-no v-delimit
                     contact.cust-name v-delimit
                     contact.addr1 v-delimit
                     contact.addr2 v-delimit
                     contact.city v-delimit
                     contact.state v-delimit
                     contact.zip .
          IF tblastorder THEN PUT STREAM st-excel v-delimit v-last-date SKIP.
          ELSE PUT STREAM st-excel SKIP.
     END.

     if v-lbl-cnt >= 3 then do:
        if v-lbl-1[4] = "" then assign v-lbl-1[4] = v-lbl-1[5]
                                       v-lbl-1[5] = "".
        if v-lbl-2[4] = "" then assign v-lbl-2[4] = v-lbl-2[5]
                                       v-lbl-2[5] = "".
        if v-lbl-3[4] = "" then assign v-lbl-3[4] = v-lbl-3[5]
                                       v-lbl-3[5] = "".
        li-cnt = li-cnt + 1. 

        if li-cnt mod 3 = 1 and li-cnt <> 1 then put skip(1).

        display v-lbl-1[1 for 5]
                v-lbl-2[1 for 5]
                v-lbl-3[1 for 5]
                with frame lbl .
        down with frame lbl.        
        if li-cnt > 9 then do:
            /*       put skip(3).  works fine in office john's printer*/
            put skip(5).
           li-cnt = 0.  /* 10 line per page */
        end.

        assign v-lbl-cnt = 0
               v-lbl-1 = ""
               v-lbl-2 = ""
               v-lbl-3 = "". 
     end.
   end.  /* for each */
   if v-lbl-cnt < 3 and (v-lbl-1[1] <> ""  or v-lbl-2[1] <> "") then do:
      if v-lbl-1[4] = "" then assign v-lbl-1[4] = v-lbl-1[5]
                                     v-lbl-1[5] = "".
      if v-lbl-2[4] = "" then assign v-lbl-2[4] = v-lbl-2[5]
                                     v-lbl-2[5] = "".
      put skip(1).
      display v-lbl-1[1 for 5]
              v-lbl-2[1 for 5]
             with frame lbl .
   end.
   OUTPUT STREAM st-excel CLOSE.
   output close.
   return.
end.
/* ==== end of label print ===============*/




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

