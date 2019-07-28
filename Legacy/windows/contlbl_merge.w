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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-3 begin_cust-no end_cust-no ~
begin_contact_sman end_contact_sman begin_contact_zip end_contact_zip ~
tbMailMrge rd-dest btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS selected-company begin_name_description ~
begin_cust-no end_name_description end_cust-no begin_contact_sman-dscr ~
begin_contact_sman end_contact_sman-dscr end_contact_sman begin_contact_zip ~
end_contact_zip tbMailMrge ls-mail-list filename FILL-IN-Title rd-dest 

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
     LABEL "Beginning Salesman" 
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
     LABEL "Ending Salesman" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_contact_sman-dscr AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31.6 BY 1
     BGCOLOR 7 .

DEFINE VARIABLE end_contact_zip AS CHARACTER FORMAT "X(10)" INITIAL "99999" 
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

DEFINE VARIABLE filename AS CHARACTER FORMAT "X(80)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64 BY 1 TOOLTIP "Enter Mail Merge Filename" NO-UNDO.

DEFINE VARIABLE FILL-IN-Title AS CHARACTER FORMAT "X(80)":U 
     LABEL "Title for Mail Merge" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64 BY 1 TOOLTIP "Title for Current Mailing" NO-UNDO.

DEFINE VARIABLE ls-mail-list AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mail List" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(10)" INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE selected-printer AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Printer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 TOOLTIP "Enter Printer Number Only when Printing to Printer".

DEFINE VARIABLE rd-dest AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.33 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 117 BY 3.81.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 4.52.

DEFINE VARIABLE t-sortby AS LOGICAL INITIAL yes 
     LABEL "Sort By Zip Code" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tbMailMrge AS LOGICAL INITIAL no 
     LABEL "Create Mail Merge File" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 TOOLTIP "Check to Create Mail Merge" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     selected-company AT ROW 1.95 COL 29 COLON-ALIGNED
     begin_name_description AT ROW 2.86 COL 49.4 COLON-ALIGNED HELP
          "Enter Beginning Description" NO-LABEL
     begin_cust-no AT ROW 2.91 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_name_description AT ROW 3.81 COL 49.4 COLON-ALIGNED HELP
          "Enter Ending Description" NO-LABEL
     end_cust-no AT ROW 3.86 COL 29 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_contact_sman-dscr AT ROW 4.76 COL 49.4 COLON-ALIGNED HELP
          "Enter Beginning Description" NO-LABEL
     begin_contact_sman AT ROW 4.81 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Salesman"
     end_contact_sman-dscr AT ROW 5.71 COL 49.4 COLON-ALIGNED HELP
          "Enter Ending Description" NO-LABEL
     end_contact_sman AT ROW 5.76 COL 29 COLON-ALIGNED HELP
          "Enter Ending Salesman"
     begin_contact_zip AT ROW 6.71 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Salesman"
     end_contact_zip AT ROW 7.67 COL 29 COLON-ALIGNED HELP
          "Enter Ending Salesman"
     t-sortby AT ROW 8.14 COL 74
     selected-printer AT ROW 8.62 COL 29 COLON-ALIGNED HELP
          "Enter Printer for Control Codes"
     tbMailMrge AT ROW 10.29 COL 5
     ls-mail-list AT ROW 10.76 COL 48 COLON-ALIGNED
     filename AT ROW 11.71 COL 48 COLON-ALIGNED
     FILL-IN-Title AT ROW 12.67 COL 48 COLON-ALIGNED
     rd-dest AT ROW 15.76 COL 9 NO-LABEL
     btn-ok AT ROW 19.81 COL 21
     btn-cancel AT ROW 19.81 COL 53
     RECT-6 AT ROW 14.81 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.57 COL 5
     RECT-3 AT ROW 10.05 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 121.8 BY 20.71.


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
         HEIGHT             = 20.95
         WIDTH              = 122.4
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

IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

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
/* SETTINGS FOR FILL-IN filename IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       filename:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN FILL-IN-Title IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-Title:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

/* SETTINGS FOR FILL-IN ls-mail-list IN FRAME FRAME-A
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
       tbMailMrge:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

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
    if tbmailmrge  and
       not can-find(first maillist where maillist.list-name = ls-mail-list:screen-value)
    then do:
        message "Invalid Mail List. Try Help." view-as alert-box error.
        apply "entry" to ls-mail-list in frame {&frame-name}.
        return no-apply.
    end.
    if tbmailmrge and filename:screen-value = ""
    then do:
        message "Mail Merge File must be entered. Try Help." view-as alert-box error.
        apply "entry" to filename .
        return no-apply.
    end.


  session:set-wait-state("general").
  assign rd-dest.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
/*       when 4 then run output-to-label. */
  end case. 
  session:set-wait-state("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filename C-Win
ON HELP OF filename IN FRAME FRAME-A /* Filename */
DO:
  DEFINE VARIABLE selected-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE sel-ok AS LOG NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  init-dir = "users\" + USERID("NOSWEAT").
  selected-name = {&SELF-NAME}:SCREEN-VALUE.
  SYSTEM-DIALOG GET-FILE selected-name
      TITLE      "Choose Mail Merge File to SAVE ..."
      FILTERS    "Text Files (*.txt)" "*.txt"
      INITIAL-DIR init-dir
      ASK-OVERWRITE
      USE-FILENAME
      UPDATE sel-ok.
  IF NOT sel-ok THEN
  RETURN NO-APPLY.
  {&SELF-NAME}:SCREEN-VALUE = selected-name.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filename C-Win
ON LEAVE OF filename IN FRAME FRAME-A /* Filename */
DO:
   if tbmailmrge and lastkey <> -1 and
      self:screen-value = ""
    then do:
        message "Mail Merge File must be entered. Try Help." view-as alert-box error.
        return no-apply.
    end.

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
    if tbmailmrge and lastkey <> -1 and
       not can-find(first maillist where maillist.list-name = self:screen-value)
    then do:
        message "Invalid Mail List. Try Help." view-as alert-box error.
        return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
    assign rd-dest.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbMailMrge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbMailMrge C-Win
ON VALUE-CHANGED OF tbMailMrge IN FRAME FRAME-A /* Create Mail Merge File */
DO:
  assign tbmailmrge.
  if tbMailMrge:checked then
    enable filename 
           fill-in-title 
           ls-mail-list
           with frame {&frame-name}.
  else
    disable filename 
            fill-in-title
            ls-mail-list
             with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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


  RUN enable_UI.

  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY selected-company begin_name_description begin_cust-no 
          end_name_description end_cust-no begin_contact_sman-dscr 
          begin_contact_sman end_contact_sman-dscr end_contact_sman 
          begin_contact_zip end_contact_zip tbMailMrge ls-mail-list filename 
          FILL-IN-Title rd-dest 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-3 begin_cust-no end_cust-no begin_contact_sman 
         end_contact_sman begin_contact_zip end_contact_zip tbMailMrge rd-dest 
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

     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
  if t-sortby then 
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 0, INPUT 0, INPUT 0, OUTPUT result).    /* portrait */
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
/*  def var lv-addr1 as cha no-undo.
  def var lv-addr2 as cha no-undo.
  def var lv-city as cha no-undo.
  def var lv-tel as cha no-undo.
  def var lv-sman-name as cha no-undo.

  def var lv-chg-method as cha no-undo.
  def var lv-fob as cha no-undo.
*/
 /* if search("c:\temp") <> ? then 
     assign list-name = "c:\temp\rpttmp.rpt"
            init-dir = "c:\temp"  .
  else assign list-name = "c:\tmp\rpttmp.rpt"
              init-dir = "c:\tmp".

  assign list-name = "users/" + USERID("NOSWEAT") + "~/" + "RFQ_list.rpt"
         init-dir = "users/" + USERID("NOSWEAT") + "~/".  
*/ 
  assign list-name = "c:\temp\contlbl.rpt"
         init-dir = "c:\temp".

 do with frame {&frame-name}:
    assign selected-company begin_cust-no end_cust-no
           begin_contact_sman end_contact_sman
           begin_contact_zip end_contact_zip
           t-sortby /*tblastorder
           selected-printer */
           tbmailmrge filename fill-in-title
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

    find first printer where printer.company eq gcompany
                     and printer.loc     eq gloc
                     and printer.pr-no   eq selected-printer no-lock no-error.
    if avail printer then
    do:
      if printer.pr-cmd ne "" then
      do i = 1 to num-entries(printer.pr-cmd):
        if entry(i,printer.pr-cmd) ne ? then
          v-start-compress = v-start-compress +
                          chr(int(entry(i,printer.pr-cmd))).
      end.
    end.

    put control v-start-compress.

    if tbMailMrge then
       output stream s-mail to value(filename) page-size 0.

/*
for each cust where cust.company eq gcompany
                and cust.cust-no ge begin_cust-no
                and cust.cust-no le end_cust-no,
*/

   for each contact where contact.company eq selected-company
                   and contact.cust-no ge begin_cust-no
                   and contact.cust-no le end_cust-no
                   and contact.sman    ge begin_contact_sman
                   and contact.sman    le end_contact_sman
                   and contact.zip     ge begin_contact_zip
                   and contact.zip     le end_contact_zip
                NO-LOCK 
                BREAK BY contact.zip by cust-no by contact.first-name:
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
        create notes.
        assign notes.rec_key = contact.rec_key
               notes.note_date = TODAY
               notes.note_time = TIME
               notes.user_id = USERID("NOSWEAT")
               notes.note_title = FILL-IN-Title
               notes.note_text = "Automatic Note Generation from Mail Merge Report. " +
                                        string(notes.note_date,"99/99/9999") + " " +
                                        string(notes.note_time,"HH:MM:SS AM").
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
          create notes.
          assign notes.rec_key = contact.rec_key
               notes.note_date = TODAY
               notes.note_time = TIME
               notes.user_id = USERID("NOSWEAT")
               notes.note_title = FILL-IN-Title
               notes.note_text = "Automatic Note Generation from Mail Merge Report. " +
                                        string(notes.note_date,"99/99/9999") + " " +
                                        string(notes.note_time,"HH:MM:SS AM").
      end.  /* for each */

   end.  /* tbmailmrge */

   output stream s-mail close.

   output close.
   return.
end.
/* ==== end of label print ===============*/





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


