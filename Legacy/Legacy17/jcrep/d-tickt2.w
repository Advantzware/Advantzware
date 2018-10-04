&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  /****** This file is not used anymore.  No need to modify it.
          Task 03300902                                         ***********/

  File: jcrep\d-tickt2.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF INPUT PARAM ip-job-no AS cha NO-UNDO.
DEF INPUT PARAM ip-job-no2 AS INT NO-UNDO.

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
 cocode = g_company
 locode = g_loc.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLJobTicket &Company=cocode} /* rstark 05181205 */
DEF NEW SHARED VAR lv-qty AS int NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.
DEF VAR lv-format-c AS CHAR NO-UNDO.
DEF VAR lv-default-f AS CHAR NO-UNDO.
DEF VAR lv-default-c AS CHAR NO-UNDO.
DEF VAR lv-int-f AS INT NO-UNDO.
DEF VAR lv-int-c AS INT NO-UNDO.
DEF VAR fl-jobord AS INT NO-UNDO.
DEFINE VARIABLE tb_app-unprinted AS LOGICAL INITIAL NO.

{jcrep/r-ticket.i "new shared"}
{custom/xprint.i}

DEF NEW SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF NEW SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF NEW SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF NEW SHARED VAR s-committed-board-only AS LOG NO-UNDO.
DEF NEW SHARED VAR s-sample-required AS LOG NO-UNDO.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

{cerep/jc-keyst.i "NEW"}
{cerep/jc-keys2.i "NEW"}
{cecrep/jc-prem.i "NEW"}
{cecrep/jc-soule.i "NEW"}
{cerep/tt-samp-ctn.i "NEW"}
{cecrep/jc-pallet.i "NEW"}
DEF TEMP-TABLE t-ef-form
   FIELD form-no LIKE ef.form-no.

DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF NEW SHARED VAR s-prt-label AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-ship-split AS LOG NO-UNDO.
{cecrep/jc-fibre.i "NEW"}
/*{cecrep/tt-artios.i "NEW"}*/

DEF BUFFER b-reftable-freeze FOR reftable.
DEF BUFFER b-reftable-split FOR reftable.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-42 RECT-6 RECT-7 begin_job1 begin_job2 ~
end_job1 end_job2 tb_fold tb_corr tb_reprint tb_box tb_fgimage spec_codes ~
tb_prt-label tb_committed tb_prt-set-header tb_prompt-ship tb_freeze-note ~
TB_sample_req tb_make_hold lines-per-page rd-dest lv-ornt td-show-parm ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job1 begin_job2 end_job1 end_job2 ~
tb_fold tb_corr tb_reprint tb_box tb_fgimage spec_codes tb_prt-mch ~
rd_print-speed tb_prt-shipto tb_prt-sellprc tb_prt-label tb_committed ~
tb_prt-set-header tb_prompt-ship tb_freeze-note TB_sample_req tb_make_hold ~
lines-per-page rd-dest lv-ornt lv-font-no lv-font-name td-show-parm ~
v-run-speed 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_job1 begin_job2 end_job1 end_job2 tb_reprint ~
tb_box tb_fgimage tb_prt-mch tb_tray-2 tb_prt-shipto tb_prt-sellprc ~
tb_prt-label td-show-parm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_job1 AS CHARACTER FORMAT "x(6)" 
     LABEL "Beginning  Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE end_job1 AS CHARACTER FORMAT "x(6)" INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE end_job2 AS INTEGER FORMAT ">9" INITIAL 99 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

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

DEFINE VARIABLE spec_codes AS CHARACTER FORMAT "X(256)":U INITIAL "QA" 
     LABEL "Spec Codes" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE v-run-speed AS CHARACTER FORMAT "X(256)":U INITIAL "Print Machine's Speed or Run Hour ?" 
      VIEW-AS TEXT 
     SIZE 37 BY .62 NO-UNDO.

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

DEFINE VARIABLE rd_print-speed AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Speed", "S",
"Run Hour", "H"
     SIZE 27 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 15.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 25.

DEFINE VARIABLE tb_box AS LOGICAL INITIAL yes 
     LABEL "Print Box Design?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_committed AS LOGICAL INITIAL no 
     LABEL "Print Only Committed Board?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .91 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL no 
     LABEL "Corrware" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_DC AS LOGICAL INITIAL no 
     LABEL "Print &Die Cutter Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fgimage AS LOGICAL INITIAL no 
     LABEL "Print FG Item Image?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fold AS LOGICAL INITIAL no 
     LABEL "Foldware" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_freeze-note AS LOGICAL INITIAL no 
     LABEL "Freeze Job Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tb_GL AS LOGICAL INITIAL no 
     LABEL "Print &Gluer /  Window Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_make_hold AS LOGICAL INITIAL no 
     LABEL "Print Hold?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_PR AS LOGICAL INITIAL no 
     LABEL "Print &Printer Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prompt-ship AS LOGICAL INITIAL no 
     LABEL "Prompt Split Shipment or Split Order?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .91 NO-UNDO.

DEFINE VARIABLE tb_prt-label AS LOGICAL INITIAL no 
     LABEL "Print Label Info?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .91 NO-UNDO.

DEFINE VARIABLE tb_prt-mch AS LOGICAL INITIAL no 
     LABEL "Print Machine Standard?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-sellprc AS LOGICAL INITIAL no 
     LABEL "Print Sell Price in place of UPC#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .91 NO-UNDO.

DEFINE VARIABLE tb_prt-set-header AS LOGICAL INITIAL no 
     LABEL "Print Set Unitization Page?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-shipto AS LOGICAL INITIAL no 
     LABEL "Print Shipto?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .91 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Tickets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_RS AS LOGICAL INITIAL no 
     LABEL "Print &Sheeter Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE TB_sample_req AS LOGICAL INITIAL no 
     LABEL "Sample(s) Required?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tb_SW AS LOGICAL INITIAL no 
     LABEL "Print Shrink &Wrap Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_tray-2 AS LOGICAL INITIAL no 
     LABEL "Copy 2 and 3 in Tray 2?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_job1 AT ROW 2.43 COL 22 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job2 AT ROW 2.43 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Run#"
     end_job1 AT ROW 2.43 COL 64 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     end_job2 AT ROW 2.43 COL 80 COLON-ALIGNED HELP
          "Enter Ending Run#"
     tb_fold AT ROW 3.62 COL 37
     tb_RS AT ROW 3.62 COL 61.6
     tb_corr AT ROW 4.57 COL 37
     tb_PR AT ROW 4.62 COL 61.6
     tb_DC AT ROW 5.62 COL 61.6
     tb_reprint AT ROW 5.76 COL 37
     tb_GL AT ROW 6.62 COL 61.6
     tb_box AT ROW 6.71 COL 58 RIGHT-ALIGNED
     tb_fgimage AT ROW 7.67 COL 62 RIGHT-ALIGNED
     tb_SW AT ROW 7.67 COL 61.6
     spec_codes AT ROW 8.62 COL 35 COLON-ALIGNED
     tb_prt-mch AT ROW 9.81 COL 37
     tb_tray-2 AT ROW 9.81 COL 91 RIGHT-ALIGNED WIDGET-ID 6
     rd_print-speed AT ROW 10.76 COL 55 NO-LABEL
     tb_prt-shipto AT ROW 11.95 COL 8
     tb_prt-sellprc AT ROW 11.95 COL 46
     tb_prt-label AT ROW 12.81 COL 8
     tb_committed AT ROW 12.81 COL 46
     tb_prt-set-header AT ROW 13.81 COL 8
     tb_prompt-ship AT ROW 13.81 COL 46
     tb_freeze-note AT ROW 14.71 COL 8
     TB_sample_req AT ROW 14.81 COL 46
     tb_make_hold AT ROW 14.81 COL 72 WIDGET-ID 8
     lines-per-page AT ROW 16.48 COL 54 COLON-ALIGNED
     rd-dest AT ROW 17.19 COL 5 NO-LABEL
     lv-ornt AT ROW 17.91 COL 31 NO-LABEL
     lv-font-no AT ROW 19.57 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 20.52 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.19 COL 31
     btn-ok AT ROW 24.57 COL 24
     btn-cancel AT ROW 24.57 COL 58
     v-run-speed AT ROW 11 COL 15 COLON-ALIGNED NO-LABEL
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.24 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-42 AT ROW 1 COL 1
     RECT-6 AT ROW 16 COL 1
     RECT-7 AT ROW 1 COL 1
     SPACE(1.00) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN begin_job1 IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       begin_job1:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_job2 IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       begin_job2:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job1 IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       end_job1:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job2 IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       end_job2:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd_print-speed IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_box IN FRAME Dialog-Frame
   ALIGN-R 1                                                            */
ASSIGN 
       tb_box:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_DC IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_DC:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_fgimage IN FRAME Dialog-Frame
   ALIGN-R 1                                                            */
ASSIGN 
       tb_fgimage:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_GL IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_GL:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       tb_make_hold:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_PR IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_PR:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_prt-label IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       tb_prt-label:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-mch IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-mch:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-sellprc IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-sellprc:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-shipto IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-shipto:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_reprint IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_RS IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_RS:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_SW IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_SW:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_tray-2 IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
       tb_tray-2:HIDDEN IN FRAME Dialog-Frame           = TRUE
       tb_tray-2:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN v-run-speed IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame
ANYWHERE
DO:
  
   IF SELF:TYPE <> "Button" THEN  do:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
   ELSE do:
       APPLY "choose" TO self.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame
DO:
  IF lv-format-f = 'Indiana-XL' AND tb_fold:CHECKED IN FRAME {&FRAME-NAME} THEN
     run CleanUp. 

  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job1 Dialog-Frame
ON LEAVE OF begin_job1 IN FRAME Dialog-Frame /* Beginning  Job# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 Dialog-Frame
ON LEAVE OF begin_job2 IN FRAME Dialog-Frame /* - */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   IF lv-format-f = 'Indiana-XL' AND tb_fold:CHECKED IN FRAME {&FRAME-NAME} THEN
     run CleanUp.

   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR hold-title AS CHAR NO-UNDO.

  /****** This file is not used anymore.  No need to modify it.
          Task 03300902                            ***********/

  ASSIGN {&DISPLAYED-OBJECTS}
         s-prt-mstandard = tb_prt-mch
         s-prt-shipto  = tb_prt-shipto
         s-prt-sellprc = tb_prt-sellprc
         s-run-speed = rd_print-speed = "S"
         s-sample-required = TB_sample_req
         hold-title = FRAME {&FRAME-NAME}:TITLE.

  IF tb_tray-2:HIDDEN = NO THEN
     ASSIGN tb_tray-2.

  IF tb_fold THEN DO:
    /*lines-per-page = IF lv-format-f EQ "HOP" THEN 64 ELSE 58. */

    RUN run-report ("Fold").

    FRAME {&FRAME-NAME}:TITLE = "Foldware " + TRIM(hold-title).

    case rd-dest:
       when 1 then run output-to-printer.
       when 2 then do:
           RUN output-to-screen.
       END.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_job1
                            &END_cust=END_job1
                            &fax-subject=hold-title
                            &fax-body=hold-title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = hold-title
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject=hold-title
                             &mail-body=hold-title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = hold-title
                                  &begin_cust= begin_job1
                                  &END_cust=end_job1
                                  &mail-subject=hold-title
                                  &mail-body=hold-title
                                  &mail-file=list-name }

           END.
 
       END. 
       WHEN 6 THEN run output-to-port.
  end case. 

    FRAME {&frame-name}:TITLE = hold-title.
  END.

  IF tb_corr THEN DO:
    /*lines-per-page = 0. ??? */

    RUN run-report ("Corr").

    FRAME {&frame-name}:TITLE = "Corrware " + TRIM(FRAME {&frame-name}:TITLE).

    case rd-dest:
       when 1 then run output-to-printer.
       when 2 then do:
           RUN output-to-screen.      
       END.
       when 3 then run output-to-file.
       when 4 then run output-to-fax.
       when 5 THEN RUN OUTPUT-to-mail.
       WHEN 6 THEN run output-to-port.
  end case. 
      FRAME {&frame-name}:TITLE = hold-title.      
  END.
  
  IF rd-dest = 2  THEN DO:
      /* not working well. keep running background
       APPLY "close":U TO this-procedure.
       RETURN NO-APPLY. 
      */  
      APPLY "entry" TO btn-cancel.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job1 Dialog-Frame
ON LEAVE OF end_job1 IN FRAME Dialog-Frame /* Ending Job# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 Dialog-Frame
ON LEAVE OF end_job2 IN FRAME Dialog-Frame /* - */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page Dialog-Frame
ON LEAVE OF lines-per-page IN FRAME Dialog-Frame /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no Dialog-Frame
ON HELP OF lv-font-no IN FRAME Dialog-Frame /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no Dialog-Frame
ON LEAVE OF lv-font-no IN FRAME Dialog-Frame /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt Dialog-Frame
ON LEAVE OF lv-ornt IN FRAME Dialog-Frame
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt Dialog-Frame
ON VALUE-CHANGED OF lv-ornt IN FRAME Dialog-Frame
DO:
  {custom/chgfont.i}
  lines-per-page = IF SELF:SCREEN-VALUE = "L" THEN 58 ELSE 99.
  DISP lines-per-page WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest Dialog-Frame
ON VALUE-CHANGED OF rd-dest IN FRAME Dialog-Frame
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_codes Dialog-Frame
ON LEAVE OF spec_codes IN FRAME Dialog-Frame /* Spec Codes */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_box Dialog-Frame
ON VALUE-CHANGED OF tb_box IN FRAME Dialog-Frame /* Print Box Design? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr Dialog-Frame
ON VALUE-CHANGED OF tb_corr IN FRAME Dialog-Frame /* Corrware */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_DC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_DC Dialog-Frame
ON VALUE-CHANGED OF tb_DC IN FRAME Dialog-Frame /* Print Die Cutter Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fgimage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fgimage Dialog-Frame
ON VALUE-CHANGED OF tb_fgimage IN FRAME Dialog-Frame /* Print FG Item Image? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold Dialog-Frame
ON VALUE-CHANGED OF tb_fold IN FRAME Dialog-Frame /* Foldware */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_GL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_GL Dialog-Frame
ON VALUE-CHANGED OF tb_GL IN FRAME Dialog-Frame /* Print Gluer /  Window Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_make_hold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_make_hold Dialog-Frame
ON VALUE-CHANGED OF tb_make_hold IN FRAME Dialog-Frame /* Print Hold? */
DO:
   ASSIGN TB_sample_req.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_PR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_PR Dialog-Frame
ON VALUE-CHANGED OF tb_PR IN FRAME Dialog-Frame /* Print Printer Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-label Dialog-Frame
ON VALUE-CHANGED OF tb_prt-label IN FRAME Dialog-Frame /* Print Label Info? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-mch Dialog-Frame
ON VALUE-CHANGED OF tb_prt-mch IN FRAME Dialog-Frame /* Print Machine Standard? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-sellprc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-sellprc Dialog-Frame
ON VALUE-CHANGED OF tb_prt-sellprc IN FRAME Dialog-Frame /* Print Sell Price in place of UPC#? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-shipto Dialog-Frame
ON VALUE-CHANGED OF tb_prt-shipto IN FRAME Dialog-Frame /* Print Shipto? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint Dialog-Frame
ON VALUE-CHANGED OF tb_reprint IN FRAME Dialog-Frame /* Reprint Tickets? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_RS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_RS Dialog-Frame
ON VALUE-CHANGED OF tb_RS IN FRAME Dialog-Frame /* Print Sheeter Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TB_sample_req
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TB_sample_req Dialog-Frame
ON VALUE-CHANGED OF TB_sample_req IN FRAME Dialog-Frame /* Sample(s) Required? */
DO:
   ASSIGN TB_sample_req.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_SW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_SW Dialog-Frame
ON VALUE-CHANGED OF tb_SW IN FRAME Dialog-Frame /* Print Shrink Wrap Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tray-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tray-2 Dialog-Frame
ON VALUE-CHANGED OF tb_tray-2 IN FRAME Dialog-Frame /* Copy 2 and 3 in Tray 2? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm Dialog-Frame
ON VALUE-CHANGED OF td-show-parm IN FRAME Dialog-Frame /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

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

  /****** This file is not used anymore.  No need to modify it.
          Task 03300902                            ***********/
  
  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_fold = NOT AVAIL sys-ctrl           OR
             sys-ctrl.char-fld EQ "Both"  OR
             sys-ctrl.char-fld EQ "Foldware"
   tb_corr = AVAIL sys-ctrl AND
             (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").
  
  IF tb_fold THEN DO:
    {sys/inc/jobcard.i "F"}
    ASSIGN
     lv-format-f = sys-ctrl.char-fld
     lv-int-f    = sys-ctrl.int-fld
     lv-default-f = sys-ctrl.char-fld.
    IF lookup(lv-format-f,"Interpac,FibreFC,HPB,metro,Dayton,Livngstn,CentBox,Frankstn,Colonial,Unipak,Ottpkg,Shelby,CCC,Accord,Carded") > 0 THEN lines-per-page = 55.
  END.  

  IF tb_corr THEN DO:
    {sys/inc/jobcard.i "C"}
    ASSIGN
     lv-format-c = sys-ctrl.char-fld
     lv-int-c    = sys-ctrl.int-fld
     lv-default-f = sys-ctrl.char-fld.
  END.
  ASSIGN begin_job1 = ip-job-no
         begin_job2 = ip-job-no2
         end_job1 = ip-job-no
         end_job2 = ip-job-no2.
  
  FIND FIRST job WHERE job.company = g_company
                   AND job.job-no = ip-job-no
                   AND job.job-no2 = ip-job-no2 NO-LOCK NO-ERROR.
  IF AVAIL job THEN FIND FIRST est WHERE est.company = g_company
                                     AND est.est-no = job.est-no NO-LOCK NO-ERROR.
  IF AVAIL est THEN DO:
     IF est.est-type < 5 THEN ASSIGN tb_fold = YES
                                     tb_corr = NO.
     ELSE ASSIGN tb_fold = NO
                 tb_corr = YES.
  END.

  RUN enable_UI.
  
  DO WITH FRAME {&frame-name}:
    
     /*Fibre doesn't want things remembered task 09120706 */
     IF NOT(lv-format-f EQ "FibreFC" OR lv-format-c EQ "Artios") THEN
     DO:
        v-prgmname = "r-ticket.".
        {custom/usrprint.i}

        tb_freeze-note:SCREEN-VALUE = "NO".

        IF can-find(FIRST b-reftable-freeze WHERE
           b-reftable-freeze.reftable EQ "FREEZENOTE" AND
           b-reftable-freeze.company  EQ g_company AND
           b-reftable-freeze.loc      EQ ip-job-no AND
           b-reftable-freeze.CODE     EQ STRING(ip-job-no2,"99")) THEN
           tb_freeze-note:SCREEN-VALUE = "YES".
     END.

     TB_sample_req:HIDDEN = TRUE.

     FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
     ASSIGN
        tb_fold = NOT AVAIL sys-ctrl           OR
                  sys-ctrl.char-fld EQ "Both"  OR
                  sys-ctrl.char-fld EQ "Foldware"
        tb_corr = AVAIL sys-ctrl AND
                  (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").
     
     IF tb_fold THEN DO:
        {sys/inc/jobcard.i "F"}
        ASSIGN
        lv-format-f = sys-ctrl.char-fld
        lv-default-f = sys-ctrl.char-fld.
        IF lookup(lv-format-f,"Interpac,Dayton,FibreFC,Livngstn,Frankstn,CentBox,Colonial,Unipak,Ottpkg,Shelby,CCC,Accord,Carded") > 0 THEN lines-per-page = 55.
     END.  
     
     IF tb_corr THEN DO:
      {sys/inc/jobcard.i "C"}
      ASSIGN
      lv-format-c = sys-ctrl.char-fld
      lv-default-c = sys-ctrl.char-fld . 
     END.
     ASSIGN begin_job1 = ip-job-no
            begin_job2 = ip-job-no2
            end_job1 = ip-job-no
            end_job2 = ip-job-no2.

     IF TRIM(begin_job1) NE ""                          AND
       TRIM(begin_job1) EQ TRIM(end_job1)  THEN DO:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(begin_job1)) +
                                   TRIM(begin_job1)
            AND job-hdr.job-no2 EQ INT(begin_job2)
          NO-ERROR.

      IF AVAIL job-hdr THEN DO:
             FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = "JOBCARDC" AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                           /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-f = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-f = lv-default-f .

              FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "JOBCARDF" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                    /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.
                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-f = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-f = lv-default-f .
      END.
    END.
     
     FIND FIRST job WHERE job.company = g_company
                      AND job.job-no = ip-job-no
                      AND job.job-no2 = ip-job-no2 NO-LOCK NO-ERROR.
     IF AVAIL job THEN FIND FIRST est WHERE est.company = g_company
                                        AND est.est-no = job.est-no NO-LOCK NO-ERROR.
     IF AVAIL est THEN DO:
        IF est.est-type < 5 THEN ASSIGN tb_fold = YES
                                        tb_corr = NO.
        ELSE ASSIGN tb_fold = NO
                    tb_corr = YES.
     END.
     tb_reprint = NO.
     DISPLAY begin_job1 begin_job2 END_job1 END_job2 tb_fold tb_corr lines-per-page tb_reprint.
     
     IF lv-format-c = "pacific" THEN DO:
         lv-ornt = "L".
         APPLY "value-changed" TO lv-ornt .
     END.
     IF tb_corr:SCREEN-VALUE EQ "No" AND lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.
     
     IF tb_fold AND (lv-format-f = "Interpac" OR lv-format-f = "Dayton" 
                     OR lv-format-f = "Livngstn" OR lv-format-f = "FibreFC" OR lv-format-f = "HPB"
                     OR lv-format-f = "metro"     or lv-format-f = "Indiana-XL" 
                     OR lv-format-f = "CentBox" OR lv-format-f = "Keystone" OR lv-format-f = "Ottpkg"
                     OR lv-format-f = "Frankstn" OR lv-format-f = "Colonial" OR lv-format-f = "Unipak"
                     OR lv-format-f = "MWFibre"  OR lv-format-f = "Shelby"  OR lv-format-f = "CCC"
                     OR lv-format-f = "Accord" OR lv-format-f = "Carded" ) THEN
        assign tb_prt-mch:SENSITIVE = YES
               tb_prt-shipto:SENSITIVE = YES
               tb_prt-sellprc:SENSITIVE = YES
               rd_print-speed:SENSITIVE = YES .            
     ELSE do:
         ASSIGN tb_prt-mch = NO
                tb_prt-shipto = NO
                tb_prt-sellprc = NO.
         ASSIGN tb_prt-mch:SCREEN-VALUE = "No"
                tb_prt-shipto:SCREEN-VALUE = "No"
                tb_prt-sellprc:SCREEN-VALUE = "No" 
                rd_print-speed:SCREEN-VALUE = "S".
     END.

     if can-do ("Indiana-XL", lv-format-f) THEN
        run HideDeptBoxes (no).
     else
        run HideDeptBoxes (yes).

    IF tb_corr = TRUE AND lv-format-c = "Hughes" THEN
       TB_sample_req:HIDDEN = FALSE.
    ELSE
       TB_sample_req:HIDDEN = TRUE.

     IF lookup(lv-format-c,"Artios,Premier,Xprint,Suthrlnd,United,Hughes,CapCity,PQP,RFC2") <= 0 THEN 
        ASSIGN tb_prt-set-header:SCREEN-VALUE = "No"
               tb_prt-set-header:SENSITIVE = NO.
     ELSE ASSIGN tb_prt-set-header:SCREEN-VALUE = "No"
               tb_prt-set-header:SENSITIVE = YES  .
     
     IF tb_fold:SCREEN-VALUE EQ "Yes" AND lv-format-f = "Indiana-XL" THEN
        ASSIGN
           tb_RS:screen-value = "YES"
           tb_PR:screen-value = "YES"
           tb_DC:screen-value = "YES"
           tb_GL:screen-value = "YES"
           tb_SW:SCREEN-VALUE = "YES".

     IF lv-format-c = "Artios" THEN
        ASSIGN
           tb_tray-2:HIDDEN = NO
           tb_tray-2:SENSITIVE = YES.

     IF lv-format-f = "Accord" THEN
        ASSIGN tb_freeze-note:SCREEN-VALUE = "NO"
               tb_freeze-note:SENSITIVE = NO.

     IF lv-format-c = "PEACHTREE" THEN
        ASSIGN
           tb_make_hold:HIDDEN = NO
           tb_make_hold:SENSITIVE = YES     .
        ELSE
        ASSIGN
           tb_make_hold:HIDDEN = YES
           tb_make_hold:SENSITIVE = NO.
  END.

  {methods/nowait.i}
  /*APPLY "entry" TO begin_job1 IN FRAME {&FRAME-NAME}. */
  APPLY "choose" TO btn-ok IN FRAME {&FRAME-NAME}.
  
  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
RUN DISABLE_ui.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cleanup Dialog-Frame 
PROCEDURE cleanup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Excel Handle */
  def var chExcelApplication  as com-handle no-undo.
  def var chWorkbook          as com-handle no-undo.
  def var chWorkSheet         as com-handle no-undo.
  def var chHyper             as com-handle no-undo.

  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication connect no-error.

  if valid-handle (chExcelApplication) then
  do:
    chWorkBook:close()                no-error.
    chExcelApplication:Quit()         no-error.
    chExcelApplication:Quit(0)        no-error.

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chHyper            NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY begin_job1 begin_job2 end_job1 end_job2 tb_fold tb_corr tb_reprint 
          tb_box tb_fgimage spec_codes tb_prt-mch rd_print-speed tb_prt-shipto 
          tb_prt-sellprc tb_prt-label tb_committed tb_prt-set-header 
          tb_prompt-ship tb_freeze-note TB_sample_req tb_make_hold 
          lines-per-page rd-dest lv-ornt lv-font-no lv-font-name td-show-parm 
          v-run-speed 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-42 RECT-6 RECT-7 begin_job1 begin_job2 end_job1 end_job2 tb_fold 
         tb_corr tb_reprint tb_box tb_fgimage spec_codes tb_prt-label 
         tb_committed tb_prt-set-header tb_prompt-ship tb_freeze-note 
         TB_sample_req tb_make_hold lines-per-page rd-dest lv-ornt td-show-parm 
         btn-ok btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideDeptBoxes Dialog-Frame 
PROCEDURE HideDeptBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Parameters */
  def input param ilHide  as log no-undo.

  /* Enable / Disable Dept Toggle Boxes. */
  do with frame {&frame-name}:
    assign
      tb_RS:hidden  = ilHide
      tb_PR:hidden  = ilHide
      tb_DC:hidden  = ilHide
      tb_GL:hidden  = ilHide
      tb_SW:hidden  = ilHide no-error.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax Dialog-Frame 
PROCEDURE output-to-fax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*run output-to-fax.*/
   DO WITH FRAME {&FRAME-NAME}:

   
           {custom/asifax.i &begin_cust=begin_job1
                            &END_cust=END_job1
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                           END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file Dialog-Frame 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
     DEF VAR lv-save-as AS cha NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE  lv-save-as
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
     OS-COPY VALUE(list-name) VALUE(lv-save-as).
   */
   {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-mail Dialog-Frame 
PROCEDURE output-to-mail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
  IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject="Dialog-Frame:title"
                             &mail-body="Dialog-Frame:title"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_job1
                                  &END_cust=end_job1
                                  &mail-subject="frame Dialog-Frame:title"
                                  &mail-body="frame Dialog-Frame:title"
                                  &mail-file=list-name }

           END.
 
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer Dialog-Frame 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE result AS LOGICAL NO-UNDO.


   IF tb_corr THEN DO:
       FILE-INFO:FILE-NAME = list-name.
       RUN printfile (FILE-INFO:FILE-NAME).
   END.
   ELSE DO:
      IF lookup(lv-format-f,"Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Keystone,Frankstn,Colonial,Unipak,OttPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Accord,Carded") > 0 THEN DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).   
      END.
      ELSE RUN custom/prntproc.p (list-name, lv-font-no, lv-ornt).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen Dialog-Frame 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-cmd AS cha NO-UNDO.
   DEF VAR lv-file AS cha NO-UNDO.
   DEF VAR lv-xpfile AS cha NO-UNDO.


 IF tb_corr THEN DO:
    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).   
 END.
 ELSE DO:

     do with frame {&frame-name}:
        if lv-format-f = 'Indiana-XL'          and 
           (logical (tb_RS:screen-value) = true or
            logical (tb_PR:screen-value) = true or
            logical (tb_DC:screen-value) = true or
            logical (tb_GL:screen-value) = true or
            logical (tb_SW:screen-value) = true) then
           return.
     end.

     IF lookup(lv-format-f,"Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Keystone,Frankstn,Colonial,Unipak,OTTPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Accord,Carded") > 0 THEN DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).   
     END.
     ELSE
        RUN scr-rpt.w (list-name,FRAME {&frame-name}:TITLE,lv-font-no,lv-ornt). /* open file-name, title */  
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report Dialog-Frame 
PROCEDURE run-report :
DEF INPUT PARAM ip-industry AS CHAR NO-UNDO.
DEF BUFFER b-reftable FOR reftable.    
DEF BUFFER b-eb FOR eb.

IF cocode = ""  THEN cocode = "001".
IF locode = "" THEN locode = "MAIN".

{sys/form/r-top.i}

ASSIGN
 fjob-no   = fill(" ",6 - length(trim(begin_job1))) + trim(begin_job1)
 tjob-no   = fill(" ",6 - length(trim(end_job1))) + trim(end_job1)
 fjob-no2  = begin_job2
 tjob-no2  = end_job2
 fjob-no   = fill(" ",6 - length(trim(fjob-no))) + trim(fjob-no) +
             string(fjob-no2,"99")
 tjob-no   = fill(" ",6 - length(trim(tjob-no))) + trim(tjob-no) +
             string(tjob-no2,"99")
 print-box = tb_box
 reprint   = tb_reprint
 spec-list = spec_codes
 s-prt-fgimage = tb_fgimage
 s-prt-label = tb_prt-label
 s-committed-board-only = tb_committed 
 s-prt-set-header = tb_prt-set-header.

{jcrep/tickrrpt.i}

/*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE). */

SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param Dialog-Frame 
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

