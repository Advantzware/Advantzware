&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

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

DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.

DEFINE STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_cust-no ~
end_cust-no RS_sort-by TG_cust-no TG_cust-name TG_charge TG_description ~
TG_Amount tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no RS_sort-by ~
TG_cust-no TG_cust-name TG_charge TG_description TG_Amount tb_excel ~
tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

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

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\rd-sucg.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE RS_sort-by AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Customer Name", 1,
"Description", 2
     SIZE 21 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 4.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 5.71.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 3.67.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE TG_Amount AS LOGICAL INITIAL no 
     LABEL "Amount" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG_charge AS LOGICAL INITIAL no 
     LABEL "Charge" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG_cust-name AS LOGICAL INITIAL no 
     LABEL "Customer Name" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE TG_cust-no AS LOGICAL INITIAL no 
     LABEL "Customer#" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE TG_description AS LOGICAL INITIAL no 
     LABEL "Description" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_cust-no AT ROW 2.19 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-no AT ROW 2.19 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     RS_sort-by AT ROW 4.57 COL 28 NO-LABEL WIDGET-ID 100
     TG_cust-no AT ROW 8.43 COL 12.8 WIDGET-ID 42
     TG_cust-name AT ROW 8.43 COL 30.4 WIDGET-ID 50
     TG_charge AT ROW 8.43 COL 52 WIDGET-ID 104
     TG_description AT ROW 8.43 COL 64.6 WIDGET-ID 106
     TG_Amount AT ROW 8.43 COL 81.8 WIDGET-ID 108
     tb_excel AT ROW 11.81 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 11.81 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 12.76 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 15.52 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 15.52 COL 60.2 WIDGET-ID 12
     "Sort by:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.62 COL 24 WIDGET-ID 110
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 7.43 COL 4 WIDGET-ID 86
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 4 WIDGET-ID 36
          BGCOLOR 2 
     RECT-6 AT ROW 6.71 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 11.24 COL 2 WIDGET-ID 84
     SPACE(0.79) SKIP(2.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Surcharge Excel Export" WIDGET-ID 100.


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

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME Dialog-Frame
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Surcharge Excel Export */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
       when "end_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode,ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* cust-no*/  
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Surcharge Excel Export */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    apply "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
DO:

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  run run-report.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file Dialog-Frame
ON LEAVE OF fi_file IN FRAME Dialog-Frame /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel Dialog-Frame
ON VALUE-CHANGED OF tb_excel IN FRAME Dialog-Frame /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel Dialog-Frame
ON VALUE-CHANGED OF tb_runExcel IN FRAME Dialog-Frame /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust-no.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY begin_cust-no end_cust-no RS_sort-by TG_cust-no TG_cust-name TG_charge 
          TG_description TG_Amount tb_excel tb_runExcel fi_file 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-6 RECT-7 RECT-8 begin_cust-no end_cust-no RS_sort-by TG_cust-no 
         TG_cust-name TG_charge TG_description TG_Amount tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report Dialog-Frame 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR v-fcust LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].

DEF VAR lv-tmp-string AS CHAR NO-UNDO.

DEF VAR v-excelheader AS CHAR NO-UNDO.

ASSIGN
   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-excelheader = ""
   v-excel-detail-lines = "".


IF TG_cust-no = YES THEN
   v-excelheader = v-excelheader + "Customer Number,".
IF TG_cust-name = YES THEN
   v-excelheader = v-excelheader + "Customer Name,".
IF TG_charge = YES THEN
   v-excelheader = v-excelheader + "Charge,".
IF TG_description = YES THEN
   v-excelheader = v-excelheader + "Description,".
IF TG_amount = YES THEN
   v-excelheader = v-excelheader + "Amount,".

       
SESSION:SET-WAIT-STATE ("general").

/* {sys/inc/print1.i}                         */
/* {sys/inc/outprint.i value(lines-per-page)} */
   
v-excel-detail-lines = "".

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
    

   IF v-excelheader NE "" THEN
      PUT STREAM excel UNFORMATTED v-excelheader SKIP.

   IF RS_sort-by = 1 THEN DO:
      FOR EACH surcharge NO-LOCK WHERE surcharge.company = cocode,
         FIRST cust NO-LOCK WHERE cust.company = surcharge.company
                              AND cust.cust-no = surcharge.cust-no
                               BY cust.name:
         RUN write-excel-detail-lines.

      END.
      
   END.
   ELSE DO:
      FOR EACH surcharge NO-LOCK WHERE surcharge.company = cocode,                        
         FIRST cust NO-LOCK WHERE cust.company = surcharge.company
                              AND cust.cust-no = surcharge.cust-no
                               BY surcharge.dscr:
         RUN write-excel-detail-lines.
      END.
   END.

   OUTPUT STREAM excel CLOSE.

   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE write-excel-detail-lines Dialog-Frame 
PROCEDURE write-excel-detail-lines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
v-excel-detail-lines = "".

IF TG_cust-no = YES THEN
   v-excel-detail-lines = v-excel-detail-lines + '"' + surcharge.cust-no + '",'.
IF TG_cust-name = YES THEN
   v-excel-detail-lines = v-excel-detail-lines + '"' + cust.name + '",'.
IF TG_charge = YES THEN
   v-excel-detail-lines = v-excel-detail-lines + '"' + surcharge.charge + '",'.
IF TG_description = YES THEN
   v-excel-detail-lines = v-excel-detail-lines + '"' + surcharge.dscr + '",'.
IF TG_amount = YES THEN
   v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(surcharge.amt / 1000) + '",'.

PUT STREAM excel UNFORMATTED 
   v-excel-detail-lines
   SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

