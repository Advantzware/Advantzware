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
DEFINE INPUT PARAMETER pcCustFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcCustTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcSalRepFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcSalRepTo   AS CHAR NO-UNDO.


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
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_cust-no end_cust-no ~
begin_salrep end_salrep begin_item end_item begin_item-cat end_item-cat ~
tb_runExcel fi_file tb_format btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_salrep ~
end_salrep begin_item end_item begin_item-cat end_item-cat tb_excel ~
tb_runExcel fi_file tb_format 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine Dialog-Frame 
FUNCTION appendXLLine RETURNS CHARACTER
( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_item AS CHARACTER FORMAT "X(15)" 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item-cat AS CHARACTER FORMAT "X(15)" 
     LABEL "From FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_salrep AS CHARACTER FORMAT "X(15)" 
     LABEL "From Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item AS CHARACTER FORMAT "X(15)" 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item-cat AS CHARACTER FORMAT "X(15)" 
     LABEL "To FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_salrep AS CHARACTER FORMAT "X(15)" 
     LABEL "To Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-itmcom.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.43.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 3.67.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_format AS LOGICAL INITIAL no 
     LABEL "Export in import format?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_cust-no AT ROW 2.43 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-no AT ROW 2.43 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_salrep AT ROW 3.52 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number" WIDGET-ID 104
     end_salrep AT ROW 3.52 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number" WIDGET-ID 106
     begin_item AT ROW 4.57 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning FG Item Number" WIDGET-ID 100
     end_item AT ROW 4.57 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending FG Item Number" WIDGET-ID 102
     begin_item-cat AT ROW 5.67 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning FG Product Category" WIDGET-ID 108
     end_item-cat AT ROW 5.67 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending FG Product Category" WIDGET-ID 110
     tb_excel AT ROW 8.24 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 8.24 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 9.19 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     tb_format AT ROW 10.29 COL 36 WIDGET-ID 112
     btn-ok AT ROW 11.95 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 11.95 COL 60.2 WIDGET-ID 12
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-7 AT ROW 1 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 7.67 COL 2 WIDGET-ID 84
     SPACE(0.79) SKIP(2.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Commission Cost By Item Excel Export" WIDGET-ID 100.


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
       begin_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_salrep:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_salrep:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
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
ON HELP OF FRAME Dialog-Frame /* Commission Cost By Item Excel Export */
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
       when "begin_salrep" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-sman.w (cocode, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* salrep*/  
       when "end_salrep" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-sman.w (cocode, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* salrep*/
       when "begin_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, begin_cust-no, begin_item, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-no*/  
       when "end_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, begin_cust-no, end_item, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-no*/  
       when "begin_item-cat" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-fgcat.w (cocode, begin_item-cat, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-cat*/  
       when "end_item-cat" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-fgcat.w (cocode, begin_item-cat, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* item-no*/  
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Commission Cost By Item Excel Export */
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


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item Dialog-Frame
ON LEAVE OF begin_item IN FRAME Dialog-Frame /* From FG Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item-cat Dialog-Frame
ON LEAVE OF begin_item-cat IN FRAME Dialog-Frame /* From FG Category */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_salrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_salrep Dialog-Frame
ON LEAVE OF begin_salrep IN FRAME Dialog-Frame /* From Sales Rep */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   apply "close" to this-procedure.
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


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item Dialog-Frame
ON LEAVE OF end_item IN FRAME Dialog-Frame /* To FG Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item-cat Dialog-Frame
ON LEAVE OF end_item-cat IN FRAME Dialog-Frame /* To FG Category */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_salrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_salrep Dialog-Frame
ON LEAVE OF end_salrep IN FRAME Dialog-Frame /* To Sales Rep */
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


&Scoped-define SELF-NAME tb_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_format Dialog-Frame
ON VALUE-CHANGED OF tb_format IN FRAME Dialog-Frame /* Export in import format? */
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
{sys/inc/f3helpw.i}
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
  DISPLAY begin_cust-no end_cust-no begin_salrep end_salrep begin_item end_item 
          begin_item-cat end_item-cat tb_excel tb_runExcel fi_file tb_format 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-7 RECT-8 begin_cust-no end_cust-no begin_salrep end_salrep 
         begin_item end_item begin_item-cat end_item-cat tb_runExcel fi_file 
         tb_format btn-ok btn-cancel 
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
DEF BUFFER b-item-comm FOR item-comm.

DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.

IF tb_format THEN
    v-excelheader = "Customer #,Item #,Set Sale Price,Base Cost,Set Sale Price UOM,Base Cost UOM," + 
       "Rebate %,Fixed Gross Profit %,Overhead %,Misc %,Freight %,Industrial %,Warehouse %,Commission %,Locked".
ELSE
    v-excelheader = "Customer #,Item #,Item Name,Customer Part #,Set Sale Price,UOM,Base Cost,UOM,Locked," + 
       "Rebate %,Fixed Gross Profit %,Overhead %,Warehouse %,Misc %,Freight %,Industrial %,Commission %".

SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN
   OUTPUT STREAM excel TO VALUE(fi_file).
   PUT STREAM excel UNFORMATTED v-excelheader SKIP.
   FOR EACH b-item-comm WHERE b-item-comm.company = cocode 
       AND b-item-comm.cust-no GE begin_cust-no
       AND b-item-comm.cust-no LE end_cust-no 
       AND b-item-comm.i-no GE begin_item 
       AND b-item-comm.i-no LE end_item,
       FIRST cust WHERE cust.company = cocode
        AND cust.cust-no = b-item-comm.cust-no
        AND cust.sman GE begin_salrep
        AND cust.sman LE end_salrep,
       FIRST itemfg WHERE itemfg.company = cocode
        AND itemfg.i-no = b-item-comm.i-no
        AND itemfg.procat GE begin_item-cat
        AND itemfg.procat LE end_item-cat
       NO-LOCK:
       IF tb_format THEN DO:
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.cust-no)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.i-no).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.set-sales-price)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.base-cost)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[2]).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[3]).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.zz-dec[1])).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.fixed-gross-profit)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.overhead-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.misc-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.freight-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.industrial-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.warehouse-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.comm-rate-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[4]).
       END.
       ELSE DO:
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.cust-no)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.i-no).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.i-name).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.part-no).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.set-sales-price)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[2]).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.base-cost)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[3]).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[4]).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.zz-dec[1])).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.fixed-gross-profit)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.overhead-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.warehouse-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.misc-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.freight-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.industrial-percent)).
        v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.comm-rate-percent)).
       END. 
       PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
       v-excel-detail-lines = "".
   END.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine Dialog-Frame 
FUNCTION appendXLLine RETURNS CHARACTER
( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR lc-line AS CHAR NO-UNDO.

ipc-append = REPLACE(ipc-append, '"', '').
ipc-append = REPLACE(ipc-append, ',', ' ').
lc-line = lc-line + '"' + ipc-append + '",'.
  RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

