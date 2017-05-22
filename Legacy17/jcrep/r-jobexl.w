&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&SCOPED-DEFINE WINDOW-NAME CURRENT-WINDOW
&SCOPED-DEFINE FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  /*Task# 08111405*/
  
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
DEFINE VARIABLE list-name AS CHARACTER no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
 
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEFINE STREAM excel.


DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.


DEFINE TEMP-TABLE tt-report NO-UNDO
    FIELD i-no    AS CHARACTER
    FIELD ord-no  LIKE oe-rel.ord-no
    FIELD vdate   LIKE oe-rel.rel-date
    FIELD carrier AS CHARACTER
    FIELD shipid  AS CHARACTER
    FIELD release# LIKE oe-relh.release#
    FIELD row-id AS ROWID
    FIELD qty AS INTEGER.


ASSIGN cTextListToSelect  = "Job#,FG Item#,Estimate#,Order#,Customer#,Start Date,Close Date," +
                            "Status,Customer Part,Job Qty,Ord Qty,Prod.Qty,On Hand Qty," +
                            "Shipped Qty,Invoice Qty,WIP Qty,O/U%,Cust Sales Rep,Job Hold Reason"

       cFieldListToSelect = "job,job-hdr.i-no,job-hdr.est-no,job-hdr.ord-no,job-hdr.cust-no,start-date,close-date," +
                            "job.stat,cust-part,job-qty,ord-qty,prod-qty,oh-qty," +
                            "ship-qty,inv-qty,wip-qty,ou-pct,sales-rep,job-hold"
                            
        cFieldLength = "10,15,9,6,9,10,10," + "6,15,10,9,9,11," + "11,11,9,7,14,15"
           cFieldType = "c,c,c,i,c,c,c," + "c,c,i,i,i,i," + "i,i,i,i,c,c"
       .

{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE DIALOG-BOX
&SCOPED-DEFINE DB-AWARE NO

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 RS-open-closed ~
begin_cust-no end_cust-no begin_job end_job begin_item end_item begin_est ~
end_est sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&SCOPED-DEFINE DISPLAYED-OBJECTS RS-open-closed begin_cust-no end_cust-no ~
begin_job end_job begin_item end_item begin_est end_est sl_avail ~
sl_selected tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue Dialog-Frame 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

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

DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_down 
     LABEL "Move Down" 
     SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.

DEFINE BUTTON btn_Up 
     LABEL "Move Up" 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" 
     LABEL "From Estimate #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item AS CHARACTER FORMAT "X(15)" 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(8)" 
     LABEL "From Job#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(8)" 
     LABEL "To Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item AS CHARACTER FORMAT "X(15)" 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_job AS CHARACTER FORMAT "X(8)" 
     LABEL "To Job#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-order.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE RS-open-closed AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", 1,
"Closed", 2
     SIZE 22.6 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 8.1.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 3.43.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.52 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.52 NO-UNDO.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RS-open-closed AT ROW 2 COLUMN 41.2 NO-LABEL WIDGET-ID 2
     begin_cust-no AT ROW 3.43 COLUMN 27.8 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-no AT ROW 3.43 COLUMN 70.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_job AT ROW 4.57 COLUMN 27.8 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 104
     end_job AT ROW 4.57 COLUMN 70.8 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 106
     begin_item AT ROW 5.71 COLUMN 27.8 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 100
     end_item AT ROW 5.71 COLUMN 70.8 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 102
     begin_est AT ROW 6.86 COLUMN 27.8 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 108
     end_est AT ROW 6.86 COLUMN 70.8 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 110
     sl_avail AT ROW 10.19 COLUMN 6.6 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 10.19 COLUMN 62.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.67 COLUMN 43.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.86 COLUMN 43.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.05 COLUMN 43.6 WIDGET-ID 40
     btn_down AT ROW 14.24 COLUMN 43.6 WIDGET-ID 42
     tb_excel AT ROW 16.76 COLUMN 36 WIDGET-ID 32
     tb_runExcel AT ROW 16.76 COLUMN 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 17.71 COLUMN 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 20.1 COLUMN 30 WIDGET-ID 14
     btn-cancel AT ROW 20.1 COLUMN 60.2 WIDGET-ID 12
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 9.33 COL 3 WIDGET-ID 86
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COLUMN 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-6 AT ROW 9.1 COLUMN 2 WIDGET-ID 30
     RECT-7 AT ROW 1 COLUMN 2 WIDGET-ID 38
     RECT-8 AT ROW 16 COLUMN 2 WIDGET-ID 84
     SPACE(0.79) SKIP(2.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Costing Excel Export" WIDGET-ID 100.


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
       begin_est:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_job:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_est:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_job:PRIVATE-DATA IN FRAME Dialog-Frame     = 
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

&SCOPED-DEFINE SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Job Costing Excel Export */
DO:
DEFINE VARIABLE lw-focus AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ls-cur-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-val2 AS RECID NO-UNDO.


   lw-focus = FOCUS.

   CASE lw-focus:NAME :

       WHEN "begin_cust-no" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-cust.w (cocode,ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/  
       WHEN "end_cust-no" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-cust.w (cocode,ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/  
       WHEN "begin_job" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-jobno.w (cocode,ls-cur-val, OUTPUT char-val, OUTPUT char-val2).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/
        WHEN "end_job" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-jobno.w (cocode,ls-cur-val, OUTPUT char-val, OUTPUT char-val2).
           if char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/
        WHEN "begin_item" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/
       WHEN "end_item" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust-no*/
       
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Job Costing Excel Export */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* From Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est Dialog-Frame
ON LEAVE OF begin_est IN FRAME Dialog-Frame /* From Estimate # */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item Dialog-Frame
ON LEAVE OF begin_item IN FRAME Dialog-Frame /* From FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
DO:
 
  ASSIGN begin_job:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(begin_job:SCREEN-VALUE))) +
                 TRIM(begin_job:SCREEN-VALUE) 
          end_job:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(end_job:SCREEN-VALUE))) +
                 TRIM(end_job:SCREEN-VALUE) .

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN GetSelectionList. 
  RUN run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add Dialog-Frame
ON CHOOSE OF Btn_Add IN FRAME Dialog-Frame /* Add >> */
DO:
  DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

  APPLY "DEFAULT-ACTION" TO sl_avail.

  /*
  DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down Dialog-Frame
ON CHOOSE OF btn_down IN FRAME Dialog-Frame /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove Dialog-Frame
ON CHOOSE OF Btn_Remove IN FRAME Dialog-Frame /* << Remove */
DO:
 /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    ldummy = sl_selected:DELETE(i).
  END
  */
  APPLY "DEFAULT-ACTION" TO sl_selected  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up Dialog-Frame
ON CHOOSE OF btn_Up IN FRAME Dialog-Frame /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* To Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est Dialog-Frame
ON LEAVE OF end_est IN FRAME Dialog-Frame /* To Estimate# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item Dialog-Frame
ON LEAVE OF end_item IN FRAME Dialog-Frame /* To FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file Dialog-Frame
ON LEAVE OF fi_file IN FRAME Dialog-Frame /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail Dialog-Frame
ON DEFAULT-ACTION OF sl_avail IN FRAME Dialog-Frame
DO:
  
   IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
       sl_selected:NUM-ITEMS = 0)
   THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
               ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
               .

  
/* for pairs
    DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
    cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF sl_avail:IS-SELECTED(i) AND
      (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
         sl_selected:NUM-ITEMS = 0) THEN
    /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
        cSelectedList = cSelectedList +
                        entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
        sl_selected:NUM-ITEMS
        SKIP cSelectedList
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
  sl_selected:LIST-ITEM-PAIRS = cSelectedList.
  sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected Dialog-Frame
ON DEFAULT-ACTION OF sl_selected IN FRAME Dialog-Frame
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
              ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              .
    END.           
  END.
  IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
    .
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel Dialog-Frame
ON VALUE-CHANGED OF tb_excel IN FRAME Dialog-Frame /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel Dialog-Frame
ON VALUE-CHANGED OF tb_runExcel IN FRAME Dialog-Frame /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
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
  RUN DisplaySelectionList.
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.    

    /* stacey */
   /* RUN Set-Sort-Data.*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList Dialog-Frame 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:   
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 Dialog-Frame 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

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
  DISPLAY RS-open-closed begin_cust-no end_cust-no begin_job end_job begin_item 
          end_item begin_est end_est sl_avail sl_selected tb_excel tb_runExcel 
          fi_file 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-6 RECT-7 RECT-8 RS-open-closed begin_cust-no end_cust-no 
         begin_job end_job begin_item end_item begin_est end_est sl_avail 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList Dialog-Frame 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     
    IF NOT AVAIL ttRptList THEN
        MESSAGE "no " i ENTRY(i,ctmplist) SKIP
        ctmplist
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
           .        
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field Dialog-Frame 
PROCEDURE Move-Field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

  DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
      WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    DO:
      IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
        ldummy = sl_selected:DELETE(i + 1)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
        .
      LEAVE.
    END.
  END.

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
DEFINE VARIABLE str-tit4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit5 AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-line AS CHARACTER FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL.f}

DEF BUFFER b-job-hdr FOR job-hdr.
DEF BUFFER bjob FOR job.

DEFINE VARIABLE v-fcust LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].

DEFINE VARIABLE lv-tmp-string AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

DEFINE VARIABLE v-wip-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-prod-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-shp-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEFINE VARIABLE li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.
DEFINE VARIABLE v-oh-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-ou-pct AS INTEGER NO-UNDO.
DEFINE VARIABLE v-closed AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-open AS LOGICAL NO-UNDO.
DEFINE VARIABLE vHoldReason AS CHARACTER NO-UNDO .
ASSIGN
   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-excelheader = "".

   
SESSION:SET-WAIT-STATE ("general").

/* {sys/inc/print1.i}                         */
/* {sys/inc/outprint.i value(lines-per-page)} */

IF RS-open-closed = 1 THEN
   v-open = YES.
ELSE
   v-closed = YES.
   
IF tb_excel THEN
   OUTPUT STREAM excel TO VALUE(fi_file).
    
   DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
          .        
          cSlist = cSlist + ttRptSelected.FieldList + ",".
 END.
   
 IF tb_excel THEN
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
 
    

   /*   IF v-excelheader NE "" THEN
         PUT STREAM excel UNFORMATTED v-excelheader SKIP. */
    
   FOR EACH job-hdr WHERE job-hdr.company EQ cocode
    /*  AND job-hdr.job = 0 */
      AND ((job-hdr.opened EQ YES AND v-open) OR (job-hdr.opened EQ NO AND v-closed)) 
      AND (job-hdr.cust-no GE begin_cust-no AND job-hdr.cust-no LE end_cust-no) 
      AND (job-hdr.i-no GE begin_item AND job-hdr.i-no LE END_item) 
      AND (TRIM(job-hdr.est-no) GE trim(begin_est) AND TRIM(job-hdr.est-no) LE trim(end_est))
      AND (job-hdr.job-no GE begin_job AND job-hdr.job-no LE END_job) NO-LOCK, 
      EACH job OF job-hdr NO-LOCK BY job-hdr.job-no DESC
                                  BY job-hdr.job-no2 DESC:

       ASSIGN
        v-prod-qty = 0 
        v-shp-qty = 0
        v-oh-qty = 0
        v-ou-pct = 0
        v-wip-qty = 0.
       
         FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ job-hdr.company
             AND itemfg.i-no    EQ job-hdr.i-no
             NO-ERROR.
        
         FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
             AND oe-ordl.i-no EQ job-hdr.i-no
             AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        
  
         FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
                                      AND oe-ordl.i-no EQ job-hdr.i-no
                                      AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
         IF AVAILABLE oe-ordl THEN DO:
             RUN oe/ordlsqty.p (ROWID(oe-ordl),
                                OUTPUT li-inv-qty, OUTPUT li-ship-qty).
             v-shp-qty = li-ship-qty.
         END.
         IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
             FOR EACH fg-bin fields(qty) NO-LOCK
                WHERE fg-bin.company EQ oe-ordl.company
                AND fg-bin.job-no EQ oe-ordl.job-no
                AND fg-bin.job-no2 EQ oe-ordl.job-no2
                AND fg-bin.i-no EQ oe-ordl.i-no:
                v-oh-qty = v-oh-qty + fg-bin.qty.
             END. /* each fg-bin */

         
         IF AVAILABLE oe-ordl THEN
         DO:
            IF oe-ordl.job-no NE '' THEN
               FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
                  WHERE fg-rcpth.company EQ oe-ordl.company
                    AND fg-rcpth.job-no EQ oe-ordl.job-no
                    AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                    AND fg-rcpth.i-no EQ oe-ordl.i-no
                    AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
                   EACH fg-rdtlh FIELDS(qty) NO-LOCK
                  WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                    AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                   v-prod-qty = v-prod-qty + fg-rdtlh.qty.
            END.
           ELSE
              FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                  WHERE fg-rcpth.company   EQ cocode
                    AND fg-rcpth.job-no    EQ job-hdr.job-no
                    AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                    AND fg-rcpth.i-no      EQ oe-ordl.i-no
                    AND fg-rcpth.rita-code EQ "R"
                    USE-INDEX job,
                  EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE
                       fg-rdtlh.r-no      EQ fg-rcpth.r-no AND
                       fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                       v-prod-qty = v-prod-qty + fg-rdtlh.qty.
              END.
         END. /* avail oe-ordl */
         ELSE DO:
         
             FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                 WHERE fg-rcpth.company   EQ cocode
                   AND fg-rcpth.job-no    EQ job-hdr.job-no
                   AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                   AND fg-rcpth.i-no      EQ job-hdr.i-no
                   AND fg-rcpth.rita-code EQ "R"
                   USE-INDEX job,
                 EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE
                      fg-rdtlh.r-no      EQ fg-rcpth.r-no AND
                      fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                      v-prod-qty = v-prod-qty + fg-rdtlh.qty.
             END.
         END.
  
         IF AVAILABLE oe-ordl THEN DO:
             FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
             v-wip-qty = oe-ordl.qty - (v-oh-qty + oe-ordl.ship-qty).
             IF v-wip-qty LT 0 OR
                 v-wip-qty LT oe-ordl.qty *
                 (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
                     v-wip-qty = 0.
         END. /* avail oe-ordl */

         IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
             v-ou-pct = ((v-prod-qty / oe-ordl.qty) - 1) * 100.
             IF v-ou-pct EQ 0 THEN v-ou-pct = 100.
             IF v-ou-pct EQ -100 THEN v-ou-pct = 0.
         END. /* avail oe-ordl */
         ASSIGN vHoldReason = "" .
         FIND FIRST cust WHERE cust.company EQ cocode 
                           AND cust.cust-no EQ job-hdr.cust-no NO-LOCK NO-ERROR .
         IF job.stat = "H" THEN DO: 
             find first rejct-cd WHERE rejct-cd.type = "JH" 
                 and rejct-cd.code = job.reason NO-LOCK no-error.
             if avail rejct-cd then
                 assign 
                 vHoldReason = rejct-cd.dscr.      
         END.

      ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
     IF AVAIL job-hdr THEN
     BUFFER b-job-hdr:FIND-BY-ROWID(ROWID(job-hdr), NO-LOCK) .
     IF AVAIL job THEN
       BUFFER bjob:FIND-BY-ROWID(ROWID(job), NO-LOCK) .

     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

       IF INDEX(cTmpField,".") > 0 THEN DO:
            cFieldName = cTmpField .
           cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
            
           IF cFieldName BEGINS "job-hdr" THEN hField = BUFFER b-job-hdr:BUFFER-FIELD(cTmpField) .
           ELSE IF cFieldName BEGINS "job" THEN hField = BUFFER bjob:BUFFER-FIELD(cTmpField).
          IF hField <> ? THEN DO:                      
           cTmpField = SUBSTRING(GetFieldValue(hField),1,INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
               cDisplay = cDisplay + cTmpField + 
                   FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

           cExcelDisplay = cExcelDisplay + QUOTER(GetFieldValue(hField)) + ",".    
          END.
          ELSE DO:
                    cTmpField = SUBSTRING(cFieldName,1,INTEGER( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + QUOTER(" ")  /*GetFieldValue(hField))*/ + ",".
                 END.
        END.
          ELSE DO:       
             CASE cTmpField:
                  WHEN "cust-part" THEN cVarValue = IF AVAIL itemfg THEN STRING(itemfg.part-no) ELSE "". 
                  WHEN "ord-qty" THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.qty,"->>,>>>,>>>") ELSE "". 
                  WHEN "prod-qty" THEN cVarValue = STRING(v-prod-qty,"->>,>>>,>>>"). 
                  WHEN "oh-qty" THEN cVarValue = STRING(v-oh-qty,"->>,>>>,>>>"). 
                  WHEN "ship-qty" THEN cVarValue = STRING(v-shp-qty,"->>,>>>,>>>").
                  WHEN "inv-qty" THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.inv-qty,"->>,>>>,>>>") ELSE "".
                  WHEN "job" THEN cVarValue = STRING(TRIM(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99")).
                  WHEN "wip-qty" THEN cVarValue = STRING(v-wip-qty,"->>,>>>,>>>").
                  WHEN "ou-pct" THEN cVarValue = STRING(v-ou-pct,"->>>>>%").
                  WHEN "job-qty" THEN cVarValue = STRING(job-hdr.qty,"->>>,>>>,>>9") .
                  WHEN "start-date" THEN cVarValue = IF job.start-date NE ? THEN STRING(job.start-date) ELSE "" .
                  WHEN "close-date" THEN cVarValue = IF job.close-date NE ? THEN STRING(job.close-date) ELSE "" .
                  WHEN "sales-rep" THEN cVarValue = IF AVAIL cust THEN STRING(cust.sman) ELSE "".
                  WHEN "job-hold" THEN cVarValue = STRING(vHoldReason).
             END CASE.

             cExcelVarValue = cVarValue.
             cDisplay = cDisplay + cVarValue +
                                   FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                       cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
         END.
      END.
      
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
     END.
         
    
    /*  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data Dialog-Frame 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    /*  /* If a customer number was entered, find first and last matching customers. */
      IF pcCustFrom <> "" THEN ASSIGN begin_cust-no:SCREEN-VALUE = pcCustFrom.
      IF pcCustTo <> "" THEN ASSIGN end_cust-no:SCREEN-VALUE   = pcCustTo.

      IF pcItemFrom <> "" THEN ASSIGN begin_item:SCREEN-VALUE = pcItemFrom.
      IF pcItemTo <> "" THEN ASSIGN end_item:SCREEN-VALUE   = pcItemTo.

      IF pcPartFrom <> "" THEN ASSIGN begin_part:SCREEN-VALUE = pcPartFrom.
      IF pcPartTo <> "" THEN ASSIGN end_part:SCREEN-VALUE   = pcPartTo.

      IF piOrderFrom <> 0 THEN ASSIGN begin_order:SCREEN-VALUE = string(piOrderFrom).
      IF piOrderTo <> 0 THEN ASSIGN end_order:SCREEN-VALUE   = string(piOrderTo).

      IF pdDateFrom <> ? THEN ASSIGN begin_date:SCREEN-VALUE = string(pdDateFrom).
      IF pdDateTo <> ? THEN ASSIGN end_date:SCREEN-VALUE   = string(pdDateTo). */
  END.


  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue Dialog-Frame 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
  RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

