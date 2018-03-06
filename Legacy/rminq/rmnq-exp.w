&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgnq-exp 
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
DEFINE INPUT PARAMETER lcSearch   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER lcsearchby AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}          */

/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/
DEFINE {&NEW} SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
{sys/inc/var.i new shared}
 v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
 v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

assign
 cocode = g_company
 /*locode = gloc*/ .

DEFINE STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "RM Item#,Job#,Job2,S,B,Required,Allocated,Job UOM,Allocated2,RM UOM,Width,Length,Due Date"

      cFieldListToSelect = "rm-ino,job,job2,s,b,requrd,alloc,job-uom,alloc2,rm-uom,wid,len,due-dt" .
{sys/inc/ttRptSel.i}

    ASSIGN cTextListToDefault  = "RM Item#,Job#,Job2,S,B,Required,Allocated,Job UOM,Allocated2,RM UOM,Width,Length,Due Date" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgnq-exp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_i-no end_i-no ~
Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no ~
sl_avail sl_selected tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgnq-exp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgnq-exp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgnq-exp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgnq-exp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHAR )  FORWARD.

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

DEFINE BUTTON Btn_Def 
     LABEL "&Default" 
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

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "x(10)" 
     LABEL "From Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzzz" 
     LABEL "To Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-frmitm.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 9.29.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

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

DEFINE FRAME rd-fgnq-exp
     begin_i-no AT ROW 3.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 142
     end_i-no AT ROW 3.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 144
     sl_avail AT ROW 12.24 COL 9 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 12.24 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 12.43 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 13.62 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 14.81 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 16 COL 44 WIDGET-ID 136
     btn_down AT ROW 17.19 COL 44 WIDGET-ID 132
     tb_excel AT ROW 18.91 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 18.91 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 19.86 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 21.71 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 21.71 COL 60.2 WIDGET-ID 12
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.52 COL 64.4 WIDGET-ID 138
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.52 COL 9.4 WIDGET-ID 140
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.52 COL 3 WIDGET-ID 86
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-6 AT ROW 10.76 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 18.62 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Customer to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgnq-exp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-fgnq-exp:SCROLLABLE       = FALSE
       FRAME rd-fgnq-exp:HIDDEN           = TRUE.

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgnq-exp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-fgnq-exp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgnq-exp rd-fgnq-exp
ON HELP OF FRAME rd-fgnq-exp /* Export Customer to Excel */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :
      /* when "begin_i-no" then do:
           ls-cur-val = lw-focus:screen-value.
           RUN windows/l-itemfj.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_i-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfj.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg*/*/

END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgnq-exp rd-fgnq-exp
ON WINDOW-CLOSE OF FRAME rd-fgnq-exp /* Export Customer to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no rd-fgnq-exp
ON LEAVE OF begin_i-no IN FRAME rd-fgnq-exp /* From Item# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgnq-exp
ON CHOOSE OF btn-cancel IN FRAME rd-fgnq-exp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgnq-exp
ON CHOOSE OF btn-ok IN FRAME rd-fgnq-exp /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  RUN GetSelectionList.  
  run run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgnq-exp
ON CHOOSE OF Btn_Add IN FRAME rd-fgnq-exp /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

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


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgnq-exp
ON CHOOSE OF Btn_Def IN FRAME rd-fgnq-exp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgnq-exp
ON CHOOSE OF btn_down IN FRAME rd-fgnq-exp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgnq-exp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgnq-exp /* << Remove */
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


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgnq-exp
ON CHOOSE OF btn_Up IN FRAME rd-fgnq-exp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no rd-fgnq-exp
ON LEAVE OF end_i-no IN FRAME rd-fgnq-exp /* To Item# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgnq-exp
ON LEAVE OF fi_file IN FRAME rd-fgnq-exp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgnq-exp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgnq-exp
DO:
  
   IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
       sl_selected:NUM-ITEMS = 0)
   THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
               ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
               .

  
/* for pairs
    DEF VAR cSelectedList AS cha NO-UNDO.
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


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgnq-exp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgnq-exp
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgnq-exp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgnq-exp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-fgnq-exp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-fgnq-exp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgnq-exp 


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
    RUN Set-Sort-Data.

    APPLY "entry" TO begin_i-no.
  END. 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgnq-exp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-fgnq-exp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgexp 
PROCEDURE DisplaySelectionDefault :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  
  DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

     cListContents = cListContents +                   
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToDefault)   .
  END.            
  sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgnq-exp 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */
                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.
  
 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgnq-exp 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */
                     
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.
  
 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgnq-exp  _DEFAULT-ENABLE
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
  DISPLAY begin_i-no end_i-no  sl_avail sl_selected tb_excel 
          tb_runExcel fi_file 
      WITH FRAME rd-fgnq-exp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_i-no end_i-no  Btn_Def sl_avail 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down tb_runExcel fi_file 
         btn-ok btn-cancel 
      WITH FRAME rd-fgnq-exp.
  VIEW FRAME rd-fgnq-exp.
  {&OPEN-BROWSERS-IN-QUERY-rd-fgnq-exp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgnq-exp 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

 DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           /* ttRptSelected.FieldLength */
        .   
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgnq-exp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF BUFFER b-cust FOR cust.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR op-qty-pal AS INT NO-UNDO.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

v-excel-detail-lines = "".
op-qty-pal = 0 .
li-pallets = 0 .

    FOR EACH job WHERE TRUE /* Join to itemfg incomplete */
      AND job.company EQ cocode 
      AND job.opened EQ YES NO-LOCK,
      EACH job-mat OF job
      WHERE job-mat.all-flg eq yes
        AND job-mat.rm-i-no GE begin_i-no 
        AND job-mat.rm-i-no LE end_i-no NO-LOCK:

        v-excel-detail-lines = "".

       
       FOR EACH ttRptSelected:
         /* IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email") <> 0    THEN do: */
              IF ttRptSelected.TextList = "RM Item#" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(job-mat.rm-i-no).
              IF ttRptSelected.TextList = "Job#" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(job-mat.job-no).
              IF ttRptSelected.TextList = "Job2" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.job-no2)).
              IF ttRptSelected.TextList = "S" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.frm)).
              IF ttRptSelected.TextList = "B" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.blank-no)).
              IF ttRptSelected.TextList = "Required" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.qty)).
              IF ttRptSelected.TextList = "Allocated" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.qty-all)).
              IF ttRptSelected.TextList = "Job UOM" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.qty-uom)).
              IF ttRptSelected.TextList = "Allocated2" THEN do:
                   DEF BUFFER b-item FOR item.
                   
                   DEF VAR v-comm      AS   DEC                                            NO-UNDO.
                   DEF VAR v-bwt       LIKE item.basis-w                                   NO-UNDO.
                   DEF VAR v-len       LIKE item.s-len                                     NO-UNDO.
                   DEF VAR v-wid       LIKE item.s-wid                                     NO-UNDO.
                   
                   
                     FIND FIRST b-item NO-LOCK
                         WHERE b-item.company EQ job-mat.company
                           AND b-item.i-no    EQ job-mat.rm-i-no
                         NO-ERROR.
                   
                     IF AVAIL b-item THEN DO WITH FRAME {&FRAME-NAME}:
                       v-comm = job-mat.qty-all.
                   
                       IF job-mat.qty-uom NE b-item.cons-uom THEN DO:
                         ASSIGN
                          v-bwt = job-mat.basis-w
                          v-len = job-mat.len
                          v-wid = job-mat.wid.
                   
                         IF v-len EQ 0 THEN v-len = b-item.s-len.
                   
                         IF v-wid eq 0 THEN
                           v-wid = IF b-item.r-wid NE 0 THEN b-item.r-wid ELSE b-item.s-wid.
                   
                         IF v-bwt EQ 0 THEN v-bwt = b-item.basis-w.
                   
                         RUN sys/ref/convquom2.p(cocode,job-mat.qty-uom, b-item.cons-uom,
                                                v-bwt, v-len, v-wid, b-item.s-dep,
                                                v-comm, OUTPUT v-comm).
                       END.
                     END.

              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(v-comm)).
              END.
              IF ttRptSelected.TextList = "RM UOM" THEN do:
                       DEF BUFFER b-item2 FOR ITEM.
                       
                       DEF VAR v-uom AS CHAR NO-UNDO.
                       
                       FIND FIRST b-item2 NO-LOCK
                             WHERE b-item2.company EQ job-mat.company
                               AND b-item2.i-no    EQ job-mat.rm-i-no
                             NO-ERROR.
                       
                       
                       IF AVAIL b-item2 THEN v-uom = b-item2.cons-uom.

                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(v-uom).
              END.
              IF ttRptSelected.TextList = "Width" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.wid)).
              IF ttRptSelected.TextList = "Length" THEN do:
                  v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job-mat.len)).
              END.
              IF ttRptSelected.TextList = "Due Date" AND job.due-date NE ? THEN do:
                  v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(job.due-date)).
              END.
              
        /*  END.

          ELSE do:
              v-excel-detail-lines = v-excel-detail-lines + 
                  appendXLLine(getValue-itemfg(BUFFER b-cust,ttRptSelected.FieldList)).
          END.  */
        END.  /* each ttrptse */

       PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

    END. /* for each phone */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgnq-exp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
 /*   IF begin_i-no:SCREEN-VALUE EQ "" THEN DO:
        FIND FIRST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
        begin_i-no:SCREEN-VALUE = fg-rcpth.i-no.
        FIND LAST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
        end_i-no:SCREEN-VALUE   = fg-rcpth.i-no .
    END. */
  /*  IF begin_title-cod:SCREEN-VALUE EQ "" THEN DO:
        FIND FIRST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
        begin_title-cod:SCREEN-VALUE = phone.titlcode .
        FIND LAST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
        end_title-cod:SCREEN-VALUE   = phone.titlcode .
    END. */

  /*  IF lcSearch NE "" THEN 
        begin_cust-type:SCREEN-VALUE = lcSearch.
    IF lcsearchby NE "" THEN 
        end_cust-type:SCREEN-VALUE = lcsearchby.*/

END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgnq-exp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEF VAR lc-line AS CHAR NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgnq-exp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR.

    IF ipl-end THEN
        lc-return = ipc-param + "ZZZZZZZZZZZZZZZ".
    ELSE
        lc-return = ipc-param.

  RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgnq-exp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lc-header AS CHAR NO-UNDO.

FOR EACH ttRptSelected:
    lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
END.
/*     lc-header = lc-header + appendXLLine ("PO #").      */
/*     lc-header = lc-header + appendXLLine ("Vendor #").  */
/*     lc-header = lc-header + appendXLLine ("Due Date").  */
/*     lc-header = lc-header + appendXLLine ("Ship ID").   */
/*     lc-header = lc-header + appendXLLine ("Ship Name"). */
/*     lc-header = lc-header + appendXLLine ("Job #").     */
/*     lc-header = lc-header + appendXLLine ("Item #").    */
/*     lc-header = lc-header + appendXLLine ("Item Name"). */

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgnq-exp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
{custom/getperd.i} 
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR ptd-profit1 AS CHAR NO-UNDO.
    DEF VAR ytd-profit1 AS CHAR NO-UNDO.
    DEF VAR lyr-profit1 AS CHAR NO-UNDO.
    DEF VAR ptd-sales1 AS DEC NO-UNDO.

    CASE ipc-field :
        WHEN "ptd-profit" THEN DO:
            lc-return = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
        END.
        WHEN "ytd-profit" THEN DO:
            lc-return = string(DECIMAL(ipb-itemfg.ytd-sales) - DECIMAL(ipb-itemfg.cost[5])) .
        END.
        WHEN "lyr-profit" THEN DO:
            lc-return = string(DECIMAL(ipb-itemfg.lyr-sales) - DECIMAL(ipb-itemfg.cost[6])) .
        END.
        WHEN "ptd-profit-pct" THEN DO:
            /*IF ptd-profit-pct NE ? THEN*/
            ptd-profit1 = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
            ptd-sales1 = (ipb-itemfg.sales[gperiod]).
            lc-return = string(decimal(ptd-profit1) / DECIMAL(ptd-sales1) * 100) .
        END.
        WHEN "ytd-profit-pct" THEN DO:
            /*IF ytd-profit-pct NE ? THEN*/
            ytd-profit1 = string(DECIMAL(ipb-itemfg.ytd-sales) - DECIMAL(ipb-itemfg.cost[5])) .
            lc-return = string(decimal(ytd-profit1) / DECIMAL(ipb-itemfg.ytd-sales) * 100) .
        END.
        WHEN "lyr-profit-pct" THEN DO:
           /* IF lyr-profit-pct NE ? THEN*/
            lyr-profit1 = string(DECIMAL(ipb-itemfg.lyr-sales) - DECIMAL(ipb-itemfg.cost[6])) .
            lc-return = string(decimal(lyr-profit1) / DECIMAL(ipb-itemfg.lyr-sales) * 100) .
        END.
        WHEN "ptd-sales" THEN DO:
            IF gperiod NE 0 THEN
            lc-return = string(ipb-itemfg.sales[gperiod]).
        END.
        WHEN "total-msf" THEN DO:
            IF gperiod NE 0 THEN
            lc-return = string(ipb-itemfg.ptd-msf[gperiod]).
        END.
        WHEN "sname"  THEN DO:
            FIND sman WHERE sman.company EQ ipb-itemfg.company
                AND sman.sman EQ  ipb-itemfg.sman NO-LOCK NO-ERROR.
            IF AVAIL sman THEN
                lc-return = sman.sNAME.
            ELSE
                lc-return = "".
            
        END.
        WHEN "terms-dscr"  THEN DO:
            FIND terms WHERE terms.company EQ ipb-itemfg.company
                AND terms.t-code EQ  ipb-itemfg.terms NO-LOCK NO-ERROR.
            IF AVAIL terms THEN
                lc-return = terms.dscr.
            ELSE
                lc-return = "".
        END.
         WHEN "tax-dscr"  THEN DO:
            FIND stax WHERE stax.company EQ ipb-itemfg.company
                AND stax.tax-group EQ  ipb-itemfg.tax-gr NO-LOCK NO-ERROR.
            IF AVAIL stax THEN
                lc-return = stax.tax-dscr1[1].
            ELSE
                lc-return = "".
        END.
        WHEN "custype-dscr" THEN DO:
            FIND custype WHERE custype.company EQ ipb-itemfg.company
                AND custype.custype EQ ipb-itemfg.TYPE NO-LOCK NO-ERROR.
            IF AVAIL custype THEN
                 lc-return = custype.dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "loc-dscr"  THEN DO:
            FIND loc WHERE loc.company EQ ipb-itemfg.company
                AND loc.loc EQ  ipb-itemfg.loc NO-LOCK NO-ERROR.
            IF AVAIL loc THEN
                lc-return = loc.dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "carrier-dscr"  THEN DO:
            FIND FIRST carrier WHERE carrier.company EQ ipb-itemfg.company
                AND carrier.carrier EQ  ipb-itemfg.carrier  NO-LOCK NO-ERROR.
 
            IF AVAIL carrier THEN
                lc-return = carrier.dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "del-dscr"  THEN DO:
            FIND FIRST carr-mtx WHERE carr-mtx.company EQ ipb-itemfg.company
                AND carr-mtx.del-zone EQ  ipb-itemfg.del-zone NO-LOCK NO-ERROR.
            IF AVAIL carr-mtx THEN
                lc-return = carr-mtx.del-dscr.
            ELSE
                lc-return = "".
        END.
        WHEN "terr-dscr"  THEN DO:
            FIND terr WHERE terr.company EQ ipb-itemfg.company
                AND terr.terr EQ  ipb-itemfg.terr NO-LOCK NO-ERROR.
            IF AVAIL terr THEN
                lc-return = terr.dscr .
            ELSE
                lc-return = "".
        END.
        WHEN "po-mand" THEN DO:
               IF ipb-itemfg.cust-no NE "" THEN
                lc-return = string(ipb-itemfg.po-mandatory). 

        END.
        WHEN "show-set" THEN DO:
                IF ipb-itemfg.cust-no NE "" THEN
                lc-return = string(ipb-itemfg.show-set).
           
        END.
        WHEN "flat-comm" THEN DO:                             
                IF ipb-itemfg.cust-no NE "" THEN
                    lc-return = string(ipb-itemfg.flatCommPct).
              END.
        WHEN "inv-meth"  THEN DO:
            CASE ipb-itemfg.inv-meth :
                WHEN NO THEN
                    lc-return = "BOL".
                WHEN Yes THEN
                    lc-return = "PO".
                OTHERWISE
                    lc-return = "Group by Date".
            END CASE.
        END.
        WHEN "sort"  THEN DO:
            CASE ipb-itemfg.SORT :
                WHEN "N" THEN
                    lc-return = "No".
                WHEN "Y" THEN
                    lc-return = "Yes".
            END CASE.
        END.
        WHEN "fob-code"  THEN DO:
            CASE ipb-itemfg.fob-code :
                WHEN "DEST" THEN
                    lc-return = "Destination".
                WHEN "ORIG" THEN
                    lc-return = "Origin".
            END CASE.
        END.
        WHEN "active" THEN DO:
            CASE ipb-itemfg.active :
                WHEN "A" THEN
                    lc-return = "Active".
                WHEN "I" THEN
                    lc-return = "Inactive".
                WHEN "X" THEN
                    lc-return = "Inhouse".
                WHEN "S" THEN
                    lc-return = "Statement".
                WHEN "E" THEN
                    lc-return = "Service".
            END CASE.
        END.
        WHEN "frt-pay" THEN DO:
            CASE ipb-itemfg.frt-pay :
                WHEN "B" THEN
                    lc-return = "Bill".
                WHEN "C" THEN
                    lc-return = "Collect".
                WHEN "P" THEN
                    lc-return = "Prepaid".
                WHEN "T" THEN
                    lc-return = "3rd Party".
            END CASE.
        END.
        WHEN "title"  THEN DO:
            lc-return = "" .
        END.
        WHEN "dfuncTotMSFPTD"  THEN DO:
            /*IF g_period NE 0 THEN lc-return = STRING(ipb-itemfg.ptd-msf[g_period]).*/
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-itemfg:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

