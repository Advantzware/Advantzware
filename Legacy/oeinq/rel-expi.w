&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-relexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-relexp 
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
DEFINE INPUT  PARAMETER ipcRelItem AS INT   NO-UNDO.
DEFINE INPUT  PARAMETER ipcCust AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcExt AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
{custom/gperiod.i}
{custom/persist.i}
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}
{custom/gcompany.i}

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
 locode = g_loc.

DEFINE STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

    ASSIGN 
        cTextListToSelect = "Release#,Order#,Customer PO,Cust #,Cust Name,Cust Part#,Ship To," + /*7*/
                            "FG Item#,Release Date,Job#,Job#2,Printed,Release Qty,Carrier,Tag," + /*8*/
                            "Whse,Bin,S/I,Units,Qty/Unit,Partial,Qty On Hand"  /*6*/
                            
        cFieldListToSelect = "rel-no,ord-no,po-no,cust,cust-name,cust-part,ship-to," +
                            "i-no,rel-date,job-no,job-no2,printed,qty,carr,tag," +
                            "loc,loc-bin,s-code,cases,qty-case,partial,qty-oh" 
                             .


{sys/inc/ttRptSel.i}
    ASSIGN cTextListToDefault  = "Release#,Order#,Customer PO,Cust #,Cust Name,Cust Part#,Ship To," + /*7*/
                            "FG Item#,Release Date,Job#,Job#2,Printed,Release Qty,Carrier,Tag," + /*8*/
                            "Whse,Bin,S/I,Units,Qty/Unit,Partial,Qty On Hand"  /*6*/ .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-relexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_rel-no end_rel-no ~
begin_ord-no end_ord-no begin_cust-no end_cust-no begin_i-no end_i-no ~
begin_cust-po end_cust-po tb_posted tb_unposted sl_avail sl_selected ~
Btn_Def Btn_Add Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rel-no end_rel-no begin_ord-no ~
end_ord-no begin_cust-no end_cust-no begin_i-no end_i-no begin_cust-po ~
end_cust-po tb_posted tb_unposted sl_avail sl_selected tb_excel tb_runExcel ~
fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-relexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-relexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-relexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-relexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-oe-relh FOR oe-relh,BUFFER ipb-oe-rell FOR oe-rell, ipc-field AS CHAR )  FORWARD.

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "x(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "x(15)" 
     LABEL "From Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "x(15)" 
     LABEL "From FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "From Order#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_rel-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "From Release#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "To Order#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_rel-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "To Release#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-relitm.csv" 
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

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL yes 
     LABEL "Posted?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_unposted AS LOGICAL INITIAL yes 
     LABEL "UnPosted?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-relexp
     begin_rel-no AT ROW 3.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Release#" WIDGET-ID 142
     end_rel-no AT ROW 3.48 COL 71 COLON-ALIGNED HELP
          "Enter Ending Release#" WIDGET-ID 144
     begin_ord-no AT ROW 4.43 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 146
     end_ord-no AT ROW 4.43 COL 71 COLON-ALIGNED HELP
          "Enter Ending Order #" WIDGET-ID 154
     begin_cust-no AT ROW 5.38 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 148
     end_cust-no AT ROW 5.38 COL 71 COLON-ALIGNED HELP
          "Enter Ending Cust #" WIDGET-ID 156
     begin_i-no AT ROW 6.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginning FG Item Number" WIDGET-ID 150
     end_i-no AT ROW 6.33 COL 71 COLON-ALIGNED HELP
          "Enter Ending FG Item #" WIDGET-ID 158
     begin_cust-po AT ROW 7.29 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer PO #" WIDGET-ID 152
     end_cust-po AT ROW 7.29 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer PO#" WIDGET-ID 160
     tb_posted AT ROW 9 COL 53.6 RIGHT-ALIGNED WIDGET-ID 162
     tb_unposted AT ROW 9 COL 71.6 RIGHT-ALIGNED WIDGET-ID 164
     sl_avail AT ROW 12.24 COL 9 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 12.24 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 12.38 COL 44.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 13.57 COL 44.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 14.76 COL 44.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 15.95 COL 44.2 WIDGET-ID 136
     btn_down AT ROW 17.14 COL 44.2 WIDGET-ID 132
     tb_excel AT ROW 18.91 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 18.91 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 19.86 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 21.71 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 21.71 COL 60.2 WIDGET-ID 12
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.52 COL 9.4 WIDGET-ID 140
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.52 COL 64.4 WIDGET-ID 138
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.52 COL 3 WIDGET-ID 86
     RECT-6 AT ROW 10.76 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 18.62 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Order Release to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-relexp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-relexp:SCROLLABLE       = FALSE
       FRAME rd-relexp:HIDDEN           = TRUE.

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       begin_cust-po:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       begin_rel-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       end_cust-po:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       end_rel-no:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-relexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_posted IN FRAME rd-relexp
   ALIGN-R                                                              */
ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-relexp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_unposted IN FRAME rd-relexp
   ALIGN-R                                                              */
ASSIGN 
       tb_unposted:PRIVATE-DATA IN FRAME rd-relexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-relexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-relexp rd-relexp
ON HELP OF FRAME rd-relexp /* Export Raw Materials Inventory to Excel */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_rm-no" then do:
          /* ls-cur-val = lw-focus:screen-value.
           RUN windows/l-carrie.w (cocode, locode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply. */
       end.  /* itemfg */
       when "end_rm-no" then do:
         /*  ls-cur-val = lw-focus:screen-value.
           run windows/l-carrie.w (cocode, locode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.*/
       end.  /* itemfg*/

END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-relexp rd-relexp
ON WINDOW-CLOSE OF FRAME rd-relexp /* Export Raw Materials Inventory to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no rd-relexp
ON LEAVE OF begin_cust-no IN FRAME rd-relexp /* From Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-po rd-relexp
ON LEAVE OF begin_cust-po IN FRAME rd-relexp /* From Customer PO# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no rd-relexp
ON LEAVE OF begin_i-no IN FRAME rd-relexp /* From FG Item# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no rd-relexp
ON LEAVE OF begin_ord-no IN FRAME rd-relexp /* From Order# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rel-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rel-no rd-relexp
ON LEAVE OF begin_rel-no IN FRAME rd-relexp /* From Release# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-relexp
ON CHOOSE OF btn-cancel IN FRAME rd-relexp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-relexp
ON CHOOSE OF btn-ok IN FRAME rd-relexp /* OK */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-relexp
ON CHOOSE OF Btn_Add IN FRAME rd-relexp /* Add >> */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-relexp
ON CHOOSE OF Btn_Def IN FRAME rd-relexp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-relexp
ON CHOOSE OF btn_down IN FRAME rd-relexp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-relexp
ON CHOOSE OF Btn_Remove IN FRAME rd-relexp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-relexp
ON CHOOSE OF btn_Up IN FRAME rd-relexp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no rd-relexp
ON LEAVE OF end_cust-no IN FRAME rd-relexp /* To Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-po rd-relexp
ON LEAVE OF end_cust-po IN FRAME rd-relexp /* To Customer PO# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no rd-relexp
ON LEAVE OF end_i-no IN FRAME rd-relexp /* To FG Item# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no rd-relexp
ON LEAVE OF end_ord-no IN FRAME rd-relexp /* To Order# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rel-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rel-no rd-relexp
ON LEAVE OF end_rel-no IN FRAME rd-relexp /* To Release# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-relexp
ON LEAVE OF fi_file IN FRAME rd-relexp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-relexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-relexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-relexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-relexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-relexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-relexp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted rd-relexp
ON VALUE-CHANGED OF tb_posted IN FRAME rd-relexp /* Posted? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-relexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-relexp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_unposted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_unposted rd-relexp
ON VALUE-CHANGED OF tb_unposted IN FRAME rd-relexp /* UnPosted? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-relexp 


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

    APPLY "entry" TO begin_rel-no.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-relexp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-relexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-relexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-relexp 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  /* MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.   */                                                 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-relexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-relexp  _DEFAULT-ENABLE
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
  DISPLAY begin_rel-no end_rel-no begin_ord-no end_ord-no begin_cust-no 
          end_cust-no begin_i-no end_i-no begin_cust-po end_cust-po tb_posted 
          tb_unposted sl_avail sl_selected tb_excel tb_runExcel fi_file 
      WITH FRAME rd-relexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_rel-no end_rel-no begin_ord-no end_ord-no 
         begin_cust-no end_cust-no begin_i-no end_i-no begin_cust-po 
         end_cust-po tb_posted tb_unposted sl_avail sl_selected Btn_Def Btn_Add 
         Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME rd-relexp.
  VIEW FRAME rd-relexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-relexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-relexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-relexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-relexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF BUFFER b-item FOR item.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.


FOR EACH  oe-relh WHERE oe-relh.company EQ cocode 
          AND oe-relh.stat NE "W"
          AND oe-relh.deleted EQ NO             
          AND( (oe-relh.posted  EQ NO  AND tb_unposted = TRUE) 
          OR (oe-relh.posted  EQ YES  AND tb_posted = TRUE) )
          AND oe-relh.release# GE begin_rel-no
          AND oe-relh.release# LE end_rel-no
          AND oe-relh.cust-no GE begin_cust-no
          AND oe-relh.cust-no LE end_cust-no NO-LOCK,
    EACH oe-rell USE-INDEX r-no NO-LOCK     
    WHERE oe-rell.company   EQ oe-relh.company  
    AND oe-rell.r-no      EQ oe-relh.r-no    
    AND oe-rell.i-no      GE begin_i-no      
    AND oe-rell.i-no      LE end_i-no      
    AND (oe-rell.ord-no   GE begin_ord-no /*OR fi_ord-no EQ 0*/ ) 
    AND (oe-rell.ord-no   LE end_ord-no /*OR fi_ord-no EQ 0*/ ) 
    AND oe-rell.po-no     GE begin_cust-po 
    AND oe-rell.po-no     LE end_cust-po BY oe-relh.release# DESC  :
  
    v-excel-detail-lines = "".

    FOR EACH ttRptSelected:
        v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue-itemfg(BUFFER oe-relh,BUFFER oe-rell,ttRptSelected.FieldList)).
/*         CASE ttRptSelected.FieldList:                                                               */
/*             WHEN "itemfg.i-no" THEN                                                                 */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-no).            */
/*             WHEN "itemfg.procat" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.procat).          */
/*             WHEN "itemfg.i-name" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-name).          */
/*             WHEN "itemfg.part-no" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.part-no).         */
/*             WHEN "itemfg.est-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.est-no).          */
/*             WHEN "itemfg.item" THEN                                                                */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.item).           */
/*             WHEN "itemfg.cust-no" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.cust-no).         */
/*             WHEN "itemfg.part-dscr1" THEN                                                           */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.part-dscr1).      */
/*             WHEN "itemfg.i-code" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.i-code).          */
/*             WHEN "itemfg.cad-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.cad-no).          */
/*             WHEN "itemfg.spc-no" THEN                                                               */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(itemfg.spc-no).          */
/*             WHEN "itemfg.stocked" THEN                                                              */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.stocked)). */
/*             WHEN "itemfg.q-onh" THEN                                                                */
/*                 v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(itemfg.q-onh)).   */
/*         END CASE.                                                                                   */
    END.

    PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-relexp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    IF ipcRelItem <> 0 THEN
        ASSIGN
        begin_rel-no:SCREEN-VALUE = string(ipcRelItem) 
        END_rel-no:SCREEN-VALUE   = string(ipcRelItem) .
    IF ipcCust <> "" THEN
        ASSIGN
        BEGIN_cust-no:SCREEN-VALUE = ipcCust
        end_cust-no:SCREEN-VALUE    = ipcCust.
END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-relexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-relexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-relexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-relexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-oe-relh FOR oe-relh,BUFFER ipb-oe-rell FOR oe-rell, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR test AS INT FORMAT "->>>,>>>,>>9" NO-UNDO.

    CASE ipc-field :
        WHEN "rel-no"  THEN 
            lc-return = string(ipb-oe-relh.release#).
        WHEN "cust"  THEN 
            lc-return = ipb-oe-relh.cust.
        WHEN "rel-date"  THEN 
            lc-return = string(ipb-oe-relh.rel-date,"99/99/99").
        WHEN "printed"  THEN 
            lc-return = string(ipb-oe-relh.printed).
        WHEN "ship-to"  THEN 
            lc-return = string(ipb-oe-relh.ship-id).
        WHEN "carr"  THEN 
            lc-return = string(ipb-oe-relh.carrier).
        WHEN "cust-part" THEN DO:
            FIND itemfg WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ ipb-oe-rell.i-no NO-LOCK NO-ERROR.
            IF AVAIL ITEMfg THEN do:
                ASSIGN 
               lc-return = STRING(itemfg.part-no,"x(15)")    .
            END.
            ELSE lc-return = "" .
        END. 
        WHEN "cust-name" THEN DO:
            FIND cust WHERE cust.company EQ cocode
                AND cust.cust-no EQ ipb-oe-relh.cust-no NO-LOCK NO-ERROR.
            IF AVAIL cust THEN do:
                ASSIGN 
               lc-return = STRING(cust.NAME,"x(30)")    .
            END.
            ELSE lc-return = "" .
        END.

        WHEN "qty-oh" THEN DO:
            FIND itemfg WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ ipb-oe-rell.i-no NO-LOCK NO-ERROR.
            IF AVAIL ITEMfg THEN do:
                ASSIGN 
               lc-return = STRING(itemfg.q-onh)    .
            END.
            ELSE lc-return = "" .
        END. 

       
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-oe-rell:BUFFER-FIELD(ipc-field).
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

