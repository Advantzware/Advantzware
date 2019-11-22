&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgexp 
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
DEFINE INPUT  PARAMETER ipcSearch AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcSearchBy AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcIndustry AS CHARACTER   NO-UNDO.

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


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

IF ipcIndustry EQ "2" THEN
  v-prgmname = v-prgmname + "C" .

IF ipcIndustry EQ "2" THEN
    ASSIGN 
        cTextListToSelect = "Industry Type,Item#,Name,DESC,Est.DESC,Item Code,Taxable,Mat'l Type,Mat'1 Dscr,Cost Type,Cost Dscr,Category,Category Dscr,QTY Usage PTD,Qty Usage YTD," + /*15 */
                            "Qty Usage Last YR,Usage Cost PTD,Usage Cost YTD,Total Cost Last YR,Ink Type,Press Type,Min Lbs/Job,SI/Lb,Wgt/100,Caliper,Shrink%,Weight/MSF,Width,Depth," + /* 14 */
                            "Length,Density,Roll W,Color,ECT,Department 1,Department 2,Department 3,Department 4,Department 5,Department 6,Department 7,Department 8,Department 9,Department 10," +  /* 15*/
                            "Reduction% 1,Reduction% 2,Reduction% 3,Reduction% 4,Reduction% 5,Reduction% 6,Reduction% 7,Reduction% 8,Reduction% 9,Reduction% 10,Case Length,Case Width,Case Depth,Avg Wt,Boxes/Bundle," + /* 15 */
                            "Bundle/Pallet,Flute,Test,Sq In/Lb,Lin In/UOM,Warehouse,Bin,Qty On Hand"  /* 8*/

        cFieldListToSelect = "ind-type,i-no,i-name,i-dscr,est-dscr,i-code,tax-rcpt,mat-type,mat-dscr,cost-type,costtype-descr,procat,procat-dscr,q-ptd,q-ytd," +   /* 14 */ 
                            "q-lyr,u-ptd,u-ytd,u-lyr,ink-type,press-type,min-lbs,yield,weight-100,cal,shrink,basis-w,s-wid,s-dep,s-len,density,r-wid,color-1,ect," +  /* 19 */
                            "dept-name[1],dept-name[2],dept-name[3],dept-name[4],dept-name[5],dept-name[6],dept-name[7],dept-name[8],dept-name[9],dept-name[10]," +     /* 10 */
                            "speed%[1],speed%[2],speed%[3],speed%[4],speed%[5],speed%[6],speed%[7],speed%[8],speed%[9],speed%[10],case-l,case-w,case-d,avg-w,box-case," +  /* 15 */
                            "case-pall,flute,reg-no,sqin-lb,linin-lb,loc,loc-bin,q-onh".   /* 8 */
ELSE 
    ASSIGN 
        cTextListToSelect = "Industry Type,Item#,Name,DESC,Est.DESC,Item Code,Taxable,Mat'l Type,Mat'1 Dscr,Cost Type,Cost Dscr,Category,Category Dscr,QTY Usage PTD,Qty Usage YTD," +
                            "Qty Usage Last YR,Usage Cost PTD,Usage Cost YTD,Total Cost Last YR,Ink Type,Press Type,Min Lbs/Job,SI/Lb,Wgt/100,Warehouse,Bin,Qty On Hand," +
                            "Caliper,Basis Weight,Reg. #,Shrink%,Sheet Width,Sheet Length,Roll Width,Core Dia,Department 1,Department 2,Department 3,Department 4,Department 5," +
                            "Department 6,Department 7,Department 8,Department 9,Department 10,Reduction% 1,Reduction% 2,Reduction% 3,Reduction% 4,Reduction% 5,Reduction% 6,Reduction% 7,Reduction% 8,Reduction% 9,Reduction% 10," +
                            "Sq In/Lb,Lin In/UOM,Length,Width,Depth,Avg Wt,Qty/Case,Case/Pallet,Flute,Density,Color,S Depth"

        cFieldListToSelect = "ind-type,i-no,i-name,i-dscr,est-dscr,i-code,tax-rcpt,mat-type,mat-dscr,cost-type,costtype-descr,procat,procat-dscr,q-ptd," +
                            "q-ytd,q-lyr,u-ptd,u-ytd,u-lyr,ink-type,press-type,min-lbs,yield,weight-100,loc,loc-bin,q-onh," +
                            "cal,basis-w,reg-no,shrink,s-wid,s-len,r-wid,ect,dept-name[1],dept-name[2],dept-name[3],dept-name[4],dept-name[5]," +
                            "dept-name[6],dept-name[7],dept-name[8],dept-name[9],dept-name[10],speed%[1],speed%[2],speed%[3],speed%[4],speed%[5],speed%[6],speed%[7],speed%[8],speed%[9],speed%[10]," +
                            "sqin-lb,linin-lb,case-l,case-w,case-d,avg-w,box-case,case-pall,flute,density,color-1,s-dep".

{sys/inc/ttRptSel.i}
    IF ipcIndustry EQ "2" THEN
        ASSIGN cTextListToDefault  = "Industry Type,Item#,Name,DESC,Est.DESC,Item Code,Taxable,Mat'l Type,Cost Type,Category,QTY Usage PTD,Qty Usage YTD," +
                            "Qty Usage Last YR,Ink Type,Press Type,Min Lbs/Job,SI/Lb,Wgt/100,Caliper,Shrink%,Weight/MSF,Width,Depth," +
                            "Length,Density,Roll W,Color,ECT,Department 1,Department 2,Department 3,Department 4,Department 5,Department 6,Department 7,Department 8,Department 9,Department 10," +
                            "Reduction% 1,Reduction% 2,Reduction% 3,Reduction% 4,Reduction% 5,Reduction% 6,Reduction% 7,Reduction% 8,Reduction% 9,Reduction% 10,Case Length,Case Width,Case Depth,Avg Wt,Boxes/Bundle," +
                            "Bundle/Pallet,Flute,Test,Sq In/Lb,Lin In/UOM,Warehouse,Bin,Qty On Hand" .
    ELSE
   ASSIGN cTextListToDefault  = "Industry Type,Item#,Name,DESC,Est.DESC,Item Code,Taxable,Mat'l Type,Cost Type,Category,QTY Usage PTD,Qty Usage YTD," +
                            "Qty Usage Last YR,Ink Type,Press Type,Min Lbs/Job,SI/Lb,Wgt/100,Caliper,Shrink%,Basis Weight,Sheet Width,S Depth," +
                            "Sheet Length,Density,Roll Width,Color,Core Dia,Department 1,Department 2,Department 3,Department 4,Department 5,Department 6,Department 7,Department 8,Department 9,Department 10," +
                            "Reduction% 1,Reduction% 2,Reduction% 3,Reduction% 4,Reduction% 5,Reduction% 6,Reduction% 7,Reduction% 8,Reduction% 9,Reduction% 10,Length,Width,Depth,Avg Wt,Qty/Case," +
                            "Case/Pallet,Flute,Reg. #,Sq In/Lb,Lin In/UOM,Warehouse,Bin,Qty On Hand" .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_rm-no end_rm-no ~
sl_avail Btn_Def Btn_Add sl_selected Btn_Remove btn_Up btn_down tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no sl_avail sl_selected ~
tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR ITEM, ipc-field AS CHAR )  FORWARD.

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

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "x(10)" 
     LABEL "From Raw Material Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzzz" 
     LABEL "To Raw Material Item" 
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

DEFINE FRAME rd-fgexp
     begin_rm-no AT ROW 3.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning RM Item Number" WIDGET-ID 142
     end_rm-no AT ROW 3.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending RM Item #" WIDGET-ID 144
     sl_avail AT ROW 12.24 COL 9 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 12.38 COL 44.20 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56 
     Btn_Add AT ROW 13.57 COL 44.20 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     sl_selected AT ROW 12.24 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 14.76 COL 44.20 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 15.95 COL 44.20 WIDGET-ID 136
     btn_down AT ROW 17.14 COL 44.20 WIDGET-ID 132
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
         TITLE "Export Raw Materials Inventory to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgexp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-fgexp:SCROLLABLE       = FALSE
       FRAME rd-fgexp:HIDDEN           = TRUE.

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export Raw Materials Inventory to Excel */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export Raw Materials Inventory to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no rd-fgexp
ON LEAVE OF begin_rm-no IN FRAME rd-fgexp /* From Raw Material Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgexp
ON CHOOSE OF btn-cancel IN FRAME rd-fgexp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgexp
ON CHOOSE OF btn-ok IN FRAME rd-fgexp /* OK */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgexp
ON CHOOSE OF btn_down IN FRAME rd-fgexp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgexp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgexp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgexp
ON CHOOSE OF btn_Up IN FRAME rd-fgexp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no rd-fgexp
ON LEAVE OF end_rm-no IN FRAME rd-fgexp /* To Raw Material Item */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgexp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-fgexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-fgexp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgexp 


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
    
  RUN DisplaySelectionList.
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    RUN Set-Sort-Data.

    APPLY "entry" TO begin_rm-no.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgexp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-fgexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgexp 
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
  
 
  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  {sys/ref/SelColCorrect.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgexp  _DEFAULT-ENABLE
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
  DISPLAY begin_rm-no end_rm-no sl_avail sl_selected tb_excel tb_runExcel 
          fi_file 
      WITH FRAME rd-fgexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_rm-no end_rm-no sl_avail Btn_Def Btn_Add 
         sl_selected Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok 
         btn-cancel 
      WITH FRAME rd-fgexp.
  VIEW FRAME rd-fgexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-fgexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF BUFFER b-item FOR item.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").
RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

FOR EACH b-item WHERE b-item.company = cocode
        AND b-item.i-no GE begin_rm-no
        AND b-item.i-no LE end_rm-no
        AND b-item.industry = ipcIndustry
        NO-LOCK:

    v-excel-detail-lines = "".

    FOR EACH ttRptSelected:
        v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue-itemfg(BUFFER b-item,ttRptSelected.FieldList)).
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
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgexp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
    IF begin_rm-no:SCREEN-VALUE EQ "" THEN do:
        FIND FIRST ITEM 
            WHERE ITEM.company EQ cocode
              AND item.industry EQ ipcIndustry 
            NO-LOCK NO-ERROR.
        begin_rm-no:SCREEN-VALUE = ITEM.i-no.
        FIND LAST ITEM 
            WHERE ITEM.company EQ cocode
              AND item.industry EQ ipcIndustry 
            NO-LOCK NO-ERROR.
        end_rm-no:SCREEN-VALUE   = ITEM.i-no.
    END.

    IF ipcSearch NE "" THEN DO:
        IF ipcSearchBy EQ "1" THEN do:
            FIND FIRST ITEM 
                WHERE ITEM.company EQ cocode
                  AND ITEM.i-no BEGINS ipcSearch
                  AND item.industry EQ ipcIndustry 
                NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
                begin_rm-no:SCREEN-VALUE = ITEM.i-no.
            FIND LAST ITEM 
                WHERE ITEM.company EQ cocode
                  AND ITEM.i-no BEGINS ipcSearch 
                  AND item.industry EQ ipcIndustry 
                NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
               end_rm-no:SCREEN-VALUE = ITEM.i-no.
        END. /* IF ipcSearchBy EQ "1" */
        IF ipcSearchBy EQ "2" THEN do:
            FIND FIRST ITEM 
                WHERE ITEM.company EQ cocode
                  AND ITEM.i-name BEGINS ipcSearch 
                  AND item.industry EQ ipcIndustry 
                NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
                begin_rm-no:SCREEN-VALUE = ITEM.i-no.
            FIND LAST ITEM 
                WHERE ITEM.company EQ cocode
                  AND ITEM.i-name BEGINS ipcSearch 
                  AND item.industry EQ ipcIndustry 
                NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
               end_rm-no:SCREEN-VALUE = ITEM.i-no.
        END. /* IF ipcSearchBy EQ "2" */
        IF ipcSearchBy EQ "3" THEN do:
            DEF VAR k AS INT INIT 1 NO-UNDO.
            k = 1.
            FOR EACH ITEM 
                WHERE ITEM.company EQ cocode
                  AND ITEM.mat-type BEGINS ipcSearch 
                  AND item.industry EQ ipcIndustry 
                NO-LOCK 
                BY ITEM.mat-type
                BY ITEM.i-name:
                IF k = 1 THEN
                    begin_rm-no:SCREEN-VALUE = ITEM.i-no.
                 ASSIGN
                     end_rm-no:SCREEN-VALUE = ITEM.i-no
                     k = k + 1.
            END.               
        END. /* IF ipcSearchBy EQ "3" */
        IF ipcSearchBy EQ "4" THEN do:
            DEF VAR t AS INT INIT 1 NO-UNDO.
            t = 1.
            FOR EACH ITEM 
                WHERE ITEM.company EQ cocode
                  AND ITEM.procat BEGINS ipcSearch 
                  AND item.industry EQ ipcIndustry 
                NO-LOCK 
                BY ITEM.procat
                BY ITEM.i-name:

                 IF t = 1 THEN
                     begin_rm-no:SCREEN-VALUE = ITEM.i-no.
                 ASSIGN
                     end_rm-no:SCREEN-VALUE = ITEM.i-no
                     t = t + 1.
            END.
        END. /* IF ipcSearchBy EQ "4" */
    END.

END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR ITEM, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR test AS INT FORMAT "->>>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE cTypelist AS CHARACTER EXTENT 14 FORMAT "x(30)" NO-UNDO.

    ASSIGN
    cTypelist[1] = "I,V"
    cTypelist[2] = "I,P,V"
    cTypelist[3] = "A,B,P,D,C,Z,1,2,3,4"
    cTypelist[4] = "D,C,Z,5,6"
    cTypelist[5] = "A,B,P,R,1,2,3,4,D,C,Z,5,6"
    cTypelist[6] = "A,B,P,1,2,3,4"
    cTypelist[7] = "A,B,P,R,1,2,3,4,W"
    cTypelist[8] = "A,B,P,R,1,2,3,4,W,D,C,Z,5,6"
    cTypelist[9] = "A,B,P,R,1,2,3,4"
    cTypelist[10] = "A,B,P,1,2,3,4,W"
    cTypelist[11] = "G,S,L,F,T"
    cTypelist[12] = "G,W,S,L,F,T"
    cTypelist[13] = "D,C,Z"
    cTypelist[14] = "1,2,3,4".
     .

    CASE ipc-field :
        WHEN "mat-dscr"  THEN DO:
            FIND mat WHERE mat.mat EQ  ipb-itemfg.mat-type NO-LOCK NO-ERROR.
            IF AVAIL mat THEN
                lc-return = mat.dscr.
            ELSE
                lc-return = "".
            
        END.
        WHEN "u-ytd" THEN DO:
            FIND ITEM WHERE ITEM.company EQ ipb-itemfg.company
                AND ITEM.i-no EQ ipb-itemfg.i-no NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN do:
                ASSIGN 
                lc-return = STRING(item.q-ytd * item.avg-cost)
                    
                    .
            END.
        END.
        WHEN "u-ptd" THEN DO:
            FIND ITEM WHERE ITEM.company EQ ipb-itemfg.company
                AND ITEM.i-no EQ ipb-itemfg.i-no NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN do:
                ASSIGN 
                lc-return = STRING(item.q-ptd * item.avg-cost)
                    .
            END.
        END.
        WHEN "u-lyr" THEN DO:
            FIND ITEM WHERE ITEM.company EQ ipb-itemfg.company
                AND ITEM.i-no EQ ipb-itemfg.i-no NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN do:
                ASSIGN 
                lc-return = STRING(item.q-lyr * item.avg-cost)
                    .
            END.
        END.
        WHEN "costtype-descr"  THEN DO:
            FIND costtype WHERE costtype.company EQ ipb-itemfg.company
                AND costtype.cost-type EQ  ipb-itemfg.cost-type NO-LOCK NO-ERROR.
            IF AVAIL costtype THEN
                lc-return = costtype.descr.
            ELSE
                lc-return = "".
            
        END.
        WHEN "procat-dscr"  THEN DO:
            FIND procat WHERE procat.company EQ ipb-itemfg.company
                AND procat.procat EQ  ipb-itemfg.procat NO-LOCK NO-ERROR.
            IF AVAIL procat THEN
                lc-return = procat.dscr.
            ELSE
                lc-return = "".
            
        END.
        WHEN "i-code"  THEN DO:
            CASE ipb-itemfg.i-code :
                WHEN "R" THEN
                    lc-return = "RM Stocked".
                WHEN "E" THEN
                    lc-return = "Estimated Mat'1".
            END CASE.
        END.
        WHEN "ink-type"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[1]) GT 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[2]) GT 0  THEN DO:
                CASE ipb-itemfg.ink-type :
                    WHEN "I" THEN
                        lc-return = "Ink".
                    WHEN "L" THEN
                        lc-return = "Lacquer".
                    WHEN "U" THEN
                        lc-return = "Ultra Violet".
                    WHEN "V" THEN
                        lc-return = "Varnish".
                    WHEN "A" THEN
                        lc-return = "Aqueous".
                 END CASE.
            END.
            ELSE DO:
                lc-return = "".
            END.
        END.
        WHEN "press-type"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[1]) GT 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[2]) GT 0  THEN DO:
                 CASE ipb-itemfg.press-type :
                     WHEN "F" THEN
                         lc-return = "Flexo".
                     WHEN "G" THEN
                         lc-return = "Gravure".
                     WHEN "L" THEN
                         lc-return = "Letterpress".
                     WHEN "O" THEN
                         lc-return = "Offset".
                     WHEN "S" THEN
                         lc-return = "Silkscreen".
                  END CASE.
            END.
            ELSE DO:
                lc-return = "".
            END.
        END.
        WHEN "flute"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[3]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[4]) > 0  THEN
                lc-return = ipb-itemfg.flute.
            ELSE
                lc-return = "".
        END.
        WHEN "reg-no"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[3]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[5]) > 0  THEN
                lc-return = ipb-itemfg.reg-no.
            ELSE
                lc-return = "".
        END.
        WHEN "cal"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[7]) > 0  THEN
                lc-return = string(ipb-itemfg.cal).
            ELSE
                lc-return = "".
        END.
        WHEN "basis-w"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[3]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[8]) > 0  THEN
                lc-return = string(ipb-itemfg.basis-w).
            ELSE
                lc-return = "".
        END.
        WHEN "ect"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[9]) > 0  THEN
                lc-return = string(ipb-itemfg.ect).
            ELSE
                lc-return = "".
        END.
        WHEN "shrink"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[10]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.shrink).
            ELSE
                lc-return = "".
        END.
        WHEN "s-wid"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[9]) > 0  THEN
                lc-return = string(ipb-itemfg.s-wid).
            ELSE
                lc-return = "".
        END.
        WHEN "s-len"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[7]) > 0  THEN
                lc-return = string(ipb-itemfg.s-len).
            ELSE
                lc-return = "".
        END.
        WHEN "r-wid"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[7]) > 0  THEN
                lc-return = string(ipb-itemfg.r-wid).
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[1]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[1].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[2]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[2].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[3]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[3].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[4]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[4].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[5]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[5].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[6]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[6].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[7]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[7].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[8]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[8].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[9]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[9].
            ELSE
                lc-return = "".
        END.
        WHEN "dept-name[10]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = ipb-itemfg.dept-name[10].
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[1]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[1]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[2]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[2]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[3]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[3]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[4]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[4]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[5]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[5]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[6]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[6]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[7]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[7]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[8]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[8]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[9]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[9]).
            ELSE
                lc-return = "".
        END.
        WHEN "speed%[10]" THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[6]) > 0  THEN
                lc-return = string(ipb-itemfg.speed%[10]).
            ELSE
                lc-return = "".
        END.
        WHEN "min-lbs"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[1]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[2]) > 0  THEN 
                lc-return = string(ipb-itemfg.min-lbs).
            ELSE
                lc-return = "".
        END.
        WHEN "yield"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[1]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[2]) > 0  THEN 
                lc-return = string(ipb-itemfg.yield).
            ELSE
                lc-return = "".
        END.
        WHEN "sqin-lb"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[11]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[12]) > 0  THEN 
                lc-return = string(ipb-itemfg.sqin-lb).
            ELSE
                lc-return = "".
        END.
        WHEN "linin-lb"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[11]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[12]) > 0  THEN 
                lc-return = string(ipb-itemfg.linin-lb).
            ELSE
                lc-return = "".
        END.
        WHEN "case-l"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[13]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[4]) > 0  THEN 
                lc-return = string(ipb-itemfg.case-l).
            ELSE
                lc-return = "".
        END.
        WHEN "case-w"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[13]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[4]) > 0  THEN 
                lc-return = string(ipb-itemfg.case-w).
            ELSE
                lc-return = "".
        END.
        WHEN "case-d"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[13]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[4]) > 0  THEN 
                lc-return = string(ipb-itemfg.case-d).
            ELSE
                lc-return = "".
        END.
        WHEN "avg-w"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[13]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[4]) > 0  THEN 
                lc-return = string(ipb-itemfg.avg-w).
            ELSE
                lc-return = "".
        END.
        WHEN "box-case"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[13]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[4]) > 0  THEN 
                lc-return = string(ipb-itemfg.box-case).
            ELSE
                lc-return = "".
        END.
        WHEN "case-pall"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[13]) > 0
             OR ipcIndustry EQ "1" AND lookup(ipb-itemfg.mat-type,cTypelist[4]) > 0  THEN 
                lc-return = string(ipb-itemfg.case-pall).
            ELSE
                lc-return = "".
        END.
        WHEN "density"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[14]) > 0 THEN 
                lc-return = string(ipb-itemfg.density).
            ELSE
                lc-return = "".
        END.
        WHEN "color-1"  THEN DO:
            IF ipcIndustry EQ "2" AND lookup(ipb-itemfg.mat-type,cTypelist[14]) > 0 THEN 
                lc-return = string(ipb-itemfg.color-1).
            ELSE
                lc-return = "".
        END.

        WHEN "ind-type"  THEN DO:
            IF ipcIndustry EQ "2" THEN
                lc-return = "Corrugated" .
            ELSE lc-return = "Folding" .
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

