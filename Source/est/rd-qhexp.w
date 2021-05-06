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
DEFINE INPUT PARAMETER ipcCustFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCategoryFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCategoryTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcSalesFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcSalesTo   AS CHARACTER NO-UNDO.   
DEFINE INPUT PARAMETER ipcDateFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcDateTo   AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy AS LOG NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS cha NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE  NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN cTextListToSelect = "Part #,Customer,Quote#,Quantity,Price,Profit %,Price UOM,ShipTo,SoldTo,Quote Date,Delivery Date,Expiration Date," +
                           "Estimate #,Contact,Sales Group,Terms Code,Carrier,Zone,FG Item #,Item Description," +
                           "Item Description 2,Est Style,Dimensions,Board,Color,FG Category, EST Category"
            cFieldListToSelect = "part-no,cust-no,quote,qty,price,profit,price-uom,shipto,soldto,quote-date,del-date,exp-date," +
                                 "est-no,contact,sale-group,terms,carrier,zone,fg-item,item-dscr," +
                                 "item-dscr2,style,dimension,board,color,itemCategory,estCategory"    .
{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Part #,Customer,Quote#,Quantity,Price,Profit %,Price UOM,ShipTo,SoldTo,Quote Date,Delivery Date,Expiration Date," +
                             "Estimate #,Contact,Sales Group,Terms Code,Carrier,Zone,FG Item #,Item Description," +
                             "Item Description 2,Est Style,Dimensions,Board,Color,FG Category,EST Category" .
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_cust end_cust ~
begin_quo# end_quo# begin_procat end_procat begin_fg-procat end_fg-procat ~
begin_slm end_slm begin_date end_date expiration_date tb_expmatrix ~
tb_qtymin sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_quo# end_quo# ~
begin_procat end_procat begin_fg-procat end_fg-procat begin_slm end_slm ~
begin_date end_date expiration_date tb_expmatrix tb_qtymin sl_avail ~
sl_selected tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "From Quote Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_fg-procat AS CHARACTER FORMAT "X(8)" 
     LABEL "From FG Category" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(8)" 
     LABEL "From Est Category" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_quo# AS INTEGER FORMAT ">>>>>>" INITIAL 0 
     LABEL "From Quote" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "X(3)" 
     LABEL "From Sales Rep #" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
     LABEL "To Customer" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "To Quote Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_fg-procat AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzz" 
     LABEL "To FG Category" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzz" 
     LABEL "To Est Category" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_quo# AS INTEGER FORMAT ">>>>>>" INITIAL 999999 
     LABEL "To Quote" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "X(3)" INITIAL "zzz" 
     LABEL "To Sales Rep #" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE expiration_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Expiration Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-quote.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 10.24.

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

DEFINE VARIABLE tb_expmatrix AS LOGICAL INITIAL yes 
     LABEL "Export to Price Matrix" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_qtymin AS LOGICAL INITIAL no 
     LABEL "Quantity -1" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
     begin_cust AT ROW 2.48 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust AT ROW 2.48 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_quo# AT ROW 3.57 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Quote Number" WIDGET-ID 146
     end_quo# AT ROW 3.57 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 148
     begin_procat AT ROW 4.71 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Category" WIDGET-ID 108
     end_procat AT ROW 4.71 COL 71 COLON-ALIGNED HELP
          "Enter Ending Category" WIDGET-ID 110
     begin_fg-procat AT ROW 5.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Category" WIDGET-ID 142
     end_fg-procat AT ROW 5.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending Category" WIDGET-ID 144
     begin_slm AT ROW 7.19 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep" WIDGET-ID 120
     end_slm AT ROW 7.19 COL 71 COLON-ALIGNED HELP
          "Enter Ending Sales Rep" WIDGET-ID 122
     begin_date AT ROW 8.43 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Quote Date" WIDGET-ID 100
     end_date AT ROW 8.43 COL 71 COLON-ALIGNED HELP
          "Enter Ending Quote Date" WIDGET-ID 102
     expiration_date AT ROW 9.81 COL 54.4 COLON-ALIGNED HELP
          "Enter Beginning Quote Date" WIDGET-ID 152
     tb_expmatrix AT ROW 9.91 COL 35.2 RIGHT-ALIGNED WIDGET-ID 150
     tb_qtymin AT ROW 9.91 COL 95.2 RIGHT-ALIGNED WIDGET-ID 154
     sl_avail AT ROW 13.29 COL 9 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 13.29 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 13.52 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 14.71 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 15.91 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 17.1 COL 44 WIDGET-ID 136
     btn_down AT ROW 18.29 COL 44 WIDGET-ID 132
     tb_excel AT ROW 19.95 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 19.95 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 20.91 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 22.76 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 22.76 COL 60.2 WIDGET-ID 12
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.57 COL 9.4 WIDGET-ID 140
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.57 COL 64.4 WIDGET-ID 138
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 11.57 COL 3 WIDGET-ID 86
     RECT-6 AT ROW 11.81 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 19.67 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Quote to Excel" WIDGET-ID 100.


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
       begin_cust:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_fg-procat:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_quo#:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_fg-procat:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_quo#:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       expiration_date:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_expmatrix IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_expmatrix:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_qtymin IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
       tb_qtymin:PRIVATE-DATA IN FRAME rd-fgexp     = 
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
ON HELP OF FRAME rd-fgexp /* Export Quote to Excel */
DO:
DEFINE VARIABLE lw-focus AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ls-cur-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

   lw-focus = FOCUS.

   CASE lw-focus:NAME :
       WHEN "begin_cust" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust */
       WHEN "end_cust" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* cust*/
       
       WHEN "begin_procat" OR WHEN "begin_fg-procat" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-fgcat.w (cocode, ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* procat */
       WHEN "end_procat" OR WHEN "end_fg-procat" THEN DO:
           ls-cur-val = lw-focus:SCREEN-VALUE.
           RUN windows/l-fgcat.w (cocode, ls-cur-val, OUTPUT char-val).
           IF char-val <> "" THEN DO:
              lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
           END.
           RETURN NO-APPLY.
       END.  /* procat */

END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export Quote to Excel */
DO:
    IF VALID-HANDLE(hdOutputProcs) THEN  
        DELETE PROCEDURE hdOutputProcs. 
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust rd-fgexp
ON LEAVE OF begin_cust IN FRAME rd-fgexp /* From Customer */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd-fgexp
ON LEAVE OF begin_date IN FRAME rd-fgexp /* From Quote Date */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_fg-procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_fg-procat rd-fgexp
ON LEAVE OF begin_fg-procat IN FRAME rd-fgexp /* From FG Category */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat rd-fgexp
ON LEAVE OF begin_procat IN FRAME rd-fgexp /* From Est Category */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_quo# rd-fgexp
ON LEAVE OF begin_quo# IN FRAME rd-fgexp /* From Quote */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm rd-fgexp
ON LEAVE OF begin_slm IN FRAME rd-fgexp /* From Sales Rep # */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgexp
ON CHOOSE OF btn-cancel IN FRAME rd-fgexp /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
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
  IF not tb_expmatrix THEN
  RUN run-report.
  ELSE RUN run-report-matrix.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
DO:
  DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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

  RUN DisplaySelectionDefault.  
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


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust rd-fgexp
ON LEAVE OF end_cust IN FRAME rd-fgexp /* To Customer */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date rd-fgexp
ON LEAVE OF end_date IN FRAME rd-fgexp /* To Quote Date */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_fg-procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_fg-procat rd-fgexp
ON LEAVE OF end_fg-procat IN FRAME rd-fgexp /* To FG Category */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat rd-fgexp
ON LEAVE OF end_procat IN FRAME rd-fgexp /* To Est Category */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_quo# rd-fgexp
ON LEAVE OF end_quo# IN FRAME rd-fgexp /* To Quote */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm rd-fgexp
ON LEAVE OF end_slm IN FRAME rd-fgexp /* To Sales Rep # */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME expiration_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL expiration_date rd-fgexp
ON LEAVE OF expiration_date IN FRAME rd-fgexp /* Expiration Date */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_expmatrix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_expmatrix rd-fgexp
ON VALUE-CHANGED OF tb_expmatrix IN FRAME rd-fgexp /* Export to Price Matrix */
DO:
  ASSIGN {&self-name}.
  RUN pSetField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_qtymin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qtymin rd-fgexp
ON VALUE-CHANGED OF tb_qtymin IN FRAME rd-fgexp /* Quantity -1 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-fgexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-fgexp /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
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
    RUN pSetField.
    APPLY "entry" TO begin_cust.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

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
  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
 

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
  DISPLAY begin_cust end_cust begin_quo# end_quo# begin_procat end_procat 
          begin_fg-procat end_fg-procat begin_slm end_slm begin_date end_date 
          expiration_date tb_expmatrix tb_qtymin sl_avail sl_selected tb_excel 
          tb_runExcel fi_file 
      WITH FRAME rd-fgexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_cust end_cust begin_quo# end_quo# 
         begin_procat end_procat begin_fg-procat end_fg-procat begin_slm 
         end_slm begin_date end_date expiration_date tb_expmatrix tb_qtymin 
         sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down 
         tb_runExcel fi_file btn-ok btn-cancel 
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
 DEFINE VARIABLE cTmpList AS cha NO-UNDO.

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
DEFINE VARIABLE v-excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.


DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE lv-pdf-file AS cha NO-UNDO.
DEFINE VARIABLE iQty AS INTEGER NO-UNDO .
DEFINE VARIABLE dPrice AS DECIMAL NO-UNDO .
DEFINE VARIABLE dProfit AS DECIMAL FORMAT "->,>>>,>>9.99%" NO-UNDO .
DEFINE VARIABLE cUom AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO . 
DEFINE VARIABLE cItemCategory AS CHARACTER  NO-UNDO .
DEFINE VARIABLE cEstCategory AS  CHARACTER  NO-UNDO . 

v-excelheader = buildHeader(). 
SESSION:SET-WAIT-STATE ("general").
RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

MAIN:
FOR EACH quotehd
        WHERE quotehd.company  EQ cocode
          AND quotehd.q-no     GE begin_quo#
          AND quotehd.q-no     LE end_quo#
          AND quotehd.cust-no  GE begin_cust
          AND quotehd.cust-no  LE end_cust
          AND quotehd.sman     GE begin_slm
          AND quotehd.sman     LE end_slm
          AND quotehd.quo-date GE begin_date    
          AND quotehd.quo-date LE end_date NO-LOCK,
       EACH quoteitm OF quotehd WHERE  NO-LOCK,
       EACH quoteqty NO-LOCK
         WHERE quoteqty.company EQ quoteitm.company 
          AND quoteqty.loc EQ quoteitm.loc 
          AND quoteqty.q-no EQ quoteitm.q-no 
          AND quoteqty.line EQ quoteitm.line  :

    v-excel-detail-lines = "".
    ASSIGN
       iQty   =  0
       dPrice = 0
       dProfit = 0
       cUom   = "".
     
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ cocode 
          AND itemfg.i-no EQ quoteitm.i-no NO-ERROR .

    IF AVAIL itemfg THEN
        IF NOT(itemfg.procat GE begin_fg-procat AND itemfg.procat LE end_fg-procat) THEN NEXT MAIN.
        ELSE cItemCategory = itemfg.procat.
     
     FIND FIRST eb NO-LOCK 
         WHERE eb.company EQ cocode
         AND eb.est-no EQ quotehd.est-no 
         AND eb.stock-no EQ quoteitm.i-no  NO-ERROR.

     IF NOT AVAILABLE eb THEN
         FIND FIRST eb NO-LOCK 
         WHERE eb.company EQ cocode
         AND eb.est-no EQ quotehd.est-no NO-ERROR.
    
     IF AVAILABLE eb THEN
         IF NOT(eb.procat GE begin_procat AND eb.procat LE end_procat) THEN NEXT MAIN.
         ELSE cEstCategory = eb.procat.

    
     
     ASSIGN
          iQty = INTEGER(quoteqty.qty)
          dPrice = DECIMAL(quoteqty.price)
          dProfit = DECIMAL(quoteqty.profit) 
          cUom   = quoteqty.uom .
     IF dProfit EQ ? THEN ASSIGN 
        dProfit = 9999999.99.
    

    FOR EACH ttRptSelected:
        
          CASE ttRptSelected.FieldList:                                                              
            WHEN "quote" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(quotehd.q-no)).   
            WHEN "part-no" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.part-no). 
            WHEN "cust-no" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.cust-no). 
            WHEN "qty" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(iQty)). 
            WHEN "price" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(dPrice)). 
            WHEN "profit" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(dProfit)).
            WHEN "price-uom" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cUom). 
            WHEN "shipto" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.ship-id).  
            WHEN "soldto" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.sold-id).
            WHEN "quote-date" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF quotehd.quo-date NE ? THEN STRING(quotehd.quo-date) ELSE ""). 
            WHEN "del-date" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine( IF quotehd.del-date NE ? THEN STRING(quotehd.del-date) ELSE ""). 
            WHEN "exp-date" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF quotehd.expireDate NE ? THEN STRING(quotehd.expireDate) ELSE "").
            WHEN "est-no" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.est-no).  
            WHEN "contact" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.contact). 
            WHEN "sale-group" THEN                                                                
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.sman).
            WHEN "terms" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.terms).    
            WHEN "carrier" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.carrier).
            WHEN "zone" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quotehd.del-zone).
            WHEN "fg-item" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.i-no).
            WHEN "item-dscr" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.part-dscr1).
            WHEN "item-dscr2" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.part-dscr2).    
            WHEN "style" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.style).
            WHEN "dimension" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.size).
            WHEN "board" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.i-dscr).
            WHEN "color" THEN                                                              
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.i-coldscr).
            WHEN "itemCategory" THEN                                                              
                  v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cItemCategory).
            WHEN "estCategory" THEN                                                              
                  v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cEstCategory).
          END CASE.  
        
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-matrix rd-fgexp 
PROCEDURE run-report-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE v-excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.


DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE lv-pdf-file AS cha NO-UNDO.
DEFINE VARIABLE iQty AS INTEGER NO-UNDO .
DEFINE VARIABLE dPrice AS DECIMAL NO-UNDO .
DEFINE VARIABLE dProfit AS DECIMAL FORMAT "->>,>>9.99%" NO-UNDO .
DEFINE VARIABLE cUom AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
DEFINE VARIABLE pricbas AS CHARACTER NO-UNDO.
DEFINE VARIABLE dPriceNextQty AS DECIMAL NO-UNDO .
DEFINE BUFFER bf-quoteqty FOR quoteqty.


v-excelheader = "Eff. Date,Customer,Type,Category,Item Code,Price Basis,Qty1,Price1,Dsc1,UOM1,Qty2,Price2,Dsc2,UOM2,"+
                "Qty3,Price3,Dsc3,UOM3,Qty4,Price4,Dsc4,UOM4,Qty5,Price5,Dsc5,UOM5,Qty6,Price6,Dsc6,UOM6," + 
                "Qty7,Price7,Dsc7,UOM7,Qty8,Price8,Dsc8,UOM8,Qty9,Price9,Dsc9,UOM9,Qty10,Price10,Dsc10,UOM10," +
                "Exp Date,ShipTo,Online,Minimum Order Qty,Customer Part #,Item Name,Item Description 1".

SESSION:SET-WAIT-STATE ("general").
RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

MAIN:
FOR EACH quotehd
        WHERE quotehd.company  EQ cocode
          AND quotehd.q-no     GE begin_quo#
          AND quotehd.q-no     LE end_quo#
          AND quotehd.cust-no  GE begin_cust
          AND quotehd.cust-no  LE end_cust
          AND quotehd.sman     GE begin_slm
          AND quotehd.sman     LE end_slm
          AND quotehd.quo-date GE begin_date    
          AND quotehd.quo-date LE end_date NO-LOCK,
       EACH quoteitm OF quotehd WHERE  NO-LOCK,
       FIRST quoteqty NO-LOCK
         WHERE quoteqty.company EQ quoteitm.company 
          AND quoteqty.loc EQ quoteitm.loc 
          AND quoteqty.q-no EQ quoteitm.q-no 
          AND quoteqty.line EQ quoteitm.line  :

    v-excel-detail-lines = "".
    ASSIGN
       iQty   =  0
       dPrice = 0
       dProfit = 0
       cUom   = ""
       dPriceNextQty = 0.
     
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ cocode 
          AND itemfg.i-no EQ quoteitm.i-no NO-ERROR .

    IF AVAIL itemfg THEN
        IF NOT(itemfg.procat GE begin_fg-procat AND itemfg.procat LE end_fg-procat) THEN NEXT MAIN.
     
     FIND FIRST eb NO-LOCK 
         WHERE eb.company EQ cocode
         AND eb.est-no EQ quotehd.est-no 
         AND eb.stock-no EQ quoteitm.i-no  NO-ERROR.

     IF NOT AVAILABLE eb THEN
         FIND FIRST eb NO-LOCK 
         WHERE eb.company EQ cocode
         AND eb.est-no EQ quotehd.est-no NO-ERROR.
    
     IF AVAILABLE eb THEN
         IF NOT(eb.procat GE begin_procat AND eb.procat LE end_procat) THEN NEXT MAIN.
      
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ cocode 
         AND cust.cust-no EQ quotehd.cust-no NO-ERROR .
    
    
     ASSIGN
            pricbas = "Price"
/*             v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(reftable.CODE)) */
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(TODAY,"99/99/9999"))
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(quotehd.cust-no))
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF AVAIL cust THEN cust.TYPE ELSE "")
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(IF AVAIL itemfg THEN itemfg.procat ELSE ""))
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(quoteitm.i-no)                      
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(pricbas)) .
    
     i = 0.
     FOR EACH bf-quoteqty NO-LOCK
         WHERE bf-quoteqty.company EQ quoteitm.company 
          AND bf-quoteqty.loc EQ quoteitm.loc 
          AND bf-quoteqty.q-no EQ quoteitm.q-no 
          AND bf-quoteqty.line EQ quoteitm.LINE BREAK BY bf-quoteqty.qty :
     
        ASSIGN
          iQty = IF tb_qtymin THEN INTEGER(bf-quoteqty.qty - 1) ELSE  INTEGER(bf-quoteqty.qty)
          dPrice = DECIMAL(bf-quoteqty.price)
          dProfit = DECIMAL(bf-quoteqty.profit) 
          cUom   = bf-quoteqty.uom .   
          i = i + 1 .
            IF i LE 10 THEN
            DO:  
               ASSIGN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(iQty    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(dPriceNextQty  ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine("0")
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(cUom    ))   .              
                dPriceNextQty = dPrice .
                IF LAST(bf-quoteqty.qty) THEN
                DO:
                ASSIGN
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("99999999"))
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(dPrice  ))
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine("0")
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(cUom    ))   .                 
                END.
            END.                          
     END.
         DO j = (i + 1) TO 9:
               ASSIGN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine("")
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine("")
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine("")
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine("")   .         
         END.
         
         ASSIGN   
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF expiration_date NE ? THEN  STRING(expiration_date,"99/99/9999") ELSE "")
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(quotehd.ship-id))
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("No")) 
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("0"))
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF AVAIL itemfg THEN itemfg.part-no ELSE "") 
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF AVAIL itemfg THEN itemfg.i-name ELSE "") 
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF AVAIL itemfg THEN itemfg.part-dscr1 ELSE "")
            
            .    

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
    ASSIGN 
        begin_cust:SCREEN-VALUE = assignParam(ipcCustFrom,NO)
        end_cust:SCREEN-VALUE   = assignParam(ipcCustTo,YES)
        begin_procat:SCREEN-VALUE = assignParam(ipcCategoryFrom,NO)
        end_procat:SCREEN-VALUE   = assignParam(ipcCategoryTo,YES)
        begin_fg-procat:SCREEN-VALUE = assignParam(ipcCategoryFrom,NO)
        end_fg-procat:SCREEN-VALUE   = assignParam(ipcCategoryTo,YES)
        begin_slm:SCREEN-VALUE = assignParam(ipcSalesFrom,NO)
        end_slm:SCREEN-VALUE   = assignParam(ipcSalesTo,YES)
        .

END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetField rd-fgexp 
PROCEDURE pSetField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    
        
    IF tb_expmatrix:SCREEN-VALUE EQ "Yes" THEN
    do:
     ASSIGN
        sl_avail:SENSITIVE    = FALSE
        sl_selected:SENSITIVE = FALSE
        Btn_Def:SENSITIVE     = FALSE
        Btn_Add:SENSITIVE     = FALSE
        Btn_Remove:SENSITIVE  = FALSE
        btn_Up:SENSITIVE      = FALSE
        btn_down:SENSITIVE    = FALSE 
        tb_qtymin:SENSITIVE   = TRUE
        expiration_date:SENSITIVE = TRUE.       
     END.
    ELSE DO:  
        ASSIGN
        sl_avail:SENSITIVE    = TRUE
        sl_selected:SENSITIVE = TRUE
        Btn_Def:SENSITIVE     = TRUE
        Btn_Add:SENSITIVE     = TRUE
        Btn_Remove:SENSITIVE  = TRUE
        btn_Up:SENSITIVE      = TRUE
        btn_down:SENSITIVE    = TRUE 
        tb_qtymin:SENSITIVE   = FALSE
        expiration_date:SENSITIVE = FALSE. 
        
    END.
    
END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.   
    
    /* This prevents null values from corrupting the entire display line */
    IF ipc-append EQ ? THEN ASSIGN 
        ipc-append = "".       
    
    ipc-append = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, ipc-append).
    
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHARACTER , ipl-end AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-return AS CHARACTER.

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
DEFINE VARIABLE lc-header AS CHARACTER NO-UNDO.

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

