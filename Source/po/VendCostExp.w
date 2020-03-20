&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-vendexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-vendexp 
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
DEFINE INPUT PARAMETER ipcItemType   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItem       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcVendor     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcCustomer   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEstimate   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEffective  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcExpires    AS CHARACTER NO-UNDO.

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
ASSIGN cTextListToSelect = "Item Type,Item Id,Vendor,Customer,Estimate,Form,Blank," + /*7*/
                            "Vender Item,Effective Date,Expires Date,Cost Uom," +     /* 4*/
                            "Width Min,Width Max,length Min,Length Max,Under Width,Under Length," + /* 6*/
                            "Upcharge Width,Upcharge Length,Min Order Qty,Max Order Qty," +   /* 4*/
                            "Level Quantity 1,Level Quantity 2," + /*2*/
                            "Level Quantity 3,Level Quantity 4," + /*2*/
                            "Level Quantity 5,Level Quantity 6," + /*2*/
                            "Level Quantity 7,Level Quantity 8," + /*2*/
                            "Level Quantity 9,Level Quantity 10"  /*2*/
                            
       cFieldListToSelect = "itemType,itemID,vendorID,customerID,estimateNo,formNo,blankNo," +
                            "vendorItemID,effectiveDate,expirationDate,vendorUOM," +
                            "dimWidthMinimum,dimWidthMaximum,dimLengthMinimum,dimLengthMaximum,dimWidthUnder,dimLengthUnder," +
                            "dimWidthOver,dimLengthOver,quantityMinimumOrder,quantityMaximumOrder," +
                            "levelQuantity1,levelQuantity2," +  
                            "levelQuantity3,levelQuantity4," +
                            "levelQuantity5,levelQuantity6," +
                            "levelQuantity7,levelQuantity8," +
                            "levelQuantity9,levelQuantity10".

{sys/inc/ttRptSel.i}

    ASSIGN cTextListToDefault  = "Item Type,Item Id,Vendor,Customer,Estimate,Form,Blank," + /*7*/
                            "Vender Item,Effective Date,Expires Date,Cost Uom," +     /* 4*/
                            "Width Min,Width Max,length Min,Length Max,Under Width,Under Length," + /* 6*/
                            "Upcharge Width,Upcharge Length,Min Order Qty,Max Order Qty," +   /* 4*/
                            "Level Quantity 1,Level Quantity 2,"  +  /*2*/
                            "Level Quantity 3,Level Quantity 4,"  + /*2*/
                            "Level Quantity 5,Level Quantity 6,"  + /*2*/
                            "Level Quantity 7,Level Quantity 8,"  + /*2*/
                            "Level Quantity 9,Level Quantity 10"  /*2*/ .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-vendexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS item_type begin_item end_item begin_vend-no ~
end_vend-no begin_cust-no end_cust-no begin_est-no end_est-no begin_date-eff ~
end_date-eff sl_avail Btn_Def Btn_Add sl_selected Btn_Remove btn_Up btn_down ~
tb_runExcel fi_file btn-ok btn-cancel begin_date-exp end_date-exp RECT-6 ~
RECT-7 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS item_type begin_item end_item begin_vend-no ~
end_vend-no begin_cust-no end_cust-no begin_est-no end_est-no begin_date-eff ~
end_date-eff sl_avail sl_selected tb_excel tb_runExcel fi_file begin_date-exp ~
end_date-exp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-vendexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-vendexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-vendexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue rd-vendexp 
FUNCTION getValue RETURNS CHARACTER
  ( BUFFER ipb-vendItemCost FOR vendItemCost, 
    ipc-field AS CHAR )  FORWARD.

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Cust #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date-eff AS DATE FORMAT "99/99/9999" INITIAL 01/01/1901 
     LABEL "From Effective Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_est-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Estimate #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.


DEFINE VARIABLE item_type AS CHARACTER FORMAT "X(5)" 
     LABEL "Item Type" 
     VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "FG","FG",
                     "RM","RM",
                      "All","All"
          DROP-DOWN-LIST
      SIZE 8 BY 1.

DEFINE VARIABLE begin_item AS CHARACTER FORMAT "X(15)" 
     LABEL "From Item #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date-exp AS DATE FORMAT "99/99/9999" INITIAL 01/01/1901 
     LABEL "From Expires Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Vendor #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Cust #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date-eff AS DATE FORMAT "99/99/9999" INITIAL 12/31/2099 
     LABEL "To Effective Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_est-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Estimate #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Item #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date-exp AS DATE FORMAT "99/99/9999" INITIAL 12/31/2099 
     LABEL "To Expires Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Vendor #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\VendCostMatrix.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 9.76.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.1 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.1 NO-UNDO.

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

DEFINE FRAME rd-vendexp
     item_type AT ROW 2.30 COL 28 COLON-ALIGNED 
     begin_item AT ROW 3.52 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 120
     end_item AT ROW 3.52 COL 71 COLON-ALIGNED HELP
          "Enter Ending Item Number" WIDGET-ID 122
     begin_vend-no AT ROW 4.71 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number" WIDGET-ID 6
     end_vend-no AT ROW 4.71 COL 71 COLON-ALIGNED HELP
          "Enter Ending Vendor Number" WIDGET-ID 16
     begin_cust-no AT ROW 5.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 100
     end_cust-no AT ROW 5.86 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 102
     begin_est-no AT ROW 7 COL 28 COLON-ALIGNED HELP
          "Enter Beginning estimate Number" WIDGET-ID 104
     end_est-no AT ROW 7 COL 71 COLON-ALIGNED HELP
          "Enter Ending estimate Number" WIDGET-ID 106
     begin_date-eff AT ROW 8.14 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Effective Date" WIDGET-ID 112
     end_date-eff AT ROW 8.1 COL 71 COLON-ALIGNED HELP
          "Enter Ending Effective Date" WIDGET-ID 114
     begin_date-exp AT ROW 9.24 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Expires Date" WIDGET-ID 142
     end_date-exp AT ROW 9.19 COL 71 COLON-ALIGNED HELP
          "Enter Ending Expires Date" WIDGET-ID 144
     sl_avail AT ROW 12.81 COL 9 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.05 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 14 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     sl_selected AT ROW 12.81 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 14.95 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 15.91 COL 44 WIDGET-ID 136
     btn_down AT ROW 16.86 COL 44 WIDGET-ID 132
     tb_excel AT ROW 18.57 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 18.57 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 19.52 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 21.24 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 21.24 COL 60.2 WIDGET-ID 12
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.1 COL 10 WIDGET-ID 140
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.1 COL 63.4 WIDGET-ID 138
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 11.14 COL 3 WIDGET-ID 86
     RECT-6 AT ROW 11.38 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 18.1 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Vendor Cost Matrix to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-vendexp
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME rd-vendexp:SCROLLABLE       = FALSE
       FRAME rd-vendexp:HIDDEN           = TRUE.

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       begin_date-eff:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       begin_est-no:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       begin_item:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".
ASSIGN 
       item_type:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".


ASSIGN 
       begin_date-exp:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       end_date-eff:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       end_est-no:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       end_item:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       end_date-exp:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-vendexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-vendexp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-vendexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-vendexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-vendexp rd-vendexp
ON HELP OF FRAME rd-vendexp /* Export Vend Cost Matrix to Excel */
DO:
    DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
    DEF VAR ls-cur-val AS CHAR NO-UNDO.
    DEF VAR char-val AS CHAR NO-UNDO.
    DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

    lw-focus = FOCUS.
    
    case lw-focus:name :

       when "begin_vend-no" then do:
           ls-cur-val = lw-focus:screen-value.
           
           RUN system/openlookup.p (g_company, "vend-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
           return no-apply.
       end.  /* vend-no*/
       when "end_vend-no" then do:
           ls-cur-val = lw-focus:screen-value.
            RUN system/openlookup.p (g_company, "vend-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
           return no-apply.
       end.  /* vend-no*/
        when "begin_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
           
           RUN system/openlookup.p (g_company, "cust-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
           return no-apply.
       end.  /* cust*/
       when "end_cust-no" then do:
           ls-cur-val = lw-focus:screen-value.
            RUN system/openlookup.p (g_company, "cust-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
           return no-apply.
       end.  /* cust*/

       when "begin_est-no" then do:
           ls-cur-val = lw-focus:screen-value.

           RUN windows/l-est.w (g_company,g_loc,"", OUTPUT char-val).
           IF char-val <> "" THEN DO:
               FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
               IF AVAIL eb THEN 
                   begin_est-no:screen-value = eb.est-no.
           END.

           return no-apply.
       end.  /* est*/
       when "end_est-no" then do:
           ls-cur-val = lw-focus:screen-value.
           RUN windows/l-est.w (g_company,g_loc,"", OUTPUT char-val).
           IF char-val <> "" THEN DO:
               FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
               IF AVAIL eb THEN 
                   end_est-no:screen-value = eb.est-no.
           END.
           return no-apply.
       end.  /* est*/

       when "begin_item" then do:
           ls-cur-val = lw-focus:screen-value.
           IF item_type:SCREEN-VALUE EQ "FG" THEN do:
               RUN system/openlookup.p (g_company, "i-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
               IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
           END.
           ELSE IF item_type:SCREEN-VALUE EQ "RM" THEN DO:
               RUN system/openlookup.p (g_company, "item", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
               IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
               END.
           return no-apply.
       end.  /* itemfg */
       when "end_item" then do:
           ls-cur-val = lw-focus:screen-value.
           IF item_type:SCREEN-VALUE EQ "FG" THEN do:
               RUN system/openlookup.p (g_company, "i-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
               IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
           END.
           ELSE IF item_type:SCREEN-VALUE EQ "RM" THEN DO:
               RUN system/openlookup.p (g_company, "item", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
               IF cMainField <> "" THEN lw-focus:screen-value = cMainField. 
               END.
           return no-apply.
       end.  /* itemfg*/
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-vendexp rd-vendexp
ON WINDOW-CLOSE OF FRAME rd-vendexp /* Export Vend Cost Matrix to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no rd-vendexp
ON LEAVE OF begin_cust-no IN FRAME rd-vendexp /* From Cust # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-eff
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-eff rd-vendexp
ON LEAVE OF begin_date-eff IN FRAME rd-vendexp /* From  Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est-no rd-vendexp
ON LEAVE OF begin_est-no IN FRAME rd-vendexp /* From Estimate # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item rd-vendexp
ON LEAVE OF begin_item IN FRAME rd-vendexp /* From Item # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-exp rd-vendexp
ON LEAVE OF begin_date-exp IN FRAME rd-vendexp /* From  Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no rd-vendexp
ON LEAVE OF begin_vend-no IN FRAME rd-vendexp /* From Vendor # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-vendexp
ON CHOOSE OF btn-cancel IN FRAME rd-vendexp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-vendexp
ON CHOOSE OF btn-ok IN FRAME rd-vendexp /* OK */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-vendexp
ON CHOOSE OF Btn_Add IN FRAME rd-vendexp /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  APPLY "DEFAULT-ACTION" TO sl_avail.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-vendexp
ON CHOOSE OF Btn_Def IN FRAME rd-vendexp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-vendexp
ON CHOOSE OF btn_down IN FRAME rd-vendexp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-vendexp
ON CHOOSE OF Btn_Remove IN FRAME rd-vendexp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-vendexp
ON CHOOSE OF btn_Up IN FRAME rd-vendexp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no rd-vendexp
ON LEAVE OF end_cust-no IN FRAME rd-vendexp /* To Cust # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date-eff
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date-eff rd-vendexp
ON LEAVE OF end_date-eff IN FRAME rd-vendexp /* To Due Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est-no rd-vendexp
ON LEAVE OF end_est-no IN FRAME rd-vendexp /* To Estimate # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item rd-vendexp
ON LEAVE OF end_item IN FRAME rd-vendexp /* To Item # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date-exp rd-vendexp
ON LEAVE OF end_date-exp IN FRAME rd-vendexp /* To PO Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no rd-vendexp
ON LEAVE OF end_vend-no IN FRAME rd-vendexp /* To Vendor # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-vendexp
ON LEAVE OF fi_file IN FRAME rd-vendexp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-vendexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-vendexp
DO:
  
   IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
       sl_selected:NUM-ITEMS = 0)
   THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
               ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
              /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
               .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-vendexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-vendexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-vendexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-vendexp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-vendexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-vendexp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-vendexp 


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

    /*APPLY "entry" TO begin_po.*/
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-vendexp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-vendexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-vendexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-vendexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-vendexp 
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

  {sys/ref/SelColCorrect.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-vendexp  _DEFAULT-ENABLE
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
  DISPLAY item_type begin_item end_item begin_vend-no end_vend-no begin_cust-no 
          end_cust-no begin_est-no end_est-no begin_date-eff end_date-eff sl_avail 
          sl_selected tb_excel tb_runExcel fi_file begin_date-exp end_date-exp 
      WITH FRAME rd-vendexp.
  ENABLE item_type begin_item end_item begin_vend-no end_vend-no begin_cust-no 
         end_cust-no begin_est-no end_est-no begin_date-eff end_date-eff sl_avail 
         Btn_Def Btn_Add sl_selected Btn_Remove btn_Up btn_down tb_runExcel 
         fi_file btn-ok btn-cancel begin_date-exp end_date-exp RECT-6 RECT-7 
         RECT-8 
      WITH FRAME rd-vendexp.
  VIEW FRAME rd-vendexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-vendexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-vendexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-vendexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-vendexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF VAR v-adder AS CHAR NO-UNDO.
DEFINE VARIABLE cFGItem AS CHAR NO-UNDO .
DEF BUFFER xjob-mat FOR job-mat.
DEF BUFFER xitem    FOR item.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.
  
FOR EACH vendItemCost WHERE vendItemCost.company = cocode
        AND (vendItemCost.itemType EQ item_type OR item_type EQ "All")
        AND vendItemCost.itemID GE begin_item
        AND vendItemCost.itemID LE end_item
        AND vendItemCost.vendorID GE begin_vend-no
        AND vendItemCost.vendorID LE end_vend-no
        AND vendItemCost.customerID GE begin_cust-no
        AND vendItemCost.customerID LE end_cust-no
        AND vendItemCost.estimateNo GE begin_est-no
        AND vendItemCost.estimateNo LE end_est-no
        AND (vendItemCost.effectiveDate GE begin_date-eff OR (begin_date-eff LE 01/01/1900 AND vendItemCost.effectiveDate LE 01/01/1900))
        AND (vendItemCost.effectiveDate LE end_date-eff OR (begin_date-eff LE 01/01/1900 AND vendItemCost.effectiveDate LE 01/01/1900))
        AND (vendItemCost.expirationDate GE  begin_date-exp OR (vendItemCost.expirationDate EQ ? OR begin_date-exp EQ ?))
        AND (vendItemCost.expirationDate LE end_date-exp OR vendItemCost.expirationDate EQ ?) :

    v-excel-detail-lines = "".
    

    FOR EACH ttRptSelected:

        IF lookup(ttRptSelected.FieldList,"levelQuantity1,levelQuantity2,levelQuantity3,levelQuantity4,levelQuantity5,levelQuantity6,levelQuantity7,levelQuantity8,levelQuantity9,levelQuantity10,effectiveDate") EQ 0 THEN do:
        v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue(BUFFER vendItemCost,ttRptSelected.FieldList)).
        END.
        ELSE IF ttRptSelected.FieldList EQ "effectiveDate" THEN do:
            IF vendItemCost.effectiveDate LE 01/01/1900 THEN
             v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string("01/01/1900")).
            ELSE v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCost.effectiveDate,"99/99/9999")).
        END.
        ELSE do:
           i = 1 .
           FOR EACH vendItemCostLevel WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID
               NO-LOCK BY vendItemCostLevel.quantityBase:

               CASE ttRptSelected.FieldList:   
                       WHEN "levelQuantity1"  THEN do:
                         IF i EQ 1 THEN do:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity2" THEN DO:
                         IF i EQ 2 THEN do:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity3"  THEN DO:
                         IF i EQ 3 THEN do:
                          v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity4" THEN DO:
                         IF i EQ 4 THEN do:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity5"  THEN DO:
                         IF i EQ 5 THEN do:
                          v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity6" THEN do:
                         IF i EQ 6 THEN do:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity7" THEN DO:
                         IF i EQ 7 THEN do:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity8" THEN DO:
                         IF i EQ 8 THEN do:
                          v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity9"  THEN DO:
                         IF i EQ 9 THEN do:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
                       WHEN "levelQuantity10" THEN DO:
                         IF i EQ 10 THEN do:
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.quantityBase,">>>>>>9.9<<")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costPerUOM,">>>>9.9999")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costSetup,"->>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.costDeviation,"->>>>>>9.99")).
                           v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(vendItemCostLevel.leadTimeDays,">>>>>>9")).
                         END.
                       END.
               END CASE.
                         i = i + 1 .
           END.
        END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-vendexp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
         IF ipcItemType NE "" THEN
             item_type:SCREEN-VALUE = string(ipcItemType) .
         IF ipcItem NE "" THEN
             ASSIGN
             begin_item:SCREEN-VALUE = ipcItem 
             end_item:SCREEN-VALUE = ipcItem .
         ELSE 
             ASSIGN
             begin_item:SCREEN-VALUE = "" 
             end_item:SCREEN-VALUE = "zzzzzzzzzzzzzzz" .
         IF ipcVendor NE "" THEN
             ASSIGN
             begin_vend-no:SCREEN-VALUE = ipcVendor
             end_vend-no:SCREEN-VALUE = ipcVendor.
         ELSE ASSIGN
             begin_vend-no:SCREEN-VALUE = ""
             end_vend-no:SCREEN-VALUE = "zzzzzzzz" .
         IF ipcCustomer NE "" THEN
             ASSIGN
             begin_cust-no:SCREEN-VALUE = ipcCustomer
             end_cust-no:SCREEN-VALUE = ipcCustomer.
         ELSE ASSIGN
             begin_cust-no:SCREEN-VALUE = ""
             end_cust-no:SCREEN-VALUE = "zzzzzzzz".
         IF ipcEstimate NE "" THEN
             ASSIGN
             begin_est-no:SCREEN-VALUE   = ipcEstimate
             end_est-no:SCREEN-VALUE = ipcEstimate.
         ELSE ASSIGN
             begin_est-no:SCREEN-VALUE   = ""
             end_est-no:SCREEN-VALUE = "zzzzzzzz" .

        ASSIGN
            begin_date-exp:SCREEN-VALUE   = ""
            end_date-exp:SCREEN-VALUE     = string("12/31/2099")
            begin_date-eff:SCREEN-VALUE   = string("01/01/1900")
            end_date-eff:SCREEN-VALUE     = string(TODAY).
         
END.
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-vendexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-vendexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-vendexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lc-header AS CHAR NO-UNDO.

FOR EACH ttRptSelected:

    IF lookup(ttRptSelected.FieldList,"levelQuantity1,levelQuantity2,levelQuantity3,levelQuantity4,levelQuantity5,levelQuantity6,levelQuantity7,levelQuantity8,levelQuantity9,levelQuantity10") EQ 0 THEN do:
        lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
    END.
    ELSE DO:

      CASE ttRptSelected.FieldList :
        WHEN "levelQuantity1"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 1").
            lc-header = lc-header + appendXLLine("Cost Per 1").
            lc-header = lc-header + appendXLLine("Setup 1").
            lc-header = lc-header + appendXLLine("Deviation Cost 1").
            lc-header = lc-header + appendXLLine("Lead Time 1").
        END.
        WHEN "levelQuantity2"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 2").
            lc-header = lc-header + appendXLLine("Cost Per 2").
            lc-header = lc-header + appendXLLine("Setup 2").
            lc-header = lc-header + appendXLLine("Deviation Cost 2").
            lc-header = lc-header + appendXLLine("Lead Time 2").
        END.
        WHEN "levelQuantity3"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 3").
            lc-header = lc-header + appendXLLine("Cost Per 3").
            lc-header = lc-header + appendXLLine("Setup 3").
            lc-header = lc-header + appendXLLine("Deviation Cost 3").
            lc-header = lc-header + appendXLLine("Lead Time 3").
        END.
        WHEN "levelQuantity4"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 4").
            lc-header = lc-header + appendXLLine("Cost Per 4").
            lc-header = lc-header + appendXLLine("Setup 4").
            lc-header = lc-header + appendXLLine("Deviation Cost 4").
            lc-header = lc-header + appendXLLine("Lead Time 4").
        END.
        WHEN "levelQuantity5"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 5").
            lc-header = lc-header + appendXLLine("Cost Per 5").
            lc-header = lc-header + appendXLLine("Setup 5").
            lc-header = lc-header + appendXLLine("Deviation Cost 5").
            lc-header = lc-header + appendXLLine("Lead Time 5").
        END.
        WHEN "levelQuantity6"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 6").
            lc-header = lc-header + appendXLLine("Cost Per 6").
            lc-header = lc-header + appendXLLine("Setup 6").
            lc-header = lc-header + appendXLLine("Deviation Cost 6").
            lc-header = lc-header + appendXLLine("Lead Time 6").
        END.
        WHEN "levelQuantity7"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 7").
            lc-header = lc-header + appendXLLine("Cost Per 7").
            lc-header = lc-header + appendXLLine("Setup 7").
            lc-header = lc-header + appendXLLine("Deviation Cost 7").
            lc-header = lc-header + appendXLLine("Lead Time 7").
        END.
        WHEN "levelQuantity8"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 8").
            lc-header = lc-header + appendXLLine("Cost Per 8").
            lc-header = lc-header + appendXLLine("Setup 8").
            lc-header = lc-header + appendXLLine("Deviation Cost 8").
            lc-header = lc-header + appendXLLine("Lead Time 8").
        END.
        WHEN "levelQuantity9"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 9").
            lc-header = lc-header + appendXLLine("Cost Per 9").
            lc-header = lc-header + appendXLLine("Setup 9").
            lc-header = lc-header + appendXLLine("Deviation Cost 9").
            lc-header = lc-header + appendXLLine("Lead Time 9").
        END.
        WHEN "levelQuantity10"  THEN DO:
            lc-header = lc-header + appendXLLine("Level Quantity 10").
            lc-header = lc-header + appendXLLine("Cost Per 10").
            lc-header = lc-header + appendXLLine("Setup 10").
            lc-header = lc-header + appendXLLine("Deviation Cost 10").
            lc-header = lc-header + appendXLLine("Lead Time 10").
        END.
       
      END CASE.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue rd-vendexp 
FUNCTION getValue RETURNS CHARACTER
  ( BUFFER ipb-vendItemCost FOR vendItemCost, 
    ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR h-field AS HANDLE.
   DEF VAR li-extent AS INT NO-UNDO.
   DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "ship-meth"  THEN DO:
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-vendItemCost:BUFFER-FIELD(ipc-field).
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



