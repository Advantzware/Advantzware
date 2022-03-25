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
DEFINE INPUT PARAMETER pcCustFrom   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcinvFrom    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pcitemFrom   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcbolFrom    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pcpoFrom     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcDateFrom   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcCustTo     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcinvTo      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pcitemTo     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcbolTo      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pcpoTo       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcDateTo     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLink      AS CHARACTER NO-UNDO .  

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name     AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.


DEFINE VARIABLE v-prgmname    AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos    AS INTEGER   NO-UNDO.
IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/defines/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE STREAM excel.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "Invoice#,Bol#,Customer,Cust Name,Inv Date,GL Account#,Acc Desc,FG Item#,Item Name," +
                            "Item Desccription,Cust Part#,Order#,Cust Po#,Est#,Shipto,Tax Group,Term Code,Term Desc," +
                            "Due Date,Discount%,Discount,Disc Days,Carrier,Invoice Amt,Freight Cost,Freight Billable,Tax,Amount Paid," +
                            "Balance Due,Line,Customer Lot#,Invoice Qty,Ship Qty,Cons Uom,Sqft,Price,Uom," +
                            "Dsct%,Amount,Amount MSF,Cost,Cost UOM,Sls Rep,% of Sales,Comm,Sls Rep2,% of Sales2,Comm2,Sls Rep3,% of Sales3,Comm3," + 
                            "Line Amount,Line Cost,Total Amount,Total Cost,Line Discount,Total Discount,Sold To Id,Sold To Name,Edi Price,Edi Price UOM"

    cFieldListToSelect = "ar-invl.inv-no,ar-invl.bol-no,ar-invl.cust-no,ar-inv.cust-name,ar-inv.inv-date,ar-invl.actnum,actdscr,ar-invl.i-no,ar-invl.i-name," +
                            "ar-invl.i-dscr,ar-invl.part-no,ar-invl.ord-no,ar-invl.po-no,ar-invl.est-no,ar-inv.ship-id,ar-inv.tax-code,ar-inv.terms,ar-inv.terms-d," +
                            "ar-inv.due-date,ar-inv.disc-%,ar-inv.disc-taken,ar-inv.disc-days,ar-inv.carrier,ar-inv.gross,ar-inv.freight,ar-inv.f-bill,ar-inv.tax-amt,ar-inv.paid," +
                            "ar-inv.due,ar-invl.LINE,ar-invl.lot-no,ar-invl.inv-qty,ar-invl.ship-qty,ar-invl.cons-uom,ar-invl.sf-sht,ar-invl.unit-pr,ar-invl.pr-qty-uom," +
                            "ar-invl.disc,amount,ar-invl.amt-msf,ar-invl.cost,dscr[1],sman[1],s-pct[1],s-comm[1],sman[2],s-pct[2],s-comm[2],sman[3],s-pct[3],s-comm[3]," + 
                            "line-amt,line-cst,total-amt,total-cst,line-dis,total-dis,cSoldToId,cSoldToName,ediPrice,ediPriceUom"
                            
    cFieldLength       = "15,15,15,20,15,30,15,15,20," + "15,15,15,20,15,30,15,15,20," + "15,15,15,20,15,30,15,15,15,20," + "15,15,15,20,15,30,15,15,20," + 
                        "15,15,15,20,15,15,15,20,15,30,15,15,15,15," + "15,15,15,15,15,15,10,12,11,13"
    cFieldType         = "i,i,c,c,c,i,c,c,c," + "c,i,c,c,c,c,c,c,c," + "c,i,i,i,c,i,i,i,i,i," + "c,i,c,c,c,c,c,c,c," +
                         "i,i,i,c,c,i,i,c,i,i,c,i,i,i," + "i,i,i,i,i,i,c,c,i,c"
    .


{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Customer,Shipto,Invoice#,Cust Po#,Inv Date,Due Date,Tax Group,Term Code,Discount,Disc Days," +
                                 "Carrier,Freight Cost, Freight Billable,Line,GL Account#,FG Item#,Item Name,Item Desccription,Customer Lot#,Invoice Qty," +
                                 "Cons Uom,Price,Uom,Amount,Line Discount,Cost,Cost UOM,Sls Rep,% of Sales,Comm,Sls Rep2,% of Sales2," + 
                                 "Comm2,Sls Rep3,% of Sales3,Comm3,Tax" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 RECT-9 begin_inv-no end_inv-no ~
begin_cust-no end_cust-no begin_i-no end_i-no begin_bol-no end_bol-no ~
begin_po-no end_po-no begin_date end_date sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_inv-no end_inv-no begin_cust-no ~
end_cust-no begin_i-no end_i-no begin_bol-no end_bol-no begin_po-no ~
end_po-no begin_date end_date sl_avail sl_selected fi_file tb_OpenCSV ~
tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine Dialog-Frame 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-actdscr Dialog-Frame 
FUNCTION get-actdscr RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.1.

DEFINE VARIABLE begin_bol-no  AS INTEGER   FORMAT "->>>>>>9" INITIAL 0 
    LABEL "From Bol" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
    LABEL "From Customer Code" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date    AS DATE      FORMAT "99/99/9999" 
    LABEL "From Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no    AS CHARACTER FORMAT "X(15)" 
    LABEL "From FG Item #" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_inv-no  AS INTEGER   FORMAT "->>>>>>>9":U INITIAL 0 
    LABEL "From Invoice" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_po-no   AS CHARACTER FORMAT "X(15)" 
    LABEL "From Cust Po#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_bol-no    AS INTEGER   FORMAT "->>>>>>9" INITIAL 9999999 
    LABEL "To Bol" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "To Customer Code" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date      AS DATE      FORMAT "99/99/9999" 
    LABEL "To Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_i-no      AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "To FG Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_inv-no    AS INTEGER   FORMAT "->>>>>>>9":U INITIAL 99999999 
    LABEL "To Invoice" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_po-no     AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "To Cust Po#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file       AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\CustomerInvoice.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 52 BY 1.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 7.33.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 2.48.

DEFINE RECTANGLE RECT-9
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 7.29.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5.58 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5.58 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel    AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    begin_inv-no AT ROW 2.1 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Date" WIDGET-ID 108
    end_inv-no AT ROW 2.1 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Date" WIDGET-ID 110
    begin_cust-no AT ROW 3.19 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 6
    end_cust-no AT ROW 3.19 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Customer Number" WIDGET-ID 16
    begin_i-no AT ROW 4.24 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Type" WIDGET-ID 104
    end_i-no AT ROW 4.24 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Type" WIDGET-ID 106
    begin_bol-no AT ROW 5.33 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Type" WIDGET-ID 112
    end_bol-no AT ROW 5.33 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Type" WIDGET-ID 114
    begin_po-no AT ROW 6.43 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning FG Item Number" WIDGET-ID 100
    end_po-no AT ROW 6.43 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending FG Item Number" WIDGET-ID 102
    begin_date AT ROW 7.48 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Invoice Date" WIDGET-ID 100
    end_date AT ROW 7.48 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Invoice Date" WIDGET-ID 120
    sl_avail AT ROW 10.24 COL 7 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 10.24 COL 42 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 10.24 COL 62.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 11.29 COL 42 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 12.33 COL 42 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.38 COL 42 WIDGET-ID 40
    btn_down AT ROW 14.48 COL 42 WIDGET-ID 42
    fi_file AT ROW 17.29 COL 18.8 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 17.38 COL 87.8 RIGHT-ALIGNED WIDGET-ID 34
    tbAutoClose AT ROW 19.19 COL 43 WIDGET-ID 60
    tb_excel AT ROW 19.29 COL 4 WIDGET-ID 32
    btn-ok AT ROW 20.24 COL 31 WIDGET-ID 14
    btn-cancel AT ROW 20.24 COL 51 WIDGET-ID 12
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 8.86 COL 5 WIDGET-ID 86
    "Selected Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 9.52 COL 69.6 WIDGET-ID 138
    "Available Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 9.52 COL 13.4 WIDGET-ID 140
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
    RECT-8 AT ROW 16.67 COL 4 WIDGET-ID 84
    RECT-9 AT ROW 9.19 COL 4 WIDGET-ID 116
    SPACE(2.99) SKIP(5.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Customer Invoice Excel Export" WIDGET-ID 100.


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
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

ASSIGN 
    begin_bol-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_inv-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_po-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_bol-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_inv-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_po-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME Dialog-Frame       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME Dialog-Frame
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Customer Invoice Excel Export */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :

            WHEN "begin_cust-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode,ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* cust-no*/  
            WHEN "end_cust-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode,ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* cust-no*/  
        /*when "begin_item" then do:
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
        end.  /* item-no*/  */
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Customer Invoice Excel Export */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol-no Dialog-Frame
ON LEAVE OF begin_bol-no IN FRAME Dialog-Frame /* From Bol */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* From Customer Code */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date Dialog-Frame
ON LEAVE OF begin_date IN FRAME Dialog-Frame /* From Invoice Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no Dialog-Frame
ON LEAVE OF begin_i-no IN FRAME Dialog-Frame /* From FG Item # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-no Dialog-Frame
ON LEAVE OF begin_inv-no IN FRAME Dialog-Frame /* From Invoice */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no Dialog-Frame
ON LEAVE OF begin_po-no IN FRAME Dialog-Frame /* From Cust Po# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
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
  
        ASSIGN 
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
        RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
        fi_file:SCREEN-VALUE =  cFileName.
  
        RUN GetSelectionList. 
        RUN run-report.
  
        IF NOT tb_OpenCSV THEN 
        DO:        
            MESSAGE "CSV file have been created." SKIP(1)
                "~"OK"~" to open CSV file?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                TITLE "" UPDATE lChoice AS LOGICAL.
 
            IF lChoice THEN
            DO:
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
            END.
        END.  /* IF NOT tb_OpenCSV THEN  */
    
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add Dialog-Frame
ON CHOOSE OF Btn_Add IN FRAME Dialog-Frame /* Add >> */
    DO:

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def Dialog-Frame
ON CHOOSE OF Btn_Def IN FRAME Dialog-Frame /* Default */
    DO:

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down Dialog-Frame
ON CHOOSE OF btn_down IN FRAME Dialog-Frame /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove Dialog-Frame
ON CHOOSE OF Btn_Remove IN FRAME Dialog-Frame /* << Remove */
    DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up Dialog-Frame
ON CHOOSE OF btn_Up IN FRAME Dialog-Frame /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol-no Dialog-Frame
ON LEAVE OF end_bol-no IN FRAME Dialog-Frame /* To Bol */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* To Customer Code */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date Dialog-Frame
ON LEAVE OF end_date IN FRAME Dialog-Frame /* To Invoice Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no Dialog-Frame
ON LEAVE OF end_i-no IN FRAME Dialog-Frame /* To FG Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-no Dialog-Frame
ON LEAVE OF end_inv-no IN FRAME Dialog-Frame /* To Invoice */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no Dialog-Frame
ON LEAVE OF end_po-no IN FRAME Dialog-Frame /* To Cust Po# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file Dialog-Frame
ON HELP OF fi_file IN FRAME Dialog-Frame /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to Save "
            FILTERS "Excel Files    (*.csv)" "*.csv",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR "c:\tmp"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file Dialog-Frame
ON LEAVE OF fi_file IN FRAME Dialog-Frame /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail Dialog-Frame
ON DEFAULT-ACTION OF sl_avail IN FRAME Dialog-Frame
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected Dialog-Frame
ON DEFAULT-ACTION OF sl_selected IN FRAME Dialog-Frame
    DO:
        DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
            IF {&SELF-NAME}:IS-SELECTED(i) THEN 
            DO:
                ASSIGN 
                    ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel Dialog-Frame
ON VALUE-CHANGED OF tb_excel IN FRAME Dialog-Frame /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV Dialog-Frame
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME Dialog-Frame /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


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
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {methods/nowait.i}
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
    
IF ipcLink EQ "AU5" THEN
    ASSIGN
        begin_cust-no:SCREEN-VALUE = pcCustFrom  
        begin_inv-no:SCREEN-VALUE  = STRING(pcinvFrom)
        begin_date:SCREEN-VALUE    = pcDateFrom  
        end_cust-no:SCREEN-VALUE   = IF pcCustTo NE "" THEN pcCustTo ELSE "zzzzzzzz"
        end_inv-no:SCREEN-VALUE    = STRING(pcinvTo) 
        end_date:SCREEN-VALUE      = pcDateTo  .         
                                                            
APPLY "entry" TO begin_inv-no.                          
fi_file:SCREEN-VALUE = "c:\tmp\CustomerInvoice.csv".                                                         
                                                              
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault Dialog-Frame 
PROCEDURE DisplaySelectionDefault :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

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
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:   
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
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
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:     
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +                     
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
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
    DISPLAY begin_inv-no end_inv-no begin_cust-no end_cust-no begin_i-no end_i-no 
        begin_bol-no end_bol-no begin_po-no end_po-no begin_date end_date 
        sl_avail sl_selected fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-7 RECT-8 RECT-9 begin_inv-no end_inv-no begin_cust-no end_cust-no 
        begin_i-no end_i-no begin_bol-no end_bol-no begin_po-no end_po-no 
        begin_date end_date sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
        IF NOT AVAILABLE ttRptList THEN
            MESSAGE "no " i ENTRY(i,ctmplist) SKIP
                ctmplist
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList     = ENTRY(i,cTmpList)
            ttRptSelected.FieldList    = ttRptList.FieldList
            ttRptSelected.FieldLength  = int(ENTRY(LOOKUP(ENTRY(i,cTmpList),cTextListToSelect),cFieldLength))
            ttRptSelected.DisplayOrder = i
            iColumnLength              = iColumnLength + ttRptSelected.FieldLength + 1.
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
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF move = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
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
    DEFINE VARIABLE str-line AS cha       FORM "x(300)" NO-UNDO.

    DEFINE BUFFER b-ar-invl  FOR ar-invl.
    DEFINE BUFFER bar-inv    FOR ar-inv.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.

    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE excelheader          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ino                  AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE amount               AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE chk-amount           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE pricbas              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE distot               AS DECIMAL   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE Amounttot            AS DECIMAL   EXTENT 2 FORMAT "->>>,>>>,>99.99" NO-UNDO.
    DEFINE VARIABLE costot               AS DECIMAL   EXTENT 2 FORMAT "->>>,>>>,>99.99" NO-UNDO.
    DEFINE VARIABLE vline                AS LOG       NO-UNDO INIT NO .

    DEFINE VARIABLE cDisplay             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField               AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lLineTotal           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dvalue               AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSoldToId            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldToName          AS CHARACTER NO-UNDO.

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO WITH FRAME {&FRAME-NAME}:

        SESSION:SET-WAIT-STATE ("general").

        IF tb_excel THEN
            OUTPUT STREAM excel TO VALUE(cFileName).

        DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
        FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

            IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                    str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                    excelheader = excelHeader + ttRptSelected.TextList + "," .        
            ELSE 
                ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                    str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                    excelheader = excelHeader + ttRptSelected.TextList + ","
                    .        
            cSlist = cSlist + ttRptSelected.FieldList + ",".
            IF LOOKUP(ttRptSelected.TextList, "Line Amount,Line Cost,Line Discount") <> 0    THEN
                lLineTotal = TRUE .
            ELSE lLineTotal = FALSE .
        END.


        IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

        dvalue     = 0 .
    
        FOR EACH ar-inv NO-LOCK WHERE 
            ar-inv.company EQ cocode AND 
            ar-inv.cust-no GE begin_cust-no AND
            ar-inv.cust-no LE end_cust-no AND 
            ar-inv.inv-no GE begin_inv-no AND 
            ar-inv.inv-no LE end_inv-no AND 
            ar-inv.inv-date GE begin_date AND 
            ar-inv.inv-date LE end_date,
            EACH ar-invl NO-LOCK WHERE 
            ar-invl.company EQ ar-inv.company AND 
            ar-invl.cust-no EQ ar-inv.cust-no AND 
            ar-invl.inv-no EQ ar-inv.inv-no AND
            ar-invl.x-no EQ ar-inv.x-no AND  
            ar-invl.posted EQ TRUE AND 
            ar-invl.i-no GE begin_i-no AND 
            ar-invl.i-no LE end_i-no AND 
            ar-invl.po-no GE begin_po-no AND 
            ar-invl.po-no LE end_po-no AND 
            ar-invl.bol-no GE begin_bol-no AND 
            ar-invl.bol-no LE end_bol-no
            BREAK BY ar-inv.inv-no DESCENDING 
            BY ar-invl.line: 
         
        
            /*   FOR EACH ar-invl NO-LOCK                  */
            /*     WHERE ar-invl.company EQ cocode         */
            /*       AND ar-invl.posted EQ YES             */
            /*       AND ar-invl.cust-no GE begin_cust-no  */
            /*       AND ar-invl.cust-no LE end_cust-no    */
            /*       AND ar-invl.inv-no GE begin_inv-no    */
            /*       AND ar-invl.inv-no LE end_inv-no      */
            /*       AND ar-invl.i-no GE begin_i-no        */
            /*       AND ar-invl.i-no LE end_i-no          */
            /*       AND ar-invl.bol-no GE begin_bol-no    */
            /*       AND ar-invl.bol-no LE end_bol-no      */
            /*       AND ar-invl.po-no GE begin_po-no      */
            /*       AND ar-invl.po-no LE end_po-no        */
            /*       AND ar-invl.inv-no NE 0               */
            /*       AND ar-invl.inv-no <> ? ,             */
            /*       FIRST ar-inv NO-LOCK                  */
            /*       WHERE ar-inv.x-no = ar-invl.x-no      */
            /*         AND ar-inv.inv-date GE begin_date   */
            /*         AND ar-inv.inv-date LE end_date     */
            /*         BREAK BY ar-invl.inv-no DESCENDING :*/

      
            ASSIGN
                amount = 0
                amount = IF NOT ar-invl.billable AND ar-invl.misc THEN 0 ELSE ar-invl.amt.
            
            ASSIGN
                cSoldToId   = ""
                cSoldToName = ""
                .
            FOR EACH soldto NO-LOCK
                WHERE soldto.company EQ ar-inv.company
                AND soldto.cust-no   EQ ar-inv.cust-no
                AND soldto.sold-id   EQ ar-inv.ship-id
                :
                ASSIGN
                    cSoldToId   = soldto.sold-id
                    cSoldToName = soldto.sold-name
                    .
            END.
        

            IF lLineTotal THEN 
            DO:
                IF FIRST-OF(ar-inv.inv-no) THEN
                    ASSIGN
                        distot[2]    = 0 
                        Amounttot[2] = 0
                        costot[2]    = 0.

                IF FIRST-OF(ar-inv.inv-no) THEN
                    FOR EACH bf-ar-invl WHERE bf-ar-invl.company = ar-invl.company 
                        AND bf-ar-invl.inv-no = ar-invl.inv-no NO-LOCK:
                        ASSIGN
                            chk-amount = 0
                            chk-amount = IF NOT bf-ar-invl.billable AND bf-ar-invl.misc THEN 0 ELSE bf-ar-invl.amt.
                        ASSIGN
                            distot[2]    = distot[2] + ar-inv.disc-taken 
                            Amounttot[2] = Amounttot[2] + chk-amount 
                            costot[2]    = costot[2] + bf-ar-invl.cost .
                    END.
            END.
                
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            distot[1]    = 0 .

            IF ar-invl.dscr[1] NE "EA" THEN
                RUN sys/ref/convcuom.p (ar-invl.dscr[1], "EA", 0, 0, 0, 0,
                    ar-invl.cost, OUTPUT dvalue).
            ELSE dvalue = ar-invl.cost .
     
            ASSIGN
                distot[1]    = distot[1] + ar-inv.disc-taken 
                Amounttot[1] = Amounttot[1] + amount 
                costot[1]    = (ar-invl.inv-qty * dvalue).

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(LOOKUP(ENTRY(i,cSelectedList),cTextListToSelect),cFieldListToSelect).
        
                CASE cTmpField:
                    WHEN "ar-invl.inv-no"  THEN 
                        cVarValue   = STRING(ar-invl.inv-no,">>>>>>>9" ).
                    WHEN "ar-invl.bol-no"  THEN 
                        cVarValue   = STRING(ar-invl.bol-no,">>>>>>9").
                    WHEN "ar-invl.cust-no" THEN 
                        cVarValue   = STRING(ar-invl.cust-no,"x(8)"). 
                    WHEN "ar-inv.cust-name" THEN 
                        cVarValue  = STRING(ar-inv.cust-name). 
                    WHEN "ar-inv.inv-date" THEN 
                        cVarValue   = IF ar-inv.inv-date NE ? THEN STRING(ar-inv.inv-date) ELSE "". 
                    WHEN "ar-invl.actnum"  THEN 
                        cVarValue   = STRING(ar-invl.actnum). 
                    WHEN "ar-invl.i-no"    THEN 
                        cVarValue   = STRING(ar-invl.i-no).
                    WHEN "ar-invl.i-name" THEN 
                        cVarValue    = STRING(ar-invl.i-name).
                    WHEN "ar-invl.i-dscr" THEN 
                        cVarValue    = STRING(ar-invl.i-dscr) .
                    WHEN "ar-invl.part-no" THEN 
                        cVarValue   = STRING(ar-invl.part-no).
                    WHEN "ar-invl.ord-no" THEN 
                        cVarValue    = STRING(ar-invl.ord-no).
                    WHEN "ar-invl.po-no" THEN 
                        cVarValue     = STRING(ar-invl.po-no).
                    WHEN "ar-invl.est-no" THEN 
                        cVarValue    = STRING(ar-invl.est-no).
                    WHEN "ar-inv.ship-id" THEN 
                        cVarValue    = STRING(ar-inv.ship-id) .
                    WHEN "ar-inv.tax-code" THEN 
                        cVarValue   = STRING(ar-inv.tax-code).
                    WHEN "ar-inv.terms" THEN 
                        cVarValue      = STRING(ar-inv.terms) .
                    WHEN "ar-inv.terms-d" THEN 
                        cVarValue    = STRING(ar-inv.terms-d).
                    WHEN "ar-inv.due-date" THEN 
                        cVarValue   = IF ar-inv.due-date NE ? THEN  STRING(ar-inv.due-date) ELSE "" .
                    WHEN "ar-inv.disc-%" THEN 
                        cVarValue     = STRING( ar-inv.disc-% ).
                    WHEN "ar-inv.disc-taken" THEN 
                        cVarValue = STRING(ar-inv.disc-taken).
                    WHEN "ar-inv.disc-days" THEN 
                        cVarValue  = STRING(ar-inv.disc-days). 
                    WHEN "ar-inv.carrier" THEN 
                        cVarValue    = STRING(ar-inv.carrier). 
                    WHEN "ar-inv.gross" THEN 
                        cVarValue      = STRING(ar-inv.gross). 
                    WHEN "ar-inv.freight" THEN 
                        cVarValue    = STRING(ar-inv.freight). 
                    WHEN "ar-inv.f-bill"  THEN 
                        cVarValue    = IF ar-inv.f-bill THEN STRING(ar-inv.freight) ELSE STRING(0). 
                    WHEN "ar-inv.tax-amt" THEN 
                        cVarValue    = STRING(ar-inv.tax-amt).
                    WHEN "ar-inv.paid" THEN 
                        cVarValue       = STRING(ar-inv.paid).
                    WHEN "ar-inv.due" THEN 
                        cVarValue        = STRING(ar-inv.due) .
                    WHEN "ar-invl.LINE" THEN 
                        cVarValue      = STRING(ar-invl.LINE).
                    WHEN "ar-invl.lot-no" THEN 
                        cVarValue    = STRING(ar-invl.lot-no).
                    WHEN "ar-invl.inv-qty" THEN 
                        cVarValue   = STRING(ar-invl.inv-qty).
                    WHEN "ar-invl.ship-qty" THEN 
                        cVarValue = STRING(ar-invl.ship-qty).
                    WHEN "ar-invl.cons-uom" THEN 
                        cVarValue  = STRING(ar-invl.cons-uom).
                    WHEN "ar-invl.sf-sht" THEN 
                        cVarValue    = STRING(ar-invl.sf-sht) .
                    WHEN "ar-invl.unit-pr" THEN 
                        cVarValue   = STRING(ar-invl.unit-pr).
                    WHEN "ar-invl.pr-qty-uom" THEN 
                        cVarValue = STRING(ar-invl.pr-qty-uom) .
                    WHEN "ar-invl.disc" THEN 
                        cVarValue      = STRING(ar-invl.disc).
                    WHEN "ar-invl.amt-msf" THEN 
                        cVarValue   = STRING(ar-invl.amt-msf).
                    WHEN "ar-invl.cost" THEN 
                        cVarValue      = STRING(ar-invl.cost).
                    WHEN "actdscr" THEN 
                        cVarValue           = STRING( get-actdscr() ).
                    WHEN "amount" THEN 
                        cVarValue            = STRING(amount).
                    WHEN "dscr[1]" THEN 
                        cVarValue           = STRING(ar-invl.dscr[1]). 
                    WHEN "sman[1]" THEN 
                        cVarValue           = STRING(ar-invl.sman[1]). 
                    WHEN "sman[2]" THEN 
                        cVarValue           = STRING(ar-invl.sman[2]). 
                    WHEN "sman[3]" THEN 
                        cVarValue           = STRING(ar-invl.sman[3]). 
                    WHEN "s-pct[1]" THEN 
                        cVarValue          = STRING(ar-invl.s-pct[1]).
                    WHEN "s-pct[2]" THEN 
                        cVarValue          = STRING(ar-invl.s-pct[2]).
                    WHEN "s-pct[3]" THEN 
                        cVarValue          = STRING(ar-invl.s-pct[3]) .
                    WHEN "s-comm[1]" THEN 
                        cVarValue         = STRING(ar-invl.s-comm[1]).
                    WHEN "s-comm[2]" THEN 
                        cVarValue         = STRING(ar-invl.s-comm[2]).
                    WHEN "s-comm[3]" THEN 
                        cVarValue         = STRING(ar-invl.s-comm[3]).
                    WHEN "line-amt" THEN 
                        cVarValue          = STRING(Amounttot[2]).
                    WHEN "line-cst" THEN 
                        cVarValue          = STRING(costot[2]) .
                    WHEN "total-amt" THEN 
                        cVarValue         = STRING(Amounttot[1]).
                    WHEN "total-cst" THEN 
                        cVarValue         = STRING(costot[1]) .
                    WHEN "line-dis" THEN 
                        cVarValue          = STRING(distot[2]).
                    WHEN "total-dis" THEN 
                        cVarValue         = STRING(distot[1]).
                    WHEN "cSoldToId" THEN 
                        cVarValue         = STRING(cSoldToId,"x(8)").
                    WHEN "cSoldToName" THEN 
                        cVarValue         = STRING(cSoldToName,"x(30)").
                    WHEN "ediPrice" THEN 
                        cVarValue         = STRING(ar-invl.ediPrice).
                    WHEN "ediPriceUom" THEN 
                        cVarValue         = STRING(ar-invl.ediPriceUOM).  
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(LOOKUP(ENTRY(i,cSelectedList),cTextListToSelect),cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        
            END.
      
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            v-excel-detail-lines = "".
      
        END.

        IF tb_excel THEN 
        DO:
            OUTPUT STREAM excel CLOSE.
            IF tb_OpenCSV THEN
                OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
        END.

        RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

        SESSION:SET-WAIT-STATE ("").
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine Dialog-Frame 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-actdscr Dialog-Frame 
FUNCTION get-actdscr RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    IF AVAILABLE ar-invl THEN 
    DO:
        FIND FIRST account WHERE account.company = g_company
            AND account.actnum = ar-invl.actnum NO-LOCK NO-ERROR.
        IF AVAILABLE account THEN RETURN account.dscr.
        ELSE RETURN "".
    END.
    ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

