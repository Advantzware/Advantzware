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
  
  Mod: Ticket - 103137 (Format Change for Order No. and Job No).
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/*DEFINE INPUT PARAMETER pcCustFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcCustTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcPartFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcPartTo   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER piOrderFrom AS INT NO-UNDO.
DEFINE INPUT PARAMETER piOrderTo   AS INT NO-UNDO.
DEFINE INPUT PARAMETER pdDateFrom  AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pdDateTo    AS DATE NO-UNDO.
*/

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
 
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


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
/*
DEF TEMP-TABLE tt-report NO-UNDO
    FIELD i-no    AS CHAR
    FIELD ord-no  LIKE oe-rel.ord-no
    FIELD vdate   LIKE oe-rel.rel-date
    FIELD carrier AS CHAR
    FIELD shipid  AS CHAR
    FIELD release# LIKE oe-relh.release#
    FIELD row-id AS ROWID
    FIELD qty AS INT. */


ASSIGN 
    cTextListToSelect  = "Invoice#,Customer#,Customer Name,Invoice Date,Bol#,Order#,Printed,Invoice Total," +
                                                "Status,Shipto,Shipto Name,Contact,Tax Group,Terms Code,Freight Pay Code," +
                                                "Cust PO#,Carrier,FOB Code,Job#,Job2,Est#,Item#," +
                                                "Name,Cust Part#,Qty Order,Item Dscr1,Item Dscr2," +
                                                "Qty Ship,Qty Invoice,UOM,Rep1,Rep Name1,Rep2,Rep Name2,Rep3,Rep Name3," +
                                                "Comm1,Comm2,Comm3,Cost,Case,Discount,Taxable,Ext. Price," +
                                                "CSR,Line Item Tax,OrderHeader ShipTo State,Order Line No,Billing Note,Auto Approval,Tag,Accountant,Invoice Comment,Tax Amount,Freight Amount," +
                                                "Customer Tax Status,Customer Tax Group,Customer Tax Id,Customer Tax Expiration Date,Ship To Tax Group,Ship To Taxable Status," +
                                                "Edi Price,Edi Price UOM,Site ID"
    cFieldListToSelect = "inv-head.inv-no,inv-head.cust-no,inv-head.cust-name,inv-head.inv-date,inv-head.bol-no,ord-no,inv-head.printed,inv-head.t-inv-rev," +
                                        "stat,inv-head.sold-no,inv-head.sold-name,inv-head.contact,inv-head.tax-gr,inv-head.terms,inv-head.frt-pay," +
                                        "po-no,inv-head.carrier,inv-head.fob-code,job-no,job-no2,est-no,i-no," +
                                        "i-name,part-no,qty,part-dscr1,part-dscr2," +
                                        "ship-qty,inv-qty,pr-uom,sman1,sname1,sman2,sname2,sman3,sname3," +
                                        "comm1,comm2,comm3,cost,cas-cnt,disc,tax,t-price," +
                                        "csr,line-sales-tax,ord-head-ship-stat,ord-line,bill-note,Auto,reason,cAccountant,cInvComment,inv-head.t-inv-tax,inv-head.t-inv-freight," +
                                        "custTaxStatus,custTaxCode,custTaxId,taxExpDate,shiptoTaxCode,shiptoStatus," +
                                        "ediPrice,ediPriceUom,siteID"
    cFieldLength       = "15,15,15,20,15,30,15,15," + "15,15,15,20,15,30,15," + "15,15,15,9,3,8,15," +
                       "30,15,10,30,30," + "15,15,5,4,25,4,25,4,25," + "7,7,7,10,10,10,10,10," + "15,15,15,15,15,10,100,12,60,10,10," +
                       "6,8,20,10,14,6," + "10,13,16"
    cFieldType         = "c,c,c,c,c,c,c,c," + "c,c,c,c,c,c,c," + "c,c,c,c,i,c,c," +
                        "c,c,i,c,c," + "i,i,c,c,c,c,c,c,c," + "i,i,i,i,i,i,c,i," + "c,c,c,i,c,c,c,c,c,d,d," +
                        "c,c,c,c,c,c," + "i,c,c"
    .

{sys/inc/ttRptSel.i}

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
end_cust-no begin_item end_item begin_part end_part begin_order end_order ~
begin_date end_date begin_bol-no end_bol-no fi_accountant tb_print-del ~
sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_item ~
end_item begin_part end_part begin_order end_order begin_date end_date ~
begin_bol-no end_bol-no fi_accountant tb_print-del sl_avail sl_selected ~
fi_file tb_OpenCSV tbAutoClose 

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
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
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

DEFINE VARIABLE begin_bol-no  AS INTEGER   FORMAT ">>>>>>>9" INITIAL 0 
    LABEL "From Bill of Lading" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date    AS DATE      FORMAT "99/99/9999" 
    LABEL "From Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_item    AS CHARACTER FORMAT "X(15)" 
    LABEL "From FG Item" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_order   AS INTEGER   FORMAT ">>>>>>>9" INITIAL 0 
    LABEL "From Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_part    AS CHARACTER FORMAT "X(15)" 
    LABEL "From Customer Part#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_bol-no    AS INTEGER   FORMAT ">>>>>>>9" INITIAL 0 
    LABEL "To Bill of Lading" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date      AS DATE      FORMAT "99/99/9999" 
    LABEL "To Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_item      AS CHARACTER FORMAT "X(15)" 
    LABEL "To FG Item" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_order     AS INTEGER   FORMAT ">>>>>>>9" INITIAL 99999999 
    LABEL "To Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_part      AS CHARACTER FORMAT "X(15)" 
    LABEL "To Customer Part#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_accountant AS CHARACTER FORMAT "X(12)" 
    LABEL "Accountant" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file       AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\InvoiceMaintenance.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 6.48.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 9.1.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 2.48.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 5.52 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 5.52 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-del AS LOGICAL   INITIAL NO 
    LABEL "Print Invoice Item Detail?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    begin_cust-no AT ROW 2.48 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 6
    end_cust-no AT ROW 2.48 COL 71 COLON-ALIGNED HELP
    "Enter Ending Customer Number" WIDGET-ID 16
    begin_item AT ROW 3.52 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Customer Item Number" WIDGET-ID 100
    end_item AT ROW 3.52 COL 71 COLON-ALIGNED HELP
    "Enter Beginning Customer Item Number" WIDGET-ID 102
    begin_part AT ROW 4.57 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Customer Part Number" WIDGET-ID 104
    end_part AT ROW 4.57 COL 71 COLON-ALIGNED HELP
    "Enter Beginning Customer Part Number" WIDGET-ID 106
    begin_order AT ROW 5.62 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Order Number" WIDGET-ID 108
    end_order AT ROW 5.62 COL 71 COLON-ALIGNED HELP
    "Enter Beginning Order Number" WIDGET-ID 110
    begin_date AT ROW 6.67 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Date Number" WIDGET-ID 112
    end_date AT ROW 6.67 COL 71 COLON-ALIGNED HELP
    "Enter Beginning Date Number" WIDGET-ID 114
    begin_bol-no AT ROW 7.71 COL 28 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" WIDGET-ID 126
    end_bol-no AT ROW 7.71 COL 71 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" WIDGET-ID 128
    fi_accountant AT ROW 8.76 COL 28 COLON-ALIGNED HELP
    "Enter Customer Accountant Name" WIDGET-ID 6
    tb_print-del AT ROW 9.1 COL 62.6
    sl_avail AT ROW 11.71 COL 4.4 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 11.71 COL 61.4 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 12.19 COL 42 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 13.38 COL 42 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 14.57 COL 42 WIDGET-ID 40
    btn_down AT ROW 15.76 COL 42 WIDGET-ID 42
    fi_file AT ROW 18.81 COL 18.6 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 18.91 COL 85 RIGHT-ALIGNED WIDGET-ID 34
    tbAutoClose AT ROW 20.62 COL 30.4 WIDGET-ID 64
    tb_excel AT ROW 20.67 COL 3.6 WIDGET-ID 32
    btn-ok AT ROW 21.48 COL 30.2 WIDGET-ID 14
    btn-cancel AT ROW 21.48 COL 52.8 WIDGET-ID 12
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 10.76 COL 4 WIDGET-ID 86
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.14 COL 4 WIDGET-ID 36
    RECT-6 AT ROW 11.19 COL 3 WIDGET-ID 30
    RECT-7 AT ROW 1.67 COL 3 WIDGET-ID 38
    RECT-8 AT ROW 18.1 COL 3 WIDGET-ID 84
    SPACE(1.59) SKIP(2.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Invoice Maintenance Excel Export" WIDGET-ID 100.


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
    begin_item:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_order:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_part:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_bol-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_item:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_order:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_part:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    fi_accountant:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

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
ON HELP OF FRAME Dialog-Frame /* Invoice Maintenance Excel Export */
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
            WHEN "fi_accountant" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-users.w (ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO: 
                        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* accountant*/
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Invoice Maintenance Excel Export */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol-no Dialog-Frame
ON LEAVE OF begin_bol-no IN FRAME Dialog-Frame /* From Bill of Lading */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* Beginning Customer# */
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


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item Dialog-Frame
ON LEAVE OF begin_item IN FRAME Dialog-Frame /* From FG Item */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_order Dialog-Frame
ON LEAVE OF begin_order IN FRAME Dialog-Frame /* From Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_part Dialog-Frame
ON LEAVE OF begin_part IN FRAME Dialog-Frame /* From Customer Part# */
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
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
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
        END.
  
        RUN valid-bill-owner(OUTPUT lReturnError) NO-ERROR.
        IF lReturnError THEN RETURN NO-APPLY.
     
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.
     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up Dialog-Frame
ON CHOOSE OF btn_Up IN FRAME Dialog-Frame /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol-no Dialog-Frame
ON LEAVE OF end_bol-no IN FRAME Dialog-Frame /* To Bill of Lading */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no Dialog-Frame
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* Ending Customer# */
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


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item Dialog-Frame
ON LEAVE OF end_item IN FRAME Dialog-Frame /* To FG Item */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_order Dialog-Frame
ON LEAVE OF end_order IN FRAME Dialog-Frame /* To Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_part Dialog-Frame
ON LEAVE OF end_part IN FRAME Dialog-Frame /* To Customer Part# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_accountant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_accountant Dialog-Frame
ON LEAVE OF fi_accountant IN FRAME Dialog-Frame /* Accountant */
    DO:
        DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
        IF LASTKEY <> -1 THEN 
        DO:
            RUN valid-bill-owner(OUTPUT lReturnError) NO-ERROR.
            IF lReturnError THEN RETURN NO-APPLY.
        END.

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

  
    /* for pairs
        DEF VAR cSelectedList AS CHARACTER NO-UNDO.
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


&Scoped-define SELF-NAME tb_print-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-del Dialog-Frame
ON VALUE-CHANGED OF tb_print-del IN FRAME Dialog-Frame /* Print Invoice Item Detail? */
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
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {methods/nowait.i}
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.    

        /* stacey */
        RUN Set-Sort-Data.
    
        fi_file:SCREEN-VALUE = "c:\tmp\InvoiceMaintenance.csv".
    
        /*  ASSIGN RS-ord-stat:SCREEN-VALUE = "1" .*/

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
    DISPLAY begin_cust-no end_cust-no begin_item end_item begin_part end_part 
        begin_order end_order begin_date end_date begin_bol-no end_bol-no 
        fi_accountant tb_print-del sl_avail sl_selected fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-6 RECT-7 RECT-8 begin_cust-no end_cust-no begin_item end_item 
        begin_part end_part begin_order end_order begin_date end_date 
        begin_bol-no end_bol-no fi_accountant tb_print-del sl_avail 
        sl_selected Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV 
        tbAutoClose btn-ok btn-cancel 
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
            ttRptSelected.FieldLength  = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
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

    {sys/form/r-top5DL.f}

    DEFINE BUFFER b-inv-head  FOR inv-head.
    DEFINE BUFFER b-inv-line  FOR inv-line.

    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER b-inv-misc  FOR inv-misc.
    DEFINE BUFFER b-oe-ord    FOR oe-ord.
    DEFINE BUFFER b-oe-ordl   FOR oe-ordl.

    DEFINE VARIABLE v-fcust              LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].

    DEFINE VARIABLE lv-tmp-string        AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE excelheader          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDisplay             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField               AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList        AS CHARACTER NO-UNDO.
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DEFINE VARIABLE v-prod-qty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-bal            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-act-rel-qty    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-wip-qty        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-pct            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-fgitem         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-stat           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-qoh            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-open-closed    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-case-count     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-pallet-count   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-skid-count     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-cnt            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vcarrier         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vshipid          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vreldate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vsman            AS CHARACTER NO-UNDO .
    DEFINE VARIABLE vsname           AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCustStatus      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCustTaxCode     AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cCustTaxId       AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cExpDate         AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cShiptoTaxCode   AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cShiptoTaxStatus AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lv-ord-no        LIKE inv-line.ord-no.
    DEFINE VARIABLE cSiteId AS CHARACTER NO-UNDO .


    DEFINE BUFFER b-shipto FOR shipto.

    ASSIGN
        v-fcust[1]    = begin_cust-no
        v-fcust[2]    = end_cust-no
        v-excelheader = "".

   
    SESSION:SET-WAIT-STATE ("general").

    /* {sys/inc/print1.i}                         */
    /* {sys/inc/outprint.i value(lines-per-page)} */



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
    END.
   
    IF tb_excel THEN
        PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
 
    ASSIGN 
        v-stat    = ""
        lv-ord-no = 0 .
   
    IF tb_print-del  THEN 
    DO:
        ASSIGN 
            lv-ord-no = 0 .
        FOR EACH inv-head NO-LOCK WHERE 
            inv-head.company EQ cocode AND 
            inv-head.multi-invoice = NO AND 
            (inv-head.cust-no >= begin_cust-no AND inv-head.cust-no <= end_cust-no) AND
            (inv-head.bol-no >= begin_bol-no AND inv-head.bol-no <= end_bol-no) AND
            (inv-head.inv-date >= date(begin_date) AND inv-head.inv-date <= date(end_date))/*AND
           inv-head.stat NE "W"*/ ,
            EACH inv-line OF inv-head WHERE (inv-line.ord-no >= int(begin_order) AND inv-line.ord-no <= int(end_order)) AND
            (inv-line.i-no >= begin_item AND inv-line.i-no <= END_item) AND
            (inv-line.part-no >= begin_part AND inv-line.part-no <= end_part),
            FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ inv-head.cust-no 
            AND (cust.accountant BEGINS fi_accountant OR fi_accountant EQ "") BY inv-head.inv-no:
         
            ASSIGN
                cCustStatus  = cust.SORT
                cCustTaxCode = cust.tax-gr
                cCustTaxId   = cust.tax-id
                cExpDate     = IF cust.date-field[2] NE ? THEN STRING(cust.date-field[2]) ELSE "".
            .
            FIND FIRST b-shipto NO-LOCK
                WHERE b-shipto.company   EQ cocode
                AND b-shipto.ship-id     EQ inv-head.cust-no  NO-ERROR.
            IF AVAILABLE b-shipto THEN 
            DO:
                ASSIGN
                    cShiptoTaxCode   = b-shipto.tax-code
                    cShiptoTaxStatus = STRING(b-shipto.tax-mandatory)
                    cSiteId          = STRING(b-shipto.siteID) .
            END.  
            IF inv-head.stat EQ "H" THEN
                v-stat = "On Hold".
            ELSE IF inv-head.stat EQ "W" THEN
                    v-stat = "Wait/App".
                ELSE v-stat = "Released".

    
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            IF AVAILABLE inv-head THEN
                BUFFER b-inv-head:FIND-BY-ROWID(ROWID(inv-head), NO-LOCK) .
            IF AVAILABLE inv-line THEN
                BUFFER b-inv-line:FIND-BY-ROWID(ROWID(inv-line), NO-LOCK) .

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                IF INDEX(cTmpField,".") > 0 THEN 
                DO:
                    cFieldName = cTmpField .
                    cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
            
                    IF cFieldName BEGINS "inv-head" THEN hField = BUFFER b-inv-head:BUFFER-FIELD(cTmpField) .
                    ELSE IF cFieldName BEGINS "inv-line" THEN hField = BUFFER b-inv-line:BUFFER-FIELD(cTmpField).
                    IF hField <> ? THEN 
                    DO:                      
                        cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                        cDisplay = cDisplay + cTmpField + 
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

                        cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".    
                    END.
                    ELSE 
                    DO:
                        cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                        cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                        cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                    END.
                END.
                ELSE 
                DO:         
                    CASE cTmpField:   
                        WHEN "ord-no" THEN 
                            cVarValue = STRING(inv-line.ord-no) .
                        WHEN "stat" THEN 
                            cVarValue = STRING(v-stat). 
                        WHEN "sman1" THEN 
                            cVarValue =  STRING(inv-line.sman[1]). 
                        WHEN "sname1" THEN 
                            cVarValue = STRING(inv-line.sname[1]). 
                        WHEN "comm1" THEN 
                            cVarValue =  STRING(inv-line.comm-amt[1]).
                        WHEN "sman2" THEN 
                            cVarValue = STRING(inv-line.sman[2]). 
                        WHEN "sname2" THEN 
                            cVarValue = STRING(inv-line.sname[2]). 
                        WHEN "comm2" THEN 
                            cVarValue = STRING(inv-line.comm-amt[2]).
                        WHEN "sman3" THEN 
                            cVarValue = STRING(inv-line.sman[3]). 
                        WHEN "sname3" THEN 
                            cVarValue = STRING(inv-line.sname[3]). 
                        WHEN "comm3" THEN 
                            cVarValue = STRING(inv-line.comm-amt[3]).
                        /*  WHEN "sold-no" THEN cVarValue = string(inv-head.sold-no). 
                          WHEN "sold-name" THEN cVarValue = string(inv-head.sold-name). 
                          WHEN "contact" THEN cVarValue = string(inv-head.contact). 
                          WHEN "tax-gr" THEN cVarValue = string(inv-head.tax-gr). 
                          WHEN "terms" THEN cVarValue = string(inv-head.terms). 
                          WHEN "frt-pay" THEN cVarValue = string(inv-head.frt-pay). */ 
                        WHEN "po-no" THEN 
                            DO: 
                                IF AVAILABLE inv-head THEN 
                                DO:
                                    FIND FIRST bf-inv-line OF inv-head NO-LOCK
                                        WHERE bf-inv-line.ord-no NE 0
                                        NO-ERROR.
                                    IF NOT AVAILABLE bf-inv-line THEN
                                        FIND FIRST b-inv-misc OF inv-head NO-LOCK
                                            WHERE b-inv-misc.ord-no NE 0
                                            NO-ERROR.
                                    lv-ord-no = IF AVAILABLE bf-inv-line THEN bf-inv-line.ord-no ELSE
                                        IF AVAILABLE b-inv-misc THEN b-inv-misc.ord-no ELSE 0.
                                    IF lv-ord-no NE 0 THEN 
                                    DO:
                                        FIND FIRST b-oe-ord WHERE b-oe-ord.company EQ inv-head.company
                                            AND b-oe-ord.ord-no EQ lv-ord-no NO-LOCK NO-ERROR.
                                        IF AVAILABLE b-oe-ord THEN
                                            cVarValue = b-oe-ord.po-no.
                                        ELSE cVarValue = "".
                                    END.
                                    ELSE cVarValue = "".
                                END.
                                ELSE cVarValue = "".
                            END.
                        /*  WHEN "carrier" THEN cVarValue = STRING(inv-head.carrier). 
                          WHEN "fob-code" THEN cVarValue = STRING(inv-head.fob-code).  */
                        WHEN "job-no" THEN 
                            cVarValue = STRING(inv-line.job-no). 
                        WHEN "job-no2" THEN 
                            cVarValue = STRING(inv-line.job-no2). 
                        WHEN "est-no" THEN 
                            cVarValue = STRING(inv-line.est-no). 
                        WHEN "i-no" THEN 
                            cVarValue = STRING(inv-line.i-no). 
                        WHEN "i-name" THEN 
                            cVarValue = STRING(inv-line.i-name). 
                        WHEN "qty"               THEN 
                            cVarValue = STRING(inv-line.qty). 
                        WHEN "part-dscr1"     THEN 
                            cVarValue = STRING(inv-line.part-dscr1). 
                        WHEN "part-dscr2" THEN 
                            cVarValue = STRING(inv-line.part-dscr2). 
                        WHEN "ship-qty"   THEN 
                            cVarValue = STRING(inv-line.ship-qty). 
                        WHEN "inv-qty"        THEN 
                            cVarValue = STRING(inv-line.inv-qty). 
                        WHEN "pr-uom"         THEN 
                            cVarValue = STRING(inv-line.pr-uom). 
                        WHEN "cost"           THEN 
                            cVarValue = STRING(inv-line.cost). 
                        WHEN "cas-cnt"        THEN 
                            cVarValue = STRING(inv-line.cas-cnt). 
                        WHEN "disc"           THEN 
                            cVarValue = STRING(inv-line.disc). 
                        WHEN "tax"            THEN 
                            cVarValue = STRING(inv-line.tax). 
                        WHEN "t-price"            THEN 
                            cVarValue = STRING(inv-line.t-price). 
                        WHEN "part-no"            THEN 
                            cVarValue = STRING(inv-line.part-no). 
                        WHEN "csr"                THEN 
                            DO:
                                FIND FIRST b-oe-ord NO-LOCK
                                    WHERE b-oe-ord.company EQ inv-head.company
                                    AND b-oe-ord.ord-no EQ inv-line.ord-no NO-ERROR.
                                IF AVAILABLE b-oe-ord THEN
                                    cVarValue = b-oe-ord.csruser_ID.
                                ELSE cVarValue = "".
                            END.
                        WHEN "line-sales-tax"     THEN 
                            DO: 
                                FIND FIRST b-oe-ord NO-LOCK
                                    WHERE b-oe-ord.company EQ inv-head.company
                                    AND b-oe-ord.ord-no EQ inv-line.ord-no
                                    NO-ERROR.
                                IF AVAILABLE b-oe-ord THEN
                                    cVarValue = b-oe-ord.tax-gr .
                                ELSE cVarValue = "".
                            END.
                        WHEN "misc-sales-tax"     THEN 
                            DO:
                                FIND FIRST b-inv-misc OF inv-head NO-LOCK
                                    WHERE b-inv-misc.ord-no NE 0
                                    NO-ERROR.
                                IF AVAILABLE b-inv-misc THEN
                                    cVarValue = b-inv-misc.spare-char-1 .
                                ELSE cVarValue = "".
                            END.
                        WHEN "ord-head-ship-stat" THEN 
                            DO:
                                FIND FIRST b-oe-ord NO-LOCK
                                    WHERE b-oe-ord.company EQ inv-head.company
                                    AND b-oe-ord.ord-no EQ inv-line.ord-no
                                    NO-ERROR.
                                IF AVAILABLE b-oe-ord THEN
                                    FIND FIRST shipto NO-LOCK
                                        WHERE shipto.company EQ inv-head.company 
                                        AND shipto.ship-id EQ b-oe-ord.ship-id NO-ERROR .
                                IF AVAILABLE b-oe-ord AND AVAILABLE shipto THEN
                                    cVarValue = shipto.ship-state .
                                ELSE cVarValue = "".
                            END.
                        WHEN "ord-line"           THEN 
                            DO: 
                                FIND FIRST b-oe-ordl NO-LOCK
                                    WHERE b-oe-ordl.company EQ inv-head.company
                                    AND b-oe-ordl.ord-no EQ inv-line.ord-no
                                    AND b-oe-ordl.i-no  EQ inv-line.i-no 
                                    NO-ERROR.
                                IF AVAILABLE b-oe-ordl THEN
                                    cVarValue = STRING(b-oe-ordl.LINE) .
                                ELSE cVarValue = "".
                            END.
                        WHEN "bill-note"          THEN 
                            DO:
                                cVarValue =  inv-head.bill-i[1] + " "  + inv-head.bill-i[2] + " " + inv-head.bill-i[3] + "  " + inv-head.bill-i[4] .
                            END.
                        WHEN "auto" THEN 
                            cVarValue = STRING(inv-head.autoApproved) .
                        WHEN "reason" THEN RUN GetTagsList(
                            INPUT  inv-head.rec_key, 
                            INPUT  "inv-head", 
                            INPUT  "",   
                            INPUT ";",
                            OUTPUT cVarValue 
                            ).  
                        WHEN "cAccountant"            THEN 
                            cVarValue = STRING(cust.accountant).
                        WHEN "cInvComment"            THEN 
                            cVarValue = STRING(inv-head.spare-char-5).
                        WHEN "custTaxStatus"          THEN 
                            cVarValue = STRING(cCustStatus).
                        WHEN "custTaxCode"            THEN 
                            cVarValue = STRING(cCustTaxCode).
                        WHEN "custTaxId"               THEN 
                            cVarValue = STRING(cCustTaxId).
                        WHEN "taxExpDate"             THEN 
                            cVarValue = STRING(cExpDate).
                        WHEN "shiptoTaxCode"          THEN 
                            cVarValue = STRING(cShiptoTaxCode).
                        WHEN "shiptoStatus"           THEN 
                            cVarValue = STRING(cShiptoTaxStatus).
                        WHEN "ediPrice"           THEN 
                            cVarValue = STRING(inv-line.ediPrice).
                        WHEN "ediPriceUom"           THEN 
                            cVarValue = STRING(inv-line.ediPriceUOM).
                        WHEN "siteID"           THEN 
                            cVarValue = STRING(cSiteId).    
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
            END.
      
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.
   
        END. /* for each */
    END.
    ELSE 
    DO:
        ASSIGN 
            lv-ord-no = 0 .
        FOR EACH inv-head NO-LOCK WHERE 
            inv-head.company EQ cocode AND 
            inv-head.multi-invoice = NO AND 
            (inv-head.cust-no >= begin_cust-no AND inv-head.cust-no <= end_cust-no) AND
            (inv-head.bol-no >= begin_bol-no AND inv-head.bol-no <= end_bol-no) AND
            (inv-head.inv-date >= date(begin_date) AND inv-head.inv-date <= date(end_date)), /*AND
           inv-head.stat NE "W"*/ 
            FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ inv-head.cust-no 
            AND (cust.accountant BEGINS fi_accountant OR fi_accountant EQ "") BY inv-head.inv-no:
          
            ASSIGN
                cCustStatus  = cust.SORT
                cCustTaxCode = cust.tax-gr
                cCustTaxId   = cust.tax-id
                cExpDate     = IF cust.date-field[2] NE ? THEN STRING(cust.date-field[2]) ELSE "".
            .
            FIND FIRST b-shipto NO-LOCK
                WHERE b-shipto.company   EQ cocode
                AND b-shipto.ship-id     EQ inv-head.cust-no  NO-ERROR.
            IF AVAILABLE b-shipto THEN 
            DO:
                ASSIGN
                    cShiptoTaxCode   = b-shipto.tax-code
                    cShiptoTaxStatus = STRING(b-shipto.tax-mandatory)
                    cSiteId          = STRING(b-shipto.siteID).
            END.  
   
            IF inv-head.stat EQ "H" THEN
                v-stat = "On Hold".
            ELSE IF inv-head.stat EQ "W" THEN
                    v-stat = "Wait/App".
                ELSE v-stat = "Released".
  
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            IF AVAILABLE inv-head THEN
                BUFFER b-inv-head:FIND-BY-ROWID(ROWID(inv-head), NO-LOCK) .
     
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                IF INDEX(cTmpField,".") > 0 THEN 
                DO:
                    cFieldName = cTmpField .
                    cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
            
                    IF cFieldName BEGINS "inv-head" THEN hField = BUFFER b-inv-head:BUFFER-FIELD(cTmpField) .
       
                    IF hField <> ? THEN 
                    DO:                      
                        cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                        cDisplay = cDisplay + cTmpField + 
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)).

                        cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".    
                    END.
                    ELSE 
                    DO:
                        cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                        cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                        cExcelDisplay = cExcelDisplay + quoter(" ")  /*GetFieldValue(hField))*/ + ",".
                    END.
                END.
                ELSE 
                DO:         
                    CASE cTmpField:
                        WHEN "ord-no" THEN 
                            DO: 
                                FIND FIRST inv-line OF inv-head NO-LOCK
                                    WHERE inv-line.ord-no NE 0
                                    NO-ERROR.
                                IF NOT AVAILABLE inv-line THEN
                                    FIND FIRST inv-misc OF inv-head NO-LOCK
                                        WHERE inv-misc.ord-no NE 0
                                        NO-ERROR.
                                cVarValue = IF AVAILABLE inv-line THEN STRING(inv-line.ord-no) ELSE IF AVAILABLE inv-misc THEN STRING(inv-misc.ord-no) ELSE "".
                            END.
                        WHEN "stat" THEN 
                            cVarValue = STRING(v-stat). 
                        WHEN "sman1" THEN 
                            cVarValue =  STRING(inv-head.sman[1]).     
                        WHEN "sname1" THEN 
                            DO:
                                FIND FIRST sman WHERE sman.company = cocode
                                    AND sman.sman = inv-head.sman[1] NO-LOCK NO-ERROR.
                                IF AVAILABLE sman THEN
                                    cVarValue = STRING(sman.sname).    
                                ELSE cVarValue = "" .
                            END.
                        WHEN "comm1" THEN 
                            cVarValue =  STRING(inv-head.s-comm[1]). 
                        WHEN "sman2" THEN 
                            cVarValue  = STRING(inv-head.sman[2]).   
                        WHEN "sname2" THEN 
                            DO:
                                FIND FIRST sman WHERE sman.company = cocode
                                    AND sman.sman = inv-head.sman[2] NO-LOCK NO-ERROR.
                                IF AVAILABLE sman THEN
                                    cVarValue = STRING(sman.sname).    
                                ELSE cVarValue = "" .
                            END.
                        WHEN "comm2" THEN 
                            cVarValue  = STRING(inv-head.s-comm[2]). 
                        WHEN "sman3" THEN 
                            cVarValue  = STRING(inv-head.sman[3]).   
                        WHEN "sname3" THEN 
                            DO: 
                                FIND FIRST sman WHERE sman.company = cocode
                                    AND sman.sman = inv-head.sman[3] NO-LOCK NO-ERROR.
                                IF AVAILABLE sman THEN
                                    cVarValue = STRING(sman.sname).    
                                ELSE cVarValue = "" .
                            END.
                        WHEN "comm3" THEN 
                            cVarValue  = STRING(inv-head.s-comm[3]). 
                        /*    WHEN "sold-no" THEN cVarValue = "". 
                            WHEN "sold-name" THEN cVarValue = "". 
                            WHEN "contact" THEN cVarValue = "". 
                            WHEN "tax-gr" THEN cVarValue = "". 
                            WHEN "terms" THEN cVarValue = "". 
                            WHEN "frt-pay" THEN cVarValue = "".  */
                        WHEN "po-no" THEN 
                            DO: 
                                IF AVAILABLE inv-head THEN 
                                DO:
                                    FIND FIRST bf-inv-line OF inv-head NO-LOCK
                                        WHERE bf-inv-line.ord-no NE 0
                                        NO-ERROR.
                                    IF NOT AVAILABLE bf-inv-line THEN
                                        FIND FIRST b-inv-misc OF inv-head NO-LOCK
                                            WHERE b-inv-misc.ord-no NE 0
                                            NO-ERROR.
                                    lv-ord-no = IF AVAILABLE bf-inv-line THEN bf-inv-line.ord-no ELSE
                                        IF AVAILABLE b-inv-misc THEN b-inv-misc.ord-no ELSE 0.
                                    IF lv-ord-no NE 0 THEN 
                                    DO:
                                        FIND FIRST b-oe-ord WHERE b-oe-ord.company EQ inv-head.company
                                            AND b-oe-ord.ord-no EQ lv-ord-no NO-LOCK NO-ERROR.
                                        IF AVAILABLE b-oe-ord THEN
                                            cVarValue = b-oe-ord.po-no.
                                        ELSE cVarValue = "".
                                    END.
                                    ELSE cVarValue = "".
                                END.
                                ELSE cVarValue = "".
                            END.
                        /*      WHEN "carrier" THEN cVarValue = "". 
                              WHEN "fob-code" THEN cVarValue = "".  */
                        WHEN "job-no" THEN 
                            cVarValue = "".
                        WHEN "job-no2" THEN 
                            cVarValue = "". 
                        WHEN "est-no" THEN 
                            cVarValue = "". 
                        WHEN "i-no" THEN 
                            cVarValue = "". 
                        WHEN "i-name" THEN 
                            cVarValue = "". 
                        WHEN "part-no" THEN 
                            cVarValue = "".
                        WHEN "qty" THEN 
                            cVarValue = "". 
                        WHEN "part-dscr1" THEN 
                            cVarValue = "". 
                        WHEN "part-dscr2" THEN 
                            cVarValue = "". 
                        WHEN "ship-qty" THEN 
                            cVarValue = "". 
                        WHEN "inv-qty" THEN 
                            cVarValue = "".
                        WHEN "pr-uom" THEN 
                            cVarValue = "". 
                        WHEN "cost" THEN 
                            cVarValue = "". 
                        WHEN "cas-cnt" THEN 
                            cVarValue = "". 
                        WHEN "disc" THEN 
                            cVarValue = "". 
                        WHEN "tax" THEN 
                            cVarValue = "".
                        WHEN "t-price" THEN 
                            cVarValue = "".
                        WHEN "auto" THEN 
                            cVarValue = STRING(inv-head.autoApproved) .
                        WHEN "reason" THEN RUN GetTagsList(
                            INPUT  inv-head.rec_key, 
                            INPUT  "inv-head", 
                            INPUT  "",   
                            INPUT ";",
                            OUTPUT cVarValue 
                            ).  
                        WHEN "cAccountant"            THEN 
                            cVarValue = STRING(cust.accountant).
                        WHEN "cInvComment"            THEN 
                            cVarValue = STRING(inv-head.spare-char-5).
                        WHEN "custTaxStatus"          THEN 
                            cVarValue = STRING(cCustStatus).
                        WHEN "custTaxCode"            THEN 
                            cVarValue = STRING(cCustTaxCode).
                        WHEN "custTaxId"               THEN 
                            cVarValue = STRING(cCustTaxId).
                        WHEN "taxExpDate"             THEN 
                            cVarValue = STRING(cExpDate).
                        WHEN "shiptoTaxCode"          THEN 
                            cVarValue = STRING(cShiptoTaxCode).
                        WHEN "shiptoStatus"           THEN 
                            cVarValue = STRING(cShiptoTaxStatus).
                        WHEN "ediPrice"           THEN 
                            cVarValue = STRING(inv-line.ediPrice).
                        WHEN "ediPriceUom"           THEN 
                            cVarValue = STRING(inv-line.ediPriceUOM).
                        WHEN "siteID"           THEN 
                            cVarValue = STRING(cSiteId).    
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
            END.
      
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.
        END. /* for each */
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.

        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
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

    /*   /* If a customer number was entered, find first and last matching customers. */
       IF pcCustFrom <> "" THEN ASSIGN begin_cust-no:SCREEN-VALUE = pcCustFrom.
       IF pcCustTo <> "" THEN ASSIGN end_cust-no:SCREEN-VALUE   = pcCustTo.
 
       IF pcItemFrom <> "" THEN ASSIGN begin_item:SCREEN-VALUE = pcItemFrom.
       IF pcItemTo <> "" THEN ASSIGN end_item:SCREEN-VALUE   = pcItemTo.
 
       IF pcPartFrom <> "" THEN ASSIGN begin_part:SCREEN-VALUE = pcPartFrom.
       IF pcPartTo <> "" THEN ASSIGN end_part:SCREEN-VALUE   = pcPartTo.
 
       IF piOrderFrom <> 0 THEN ASSIGN begin_order:SCREEN-VALUE = string(piOrderFrom).
       IF piOrderTo <> 0 THEN ASSIGN end_order:SCREEN-VALUE   = string(piOrderTo).
 
       IF pdDateFrom <> ? THEN ASSIGN begin_date:SCREEN-VALUE = string(pdDateFrom).
       IF pdDateTo <> ? THEN ASSIGN end_date:SCREEN-VALUE   = string(pdDateTo).*/
    END.


    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bill-owner Dialog-Frame 
PROCEDURE valid-bill-owner :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    {methods/lValidateError.i YES}

    IF fi_accountant:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN 
    DO:
        IF NOT CAN-FIND(FIRST users WHERE users.USER_ID EQ fi_accountant:SCREEN-VALUE IN FRAME {&FRAME-NAME})
            THEN 
        DO:
            MESSAGE "Invalid customer Accountant. Try help." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_accountant.
            oplReturnError = YES.
        END.
    END.

    {methods/lValidateError.i NO}
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
