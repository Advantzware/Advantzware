&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-sqrshipN.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}    
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE is-xprint-form AS LOG NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS cha NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS cha     NO-UNDO.

/* 10 + 6 + 8 = 24 columns */
ASSIGN 
    cTextListToSelect  = "Cust#,Customer Name,PO#,Order#,Rel#,Item,Description," +
                           "Job#,Shipped Qty,Order Qty,Customer Part#,Del Zone,Terr," +
                           "Trailer #,Total Sq Ft,Bol#,Ship To,Ship To Name,Bol Date" 
    cFieldListToSelect = "cust-no,cust-name,po-num,ord-no,rel-no,i-no,i-name," +
                                "job,shp-qty,ord-qty,part-no,v-del-zone,v-terr," +
                                "v-trailer,v-totl-sq,v-bol-no,ship-id,ship-name,bol-date"
    cFieldLength       = "8,30,15,6,6,15,30," + "9,11,11,15,8,4," + "21,12,7,8,30,10"
    cFieldType         = "c,c,c,i,i,c,c," + "c,i,i,c,c,c," + "c,i,i,c,c,c"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Bol#,Bol Date,Job#,Cust#,Customer Name,Trailer #,PO#,Order#,Rel#,Shipped Qty,Order Qty,Total Sq Ft,Item,Description" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 from_date to_date ~
begin_trailer end_trailer begin_ord-no end_ord-no begin_bol-no end_bol-no ~
Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
lines-per-page lv-ornt lv-font-no td-show-parm tb_runExcel tb_excel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS from_date to_date begin_trailer ~
end_trailer begin_ord-no end_ord-no begin_bol-no end_bol-no sl_avail ~
sl_selected rd-dest lines-per-page lv-ornt lv-font-no lv-font-name ~
td-show-parm tb_runExcel tb_excel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
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

DEFINE VARIABLE begin_bol-no   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "Beginning BOL#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_ord-no   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_trailer  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Trailer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_bol-no     AS INTEGER   FORMAT ">>>>>>>9" INITIAL 0 
    LABEL "Ending BOL#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_ord-no     AS INTEGER   FORMAT "99999999":U INITIAL 99999999 
    LABEL "Ending Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_trailer    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Trailer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-sqrshipN.csv" 
    LABEL "Excel File Name" 
    VIEW-AS FILL-IN 
    SIZE 43 BY 1
    FGCOLOR 9 .

DEFINE VARIABLE from_date      AS DATE      FORMAT "99/99/9999":U 
    LABEL "Beginning BOL Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "10" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE to_date        AS DATE      FORMAT "99/99/9999":U 
    LABEL "Ending BOL Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To File", 3,
    "To Fax", 4,
    "To Email", 5,
    "To Port Directly", 6
    SIZE 21 BY 6.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 7.14.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 7.86.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_runExcel  AS LOGICAL   INITIAL NO 
    LABEL "Auto Run Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL YES 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    from_date AT ROW 2.81 COL 24.2 COLON-ALIGNED
    to_date AT ROW 2.81 COL 67.2 COLON-ALIGNED
    begin_trailer AT ROW 4 COL 24.2 COLON-ALIGNED HELP
    "Enter Beginning Trailer Number" WIDGET-ID 2
    end_trailer AT ROW 4 COL 67.2 COLON-ALIGNED HELP
    "Enter Ending Trailer Number" WIDGET-ID 4
    begin_ord-no AT ROW 5.19 COL 24.2 COLON-ALIGNED HELP
    "Enter Beginning Order Number" WIDGET-ID 6
    end_ord-no AT ROW 5.19 COL 67.2 COLON-ALIGNED HELP
    "Enter Ending Order Number" WIDGET-ID 8
    begin_bol-no AT ROW 6.43 COL 24.2 COLON-ALIGNED HELP
    "Enter Beginning Bol#" WIDGET-ID 46
    end_bol-no AT ROW 6.43 COL 67.2 COLON-ALIGNED HELP
    "Enter Ending Bol#" WIDGET-ID 48
    Btn_Def AT ROW 9.71 COL 40 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_avail AT ROW 9.76 COL 6 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 9.76 COL 60 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 10.71 COL 40 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 11.71 COL 40 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 12.76 COL 40 WIDGET-ID 40
    btn_down AT ROW 13.81 COL 40 WIDGET-ID 42
    rd-dest AT ROW 15.76 COL 5 NO-LABELS
    lines-per-page AT ROW 15.86 COL 84 COLON-ALIGNED
    lv-ornt AT ROW 15.91 COL 31 NO-LABELS
    lv-font-no AT ROW 17.19 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 18.14 COL 28 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 19.33 COL 31
    tb_runExcel AT ROW 19.95 COL 93 RIGHT-ALIGNED
    tb_excel AT ROW 20 COL 71 RIGHT-ALIGNED
    fi_file AT ROW 20.71 COL 49 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 22.38 COL 24
    btn-cancel AT ROW 22.38 COL 59
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 9.05 COL 2 WIDGET-ID 38
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1 COL 3
    BGCOLOR 2 
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 15.1 COL 4
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 9.05 COL 59.4 WIDGET-ID 44
    RECT-6 AT ROW 15 COL 1
    RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 94.4 BY 23.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Sq Ft Totals Shipped "
        HEIGHT             = 23.52
        WIDTH              = 95.6
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 204.8
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 204.8
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
    begin_bol-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_trailer:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_bol-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_trailer:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    from_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    to_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Posting History Report */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Posting History Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol-no C-Win
ON LEAVE OF begin_bol-no IN FRAME FRAME-A /* Beginning BOL# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_trailer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_trailer C-Win
ON HELP OF begin_trailer IN FRAME FRAME-A /* Beginning Trailer# */
    DO:
       .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME begin_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol-no C-Win
ON HELP OF begin_bol-no IN FRAME FRAME-A
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.
   
   RUN windows/l-bolh.w (cocode,SELF:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
   IF char-val <> "" THEN
      ASSIGN begin_bol-no:SCREEN-VALUE = ENTRY(1,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME end_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol-no C-Win
ON HELP OF end_bol-no IN FRAME FRAME-A
DO:
    DEF VAR char-val AS cha NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.
   
   RUN windows/l-bolh.w (cocode,SELF:SCREEN-VALUE,OUTPUT char-val,OUTPUT rec-val).
   IF char-val <> "" THEN
      ASSIGN end_bol-no:SCREEN-VALUE = ENTRY(1,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_trailer C-Win
ON LEAVE OF begin_trailer IN FRAME FRAME-A /* Beginning Trailer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        RUN GetSelectionList.
 
        RUN run-report.

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                /*run output-to-fax.*/
                /*{custom/asifax.i &type= "Customer"
                                 &begin_cust= string(begin_bol-no)
                                 &END_cust= string(end_bol-no)
                                 &fax-subject=c-win:title
                                 &fax-body=c-win:title
                                 &fax-file=list-name } */
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= string(begin_bol-no)
                             &END_cust= STRING(End_bol-no)
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr7.i &TYPE = "Customer"              
                                  &begin_cust= string(begin_bol-no)
                                  &END_cust= STRING(end_bol-no)
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.
                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
        SESSION:SET-WAIT-STATE("").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME FRAME-A /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol-no C-Win
ON LEAVE OF end_bol-no IN FRAME FRAME-A /* Ending BOL# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_trailer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_trailer C-Win
ON HELP OF end_trailer IN FRAME FRAME-A /* Ending Trailer# */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_trailer C-Win
ON LEAVE OF end_trailer IN FRAME FRAME-A /* Ending Trailer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Excel File Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME from_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_date C-Win
ON LEAVE OF from_date IN FRAME FRAME-A /* Beginning BOL Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
                LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
    DO:
        ASSIGN lv-font-no.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
    DO:
        ASSIGN lv-ornt.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
    DO:
        IF SELF:SCREEN-VALUE BEGINS "L" THEN 
            ASSIGN lv-font-no     = "12"
                lines-per-page = 55
                lv-font-name   = "Courier New Size=8 (15CPI)".

        ELSE
            ASSIGN lv-font-no     = "10"
                lines-per-page = 99
                lv-font-name   = "Courier NEW SIZE=6 (20 CPI)".

        DISPLAY lv-font-no lines-per-page lv-font-name WITH FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_date C-Win
ON LEAVE OF to_date IN FRAME FRAME-A /* Ending BOL Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    ASSIGN
        from_date = TODAY
        to_date   = TODAY.

    RUN DisplaySelectionList.
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:      
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO from_date.
    END.
  
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:

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
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.

    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
    
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
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
    END.
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

    {sys/ref/SelColCorrect.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY from_date to_date begin_trailer end_trailer begin_ord-no end_ord-no 
        begin_bol-no end_bol-no sl_avail sl_selected rd-dest lines-per-page 
        lv-ornt lv-font-no lv-font-name td-show-parm tb_runExcel tb_excel 
        fi_file 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 from_date to_date begin_trailer end_trailer begin_ord-no 
        end_ord-no begin_bol-no end_bol-no Btn_Def sl_avail sl_selected 
        Btn_Add Btn_Remove btn_Up btn_down rd-dest lines-per-page lv-ornt 
        lv-font-no td-show-parm tb_runExcel tb_excel fi_file btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpList AS cha NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    iColumnLength = 0.
    DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     
        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
        .         
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-values C-Win 
PROCEDURE init-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  ASSIGN
   tb_rec   = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "R")
   tb_ship  = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "S")
   tb_tran  = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "T")
   tb_adj   = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "A")
   tb_ret   = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "E")
   tb_count = CAN-FIND(FIRST fg-rcpth
                       WHERE fg-rcpth.company   EQ gcompany
                         AND fg-rcpth.rita-code EQ "C").*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {custom/out2file.i} 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN custom/d-print.w (list-name).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE excelheader      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
    DEFINE VARIABLE v-del-zone     AS cha       FORM "x(8)" NO-UNDO.
    DEFINE VARIABLE v-terr         AS cha       FORM "x(4)" NO-UNDO.
    DEFINE VARIABLE tboldate       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fbol           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE tbol           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ftrailer       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ttrailer       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ford           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE tord           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-trailer      AS CHARACTER FORMAT "x(8)" NO-UNDO.
    DEFINE VARIABLE v-bol-no       AS INTEGER   FORMAT ">>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-totl-sq      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dTotalSqft     AS DECIMAL  FORMAT "->>,>>>,>>>.9<<<<"  NO-UNDO.
    DEFINE VARIABLE cCustName         AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE v-i-name       LIKE oe-ordl.i-name NO-UNDO.

    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE cShipName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iShipQty       AS INTEGER NO-UNDO.
    DEFINE VARIABLE iOrdQty        AS INTEGER NO-UNDO.
    DEFINE VARIABLE dTtlSqFt       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
  
    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
        
        fbol     = begin_bol-no
        tbol     = end_bol-no
        ftrailer = begin_trailer
        ttrailer = end_trailer
        ford     = begin_ord-no 
        tord     = end_ord-no .
  
    /*{sys/inc/ctrtext.i str-tit3 132}.*/
   
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ",".        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
           ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ",".  
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    DISPLAY "" WITH FRAME r-top.

    ASSIGN 
        v-i-name = "".

    FOR EACH oe-boll 
        WHERE oe-boll.company   EQ cocode
        AND oe-boll.ord-no  GE begin_ord-no
        AND oe-boll.ord-no  LE end_ord-no
        AND oe-boll.bol-date      GE from_date
        AND oe-boll.bol-date       LE to_date
        NO-LOCK,
        FIRST oe-bolh 
        WHERE oe-bolh.company EQ oe-boll.company
        AND oe-bolh.b-no EQ oe-boll.b-no
        AND oe-bolh.trailer GE begin_trailer
        AND oe-bolh.trailer LE end_trailer
        AND oe-bolh.bol-no GE begin_bol-no
        AND oe-bolh.bol-no LE end_bol-no
        NO-LOCK,
        FIRST itemfg WHERE
        itemfg.company EQ cocode AND
        itemfg.i-no    EQ oe-boll.i-no 
        NO-LOCK
        BREAK BY oe-bolh.bol-no
              BY oe-boll.i-no:

        STATUS DEFAULT "Processing Order#: " +
            TRIM(STRING(oe-boll.ord-no,">>>>>>>>")) .

        IF FIRST-OF (oe-boll.i-no) THEN
            ASSIGN iShipQty = 0
                   iOrdQty = 0
                   dTtlSqFt = 0 .

        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ oe-boll.ord-no
            AND oe-ordl.i-no EQ oe-boll.i-no  NO-ERROR .

        FIND FIRST  oe-ord NO-LOCK
            WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no EQ oe-boll.ord-no NO-ERROR .
      
        
        IF AVAILABLE oe-ordl THEN 
            v-i-name = oe-ordl.i-name.
        ELSE IF AVAILABLE itemfg THEN 
                v-i-name = itemfg.i-name.
        
        IF AVAILABLE oe-bolh THEN
            ASSIGN v-trailer = oe-bolh.trailer
                v-bol-no  = oe-bolh.bol-no.
        cCustName = "" .
        IF AVAIL oe-ord THEN
            FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode 
            AND cust.cust-no EQ oe-ord.cust-no NO-ERROR .

        IF AVAILABLE cust THEN
            ASSIGN cCustName = cust.NAME
            v-del-zone = cust.del-zone
            v-terr     = cust.terr.

        RELEASE cust .
       ASSIGN cShipName = "" .
        RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).
        IF AVAIL shipto THEN
            ASSIGN cShipName = shipto.ship-name 
                   v-del-zone = dest-code
            .

        RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT dTotalSqft).
 
        /*IF AVAILABLE oe-ordl AND oe-ordl.ship-qty GT 0 THEN*/
         IF oe-boll.qty GT 0 THEN
             dTotalSqft = round(dTotalSqft,5) * oe-boll.qty .

        iShipQty = iShipQty + /*(IF AVAIL oe-ordl THEN oe-ordl.ship-qty ELSE 0)*/ oe-boll.qty . /* ticket 60604*/
        
        IF FIRST-OF (oe-boll.i-no) THEN
        iOrdQty = iOrdQty + (IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0).
        
        dTtlSqFt = dTtlSqFt + dTotalSqft.
      IF LAST-OF(oe-boll.i-no) THEN DO:
            
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".    

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
         
            CASE cTmpField:                               
                WHEN "v-del-zone" THEN 
                    cVarValue = STRING(v-del-zone).
                WHEN "v-terr" THEN 
                    cVarValue = STRING(v-terr).                
                WHEN "v-trailer" THEN 
                    cVarValue = STRING(v-trailer,"x(21)").
                WHEN "v-totl-sq" THEN 
                    cVarValue = STRING(dTtlSqFt,"->,>>>,>>>.9<<<<").
                WHEN "v-bol-no" THEN 
                    cVarValue = STRING(v-bol-no,">>>>>>9").

                WHEN "cust-no" THEN 
                    cVarValue = IF AVAIL oe-ord THEN STRING(oe-ord.cust-no,"x(8)") ELSE "".
                WHEN "cust-name" THEN 
                    cVarValue = STRING(cCustName,"x(30)") .                
                WHEN "po-num" THEN 
                    cVarValue = STRING(oe-boll.po-no) .
                WHEN "ord-no" THEN 
                    cVarValue = STRING(oe-boll.ord-no,">>>>>9").
                WHEN "rel-no" THEN 
                    cVarValue = STRING(oe-bolh.release#,">>>>>9").
                WHEN "i-no" THEN 
                    cVarValue = STRING(oe-boll.i-no).
                WHEN "i-name" THEN 
                    cVarValue = STRING(v-i-name,"x(30)").
                WHEN "job" THEN 
                    cVarValue = IF oe-boll.job-no NE "" THEN STRING(oe-boll.job-no + "-" + string(oe-boll.job-no2,"99")) ELSE "" .                
                WHEN "shp-qty" THEN 
                    cVarValue = STRING(iShipQty,">>>,>>>,>>9").
                WHEN "ord-qty" THEN 
                    cVarValue = STRING(iOrdQty,">>>,>>>,>>9").
                WHEN "part-no" THEN 
                    cVarValue = STRING(itemfg.part-no).
                WHEN "bol-date" THEN 
                    cVarValue = STRING(oe-bolh.bol-date,"99/99/9999").
                WHEN "ship-id" THEN 
                    cVarValue = STRING(oe-bolh.ship-id,"x(8)").
                WHEN "ship-name" THEN 
                    cVarValue = STRING(cShipName,"x(30)").
               
            END CASE.
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".  
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.
      END.
    END.

    STATUS DEFAULT "Printing...".

    STATUS DEFAULT "".

    SESSION:SET-WAIT-STATE ("").

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            begin_trailer:SENSITIVE = NOT iplChecked
            end_trailer:SENSITIVE   = NOT iplChecked
            begin_trailer:VISIBLE   = NOT iplChecked
            end_trailer:VISIBLE     = NOT iplChecked
        
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP("parm",lv-field-hdl:PRIVATE-DATA) > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF LOOKUP(lv-field-hdl:NAME,lv-field2-hdl:PRIVATE-DATA) > 0 THEN 
                    DO:
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                    END.
                    lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
                END.       
            END.                 
        END.            
        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
    END.
    PUT SPACE(28)
        "< Selection Parameters >"
        SKIP(1).

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

