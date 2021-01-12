&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: oerep/loadtagJob.w

  Description: Creates a Work In Process tag for an item

  Input Parameters:
    ipcCompany     :Company code
    ipcLocation    :Location code
    ipcJobno       :Primary Job number   
    ipcFGItem      :FG Item number 
    ipiJobno2      :Second Job number
    ipiFormno      :Form number of the Job
    ipiBlankno     :Blank number of the Job

  Output Parameters:
      <none>

  History: 
          
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
DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobno    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItem  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiJobno2   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiFormno   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiBlankno  AS INTEGER   NO-UNDO. 
DEFINE VARIABLE hdJobDetails      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobDetailsWin   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachineListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateJobno    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormattedJobno   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCopies           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotTags          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFilterBy         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE list-name         AS cha       NO-UNDO.
DEFINE VARIABLE init-dir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ship-id         AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_16ths          AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE lv-got-shipto     AS LOGICAL   NO-UNDO.  
DEFINE VARIABLE scr-label-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE scr-auto-print    AS LOGICAL   INITIAL YES NO-UNDO. 
DEFINE VARIABLE loadtagFunction   AS CHARACTER INITIAL "Order" NO-UNDO.
DEFINE VARIABLE g_company         AS CHARACTER NO-UNDO.

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
{methods/defines/sortByDefs.i}
{wip/keyboardDefs.i}


{sys/inc/var.i new shared}
{custom/xprint.i}

ASSIGN
    cocode    = ipcCompany 
    locode    = ipcLocation
    g_company = cocode.


DEFINE VARIABLE lines-per-page AS INTEGER NO-UNDO.
DEFINE VARIABLE save_id        AS RECID.
DEFINE VARIABLE time_stamp     AS ch.
ASSIGN 
    time_stamp = STRING(TIME, "hh:mmam").

DEFINE VARIABLE v-ford-no      AS INTEGER   FORMAT ">>>>>>" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-ford-line    AS INTEGER   FORMAT ">>" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-orders       AS CHARACTER FORMAT "x(78)" EXTENT 10.
DEFINE VARIABLE v-fitem        AS CHARACTER FORMAT "x(15)" EXTENT 2 INIT ["","zzzzzzzzzzzzzzz"].
DEFINE VARIABLE v-po-no-source AS CHARACTER FORMAT "!" INIT "R".
DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "O".  
DEFINE VARIABLE v-out          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE v-job          AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE num-rec        AS INTEGER   INIT 0 NO-UNDO.
DEFINE VARIABLE by-release     AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE lv-rd_print    AS CHARACTER NO-UNDO.      /* 9812 CAH: */
DEFINE VARIABLE v-loadtag      AS CHARACTER NO-UNDO INIT "ASI".  /* sys ctrl option */
DEFINE VARIABLE v-mult         AS INTEGER   NO-UNDO INIT 0.  /* sys ctrl option */
DEFINE VARIABLE v-cas-lab      AS LOG       NO-UNDO.  /* sys ctrl option */
DEFINE VARIABLE v-tags         AS DECIMAL   NO-UNDO INIT 0.  /* sys ctrl option */
DEFINE VARIABLE v-count        AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE v-fgrecpt      AS LOG       NO-UNDO.  /* sys ctrl option */
DEFINE VARIABLE glOverrideMult AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDeptNote      AS LOGICAL   NO-UNDO INIT YES.

/* mdp var used for posting to finish goods */

DEFINE VARIABLE lv-r-no        LIKE rm-rctd.r-no NO-UNDO.

/* 9812 CAH: Variables for Intermec Support */
DEFINE VARIABLE stx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~002".
DEFINE VARIABLE etx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~003".
DEFINE VARIABLE esc            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~033".
DEFINE VARIABLE etb            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~027".
DEFINE VARIABLE cr             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~015".
DEFINE VARIABLE can            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~030".
DEFINE VARIABLE rs             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~036".
DEFINE VARIABLE us             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~037".

DEFINE STREAM s-form.
DEFINE STREAM s-bar.

DEFINE VARIABLE form_fid                 AS CHARACTER NO-UNDO INITIAL "barcode.frm" FORMAT "X(40)".
DEFINE VARIABLE form#                    AS INTEGER   NO-UNDO FORMAT "9" INITIAL 3.
DEFINE VARIABLE char_units               AS CHARACTER NO-UNDO.
DEFINE VARIABLE copy_count               AS INTEGER   NO-UNDO INITIAL 2.
DEFINE VARIABLE n                        AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE var-display-warning      AS LOG       NO-UNDO.

/* Vars for create-text-file */
DEFINE VARIABLE lv-text                  AS cha       NO-UNDO.
DEFINE VARIABLE v-dept-note              AS cha       FORM "x(80)" EXTENT 18 NO-UNDO.
DEFINE VARIABLE lv-middlesex-job         AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE lv-middlesex-po          AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE lv-tag-no                AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-how-many-tags         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-total-tags            AS INTEGER   NO-UNDO.
DEFINE VARIABLE gvlCreateWithMaxPrompted AS LOG       NO-UNDO.
DEFINE VARIABLE gvcSkippedJob            AS CHARACTER NO-UNDO.
DEFINE VARIABLE gvcSkippedItem           AS CHARACTER NO-UNDO.

/* gdm - 10160905*/
DEFINE VARIABLE v-fgdsc1                 LIKE itemfg.part-dscr1 NO-UNDO.
DEFINE VARIABLE v-fgdsc2                 LIKE itemfg.part-dscr2 NO-UNDO.
DEFINE VARIABLE v-fgdsc3                 LIKE itemfg.part-dscr3 NO-UNDO.
DEFINE VARIABLE cPrevFromItem            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrevToItem              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lReturn                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hLoadtagProcs            AS HANDLE    NO-UNDO.  
DEFINE VARIABLE hdOutboundProcs          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdInventoryProcs         AS HANDLE    NO-UNDO.

/* Procedure to prepare and execute API calls */
RUN api/OutboundProcs.p        PERSISTENT SET hdOutboundProcs.
RUN Inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

DEFINE TEMP-TABLE tt-ordjobs
    FIELD job-no  LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2.

DEFINE TEMP-TABLE tt-comps
    FIELD comp AS CHARACTER 
    INDEX i1 comp.
DEFINE WORKFILE w-file 
    FIELD w-key AS ROWID.
DEFINE TEMP-TABLE tt-tag 
    FIELD tt-recid AS RECID.
DEFINE WORKFILE w-shipto LIKE shipto
    FIELD stat AS CHARACTER
    FIELD row-id AS ROWID.

DEFINE BUFFER b-oe-rel   FOR oe-rel.
DEFINE BUFFER ref-lot-no FOR reftable.

DEFINE TEMP-TABLE ttblJob NO-UNDO
    FIELD company AS CHARACTER
    FIELD job-no  AS CHARACTER
    FIELD job-no2 AS INTEGER
    FIELD ord-no  AS INTEGER
    INDEX ttblJob IS PRIMARY UNIQUE
    company job-no job-no2 ord-no
    INDEX ord-no                    company ord-no.
DEFINE TEMP-TABLE tt-fgrctd-created NO-UNDO
    FIELD fg-rctd-rowid AS ROWID
    FIELD is-component  AS LOG.

DEFINE VARIABLE SSLoadTag-log AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lvReturnChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvFound       AS LOG       NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "SSLoadTag", "L", NO, NO, "", "", 
    OUTPUT lvReturnChar, OUTPUT lvFound).
IF lvFound THEN
    SSLoadTag-log = LOGICAL(lvReturnChar).
{oerep/r-loadtg.i NEW}

{fg/fullset.i NEW}

ASSIGN  
    tmpstore = FILL("_",50).

{sys/form/r-top3.f}

DEFINE VARIABLE lv-ok-ran AS LOG NO-UNDO.
{custom/formtext.i NEW}

DEFINE WORKFILE w-fg-rctd LIKE fg-rctd 
    FIELD row-id   AS ROWID
    FIELD invoiced AS LOG INIT NO.

{fg/fg-post3.i NEW}
DEFINE VARIABLE v-fgpostgl AS CHARACTER NO-UNDO.
{jc/jcgl-sh.i NEW}

DEFINE VARIABLE ordNo         AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo         AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobNo2        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNo           AS CHARACTER NO-UNDO.
DEFINE VARIABLE poNopo        AS INTEGER   NO-UNDO.

/* gdm - 04090909 */
DEFINE VARIABLE v-barflg      AS LOG       NO-UNDO.
DEFINE VARIABLE v-auto-print  AS LOG       NO-UNDO.
DEFINE VARIABLE UserlabelPath AS cha       NO-UNDO.

/* gdm - 06100901 */
DEFINE VARIABLE v-txtflg      AS LOG       NO-UNDO.

DO TRANSACTION:
    {sys/inc/fgpofrt.i}
    {sys/inc/fgrecpt.i} /* gdm - 12010901*/
    {sys/inc/rfidtag.i}
    {sys/inc/fgsetrec.i}
END.
DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFGSetAssembly   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGSetAssembly   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lGetBin          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cBarCodeProgram  AS CHARACTER INIT "Xprint" NO-UNDO .
DEFINE VARIABLE i-bardir-int     AS INTEGER   NO-UNDO .
DEFINE VARIABLE i-xprint-int     AS INTEGER   NO-UNDO .
DEFINE VARIABLE hdOutputProcs    AS HANDLE.

DEFINE VARIABLE lFGTagValidation AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGTagValidation AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "FGSetAssembly",
    INPUT "L",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cFGSetAssembly,
    OUTPUT lFound).
IF lFound THEN
    lFGSetAssembly = cFGSetAssembly EQ "YES".
RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "FGSetAssembly",
    INPUT "C",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cFGSetAssembly,
    OUTPUT lFound).
DEFINE VARIABLE lLabelMatrixLock AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "lmLock",
    INPUT "L",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cResult,
    OUTPUT lFound).
IF lFound THEN
    lLabelMatrixLock = LOGICAL(cResult).

/* rstark - zoho13731 */
DEFINE VARIABLE lSSCC AS LOG NO-UNDO.
RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "LoadTagSSCC",
    INPUT "L",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cResult,
    OUTPUT lFound).
lSSCC = LOGICAL(cResult).

/* gdm - 09210907 */
DEFINE VARIABLE v-bardir     AS LOG       NO-UNDO.
DEFINE VARIABLE v-bardir-chr AS CHARACTER NO-UNDO.  
DEFINE VARIABLE iForm        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lForm        AS LOG       NO-UNDO. 
DEFINE VARIABLE lcRtnChar    AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound   AS LOG       NO-UNDO.

DEFINE BUFFER bf-oe-ord  FOR oe-ord.
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

/* gdm - 0930916 */
DEFINE BUFFER bf-po-ord  FOR po-ord.
DEFINE BUFFER bf-po-ordl FOR po-ordl.

DEFINE BUFFER bf-jobhdr  FOR job-hdr.
DEFINE NEW SHARED TEMP-TABLE tt-word-print LIKE w-ord 
    FIELD tag-no AS CHARACTER .

DEFINE VARIABLE hdCostProcs AS HANDLE.
RUN system\CostProcs.p PERSISTENT SET hdCostProcs.

DEFINE VARIABLE hdPoProcs  AS HANDLE NO-UNDO.
DEFINE VARIABLE hdJobProcs AS HANDLE NO-UNDO.

RUN po/PoProcs.p    PERSISTENT SET hdPoProcs.
RUN jc/JobProcs.p   PERSISTENT SET hdJobProcs.
                          
DEFINE VARIABLE cReturnValue        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCheckClosedStatus AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE cFGDefWhse          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFGDefBin           AS CHARACTER NO-UNDO.


RUN sys/ref/nk1look.p (
    INPUT cocode,           /* Company Code */ 
    INPUT "FGReceiptRules", /* sys-ctrl name */
    INPUT "I",              /* Output return value */
    INPUT NO,               /* Use ship-to */
    INPUT NO,               /* ship-to vendor */
    INPUT "",               /* ship-to vendor value */
    INPUT "",               /* shi-id value */
    OUTPUT cReturnValue, 
    OUTPUT lRecFound
    ). 

glCheckClosedStatus = IF (lRecFound AND INTEGER(cReturnValue) EQ 1) THEN YES ELSE NO.

RUN Inventory_GetDefaultWhse IN hdInventoryProcs(
    INPUT  cocode,
    OUTPUT cFGDefWhse
    ).
RUN Inventory_GetDefaultBin IN hdInventoryProcs(
    INPUT  cocode,
    OUTPUT cFGDefBin
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 btnKeyboard ls-jobno cb-jobno2 ~
bt-exit cb-formno cb-blankno cb-FGItem btnNumPad-2 btnNumPad-1 btnNumPad-3 btnNumPad-4 ~
ls-total-run-qty ls-qty-per-tag ls-num-tags ls-num-partial cbUserPrintCopy bt-print ~
cUserField1 cUserField2 cb-userfield 
&Scoped-Define DISPLAYED-OBJECTS ls-jobno cb-jobno2 cb-formno cb-blankno ~
cb-FGItem cItemName cItemDesc ls-total-run-qty ls-qty-per-tag ls-num-tags ~
ls-num-partial cbUserPrintCopy cUserField1 cUserField2 cb-userfield 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars W-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-print 
    LABEL "Print" 
    SIZE 22.2 BY 3 TOOLTIP "Print"
    FONT 37.

DEFINE BUTTON bt-exit AUTO-END-KEY 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U
    LABEL "" 
    SIZE 9.6 BY 2.29 TOOLTIP "Exit".

DEFINE BUTTON btJobDetails 
    IMAGE-UP FILE "Graphics/32x32/form.ico":U
    LABEL "" 
    SIZE 9.6 BY 2.29 TOOLTIP "Job Details".

DEFINE BUTTON btnKeyboard 
    IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
    LABEL "Keyboard" 
    SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnNumPad-1 
    IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U
    LABEL "NumPad1" 
    SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-2 
    IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U
    LABEL "NumPad1" 
    SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-3 
    IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U
    LABEL "NumPad1" 
    SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".
     
DEFINE BUTTON btnNumPad-4 
    IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U
    LABEL "NumPad1" 
    SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".     

DEFINE VARIABLE cb-blankno       AS INTEGER   FORMAT "99":U INITIAL 0 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "0" 
    DROP-DOWN-LIST
    SIZE 9.8 BY 1
    FONT 37 NO-UNDO.

DEFINE VARIABLE cb-FGItem        AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS COMBO-BOX INNER-LINES 5
    DROP-DOWN-LIST
    SIZE 37.4 BY 1
    FONT 37 NO-UNDO.

DEFINE VARIABLE cb-formno        AS INTEGER   FORMAT "99":U INITIAL 0 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "0" 
    DROP-DOWN-LIST
    SIZE 9.8 BY 1
    FONT 37 NO-UNDO.

DEFINE VARIABLE cb-jobno2        AS INTEGER   FORMAT "99":U INITIAL 0 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "00" 
    DROP-DOWN-LIST
    SIZE 9.8 BY 1
    FONT 37 NO-UNDO.

DEFINE VARIABLE cb-userfield     AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEM-PAIRS "Lot #","lot",
    "Weight","Weight",
    "Cust PO #","CustPo",
    "Comment","comment"
    DROP-DOWN-LIST
    SIZE 22.6 BY 1
    FONT 37 NO-UNDO.

DEFINE VARIABLE cbUserPrintCopy  AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEM-PAIRS "1","1",
    "2","2",
    "3","3",
    "4","4",
    "5","5",
    "6","6",
    "7","7",
    "8","8",
    "9","9",
    "10","10"
    DROP-DOWN-LIST 
    SIZE 9.8 BY 1
    FONT 37 NO-UNDO.

DEFINE VARIABLE cItemDesc        AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 63.4 BY 1.1
    BGCOLOR 8 FONT 37 NO-UNDO.

DEFINE VARIABLE cItemName        AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 63.4 BY 1.1
    BGCOLOR 8 FONT 37 NO-UNDO.

DEFINE VARIABLE cUserField1      AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 41 BY 1.1
    FONT 37 NO-UNDO.

DEFINE VARIABLE cUserField2      AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.1
    FONT 37 NO-UNDO.

DEFINE VARIABLE ls-jobno         AS CHARACTER FORMAT "X(15)":U 
    VIEW-AS FILL-IN 
    SIZE 44 BY 1.38
    FONT 37 NO-UNDO.

DEFINE VARIABLE ls-num-partial   AS INTEGER   FORMAT ">>>9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.8 BY 1.38
    BGCOLOR 8 FONT 37 NO-UNDO.

DEFINE VARIABLE ls-num-tags      AS INTEGER   FORMAT ">>>9":U INITIAL 1 
    VIEW-AS FILL-IN 
    SIZE 9.8 BY 1.38
    FONT 37 NO-UNDO.

DEFINE VARIABLE ls-qty-per-tag   AS DECIMAL   FORMAT ">,>>>,>>9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.38
    FONT 37 NO-UNDO.

DEFINE VARIABLE ls-total-run-qty AS DECIMAL   FORMAT ">,>>>,>>9":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.38
    FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 125 BY 26.91.

DEFINE RECTANGLE RECT-26
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 121.8 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    btnKeyboard AT ROW 1.95 COL 7 WIDGET-ID 132
    ls-jobno AT ROW 2.05 COL 25 COLON-ALIGNED NO-LABELS WIDGET-ID 10
    cb-jobno2 AT ROW 2.19 COL 72 COLON-ALIGNED NO-LABELS WIDGET-ID 50
    btJobDetails AT ROW 2.19 COL 95 WIDGET-ID 154
    bt-exit AT ROW 2.19 COL 110.4 WIDGET-ID 84
    cb-formno AT ROW 4.1 COL 25 COLON-ALIGNED NO-LABELS WIDGET-ID 54
    cb-blankno AT ROW 4.1 COL 52 COLON-ALIGNED NO-LABELS WIDGET-ID 56
    cb-FGItem AT ROW 6.05 COL 25 COLON-ALIGNED NO-LABELS WIDGET-ID 94
    cItemName AT ROW 7.67 COL 25 COLON-ALIGNED NO-LABELS WIDGET-ID 88
    cItemDesc AT ROW 9.19 COL 25 COLON-ALIGNED NO-LABELS WIDGET-ID 156
    btnNumPad-2 AT ROW 10.95 COL 41.4 WIDGET-ID 126
    btnNumPad-1 AT ROW 10.95 COL 82 WIDGET-ID 124
    btnNumPad-3 AT ROW 10.95 COL 112.6 WIDGET-ID 128
    btnNumPad-4 AT ROW 12.57 COL 112.6 WIDGET-ID 128
    ls-total-run-qty AT ROW 11.05 COL 21.2 COLON-ALIGNED NO-LABELS WIDGET-ID 102
    ls-qty-per-tag AT ROW 11.05 COL 61.2 COLON-ALIGNED NO-LABELS WIDGET-ID 98
    ls-num-tags AT ROW 11.05 COL 100 COLON-ALIGNED NO-LABELS WIDGET-ID 106
    ls-num-partial AT ROW 12.67 COL 100 COLON-ALIGNED NO-LABELS WIDGET-ID 170
     
    bt-print AT ROW 15.29 COL 52 WIDGET-ID 108
    cbUserPrintCopy AT ROW 16.00 COL 100 COLON-ALIGNED NO-LABELS WIDGET-ID 172
    cUserField1 AT ROW 21.14 COL 21 COLON-ALIGNED NO-LABELS WIDGET-ID 160
    cUserField2 AT ROW 22.71 COL 21 COLON-ALIGNED NO-LABELS WIDGET-ID 164
    cb-userfield AT ROW 22.76 COL 39.4 COLON-ALIGNED NO-LABELS WIDGET-ID 168
    "Print Copy:" VIEW-AS TEXT
    SIZE 22 BY 1.33 AT ROW 16.00 COL 84.5 WIDGET-ID 174
    FONT 36
    "Job #:" VIEW-AS TEXT
    SIZE 8 BY 1.33 AT ROW 1.95 COL 16.6 WIDGET-ID 12
    FONT 36
    "Item Desc:" VIEW-AS TEXT
    SIZE 16 BY 1.1 AT ROW 9.19 COL 10 WIDGET-ID 158
    FONT 36
    "Qty/Tag:" VIEW-AS TEXT
    SIZE 13.4 BY 1.33 AT ROW 11.05 COL 49.4 WIDGET-ID 96
    FONT 36
    "Item Name:" VIEW-AS TEXT
    SIZE 17 BY 1.1 AT ROW 7.67 COL 8 WIDGET-ID 86
    FONT 36
    "FG Item:" VIEW-AS TEXT
    SIZE 12 BY 1.33 AT ROW 6 COL 12.6 WIDGET-ID 92
    FONT 36
    "#Tags:" VIEW-AS TEXT
    SIZE 11 BY 1.33 AT ROW 11.05 COL 90.8 WIDGET-ID 104
    FONT 36
    "Partial:" VIEW-AS TEXT
    SIZE 10.2 BY 1.33 AT ROW 12.57 COL 91 WIDGET-ID 104
    FONT 36
    "Blank #:" VIEW-AS TEXT
    SIZE 10.8 BY 1.33 AT ROW 4.19 COL 41.6 WIDGET-ID 58
    FONT 36
    "User Field 2:" VIEW-AS TEXT
    SIZE 14 BY 1.1 AT ROW 22.81 COL 5 WIDGET-ID 166
    FONT 36
    "User Field 1:" VIEW-AS TEXT
    SIZE 14 BY 1.1 AT ROW 21.24 COL 5 WIDGET-ID 162
    FONT 36
    "Run Qty:" VIEW-AS TEXT
    SIZE 13 BY 1.33 AT ROW 11.1 COL 8.8 WIDGET-ID 100
    FONT 36
    "Form #:" VIEW-AS TEXT
    SIZE 9 BY 1.33 AT ROW 4.14 COL 15.4 WIDGET-ID 48
    FONT 36
    RECT-26 AT ROW 5.52 COL 2.2 WIDGET-ID 18
    RECT-1 AT ROW 1 COL 1 WIDGET-ID 118
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 126 BY 27.24
    BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW W-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Create Job Loadtag"
        HEIGHT             = 27.19
        WIDTH              = 125.8
        MAX-HEIGHT         = 33.14
        MAX-WIDTH          = 213
        VIRTUAL-HEIGHT     = 33.14
        VIRTUAL-WIDTH      = 213
        MAX-BUTTON         = NO
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = NO
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btJobDetails IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    btnKeyboard:HIDDEN IN FRAME F-Main = TRUE.

ASSIGN 
    btnNumPad-1:HIDDEN IN FRAME F-Main = TRUE.

ASSIGN 
    btnNumPad-2:HIDDEN IN FRAME F-Main = TRUE.

ASSIGN 
    btnNumPad-3:HIDDEN IN FRAME F-Main = TRUE.
       
ASSIGN 
    btnNumPad-4:HIDDEN IN FRAME F-Main = TRUE.       

/* SETTINGS FOR FILL-IN cItemDesc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
    THEN W-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create Job Loadtag */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create Job Loadtag */
    DO:    
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */

        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-print W-Win
ON CHOOSE OF bt-print IN FRAME F-Main /* Print */
    DO: 
        DEFINE VARIABLE cCheckField AS CHARACTER NO-UNDO.
        IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) EQ 0 THEN 
        DO:
            MESSAGE "Quantity Per Tag cannot be 0.00"
                VIEW-AS ALERT-BOX ERROR.               
            APPLY "ENTRY" TO ls-qty-per-tag.
            RETURN.    
        END.
        
        IF cb-FGItem:SCREEN-VALUE EQ ? THEN 
        DO:
            MESSAGE "FG Item can't be blank"
                VIEW-AS ALERT-BOX ERROR.               
            APPLY "ENTRY" TO cb-FGItem.
            RETURN.    
        END.
    
        IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) LT DECIMAL(ls-qty-per-tag:SCREEN-VALUE) THEN 
        DO:
            MESSAGE "Total Run Qty " + STRING(ls-total-run-qty:SCREEN-VALUE) +
                " cannot be less than Quantity Per tag " + STRING(ls-qty-per-tag:SCREEN-VALUE)
                VIEW-AS ALERT-BOX ERROR.               
            APPLY "ENTRY" TO ls-total-run-qty.
            RETURN.
        END.
        
        IF cb-userfield:SCREEN-VALUE EQ "Weight" THEN
        DO:
            cCheckField =  DYNAMIC-FUNCTION('sfCommon_CheckIntDecValue', cUserField2:SCREEN-VALUE)  .
            IF cCheckField EQ ?  
                THEN 
            DO:
                MESSAGE "User Field  - Weight is invalid:" + cUserField2:SCREEN-VALUE
                    VIEW-AS ALERT-BOX ERROR .
                RETURN.
            END. 
            
        END.
    
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
    
        ASSIGN
            cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
        
        FOR EACH tt-word-print:
            DELETE tt-word-print .
        END.     
        
        EMPTY TEMP-TABLE w-ord.        
        
        RUN pCreateTempTable .
        
        RUN pCreateTagProc .    
     
        RUN pXprintTag .                   
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit W-Win
ON CHOOSE OF bt-exit IN FRAME F-Main
    DO:
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.

        IF VALID-HANDLE(hdInventoryProcs) THEN
            DELETE OBJECT hdInventoryProcs.

        IF VALID-HANDLE(hdOutputProcs) THEN
            DELETE OBJECT hdOutputProcs.

        IF VALID-HANDLE(hdJobDetailsWin) THEN
            APPLY "WINDOW-CLOSE" TO hdJobDetailsWin.

        IF VALID-HANDLE(hdJobDetails) THEN
            DELETE OBJECT hdInventoryProcs.

        APPLY "CLOSE":U TO THIS-PROCEDURE.
    
        RETURN.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJobDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJobDetails W-Win
ON CHOOSE OF btJobDetails IN FRAME F-Main
    DO:
        DO WITH FRAME {&FRAME-NAME}:
        END.

        IF NOT VALID-HANDLE(hdJobDetails) THEN 
        DO:         
            RUN inventory/job-details.w PERSISTENT SET hdJobDetails.

            RUN dispatch IN hdJobDetails (
                INPUT 'initialize':U
                ) NO-ERROR.
        
            hdJobDetailsWin = hdJobDetails:CURRENT-WINDOW.
        END.
                                                 
        IF VALID-HANDLE(hdJobDetails) AND
            VALID-HANDLE(hdJobDetailsWin) THEN 
        DO:        
            RUN pInit IN hdJobDetails (
                INPUT ipcCompany,
                INPUT ipcLocation,
                INPUT cFormattedJobno,
                INPUT INTEGER(cb-jobno2:SCREEN-VALUE),
                INPUT INTEGER(cb-formno:SCREEN-VALUE),
                INPUT INTEGER(cb-blankno:SCREEN-VALUE)
                ) NO-ERROR.            

            IF hdJobDetailsWin:WINDOW-STATE EQ 2 THEN ASSIGN 
                    hdJobDetailsWin:WINDOW-STATE = 3.
        
            hdJobDetailsWin:MOVE-TO-TOP().
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard W-Win
ON CHOOSE OF btnKeyboard IN FRAME F-Main /* Keyboard */
    DO:
        APPLY "ENTRY":U TO ls-jobno.
        RUN pKeyboard (ls-jobno:HANDLE, "Qwerty").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad-1 W-Win
ON CHOOSE OF btnNumPad-1 IN FRAME F-Main /* NumPad1 */
    DO:
        APPLY "ENTRY":U TO ls-qty-per-tag.
        RUN pKeyboard (ls-qty-per-tag:HANDLE, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad-2 W-Win
ON CHOOSE OF btnNumPad-2 IN FRAME F-Main /* NumPad1 */
    DO:
        APPLY "ENTRY":U TO ls-total-run-qty.
        RUN pKeyboard (ls-total-run-qty:HANDLE, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad-3 W-Win
ON CHOOSE OF btnNumPad-3 IN FRAME F-Main /* NumPad1 */
    DO:
        APPLY "ENTRY":U TO ls-num-tags.
        RUN pKeyboard (ls-num-tags:HANDLE, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnNumPad-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad-4 W-Win
ON CHOOSE OF btnNumPad-4 IN FRAME F-Main /* NumPad1 */
    DO:
        APPLY "ENTRY":U TO ls-num-partial.
        RUN pKeyboard (ls-num-partial:HANDLE, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-blankno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-blankno W-Win
ON VALUE-CHANGED OF cb-blankno IN FRAME F-Main
    DO:
        APPLY "VALUE-CHANGED" TO cb-jobno2.
        RUN onValueChangedOfJobDetails.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-FGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-FGItem W-Win
ON VALUE-CHANGED OF cb-FGItem IN FRAME F-Main
    DO:
        RUN onValueChangedOfJobDetails.    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-formno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-formno W-Win
ON VALUE-CHANGED OF cb-formno IN FRAME F-Main
    DO:
        APPLY "VALUE-CHANGED" TO cb-jobno2.
        RUN onValueChangedOfJobDetails.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-jobno2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-jobno2 W-Win
ON VALUE-CHANGED OF cb-jobno2 IN FRAME F-Main
    DO:
        RUN pUpdateFgItemList (
            INPUT  cFormattedJobno,
            INPUT  cb-jobno2:SCREEN-VALUE,
            INPUT  cb-formno:SCREEN-VALUE,
            INPUT  cb-blankno:SCREEN-VALUE,
            OUTPUT cMachineListItems
            ).
        
        RUN onValueChangedOfJobDetails.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-userfield
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-userfield W-Win
ON VALUE-CHANGED OF cb-userfield IN FRAME F-Main
    DO:
    /*RUN onValueChangedOfJobDetails.    */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbUserPrintCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbUserPrintCopy W-Win
ON VALUE-CHANGED OF cbUserPrintCopy IN FRAME F-Main
    DO:
    /*RUN onValueChangedOfJobDetails.    */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-jobno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON ENTRY OF ls-jobno IN FRAME F-Main
    DO:
        hFocusField = SELF.
        IF lKeyboard THEN
            RUN pKeyboard (SELF, "Qwerty").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON HELP OF ls-jobno IN FRAME F-Main
    DO:
        DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

        RUN system/openlookup.p (
            INPUT  ipcCompany, 
            INPUT  "job-no",        /* Lookup ID */
            INPUT  0,               /* Subject ID */
            INPUT  "",              /* User ID */
            INPUT  0,               /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).
        
        IF cFoundValue NE "" THEN 
        DO:    
            SELF:SCREEN-VALUE = cFoundValue.
        
            APPLY "LEAVE":U TO SELF.
                    
            ASSIGN
                cb-jobno2:SCREEN-VALUE  = IF NUM-ENTRIES(cFieldsValue,"|") GE 6 AND
                                         INDEX(cb-jobno2:LIST-ITEMS, STRING(INTEGER(ENTRY(6,cFieldsValue,"|")),"99")) GT 0 THEN
                                          ENTRY(6,cFieldsValue,"|")
                                      ELSE
                                          ENTRY(1,cb-jobno2:LIST-ITEMS)
                cb-formno:SCREEN-VALUE  = IF NUM-ENTRIES(cFieldsValue,"|") GE 8 AND
                                         INDEX(cb-formno:LIST-ITEMS, STRING(INTEGER(ENTRY(8,cFieldsValue,"|")),"99")) GT 0 THEN
                                          ENTRY(8,cFieldsValue,"|")
                                      ELSE
                                          ENTRY(1,cb-formno:LIST-ITEMS)
                cb-blankno:SCREEN-VALUE = IF NUM-ENTRIES(cFieldsValue,"|") GE 10 AND
                                         INDEX(cb-blankno:LIST-ITEMS, STRING(INTEGER(ENTRY(10,cFieldsValue,"|")),"99")) GT 0 THEN
                                          ENTRY(10,cFieldsValue,"|")
                                      ELSE
                                          ENTRY(1,cb-blankno:LIST-ITEMS)
            NO-ERROR.
            
            APPLY "VALUE-CHANGED" TO cb-jobno2.
        
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON LEAVE OF ls-jobno IN FRAME F-Main
    DO:
        DEFINE VARIABLE cJobNo    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cJobNo2   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFormNo   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBlankNo  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lParse    AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lValidJob AS LOGICAL   NO-UNDO.    
    
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.
    
        IF cValidateJobno EQ ls-jobno:SCREEN-VALUE THEN
            RETURN.              
                    
        SESSION:SET-WAIT-STATE("GENERAL").     
    
        RUN disableCreate(NO).

        ASSIGN 
            cJobno2ListItems       = ""
            cFormnoListItems       = ""
            cBlanknoListitems      = ""
            cMachineListItems      = ""
            cMessage               = ""            
            btJobDetails:SENSITIVE = FALSE
            .
        
        RUN JobParser IN hdJobProcs (
            SELF:SCREEN-VALUE,
            OUTPUT cJobNo,
            OUTPUT cJobNo2,
            OUTPUT cFormNo,
            OUTPUT cBlankNo,
            OUTPUT lParse,
            OUTPUT cMessage
            ).

        cFormattedJobno = DYNAMIC-FUNCTION (
            "fAddSpacesToString" IN hdJobProcs, ls-jobno:SCREEN-VALUE, 6, TRUE
            ).                                  

        IF lParse THEN
            ASSIGN
                SELF:SCREEN-VALUE = cJobNo    
                cFormattedJobno   = DYNAMIC-FUNCTION (
                              "fAddSpacesToString" IN hdJobProcs, cJobNo, 6, TRUE
                              ).

        IF cMessage NE "" THEN 
        DO:
            SESSION:SET-WAIT-STATE("").
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
                    
        RUN updateComboBoxes.

        IF lParse THEN
            IF (cJobNo2 NE "" AND INDEX(cJobno2ListItems,STRING(INTEGER(cJobNo2),"99")) LE 0) OR
                (cFormNo NE "" AND INDEX(cFormnoListItems,STRING(INTEGER(cFormNo),"99")) LE 0) OR
                (cBlankNo NE "" AND INDEX(cBlanknoListitems,STRING(INTEGER(cBlankNo),"99")) LE 0) THEN 
            DO:
                MESSAGE "Invalid Job Scan, please scan a valid Job Number." 
                    VIEW-AS ALERT-BOX ERROR.
            
                ASSIGN
                    cFormattedJobNo         = ""
                    cValidateJobno          = ""
                    SELF:SCREEN-VALUE       = ""
                    cb-jobno2:LIST-ITEMS    = "00"
                    cb-formno:LIST-ITEMS    = "00"
                    cb-blankno:LIST-ITEMS   = "00"
                    cb-FGItem:LIST-ITEMS    = ""
                    cb-jobno2:SCREEN-VALUE  = "00"
                    cb-formno:SCREEN-VALUE  = "00"
                    cb-blankno:SCREEN-VALUE = "00"
                    cb-FGItem:SCREEN-VALUE  = ""
                    .           
            
                SESSION:SET-WAIT-STATE("").
            
                RETURN NO-APPLY.
            END.
            ELSE
                ASSIGN
                    cb-jobno2:SCREEN-VALUE  = IF cJobNo2 EQ "" THEN 
                                              ENTRY(1,cJobno2ListItems)
                                          ELSE
                                              STRING(INTEGER(cJobNo2),"99")
                    cb-formno:SCREEN-VALUE  = IF cFormNo EQ "" THEN
                                              ENTRY(1,cFormnoListItems)
                                          ELSE
                                              STRING(INTEGER(cFormNo),"99")
                    cb-blankno:SCREEN-VALUE = IF cBlankNo EQ "" THEN
                                              ENTRY(1,cBlanknoListItems)
                                          ELSE
                                              STRING(INTEGER(cBlankNo),"99")
                    .
        ELSE
            ASSIGN 
                cb-jobno2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
                cb-formno:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
                cb-blankno:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
                .
            
        APPLY "VALUE-CHANGED" TO cb-jobno2. 
          
        RUN ValidateJob IN hdJobProcs (
            INPUT ipcCompany,
            INPUT cFormattedJobno,
            INPUT "",
            INPUT INTEGER(cb-jobno2:SCREEN-VALUE),
            INPUT INTEGER(cb-formno:SCREEN-VALUE),
            INPUT INTEGER(cb-blankno:SCREEN-VALUE),
            OUTPUT lValidJob
            ).
            
        IF lValidJob THEN
            RUN enableCreate.

        /* Additional validation to check if job still doesn't exist. 
           In this case machine code is passed empty to check if job is valid*/
        IF NOT lValidJob THEN
            RUN ValidateJob IN hdJobProcs (
                INPUT ipcCompany,
                INPUT cFormattedJobno,
                INPUT "", /* Blank Machine code */
                INPUT INTEGER(cb-jobno2:SCREEN-VALUE),
                INPUT INTEGER(cb-formno:SCREEN-VALUE),
                INPUT INTEGER(cb-blankno:SCREEN-VALUE),
                OUTPUT lValidJob
                ).
    
        IF NOT lValidJob THEN 
        DO:
            MESSAGE "Invalid Job Number " SELF:SCREEN-VALUE 
                ", please enter a valid Job Number." 
                VIEW-AS ALERT-BOX ERROR.
        
            ASSIGN
                SELF:SCREEN-VALUE = ""
                cValidateJobno    = ""
                .
        
            RETURN NO-APPLY.
        END.

        btJobDetails:SENSITIVE = TRUE.
                            
        cValidateJobno = ls-jobno:SCREEN-VALUE.    
                           
   
        SESSION:SET-WAIT-STATE("").        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-num-partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-num-partial W-Win
ON ENTRY OF ls-num-partial IN FRAME F-Main
    DO:
        hFocusField = SELF.
        IF lKeyboard THEN
            RUN pKeyboard (SELF, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-num-partial W-Win
ON LEAVE OF ls-num-partial IN FRAME F-Main
    DO:
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-num-tags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-num-tags W-Win
ON ENTRY OF ls-num-tags IN FRAME F-Main
    DO:
        hFocusField = SELF.
        IF lKeyboard THEN
            RUN pKeyboard (SELF, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-num-tags W-Win
ON LEAVE OF ls-num-tags IN FRAME F-Main
    DO:
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-qty-per-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-qty-per-tag W-Win
ON ENTRY OF ls-qty-per-tag IN FRAME F-Main
    DO:
        hFocusField = SELF.
        IF lKeyboard THEN
            RUN pKeyboard (SELF, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-qty-per-tag W-Win
ON LEAVE OF ls-qty-per-tag IN FRAME F-Main
    DO: 
        DEFINE VARIABLE dBundle AS DECIMAL NO-UNDO.
        
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.
    
        IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) EQ 0 THEN
            ls-total-run-qty:SCREEN-VALUE = ls-qty-per-tag:SCREEN-VALUE.
    
        IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) NE 0 AND
            DECIMAL(ls-total-run-qty:SCREEN-VALUE) NE 0 AND
            DECIMAL(ls-qty-per-tag:SCREEN-VALUE) LT DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN 
        DO:             
            ASSIGN
                ls-num-tags:SCREEN-VALUE = STRING(INTEGER(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)))
                .
                  
            dBundle =  DECIMAL(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)) .
            ls-num-partial:SCREEN-VALUE = STRING(int(ls-total-run-qty:SCREEN-VALUE) - ( DECIMAL(dBundle) * DECIMAL(ls-qty-per-tag:SCREEN-VALUE)) ).
        END.
        ELSE
            ASSIGN
                ls-num-tags:SCREEN-VALUE = "1"              .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-total-run-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-total-run-qty W-Win
ON ENTRY OF ls-total-run-qty IN FRAME F-Main
    DO:
        hFocusField = SELF.
        IF lKeyboard THEN
            RUN pKeyboard (SELF, "Numeric").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-total-run-qty W-Win
ON LEAVE OF ls-total-run-qty IN FRAME F-Main
    DO:   
        DEFINE VARIABLE dBundle AS DECIMAL NO-UNDO.
    
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.
    
        IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) NE 0 AND
            DECIMAL(ls-total-run-qty:SCREEN-VALUE) NE 0 AND
            DECIMAL(ls-qty-per-tag:SCREEN-VALUE) LT DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN 
        DO:            
            ASSIGN
                ls-num-tags:SCREEN-VALUE = STRING(INTEGER(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)))
                .
                
            dBundle =  DECIMAL(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)) .
            ls-num-partial:SCREEN-VALUE = STRING(int(ls-total-run-qty:SCREEN-VALUE) - ( dBundle * DECIMAL(ls-qty-per-tag:SCREEN-VALUE)) ).
        END.
        ELSE
            ASSIGN
                ls-num-tags:SCREEN-VALUE = "1"
                .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/*{wip/pNavigate.i} */
{wip/pKeyboard.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
    /*------------------------------------------------------------------------------
      Purpose:     Dispatched to this procedure when the Record-
                   Source has a new row available.  This procedure
                   tries to get the new row (or foriegn keys) from
                   the Record-Source and process it.
      Parameters:  <none>
    ------------------------------------------------------------------------------*/

    /* Define variables needed by this internal procedure.             */
    {src/adm/template/row-head.i}

    /* Process the newly available records (i.e. display fields,
       open queries, and/or pass records on to any RECORD-TARGETS).    */
    {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPrint W-Win 
PROCEDURE AutoPrint :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBarDir         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDB             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUserSpecific   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cPath           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLockPath       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lLockWasRemoved AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cProtocol       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cComputerName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSharedFolder   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDrive          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDir            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExt            AS CHARACTER NO-UNDO.


    /* Need to check if labelMatrix has removed the lockFile */
    IF  lLabelMatrixLock THEN 
    DO:
        RUN win_breakPath (INPUT scr-label-file,
            OUTPUT cProtocol        ,
            OUTPUT cComputerName    ,
            OUTPUT cSharedFolder    ,
            OUTPUT cDrive           ,
            OUTPUT cDir             ,
            OUTPUT cFile            ,
            OUTPUT cExt             ).
        /* Put the lock file in the label matrix path */

        cLockPath = cDrive + cDir + "lm.lock".

        lLockWasRemoved = TRUE.
        IF SEARCH(cLockPath) NE ? THEN
            RUN oe/w-lockwait.w (INPUT cLockPath, OUTPUT lLockWasRemoved).


        /* Test if User Hit Cancel */
        IF NOT lLockWasRemoved THEN
            RETURN.

    END.

    IF scr-auto-print THEN 
    DO:
        RUN sys/ref/GetBarDir.p (INPUT cocode,
            INPUT "loadtag",
            OUTPUT cBarDir,
            OUTPUT cDB,
            OUTPUT lUserSpecific).

        IF lUserSpecific THEN 
            RUN custom/lmprint.p (INPUT cocode,
                INPUT scr-label-file, 
                INPUT cDB,
                INPUT cBarDir).
        ELSE
            RUN custom/lmprint.p (INPUT cocode,
                INPUT scr-label-file,
                INPUT "",
                INPUT "").
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag W-Win 
PROCEDURE create-loadtag :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER io-tag-no AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-total-unit LIKE w-ord.total-unit NO-UNDO.
    DEFINE VARIABLE cRfidTag AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-loadtag FOR loadtag.
    DEFINE BUFFER b-po-ordl FOR po-ordl.
    DEFINE VARIABLE lvCalcCostUom         LIKE fg-rctd.cost-uom NO-UNDO.
    DEFINE VARIABLE lvCalcStdCost         LIKE fg-rctd.std-cost NO-UNDO.
    DEFINE VARIABLE lvCalcExtCost         LIKE fg-rctd.ext-cost NO-UNDO.
    DEFINE VARIABLE lvCalcFrtCost         LIKE fg-rctd.frt-cost NO-UNDO.
    DEFINE VARIABLE lvSetupPerCostUom     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE li                    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-got-job            AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-out-cost           AS DECIMAL   DECIMALS 4 NO-UNDO.
    DEFINE VARIABLE lv-out-qty            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-from-uom           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-cost-uom           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-ord-qty            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-ord-uom            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-setup-included     AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-setup-per-cost-uom AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-full-qty           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-adjusted-qty       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-use-full-qty       AS LOG       NO-UNDO.
    DEFINE VARIABLE v-bwt                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-len                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep                 LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE dCostPerUOM           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostExtended         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostExtendedFreight  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM              AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cNextLoadtag          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE dRFIDTag              AS DECIMAL   NO-UNDO.
    /*   DEF BUFFER bf-eb FOR eb. */
    DEFINE BUFFER bf-itemfg FOR itemfg.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no
        NO-ERROR.
  
    RUN loadtags\GetNextTag.p(cocode, w-ord.i-no, OUTPUT cNextLoadtag).    

    /* rstark - zoho13731 */
    IF CAN-FIND(FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ 'LoadTagSSCC'
        AND sys-ctrl.log-fld EQ YES) AND
        CAN-FIND(FIRST sys-ctrl-shipto
        WHERE sys-ctrl-shipto.company EQ cocode
        AND sys-ctrl-shipto.name EQ 'LoadTagSSCC'
        AND sys-ctrl-shipto.cust-vend EQ YES
        AND sys-ctrl-shipto.cust-vend-no EQ w-ord.cust-no
        AND sys-ctrl-shipto.log-fld EQ YES) THEN
        RUN oerep/ldtagSSCC.p (cocode,w-ord.cust-no,OUTPUT w-ord.SSCC).

    CREATE loadtag.
    ASSIGN
        loadtag.company      = cocode
        loadtag.tag-no       = cNextLoadtag 
        loadtag.item-type    = NO /*FGitem*/
        loadtag.job-no       = w-ord.job-no
        loadtag.job-no2      = w-ord.job-no2
        loadtag.ord-no       = IF CAN-FIND(FIRST cust WHERE cust.company = cocode
                                      AND cust.cust-no = itemfg.cust-no
                                      AND cust.active = "X")
        THEN 0 ELSE w-ord.ord-no /* task# 07120508*/
        loadtag.i-no         = CAPS(w-ord.i-no)
        loadtag.i-name       = w-ord.i-name
        loadtag.qty          = w-ord.ord-qty
        loadtag.qty-case     = w-ord.pcs
        loadtag.case-bundle  = w-ord.bundle
        loadtag.pallet-count = ip-total-unit /*w-ord.pcs * w-ord.bundle*/
        loadtag.partial      = w-ord.partial /*w-ord.total-unit MOD w-ord.pcs*/
        loadtag.sts          = "Printed"  /* task 10190414 */
        loadtag.tag-date     = TODAY
        loadtag.tag-time     = TIME
        /* gdm - 07170905 */
        loadtag.misc-dec[1]  = w-ord.unit-wt 
        loadtag.misc-dec[2]  = w-ord.pallt-wt
        loadtag.misc-char[2] = w-ord.lot
        /* gdm - 07170905  end */
        loadtag.spare-char-1 = w-ord.SSCC
        loadtag.pallet-no    = IF AVAILABLE itemfg THEN itemfg.trno ELSE ""
        .

    /* gdm - 08260916 */
    IF loadtagFunction EQ 'PO' 
        THEN ASSIGN
            loadtag.po-no = INT(w-ord.po-no)
            loadtag.line  = w-ord.po-line .   

    /*IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.*/

    IF v-loadtag = "CentBox" THEN 
    DO:
        ASSIGN 
            loadtag.loc     = itemfg.def-loc
            loadtag.loc-bin = itemfg.def-loc-bin.
        FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no
            AND fg-bin.job-no  EQ w-ord.job-no
            AND fg-bin.tag = loadtag.tag-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE fg-bin THEN
            ASSIGN loadtag.loc     = fg-bin.loc
                loadtag.loc-bin = fg-bin.loc-bin.

    END.
    ELSE RUN fg/autopost.p (ROWID(itemfg), w-ord.job-no, w-ord.job-no2,
            OUTPUT loadtag.loc , OUTPUT loadtag.loc-bin).

    IF RFIDTag-log THEN 
    DO:

        RUN nextRfidTag (cocode , OUTPUT cRfidTag).

        CREATE rfidtag.
        ASSIGN 
            rfidtag.company   = loadtag.company
            rfidtag.item-type = loadtag.item-type
            rfidtag.tag-no    = loadtag.tag-no
            rfidtag.rfidtag   = cRfidTag /* string(dRFIDTag)*/.
        RELEASE oe-ctrl.
    END.
  
    FIND CURRENT loadtag NO-LOCK NO-ERROR.

    IF v-fgrecpt AND glCheckClosedStatus THEN 
    DO:
        RUN CheckPOLineStatus IN hdPoProcs(
            INPUT cocode,
            INPUT w-ord.po-no,
            INPUT w-ord.po-line
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            LEAVE.
 
        RUN CheckJobStatus IN hdJobProcs(
            INPUT cocode,
            INPUT w-ord.job-no,
            INPUT w-ord.job-no2
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            LEAVE. 
    END.    
    
    
    IF v-fgrecpt /*AND NOT tb_ret*/ THEN 
    DO:
        IF AVAILABLE itemfg THEN 
        DO:
            li = 0.
            FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
            IF AVAILABLE fg-rctd AND fg-rctd.r-no GT li THEN li = fg-rctd.r-no.

            FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.

            DO WHILE TRUE:
                li = li + 1.
                FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ li USE-INDEX r-no NO-LOCK NO-ERROR.
                IF AVAILABLE fg-rcpth THEN NEXT.
                FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ li USE-INDEX fg-rctd NO-LOCK NO-ERROR.
                IF AVAILABLE fg-rctd THEN NEXT.
                LEAVE.
            END.

            CREATE fg-rctd.
            ASSIGN
                fg-rctd.r-no       = li + 1
                fg-rctd.rct-date   = TODAY
                fg-rctd.trans-time = TIME
                fg-rctd.company    = cocode
                fg-rctd.rita-code  = "R"
                fg-rctd.i-name     = itemfg.i-name
                fg-rctd.i-no       = loadtag.i-no
                fg-rctd.job-no     = loadtag.job-no
                fg-rctd.job-no2    = loadtag.job-no2
                fg-rctd.t-qty      = loadtag.pallet-count /*loadtag.qty*/
                fg-rctd.pur-uom    = itemfg.prod-uom
                fg-rctd.cost-uom   = itemfg.prod-uom
                /*     fg-rctd.std-cost   = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost */
                fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
                fg-rctd.qty-case   = loadtag.qty-case
                fg-rctd.partial    = loadtag.partial
                fg-rctd.cases      = IF loadtag.qty-case NE 0 THEN TRUNC(fg-rctd.t-qty / loadtag.qty-case,0) ELSE 0
                fg-rctd.cases-unit = loadtag.case-bundle
                fg-rctd.loc        = IF cFGDefWhse NE "" THEN cFGDefWhse ELSE loadtag.loc
                fg-rctd.loc-bin    = IF cFGDefBin  NE "" THEN cFGDefBin  ELSE loadtag.loc-bin
                fg-rctd.tag        = loadtag.tag-no
                fg-rctd.stack-code = loadtag.misc-char[2]
                fg-rctd.tot-wt     = loadtag.misc-dec[1] .
            /* Instead of 0 at 500 with -300 partial, make it */
            /* 1 at -300 with 0 partial 12101418 */
            IF fg-rctd.cases EQ 0 AND fg-rctd.partial NE 0 THEN
                ASSIGN 
                    fg-rctd.cases    = (IF  fg-rctd.partial LT 0 THEN -1 ELSE 1)
                    fg-rctd.qty-case = (IF fg-rctd.partial LT 0 THEN - fg-rctd.partial ELSE fg-rctd.partial)
                    fg-rctd.partial  = 0
                    .
            CREATE tt-fgrctd-created.
            ASSIGN 
                fg-rctd-rowid                  = ROWID(fg-rctd)
                tt-fgrctd-created.is-component = w-ord.is-component.

            IF loadtagFunction EQ 'PO' THEN 
            DO:
                fg-rctd.po-no = TRIM(STRING(loadtag.po-no,">>>>>>>>>>")).
                fg-rctd.po-line = loadtag.line .
                /* Task 09051410 */
                IF loadtag.po-no GT 0 THEN
                    ASSIGN fg-rctd.job-no  = ""
                        fg-rctd.job-no2 = 0.
            END.  


            RELEASE job.

            IF TRIM(fg-rctd.job-no) NE "" THEN
                FIND FIRST job
                    WHERE job.company EQ fg-rctd.company
                    AND job.job-no  EQ fg-rctd.job-no
                    AND job.job-no2 EQ fg-rctd.job-no2
                    USE-INDEX job NO-LOCK NO-ERROR.

            IF AVAILABLE job THEN 
            DO:
                FIND FIRST job-hdr NO-LOCK
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job-no  EQ loadtag.job-no
                    AND job-hdr.job-no2 EQ loadtag.job-no2
                    AND job-hdr.i-no    EQ itemfg.i-no
                    NO-ERROR.
                IF AVAILABLE job-hdr THEN fg-rctd.std-cost = job-hdr.std-tot-cost.

                ELSE
                    FIND FIRST reftable
                        WHERE reftable.reftable EQ "jc/jc-calc.p"
                        AND reftable.company  EQ job.company
                        AND reftable.loc      EQ ""
                        AND reftable.code     EQ STRING(job.job,"999999999")
                        AND reftable.code2    EQ fg-rctd.i-no
                        USE-INDEX reftable NO-LOCK NO-ERROR.

                IF AVAILABLE reftable AND reftable.val[5] NE 0 THEN
                    fg-rctd.std-cost = reftable.val[5].
            END.

            IF NOT AVAILABLE job-hdr AND NOT AVAILABLE reftable THEN 
            DO:
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ itemfg.company
                    AND fg-bin.i-no    EQ itemfg.i-no
                    AND fg-bin.job-no  EQ loadtag.job-no
                    AND fg-bin.job-no2 EQ loadtag.job-no2 
                    /*AND fg-bin.tag = loadtag.tag-no*/
                    NO-LOCK NO-ERROR.
                fg-rctd.std-cost = IF AVAILABLE fg-bin THEN fg-bin.std-tot-cost
                ELSE itemfg.std-tot-cost.
            END.   

            /* WFK - check here is qty avail for components is sufficient, so */
            /*       it can be changed before updating the PO line            */     
            IF NOT fgSetRec-Int EQ 1 THEN
                RUN component-qty-check (INPUT ROWID(fg-rctd)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                /* User chose not to create the tag with a lower qty */
                /* so remove created record                          */
                IF AVAIL(fg-rctd) THEN
                    DELETE fg-rctd.

                RETURN ERROR.
            END.


            IF w-ord.po-no NE 0 AND (loadtagFunction EQ 'Order' OR
                loadtagFunction EQ 'PO') THEN
            DO:
                fg-rctd.po-no = TRIM(STRING(w-ord.po-no,">>>>>>>>>>")).
                fg-rctd.po-line = w-ord.po-line .

                FIND FIRST b-po-ordl WHERE
                    b-po-ordl.company EQ cocode AND
                    b-po-ordl.po-no EQ w-ord.po-no AND
                    b-po-ordl.item-type EQ NO AND
                    b-po-ordl.i-no EQ loadtag.i-no AND
                    (b-po-ordl.line EQ w-ord.po-line OR w-ord.po-line EQ 0)
                    NO-LOCK NO-ERROR.

                IF AVAILABLE b-po-ordl THEN
                DO:
                    IF fg-rctd.po-line EQ 0 THEN fg-rctd.po-line = 1.
                    RUN pGetCostFromPO (b-po-ordl.company, b-po-ordl.po-no, b-po-ordl.line, b-po-ordl.i-no, fg-rctd.t-qty,
                        OUTPUT fg-rctd.std-cost, OUTPUT fg-rctd.cost-uom, OUTPUT fg-rctd.ext-cost, OUTPUT fg-rctd.frt-cost).
                    RUN Conv_ValueFromUOMtoUOM(cocode, 
                        b-po-ordl.i-no, "FG", 
                        fg-rctd.std-cost, fg-rctd.cost-uom, b-po-ordl.cons-uom, 
                        0, b-po-ordl.s-len,po-ordl.s-wid,po-ordl.s-dep, 0, 
                        OUTPUT fg-rctd.std-cost, OUTPUT lError, OUTPUT cMessage).
                    ASSIGN 
                        fg-rctd.pur-uom  = b-po-ordl.cons-uom 
                        fg-rctd.cost-uom = b-po-ordl.cons-uom
                        .       

                END.
            END. /*info from PO on Order*/

            ELSE 
            DO:
                RUN calc-ext-cost .
            END.

        END.  /* avail itemfg */
        /* mdp adds logic to post loadtags 07/24/08 */

        /* gdm - */
        IF v-fgrecpt AND w-ord.est-no NE "" AND AVAILABLE fg-rctd THEN 
        DO:

            FIND FIRST bf-itemfg 
                WHERE bf-itemfg.company EQ cocode
                AND bf-itemfg.i-no EQ w-ord.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE bf-itemfg THEN 
            DO:
                IF bf-itemfg.pur-man THEN 
                DO:
                    IF TRIM(fg-rctd.job-no) NE "" AND TRIM(fg-rctd.po-no) NE "" AND loadtagFunction EQ 'PO' THEN
                        ASSIGN fg-rctd.job-no  = "" 
                            fg-rctd.job-no2 = 0
                            fg-rctd.po-no   = TRIM(STRING(w-ord.po-no,">>>>>>>>>>"))
                            fg-rctd.po-line = w-ord.po-line .
                    ELSE  IF TRIM(fg-rctd.job-no) NE "" AND TRIM(fg-rctd.po-no) NE "" AND loadtagFunction EQ 'Order' THEN
                            ASSIGN
                                fg-rctd.po-no   = ""
                                fg-rctd.job-no  = loadtag.job-no
                                fg-rctd.job-no2 = loadtag.job-no2.

                END.
                /*         ELSE IF NOT bf-eb.pur-man THEN DO: */
                ELSE IF NOT bf-itemfg.pur-man THEN 
                    DO:
                        IF TRIM(fg-rctd.po-no) NE "" AND TRIM(loadtag.job-no) NE "" AND loadtagFunction EQ 'Order' THEN
                            ASSIGN fg-rctd.po-no   = ""
                                fg-rctd.job-no  = loadtag.job-no
                                fg-rctd.job-no2 = loadtag.job-no2.
                        ELSE IF TRIM(fg-rctd.po-no) NE "" AND TRIM(loadtag.job-no) NE "" AND loadtagFunction EQ 'Po' THEN
                                ASSIGN fg-rctd.job-no  = "" 
                                    fg-rctd.job-no2 = 0
                                    fg-rctd.po-no   = TRIM(STRING(w-ord.po-no,">>>>>>>>>>"))
                                    fg-rctd.po-line = w-ord.po-line.
                    END.
            END.
        END.
        /* gdm - */
        /*BV - added the following call to add Set Parts to IU1 ( */
        IF NOT (FGSetRec-Int EQ 1 AND itemfg.alloc NE YES) THEN
            RUN fg/comprcpt.p (ROWID(fg-rctd)).
    /* mdp posting logic ends here */
    END.  /* v-fgrecpt */
  
    
    /* Update the other tags with this new quantity */
    IF AVAILABLE fg-rctd AND lv-use-full-qty THEN
        RUN get-set-full-qty (INPUT fg-rctd.job-no, INPUT fg-rctd.job-no2, 
            INPUT fg-rctd.i-no, INPUT 0 /* new qty */, 
            INPUT fg-rctd.std-cost /* cost to set */, OUTPUT lv-full-qty).

    FIND CURRENT fg-rctd NO-LOCK NO-ERROR.
    
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-text-file W-Win 
PROCEDURE create-text-file :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPalletID      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartPalletID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndPalletID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTotalUnit     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRFIDTag       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vError         AS LOG       NO-UNDO.
    DEFINE VARIABLE liTagCounter   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTmpFile       AS CHARACTER NO-UNDO.

    cTmpFile = SESSION:TEMP-DIRECTORY /*+ "/"*/ + USERID("NOSWEAT") + STRING(TIME).
    FIND FIRST w-ord NO-ERROR.
    DEFINE BUFFER bf-cust FOR cust.

    IF v-loadtag = "TRIAD" THEN 
    DO:

        IF form_fid > "" THEN 
        DO:   /* download the form file into the printer ~*/
            INPUT stream s-form from value(form_fid) no-echo.
            _form: DO WHILE TRUE:
                READKEY STREAM s-form.
                IF LASTKEY < 0 THEN LEAVE _form.
                PUT STREAM s-bar CONTROL CHR(LASTKEY).
            END.
            INPUT stream s-form close.
        END.

        EACH-ORD:
        FOR EACH w-ord:
            v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
            IF v-job BEGINS "-" OR v-job = ? /* 9901 CAH */
                THEN v-job = STRING(W-ORD.ORD-NO).   /* 9812 CAH in case blank */
            FIND FIRST itemfg WHERE itemfg.company = cocode
                AND itemfg.i-no = w-ord.i-no NO-LOCK NO-ERROR.
            IF w-ord.total-tags GT -1 THEN 
            DO:
                DO i = 1 TO (w-ord.total-tags + 1):
                    /* select the form */
                    PUT STREAM s-bar CONTROL stx esc "E" STRING(form#) ",1" can etx.
                    /* 9901 CAH: done above ... 
                    /* clear the variable data fields */
                    put stream s-bar control stx can etx.
                    */
                    char_units = (IF i <= w-ord.total-tags 
                        THEN STRING(w-ord.total-unit) ELSE "    ").  
                    DEFINE VARIABLE char_date AS CHARACTER FORMAT 'x(10)' NO-UNDO.
                    char_date = STRING(TODAY,"99/99/9999").
                    /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
                    IF LENGTH(w-ord.ship-name) > 19
                        THEN w-ord.ship-name = SUBSTRING(w-ord.ship-name,1,19).

                    DEFINE VARIABLE vcFGItem AS CHARACTER NO-UNDO.
                    vcFGItem = 
                        IF AVAILABLE itemfg THEN itemfg.i-no ELSE w-ord.i-no.
                    DO n = copy_count TO 1 BY -1:
                        /* send the variable data to the printer */
                        PUT STREAM s-bar UNFORMATTED
                            stx w-ord.cust-po-no    cr etx
                            stx w-ord.cust-po-no    cr etx
                            stx w-ord.cust-part-no  cr etx
                            stx w-ord.cust-part-no  cr etx
                            stx char_units          cr etx
                            stx char_units          cr etx
                            stx char_date           cr etx
                            stx v-job               cr etx
                            stx w-ord.ord-qty       cr etx /* 9902 CAH was total-unit */
                            stx STRING(i)           cr etx /* 08.20 was n */
                            stx STRING(w-ord.total-tags + 1) cr etx /* 08.20 was copy_count */
                            stx w-ord.ship-name     cr etx
                            stx vcFGItem            cr etx.
                        /* issue the print command */    
                        PUT STREAM s-bar CONTROL     
                            stx rs "1" us "1" etb etx.
                    END.
                END.   /* tag count loop */
            END.  /* non zero */  
        END.    /* each w-ord */
        /*  {sys/inc/close.i "" "stream s-bar"} */
        OUTPUT CLOSE.
    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */
    ELSE
    DO:         
        /* Output to temporary file first, then rename it at end to make sure */
        /* it is not picked up before it is complete */
        OUTPUT TO VALUE(cTmpFile).          
        IF cBarCodeProgram NE "Loftware" THEN 
        DO: 
            PUT UNFORMATTED
                "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL,"
                "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP,"
                "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP,"
                "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT,"
                "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
                "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"
                "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
            IF CAN-DO("ASI,SSLABEL",v-loadtag) THEN
                PUT UNFORMATTED ",COUNTER#,RFIDTag".

            PUT UNFORMATTED 
                ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#,"
                "PalletCode,PalletID,TagCounter,TagCountTotal,"
                "RN1,RN2,RN3,RN4,WareHouse,Bin,JobQty,RunShip,Pallet type,Zone,CreatedBy,CreateDate,CreateTime,PrintDate,PrintTime".

            /* rstark - */
            IF lSSCC THEN PUT UNFORMATTED ",SSCC".
           
            PUT SKIP.
        END.
        FOR EACH w-ord
            BREAK BY w-ord.i-no:

            IF tb_16ths THEN
                ASSIGN
                    w-ord.box-len = ROUND((w-ord.box-len - TRUNC(w-ord.box-len,0)) / 6.25,2) +
                           TRUNC(w-ord.box-len,0)
                    w-ord.box-wid = ROUND((w-ord.box-wid - TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                           TRUNC(w-ord.box-wid,0)
                    w-ord.box-dep = ROUND((w-ord.box-dep - TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                           TRUNC(w-ord.box-dep,0).

            ASSIGN
                lv-text     = ""
                v-dept-note = ""

                /* gdm - 10160905 */
                v-fgdsc1    = ""
                v-fgdsc2    = ""
                v-fgdsc3    = "".

            FIND FIRST itemfg WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ w-ord.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN 
            DO:        
                ASSIGN 
                    w-ord.net-wt   = itemfg.weight-100 * w-ord.total-unit / 100
                    w-ord.sheet-wt = itemfg.weight-100 / 100 
                    /*   w-ord.cust-part-no = itemfg.part-no */ .

                FOR EACH tt-formtext:
                    DELETE tt-formtext.
                END.
                FOR EACH notes NO-LOCK WHERE notes.rec_key = itemfg.rec_key
                    AND notes.note_code = "SN" :
                    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                END.
                DO li = 1 TO 8:
                    CREATE tt-formtext.
                    ASSIGN 
                        tt-line-no = li
                        tt-length  = 80.
                END.
                RUN custom/formtext.p (lv-text).
                i = 0.           
                FOR EACH tt-formtext:
                    i = i + 1.
                    IF  i <= 8 THEN v-dept-note[i] = tt-formtext.tt-text.      
                END.

                /* gdm - 101610905 */
                ASSIGN 
                    v-fgdsc1 = itemfg.part-dscr1
                    v-fgdsc2 = itemfg.part-dscr2
                    v-fgdsc3 = itemfg.part-dscr3.

            END.  /* avail itemfg */
                 
            IF lDeptNote THEN 
            DO:
                lv-text = "".
                FOR EACH tt-formtext:
                    DELETE tt-formtext.
                END.
     
                IF w-ord.ord-no NE 0 THEN 
                DO:
                    FOR EACH job-hdr NO-LOCK
                        WHERE job-hdr.company EQ cocode
                        AND job-hdr.ord-no  EQ w-ord.ord-no 
                        AND job-hdr.job-no  EQ w-ord.job-no
                        AND job-hdr.job-no2 EQ w-ord.job-no2
                        BREAK BY job-hdr.job
                        BY job-hdr.job-no
                        BY job-hdr.job-no2:
                        IF LAST-OF(job-hdr.job-no2) THEN
                            FOR EACH job NO-LOCK
                                WHERE job.company EQ job-hdr.company
                                AND job.job     EQ job-hdr.job
                                AND job.job-no  EQ job-hdr.job-no
                                AND job.job-no2 EQ job-hdr.job-no2,
                                EACH notes NO-LOCK
                                WHERE notes.rec_key EQ job.rec_key :
                                IF notes.note_form_no = 0 OR notes.note_form_no = w-ord.form-no THEN
                                    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
                            END.
                    END.
     
                END.
                IF lv-text NE "" THEN 
                DO:
                    DO li = 1 TO 10:
                        CREATE tt-formtext.
                        ASSIGN 
                            tt-line-no = li
                            tt-length  = 80.
                    END.
                    RUN custom/formtext.p (lv-text).
                    i = 8.           
                    FOR EACH tt-formtext:
                        i = i + 1.
                        IF i <= 18 THEN v-dept-note[i] = tt-formtext.tt-text.      
     
                    END.
                END.
            END. /* lDeptNote*/   

            ASSIGN
                w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
                v-job          = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
            IF v-job BEGINS "-" THEN v-job = "".
            ASSIGN
                lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,6)
                lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                            "%MX" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                            TRIM(lv-middlesex-job)
                lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,6)
                lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                            "BNJ" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                            TRIM(lv-middlesex-po).

            IF w-ord.total-tags GT 0 THEN 
            DO:
                lv-how-many-tags =  IF CAN-DO("SSLABEL,CentBox",v-loadtag) OR w-ord.total-tags = 1 THEN w-ord.total-tags
                ELSE (w-ord.total-tags - 1).
                FIND bf-cust WHERE bf-cust.company = cocode
                    AND bf-cust.cust-no EQ w-ord.cust-no
                    NO-LOCK NO-ERROR.

                RUN incrementPalletID (BUFFER bf-cust, lv-how-many-tags * w-ord.mult,
                    OUTPUT iStartPalletID, OUTPUT iEndPalletID).
                IF iEndPalletID EQ -1 THEN 
                DO:
                    RUN askNextPalletID (INPUT w-ord.cust-no, OUTPUT vError).
                    RETURN.
                END.
                
                iPalletId = iStartPalletID.   
                DO i = 1 TO (lv-how-many-tags * w-ord.mult):
                    /* loadtags generation */
                    IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  THEN 
                    DO:
                        liTagCounter = liTagCounter + 1.
                        IF i = 1 THEN lv-tag-no = i.
                        /*  create-loadtag may prompt, so need to close default stream */
                        OUTPUT CLOSE.
                        RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit) NO-ERROR.
                        OUTPUT TO VALUE(cTmpFile) APPEND.

                    END.

                    IF CAN-DO("ASI,SSLABEL",v-loadtag) THEN 
                    DO:

                        FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
                        cRFIDTag = IF AVAILABLE rfidtag THEN rfidtag.rfidtag ELSE "".

                    END.
                    cTotalUnit = STRING(w-ord.total-unit, ">>>>>>>9").
                    RUN write-loadtag-line (INPUT cRFIDTag, INPUT cTotalUnit, INPUT iPalletID, INPUT liTagCounter).
                    iPalletID = iPalletID + 1.
                END. /* DO i = 1 TO (lv-how-many-tags * w-ord.mult): */
                
                 
                IF NOT CAN-DO("SSLABEL,CentBox",v-loadtag) THEN 
                DO:
                    RUN incrementPalletID (BUFFER bf-cust, w-ord.mult,
                        OUTPUT iStartPalletID, OUTPUT iEndPalletID).
                    IF iEndPalletID EQ -1 THEN 
                    DO:
                        RUN askNextPalletID (INPUT w-ord.cust-no, OUTPUT vError).
                        RETURN.
                    END.


                    iPalletId = iStartPalletID.
                    DO v-count = 1 TO w-ord.mult: /* for partial print */
                        /* loadtags generation */
                        IF v-count EQ 1 THEN 
                        DO:
                            liTagCounter = liTagCounter + 1.
                                            
                            /* Create-loadtag may prompt, so need to close default stream */
                            OUTPUT CLOSE.         
                            RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0) NO-ERROR.
                            OUTPUT TO VALUE(cTmpFile) APPEND.

                        END.
                        cTotalUnit = "".      
                        RUN write-loadtag-line (INPUT cRFIDTag, cTotalUnit, INPUT iPalletID, INPUT liTagCounter).
                        iPalletID = iPalletID + 1.
                    END.
                END. /*not SSLABEL, Centbox*/

            END. /*w-ord.total-tags > 0*/
            /* IF FIRST-OF(w-ord.i-no) THEN
                 RUN pRunAPIOutboundTriggerForItem (
                     INPUT cocode,
                     INPUT w-ord.i-no,
                     INPUT w-ord.job-no,
                     INPUT w-ord.job-no2,
                     INPUT w-ord.po-no
                     ).   */     
            DELETE w-ord.
        END.


        OUTPUT close.
        IF SEARCH(v-out) NE ? THEN
            OS-DELETE VALUE(v-out).
        /* Rename to expected file name / location */
        IF cBarCodeProgram EQ ""  OR cBarCodeProgram EQ 'Loftware' THEN 
        DO:
            OS-RENAME VALUE(cTmpFile) VALUE(v-out).
        END.
        ELSE 
        DO:
            IF SEARCH(cTmpFile) NE ? THEN
                OS-DELETE VALUE(cTmpFile).
        END.


    END.    /* NOT TRIAD */
/*IF ssPostFG-log AND SSPostFG-char = "Loadtag"  THEN
  RUN post-all.     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableCreate W-Win 
PROCEDURE disableCreate :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplChange AS LOGICAL NO-UNDO.
    IF iplChange THEN
        ASSIGN 
            ls-qty-per-tag:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "0"
            ls-total-run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
            ls-num-tags:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = "0"
            ls-num-partial:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "0".
    ASSIGN
        ls-qty-per-tag:SENSITIVE IN FRAME {&FRAME-NAME}   = FALSE
        ls-total-run-qty:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        ls-num-tags:SENSITIVE IN FRAME {&FRAME-NAME}      = FALSE
        bt-print:SENSITIVE IN FRAME {&FRAME-NAME}         = FALSE
        cbUserPrintCopy:SENSITIVE IN FRAME {&FRAME-NAME}  = FALSE
        ls-qty-per-tag:BGCOLOR                            = 8
        ls-total-run-qty:BGCOLOR                          = 8
        ls-num-tags:BGCOLOR                               = 8
        ls-num-partial:BGCOLOR                            = 8
        btnNumPad-1:HIDDEN                                = YES
        btnNumPad-2:HIDDEN                                = YES
        btnNumPad-3:HIDDEN                                = YES
        btnNumPad-4:HIDDEN                                = YES
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
        THEN DELETE WIDGET W-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableCreate W-Win 
PROCEDURE enableCreate :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    ASSIGN 
        ls-qty-per-tag:SENSITIVE IN FRAME {&FRAME-NAME}   = TRUE
        ls-total-run-qty:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
        ls-num-tags:SENSITIVE IN FRAME {&FRAME-NAME}      = TRUE
        bt-print:SENSITIVE IN FRAME {&FRAME-NAME}         = TRUE
        cbUserPrintCopy:SENSITIVE IN FRAME {&FRAME-NAME}  = TRUE
        ls-qty-per-tag:BGCOLOR                            = ?
        ls-total-run-qty:BGCOLOR                          = ?
        ls-num-tags:BGCOLOR                               = ?
        ls-num-partial:BGCOLOR                            = ?
        btnNumPad-1:HIDDEN                                = NO
        btnNumPad-2:HIDDEN                                = NO
        btnNumPad-3:HIDDEN                                = NO
        btnNumPad-4:HIDDEN                                = NO
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
    DISPLAY ls-jobno cb-jobno2 cb-formno cb-blankno cb-FGItem cItemName cItemDesc 
        ls-total-run-qty ls-qty-per-tag ls-num-tags ls-num-partial 
        cbUserPrintCopy cUserField1 cUserField2 cb-userfield 
        WITH FRAME F-Main IN WINDOW W-Win.
    ENABLE RECT-26 btnKeyboard ls-jobno cb-jobno2 bt-exit cb-formno cb-blankno 
        cb-FGItem btnNumPad-2 btnNumPad-1 btnNumPad-3 btnNumPad-4 ls-total-run-qty 
        ls-qty-per-tag ls-num-tags ls-num-partial cbUserPrintCopy bt-print cUserField1 
        cUserField2 cb-userfield 
        WITH FRAME F-Main IN WINDOW W-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rel-info W-Win 
PROCEDURE get-rel-info :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-pono LIKE w-ord.cust-po-no NO-UNDO.
    DEFINE OUTPUT PARAMETER op-date LIKE w-ord.rel-date NO-UNDO.
    DEFINE OUTPUT PARAMETER op-lot# LIKE w-ord.rel-lot# NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipNotes LIKE w-ord.ship-notes NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRelQty AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-job AS ROWID NO-UNDO .
    DEFINE BUFFER b-job-hdr FOR job-hdr.
    RELEASE oe-rell.
    RELEASE oe-rel.
  
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company  EQ oe-ordl.company
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          /*AND oe-relh.rel-date GE begin_date
          AND oe-relh.rel-date LE end_date
          AND oe-relh.ship-id GE begin_ship-to
          AND oe-relh.ship-id LE end_ship-to
          AND (oe-relh.release# GE begin_rel OR begin_rel EQ 0)
          AND (oe-relh.release# LE end_rel OR end_rel EQ 0)*/
        BY oe-relh.rel-date
        BY oe-relh.r-no:

        ASSIGN
            op-pono         = oe-rell.po-no
            op-date         = oe-relh.rel-date
            opcShipNotes[1] = oe-relh.ship-i[1]
            opcShipNotes[2] = oe-relh.ship-i[2]
            opcShipNotes[3] = oe-relh.ship-i[3]
            opcShipNotes[4] = oe-relh.ship-i[4]
            opcRelQty       = oe-rell.qty 
            .
        LEAVE.
    END.

    IF AVAILABLE oe-rell THEN
        FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

    ELSE
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line
            AND oe-rel.rel-no   EQ 0
          /*AND oe-rel.rel-date GE begin_date
          AND oe-rel.rel-date LE end_date
          AND oe-rel.ship-id GE begin_ship-to
          AND oe-rel.ship-id LE end_ship-to  */
            BY oe-rel.rel-date
            BY oe-rel.r-no:

            ASSIGN
                op-pono         = (IF oe-rel.po-no GT "" THEN oe-rel.po-no ELSE oe-ordl.po-no)
                op-date         = oe-rel.rel-date
                opcShipNotes[1] = oe-rel.ship-i[1]
                opcShipNotes[2] = oe-rel.ship-i[2]
                opcShipNotes[3] = oe-rel.ship-i[3]
                opcShipNotes[4] = oe-rel.ship-i[4]
                .
            LEAVE.
        END.
  

    IF NOT AVAILABLE oe-rel THEN
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line
            BY oe-rel.rel-date
            BY oe-rel.r-no:

            ASSIGN 
                op-pono         = (IF oe-rel.po-no GT "" THEN oe-rel.po-no ELSE oe-ordl.po-no)
                op-date         = oe-rel.rel-date
                opcShipNotes[1] = oe-rel.ship-i[1]
                opcShipNotes[2] = oe-rel.ship-i[2]
                opcShipNotes[3] = oe-rel.ship-i[3]
                opcShipNotes[4] = oe-rel.ship-i[4]
                .
            LEAVE.
        END.
  
    IF AVAILABLE oe-rel THEN 
        ASSIGN op-lot# = oe-rel.lot-no.
    /*IF v-po-no-source EQ "J" THEN*/
    FIND FIRST b-job-hdr WHERE ROWID(b-job-hdr) EQ ip-job NO-LOCK NO-ERROR .

    /*IF v-po-no-source NE "R"                    OR
       (NOT AVAIL oe-rel AND NOT AVAIL oe-rell) THEN DO:  */
    /*IF  v-po-no-source NE "J" THEN
    op-pono = IF v-po-no-source EQ "L" AND AVAIL oe-ordl THEN oe-ordl.po-no
                                       ELSE IF v-po-no-source EQ "H" AND AVAIL oe-ord THEN oe-ord.po-no
                                       ELSE "".  */
    /*IF  v-po-no-source EQ "J" THEN */
    op-pono =  IF AVAILABLE b-job-hdr THEN b-job-hdr.po-no 
    ELSE "" .

/*END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE incrementPalletID W-Win 
PROCEDURE incrementPalletID :
    /*------------------------------------------------------------------------------
          Purpose:     Increment the pallet number for a given customer and return the
                        new value
          Parameters:  INPUT: cust buffer OUTPUT: next pallet #
          Notes:       Defaults value if not set for given cust
                       Returns error code of -1   
        ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipb-cust FOR cust.
    DEFINE INPUT PARAMETER ipi-tags AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-start-pallet-no LIKE cust.spare-int-1.
    DEFINE OUTPUT PARAMETER op-end-pallet-no LIKE cust.spare-int-1.
    DEFINE BUFFER bf-cust FOR cust.
    DEFINE VARIABLE li AS INTEGER INIT 0.
    DEFINE VARIABLE lj AS INTEGER INIT 0.

    FIND bf-cust WHERE ROWID(bf-cust) EQ ROWID(ipb-cust) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-cust THEN 
    DO:
        op-end-pallet-no = 0.
        RETURN.
    END.

    IF bf-cust.spare-int-1 EQ 0 THEN 
    DO:
        op-end-pallet-no = 0.
        RETURN.
    END.
    li = bf-cust.spare-int-1.
    IF li MOD 1000000 = 999999 THEN 
    DO:
        /*protection code*/
        op-end-pallet-no = -1.
        RETURN.
    END.
    op-start-pallet-no = li + 1.
    DO lj = 1 TO ipi-tags:
        li = li + 1.
        IF li MOD 1000000 = 999999 THEN 
        DO:
            /*protection code*/
            op-end-pallet-no = -1.
            RETURN.
        END.
    END.

    ASSIGN 
        op-end-pallet-no    = li
        bf-cust.spare-int-1 = li.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init W-Win 
PROCEDURE init :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ ipcCompany
        NO-ERROR .
    IF AVAILABLE company THEN
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
            + " - {&awversion}" + " - " 
            + STRING(company.name) + " - " + ipcLocation.  
             
    RUN pGetSettings.
    cbUserPrintCopy:screen-value IN FRAME {&FRAME-NAME} = "1" .
    cb-userfield:screen-value IN FRAME {&FRAME-NAME} = "Lot" .
    
    APPLY "ENTRY" TO ls-jobno IN FRAME {&FRAME-NAME}.

    RUN disableCreate(YES).

    IF ipcJobNo NE "" THEN 
        RUN jobScan (
            INPUT ipcCompany,
            INPUT ipcJobno,
            INPUT ipcFGItem,
            INPUT ipiJobno2,
            INPUT ipiFormno,
            INPUT ipiBlankno
            ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobScan W-Win 
PROCEDURE jobScan :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcFGItem  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.

    RUN ValidateJob IN hdJobProcs (
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT "",
        INPUT ipiJobno2,
        INPUT ipiFormno,
        INPUT ipiBlankno,
        OUTPUT lValidJob
        ).
        
    IF NOT lValidJob THEN 
    DO: 
        MESSAGE "Invalid Job scan" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO ls-jobno IN FRAME {&FRAME-NAME}.
        RETURN ERROR.
    END.    

    ASSIGN
        ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ipcJobno
        cFormattedJobno                              = ipcJobno.
    
    RUN updateComboBoxes.
       
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cb-jobno2:LIST-ITEMS   = cJobno2ListItems
            cb-jobno2:SCREEN-VALUE = STRING(ipiJobno2,"99")
            .
            
        APPLY "VALUE-CHANGED" TO cb-jobno2.
        
        ASSIGN 
            cb-formno:LIST-ITEMS    = cFormnoListItems
            cb-blankno:LIST-ITEMS   = cBlanknoListItems
            cb-FGItem:LIST-ITEMS    = cMachineListItems
            cb-formno:SCREEN-VALUE  = STRING(ipiFormno,"99")
            cb-blankno:SCREEN-VALUE = STRING(ipiBlankno,"99")
            cb-FGItem:SCREEN-VALUE  = ipcFGItem
            .
    END.
    
    cValidateJobno = ls-jobno:SCREEN-VALUE.
                       
    RUN enableCreate.       
                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
    /*------------------------------------------------------------------------------
              Purpose:     Override standard ADM method
              Notes:       
            ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN init.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
    /* -----------------------------------------------------------
              Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
              Parameters:  <none>
              Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
            -------------------------------------------------------------*/
    APPLY "CLOSE":U TO THIS-PROCEDURE.
   
    RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextRfidTag W-Win 
PROCEDURE nextRfidTag :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRfidTag AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dRFIDTag AS DECIMAL NO-UNDO.
    FIND FIRST oe-ctrl WHERE oe-ctrl.company = ipcCompany NO-ERROR.
    dRFIDTag = IF AVAILABLE oe-ctrl AND oe-ctrl.spare-char-1 <> "" 
        THEN dec(oe-ctrl.spare-char-1) ELSE 111110000000000000000001. 
    oe-ctrl.spare-char-1 = STRING(dRFIDTag + 1).

    opcRfidTag = STRING(dRFIDTag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onValueChangedOfJobDetails W-Win 
PROCEDURE onValueChangedOfJobDetails :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.      
       
    RUN ValidateJob IN hdJobProcs (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT /*cb-FGItem:SCREEN-VALUE IN FRAME {&FRAME-NAME}*/ "",
        INPUT INTEGER(cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
        INPUT INTEGER(cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
        INPUT INTEGER(cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
        OUTPUT lValidJob
        ).
    
    IF lValidJob THEN
        RUN enableCreate.
    ELSE  RUN disableCreate(YES).    

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTagProc W-Win 
PROCEDURE pCreateTagProc :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLoadtagFile AS CHARACTER NO-UNDO.

    SESSION:SET-WAIT-STATE ("general").
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)} 
    VIEW FRAME r-top.
    VIEW FRAME top.
    IF cBarCodeProgram EQ 'Loftware' THEN 
        cLoadtagFile = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME) + SUBSTRING(STRING(NOW),21,3) + '.csv'.
    ELSE cLoadtagFile = 'loadtag.txt'.
    IF v-out = "" THEN v-out = "c:~\ba~\label~\" + cLoadtagFile.
    ELSE 
    DO:
        IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
            SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
        ELSE v-out = v-out + "/".
        v-out = v-out + cLoadtagFile.
    END.
    /*  IF choice THEN DO:*/
    IF OPSYS EQ "UNIX" AND v-loadtag NE "TRIAD" THEN 
    DO:
        MESSAGE "Unable to Create Loadtag File for Non MSDos Platform.".
        PAUSE.
        RETURN.
    END.
         
    RUN create-text-file.
    /*END.*/

    SESSION:SET-WAIT-STATE ("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTempTable W-Win 
PROCEDURE pCreateTempTable :
    DEFINE VARIABLE lWarning  AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lAvailOrd AS LOGICAL NO-UNDO.

    FIND FIRST job NO-LOCK
        WHERE job.company = ipcCompany
        AND job.job-no = cFormattedJobno
        AND job.job-no2 = INTEGER(cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME})  NO-ERROR.
      
    IF AVAILABLE job THEN
    DO:
        FIND FIRST job-hdr  NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.i-no    EQ (cb-FGItem:SCREEN-VALUE)   NO-ERROR .
   
        lAvailOrd = IF AVAILABLE job-hdr AND job-hdr.ord-no NE 0 THEN TRUE ELSE FALSE .
   
        RUN pFromJob(ROWID(job),lAvailOrd,OUTPUT lWarning) .
   
        IF AVAILABLE job-hdr AND job-hdr.ord-no NE 0 THEN
        DO:
            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company  EQ ipcCompany 
                AND oe-ord.ord-no EQ  job-hdr.ord-no NO-ERROR.
          
            IF AVAILABLE oe-ord THEN RUN pFromOrd (ROWID(oe-ord)).     
       
        END.
        ELSE        
     
    END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromJob W-Win 
PROCEDURE pFromJob :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/ 
      
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplOrder AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-warning AS LOG NO-UNDO.

    DEFINE BUFFER b-job-hdr-2 FOR job-hdr.

    DEFINE VARIABLE lv-rel-date   AS DATE NO-UNDO.
    DEFINE VARIABLE lv-tt-created AS LOG  NO-UNDO.
         
    FIND FIRST job NO-LOCK
        WHERE ROWID(job) EQ ip-rowid
        /*AND (v-stat EQ "A"                      OR
             (v-stat EQ "C" AND NOT job.opened) OR
             (v-stat EQ "O" AND job.opened)) */
        NO-ERROR.

    IF NOT AVAILABLE job THEN RETURN.
    IF (iplOrder) AND
        NOT CAN-FIND(ttblJob WHERE ttblJob.company EQ job.company
        AND ttblJob.job-no EQ job.job-no
        AND ttblJob.job-no2 EQ job.job-no2) THEN 
    DO:

        FOR EACH b-job-hdr-2 FIELDS(company job-no job-no2 ord-no) WHERE
            b-job-hdr-2.company EQ job.company AND
            b-job-hdr-2.job     EQ job.job AND
            b-job-hdr-2.job-no  EQ job.job-no AND
            b-job-hdr-2.job-no2 EQ job.job-no2 AND
            b-job-hdr-2.i-no    EQ cb-FGItem:SCREEN-VALUE IN FRAME {&FRAME-NAME} /*AND
        b-job-hdr-2.i-no    LE v-fitem[2]*/
            NO-LOCK:

            IF NOT CAN-FIND(FIRST ttblJob WHERE
                ttblJob.company EQ b-job-hdr-2.company AND
                ttblJob.job-no EQ b-job-hdr-2.job-no AND
                ttblJob.job-no2 EQ b-job-hdr-2.job-no2 AND
                ttblJob.ord-no  EQ b-job-hdr-2.ord-no) THEN
            DO:
                CREATE ttblJob.
                ASSIGN
                    ttblJob.company = b-job-hdr-2.company
                    ttblJob.job-no  = b-job-hdr-2.job-no
                    ttblJob.job-no2 = b-job-hdr-2.job-no2
                    ttblJob.ord-no  = b-job-hdr-2.ord-no
                    lv-tt-created   = YES.
                RELEASE ttblJob.
            END.
        END.

        IF lv-tt-created THEN
            RETURN.
    END.
      
      

    IF AVAILABLE job THEN
        FOR EACH job-hdr
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.i-no    EQ (cb-FGItem:SCREEN-VALUE) 
          
            NO-LOCK,
            FIRST cust
            WHERE cust.company EQ ipcCompany
            AND cust.cust-no EQ job-hdr.cust-no
            NO-LOCK,
            FIRST itemfg
            WHERE itemfg.company EQ ipcCompany
            AND itemfg.i-no    EQ job-hdr.i-no
            NO-LOCK:

            CREATE w-ord.
            ASSIGN
                w-ord.ord-no       = job-hdr.ord-no
                w-ord.job-no       = job-hdr.job-no
                w-ord.job-no2      = job-hdr.job-no2
                w-ord.cust-no      = cust.cust-no
                w-ord.cust-name    = cust.name
                w-ord.i-no         = job-hdr.i-no
                w-ord.cust-part-no = itemfg.part-no
                w-ord.over-pct     = /*IF tb_over THEN cust.over-pct ELSE*/ 0
                w-ord.qty-before   = job-hdr.qty
                w-ord.ord-qty      = w-ord.qty-before *
                                 (1 + (w-ord.over-pct / 100))
                w-ord.i-name       = itemfg.i-name
                w-ord.upc-no       = itemfg.upc-no
                w-ord.due-date     = job.start-date
                w-ord.est-no       = job.est-no
                w-ord.form-no      = job-hdr.frm
                w-ord.upc-no       = itemfg.upc-no
                w-ord.box-len      = itemfg.l-score[50]
                w-ord.box-wid      = itemfg.w-score[50]
                w-ord.box-dep      = itemfg.d-score[50]
                w-ord.style        = itemfg.style
                w-ord.vendor       = company.name
                w-ord.tare-wt      = 10
                w-ord.uom          = "EA"
                w-ord.mult         = INTEGER(cbUserPrintCopy) /*if cust.int-field[1] ne 0 AND NOT glOverrideMult then
                cust.int-field[1] else v-mult*/
                num-rec            = num-rec + 1
                w-ord.due-date-job = IF job.due-date <> ? THEN STRING(job.due-date, "99/99/9999") ELSE "".
            w-ord.due-date-jobhdr = IF job-hdr.due-date <> ? THEN STRING(job-hdr.due-date, "99/99/9999") ELSE "".
            w-ord.job-qty      = job-hdr.qty  .
            w-ord.zone         = itemfg.spare-char-4. 
            w-ord.ipReturn     = /*tb_ret*/ NO.
            FOR EACH cust-part NO-LOCK 
                WHERE cust-part.company EQ ipcCompany   
                AND cust-part.i-no EQ itemfg.i-no 
                AND cust-part.cust-no EQ cust.cust-no
                AND cust-part.part-no NE "" :
                ASSIGN  
                    w-ord.cust-part-no = cust-part.part-no .
                LEAVE.
            END.


            IF w-ord.style NE "" THEN
            DO:
                FIND FIRST style WHERE
                    style.company EQ ipcCompany AND
                    style.style EQ w-ord.style
                    NO-LOCK NO-ERROR.

                IF AVAILABLE style THEN
                DO:
                    w-ord.style-desc = style.dscr.
                    RELEASE style.
                END.
            END.

            IF job-hdr.ord-no NE 0 THEN
            DO:
                FIND FIRST oe-ordl WHERE
                    oe-ordl.company EQ ipcCompany AND
                    oe-ordl.ord-no  EQ job-hdr.ord-no AND
                    oe-ordl.i-no    EQ job-hdr.i-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE oe-ordl THEN
                DO:
                    FIND FIRST oe-ord WHERE
                        oe-ord.company EQ ipcCompany AND
                        oe-ord.ord-no  EQ job-hdr.ord-no
                        NO-LOCK NO-ERROR.

                    RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#,
                        OUTPUT w-ord.ship-notes,
                        OUTPUT w-ord.rel-qty,
                        INPUT ROWID(job-hdr)).
                    IF cb-userfield EQ "Lot"  THEN w-ord.lot# = w-ord.rel-lot#.

                    ASSIGN
                        w-ord.ord-desc1 = oe-ordl.part-dscr1
                        w-ord.ord-desc2 = oe-ordl.part-dscr2
                        w-ord.runShip   = oe-ordl.whsed.

                    RELEASE oe-ordl.
                    RELEASE oe-ord.
                END.
            END.
            ELSE 
            DO:
                /*op-warning = YES.*/
                /*IF v-po-no-source = "J"  THEN*/
                RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                    OUTPUT w-ord.rel-date,
                    OUTPUT w-ord.rel-lot#,
                    OUTPUT w-ord.ship-notes,
                    OUTPUT w-ord.rel-qty,
                    INPUT ROWID(job-hdr)).
            END.

            /*IF NOT tb_ship-id THEN*/ 
            v-ship-id = job-hdr.cust-no.
            FOR EACH shipto
                WHERE shipto.company EQ ipcCompany
                AND shipto.cust-no EQ job-hdr.cust-no
                USE-INDEX ship-id NO-LOCK
                BREAK BY shipto.ship-no DESCENDING:
                IF shipto.ship-id EQ v-ship-id OR
                    LAST(shipto.ship-no)              THEN 
                DO:
                    ASSIGN
                        w-ord.ship-code  = shipto.ship-id
                        w-ord.ship-name  = shipto.ship-name
                        w-ord.ship-add1  = shipto.ship-add[1]
                        w-ord.ship-add2  = shipto.ship-add[2]
                        w-ord.ship-city  = shipto.ship-city
                        w-ord.ship-state = shipto.ship-state
                        w-ord.ship-ctry  = shipto.country
                        w-ord.ship-zip   = shipto.ship-zip
                        w-ord.broker     = shipto.broker.
                    LEAVE.
                END.
            END.
       
            FIND FIRST est
                WHERE est.company EQ job.company
                AND est.est-no  EQ job.est-no
                NO-LOCK NO-ERROR.
            RELEASE eb.
            IF AVAILABLE est THEN
                FIND FIRST eb
                    WHERE eb.company   EQ est.company
                    AND eb.est-no    EQ est.est-no
                    AND eb.form-no   EQ job-hdr.frm
                    AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
                    NO-LOCK NO-ERROR.

            IF AVAILABLE eb THEN
                ASSIGN
                    w-ord.flute      = eb.flute
                    w-ord.test       = eb.test
                    w-ord.pcs        = eb.cas-cnt
                    w-ord.bundle     = eb.cas-pal
                    w-ord.total-unit = w-ord.pcs * w-ord.bundle
                    w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
                    w-ord.cas-no     = eb.cas-no
                    w-ord.pallt-no   = eb.tr-no
                    w-ord.part-dscr2 = eb.part-dscr2.

            IF AVAILABLE eb AND eb.est-type EQ 2 THEN
                w-ord.job-qty =  job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
            ELSE IF  AVAILABLE eb AND eb.est-type EQ 6  THEN
                    w-ord.job-qty =  job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .


            /*IF NOT v-oecount THEN
              ASSIGN
               w-ord.pcs    = w-ord.total-unit
               w-ord.bundle = 1.   */
            ASSIGN
                w-ord.pcs        = ls-qty-per-tag
                w-ord.bundle     = TRUNC(ls-total-run-qty / ls-qty-per-tag, 0)
                w-ord.partial    = ls-num-partial
                w-ord.total-unit = ls-total-run-qty
                w-ord.total-tags = ls-num-tags   .
                
            IF cb-userfield EQ "Weight" THEN
                w-ord.unit-wt    = DECIMAL(cUserField2) . 
            ELSE IF cb-userfield EQ "CustPo" THEN
                    w-ord.cust-po-no = cUserField2 .     
            IF w-ord.partial EQ ? THEN w-ord.partial = 0 . 
        
        END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFromOrd W-Win 
PROCEDURE pFromOrd :
    /*------------------------------------------------------------------------------
         Purpose:     
         Parameters:  <none>
         Notes:       
       ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE lv-stat     AS cha  NO-UNDO.
    DEFINE VARIABLE lv-over     LIKE oe-ordl.over-pct NO-UNDO.
    DEFINE VARIABLE lv-rel-date AS DATE NO-UNDO.
    DEFINE VARIABLE lv-job-no2  LIKE job-hdr.job-no2 NO-UNDO.
    DEFINE VARIABLE lv-job-no   LIKE job.job-no NO-UNDO.
    DEFINE BUFFER b-job-hdr FOR job-hdr. /* rtc */
    DEFINE BUFFER b-job     FOR job.         /* rtc */

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE BUFFER b-loadtag FOR loadtag. 

    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
        /* AND (v-stat EQ "A"                                    OR
              (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
              (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))*/
        NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord THEN
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no
            NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord THEN
        FIND FIRST soldto NO-LOCK
            WHERE soldto.company EQ cocode
            AND soldto.cust-no EQ oe-ord.cust-no
            AND soldto.sold-id EQ oe-ord.sold-id
            USE-INDEX sold-id NO-ERROR. 

    IF AVAILABLE cust THEN
    FOR EACH oe-ordl
        WHERE oe-ordl.company eq oe-ord.company
          AND oe-ordl.ord-no  eq oe-ord.ord-no

          AND (oe-ordl.i-no    EQ cb-FGItem:SCREEN-VALUE IN FRAME {&FRAME-NAME}           
          AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
          AND (NOT CAN-FIND(FIRST ttblJob)
               OR (oe-ordl.job-no NE "" AND CAN-FIND(FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                                     AND ttblJob.job-no EQ oe-ordl.job-no
                                     AND ttblJob.job-no2 EQ oe-ordl.job-no2))
               OR (oe-ordl.job-no EQ "" AND
                   CAN-FIND(FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                                     AND ttblJob.ord-no EQ oe-ordl.ord-no)))
               OR CAN-FIND(FIRST tt-comps WHERE tt-comps.comp EQ oe-ordl.i-no))
        use-index ord-no NO-LOCK BREAK BY oe-ordl.i-no:
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.

    ASSIGN 
        lv-job-no2 = 0 
        lv-job-no  = "".
    FIND FIRST ttbljob WHERE ttbljob.company = cocode
        AND ttbljob.ord-no = oe-ordl.ord-no
        AND ttbljob.job-no = oe-ordl.job-no
        AND ttbljob.job-no2 = oe-ordl.job-no2
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttbljob THEN
        FIND FIRST ttbljob WHERE ttbljob.company = cocode
            AND ttbljob.ord-no = oe-ordl.ord-no
            NO-LOCK NO-ERROR.
    IF AVAILABLE ttbljob THEN  
        FIND FIRST b-job-hdr WHERE b-job-hdr.company = cocode
            AND b-job-hdr.ord-no  = oe-ordl.ord-no
            AND b-job-hdr.job-no = job.job-no
            AND b-job-hdr.job-no2 = job.job-no2
            AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR.
    ELSE
        FIND FIRST b-job-hdr WHERE b-job-hdr.company = cocode 
            AND b-job-hdr.ord-no  = oe-ordl.ord-no  
            AND b-job-hdr.i-no    = oe-ordl.i-no NO-LOCK NO-ERROR. 
    IF AVAILABLE b-job-hdr THEN
        FIND FIRST b-job WHERE b-job.company = b-job-hdr.company
            AND b-job.job     = b-job-hdr.job
            AND b-job.job-no  = b-job-hdr.job-no
            AND b-job.job-no2 = b-job-hdr.job-no2 NO-LOCK NO-ERROR.

    IF lv-job-no = "" THEN 
    DO:
        IF AVAILABLE b-job-hdr THEN
            FIND FIRST ttbljob WHERE ttbljob.company = b-job-hdr.company
                AND ttbljob.job-no = b-job-hdr.job-no
                AND ttbljob.job-no2 = b-job-hdr.job-no2
                AND ttbljob.ord-no = oe-ordl.ord-no
                NO-LOCK NO-ERROR.
        ELSE FIND FIRST ttblJob WHERE ttblJob.company EQ oe-ordl.company
                AND ttblJob.ord-no EQ oe-ordl.ord-no
                NO-LOCK NO-ERROR.

        IF AVAILABLE ttblJob THEN
            ASSIGN lv-job-no  = ttblJob.job-no
                lv-job-no2 = ttblJob.job-no2.
        ELSE 
            IF AVAILABLE b-job-hdr THEN
                ASSIGN lv-job-no  = b-job-hdr.job-no
                    lv-job-no2 = b-job-hdr.job-no2.
    END.
    IF lv-job-no = "" THEN lv-job-no = oe-ordl.job-no.
    IF lv-job-no2 = 0 THEN lv-job-no2 = oe-ordl.job-no2.
    IF lv-job-no = "" THEN lv-job-no = oe-ord.job-no.
    IF lv-job-no2 = 0 THEN lv-job-no2 = oe-ord.job-no2.
    IF oe-ordl.est-no NE "" THEN
        FIND FIRST eb
            WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.

    lv-over = /*IF tb_over THEN oe-ordl.over-pct ELSE*/ 0.

    IF NOT by-release OR NOT AVAILABLE oe-ordl THEN
    DO:
        IF FIRST-OF(oe-ordl.i-no) THEN
        DO:
            CREATE w-ord.
            ASSIGN
                w-ord.ord-no       = oe-ord.ord-no
                w-ord.job-no       = lv-job-no
                w-ord.job-no2      = lv-job-no2
                w-ord.cust-no      = oe-ord.cust-no
                w-ord.cust-name    = oe-ord.cust-name
                w-ord.i-no         = oe-ordl.i-no
                w-ord.cust-part-no = oe-ordl.part-no
                w-ord.over-pct     = lv-over
                w-ord.qty-before   = oe-ordl.qty
                w-ord.ord-qty      = w-ord.qty-before *
                                 (1 + (w-ord.over-pct / 100))
                w-ord.po-no        = oe-ordl.po-no-po
                w-ord.sold-code    = oe-ord.sold-id
                w-ord.sold-name    = oe-ord.sold-name
                w-ord.sold-add1    = oe-ord.sold-add[1]
                w-ord.sold-add2    = oe-ord.sold-add[2]
                w-ord.sold-city    = oe-ord.sold-city
                w-ord.sold-state   = oe-ord.sold-state
                w-ord.sold-zip     = oe-ord.sold-zip
                w-ord.i-name       = oe-ordl.i-name
                w-ord.due-date     = IF oe-ord.due-date NE ? THEN
                                   oe-ord.due-date
                                 ELSE
                                 IF oe-ordl.req-date NE ? THEN
                                   oe-ordl.req-date
                                 ELSE TODAY
                w-ord.est-no       = oe-ordl.est-no
                w-ord.form-no      = oe-ordl.form-no
                w-ord.vendor       = company.name
                w-ord.tare-wt      = 10
                w-ord.uom          = "EA"
                w-ord.mult         = INTEGER(cbUserPrintCopy) /*IF AVAILABLE cust AND cust.int-field[1] ne 0 AND NOT glOverrideMult then*/
                /*    cust.int-field[1] else v-mult*/
                w-ord.dont-run-set = oe-ordl.is-a-component
                w-ord.ord-desc1    = oe-ordl.part-dscr1
                w-ord.ord-desc2    = oe-ordl.part-dscr2
                w-ord.runShip      = oe-ordl.whsed

                /* gdm - 08130804*/
                w-ord.linenum      = oe-ordl.e-num
                w-ord.ipReturn     = NO /*tb_ret*/
                num-rec            = num-rec + 1.

            IF AVAILABLE b-job-hdr THEN 
            DO:
                w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
                w-ord.job-qty = IF AVAILABLE b-job AND AVAILABLE b-job-hdr THEN b-job-hdr.qty ELSE 0 .
                IF  AVAILABLE b-job AND AVAILABLE eb AND eb.est-type EQ 2 THEN
                    w-ord.job-qty =  b-job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
                ELSE IF  AVAILABLE b-job AND AVAILABLE eb AND eb.est-type EQ 6 THEN
                        w-ord.job-qty =  b-job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .
            END.
            IF AVAILABLE b-job THEN
                w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
            
            RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                OUTPUT w-ord.rel-date,
                OUTPUT w-ord.rel-lot#,
                OUTPUT w-ord.ship-notes,
                OUTPUT w-ord.rel-qty,
                INPUT ROWID(b-job-hdr)).
            IF cb-userfield EQ "Lot" THEN w-ord.lot# = w-ord.rel-lot#.

            IF AVAILABLE itemfg THEN
                ASSIGN
                    w-ord.upc-no  = itemfg.upc-no
                    w-ord.box-len = itemfg.l-score[50]
                    w-ord.box-wid = itemfg.w-score[50]
                    w-ord.box-dep = itemfg.d-score[50]
                    w-ord.flute   = itemfg.flute
                    w-ord.test    = itemfg.test
                    w-ord.pcs     = itemfg.case-count
                    w-ord.bundle  = itemfg.case-pall
                    w-ord.style   = itemfg.style
                    w-ord.zone    = itemfg.spare-char-4.

            IF w-ord.style NE "" THEN
            DO:
                FIND FIRST style WHERE
                    style.company EQ cocode AND
                    style.style EQ w-ord.style
                    NO-LOCK NO-ERROR.

                IF AVAILABLE style THEN
                DO:
                    w-ord.style-desc = style.dscr.
                    RELEASE style.
                END.
            END.

            IF NOT AVAILABLE eb AND AVAILABLE itemfg AND itemfg.est-no NE "" THEN
                FIND FIRST eb
                    WHERE eb.company  EQ itemfg.company
                    AND eb.est-no   EQ itemfg.est-no
                    AND eb.stock-no EQ itemfg.i-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE eb THEN
                ASSIGN
                    w-ord.flute      = eb.flute
                    w-ord.test       = eb.test
                    w-ord.pcs        = eb.cas-cnt
                    w-ord.bundle     = eb.cas-pal
                    w-ord.cas-no     = eb.cas-no
                    w-ord.form-no    = eb.form-no
                    w-ord.pallt-no   = eb.tr-no
                    w-ord.part-dscr2 = eb.part-dscr2.

            /* get it from order    task# 04120602 */
            ASSIGN 
                w-ord.pcs    = oe-ordl.cas-cnt
                w-ord.bundle = oe-ordl.cases-unit.

            /* get shipto from open oe-rel  */

            lv-got-shipto = NO.
            FOR EACH w-shipto:
                DELETE w-shipto.
            END.

            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.line    EQ oe-ordl.line:
                /*IF NOT tb_ship-id THEN*/ 
                v-ship-id = oe-rel.ship-id.
                RUN oe/custxship.p (oe-rel.company,
                    oe-rel.cust-no,
                    v-ship-id,
                    BUFFER shipto).

                IF AVAILABLE shipto THEN 
                DO:
                    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

                    CREATE w-shipto.
                    BUFFER-COPY shipto EXCEPT rec_key TO w-shipto
                        ASSIGN
                        w-shipto.stat   = lv-stat
                        w-shipto.row-id = ROWID(oe-rel).
                END.
            END.

            FOR EACH w-shipto,
                FIRST oe-rel WHERE ROWID(oe-rel) EQ w-shipto.row-id NO-LOCK
                BREAK BY oe-rel.rel-date
                BY oe-rel.po-no
                BY oe-rel.ship-no 
                BY oe-rel.qty:

                IF CAN-DO("L,S,I",w-shipto.stat) OR
                    LAST(oe-rel.rel-date)          THEN 
                DO:
                    ASSIGN
                        lv-got-shipto    = YES
                        w-ord.ship-code  = w-shipto.ship-id
                        w-ord.ship-name  = w-shipto.ship-name
                        w-ord.ship-add1  = w-shipto.ship-add[1]
                        w-ord.ship-add2  = w-shipto.ship-add[2]
                        w-ord.ship-city  = w-shipto.ship-city
                        w-ord.ship-state = w-shipto.ship-state
                        w-ord.ship-ctry  = w-shipto.country
                        w-ord.ship-zip   = w-shipto.ship-zip
                        w-ord.broker     = w-shipto.broker.
                    LEAVE.
                END.
            END.
            FOR EACH w-shipto:
                DELETE w-shipto.
            END.

            IF NOT lv-got-shipto THEN
                FOR EACH shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ oe-ord.cust-no
                    USE-INDEX ship-id NO-LOCK
                    BREAK BY shipto.ship-no DESCENDING:
                    /*IF NOT tb_ship-id THEN*/ 
                    v-ship-id = oe-ord.cust-no.
                    IF shipto.ship-id EQ v-ship-id OR
                        LAST(shipto.ship-no)             THEN 
                    DO:
                        ASSIGN
                            w-ord.ship-code  = shipto.ship-id
                            w-ord.ship-name  = shipto.ship-name
                            w-ord.ship-add1  = shipto.ship-add[1]
                            w-ord.ship-add2  = shipto.ship-add[2]
                            w-ord.ship-city  = shipto.ship-city
                            w-ord.ship-state = shipto.ship-state
                            w-ord.ship-ctry  = shipto.country
                            w-ord.ship-zip   = shipto.ship-zip
                            w-ord.broker     = shipto.broker.
                        LEAVE.
                    END.
                END.

            FIND FIRST soldto NO-LOCK
                WHERE soldto.company EQ cocode
                AND soldto.cust-no EQ oe-ord.cust-no
                AND soldto.sold-id EQ oe-ord.sold-id
                USE-INDEX sold-id NO-ERROR.

            IF AVAILABLE soldto THEN w-ord.sold-ctry = soldto.country.
              
            ASSIGN
                w-ord.pcs        = ls-qty-per-tag
                w-ord.bundle     = TRUNC(ls-total-run-qty / ls-qty-per-tag, 0)
                w-ord.partial    = ls-num-partial
                w-ord.total-unit = ls-total-run-qty
                w-ord.total-tags = ls-num-tags .
            IF cb-userfield EQ "Weight" THEN
                w-ord.unit-wt    = DECIMAL(cUserField2) .
            ELSE IF cb-userfield EQ "CustPo" THEN
                    w-ord.cust-po-no = cUserField2 .
                
            IF w-ord.partial EQ ? THEN w-ord.partial = 0 .             
        

        END.  /* first-of */
    END.  /* not by-release */

    ELSE
        FOR EACH oe-rel
            WHERE oe-rel.company EQ cocode
            AND oe-rel.i-no      EQ oe-ordl.i-no
            AND oe-rel.ord-no    EQ oe-ordl.ord-no
            AND oe-rel.line      EQ oe-ordl.line
            AND oe-rel.link-no   NE 0 NO-LOCK:

            CREATE w-ord.
            ASSIGN
                w-ord.ord-no       = oe-ord.ord-no
                w-ord.job-no       = oe-ordl.job-no
                w-ord.job-no2      = oe-ordl.job-no2
                w-ord.cust-no      = oe-ord.cust-no
                w-ord.cust-name    = oe-ord.cust-name
                w-ord.i-no         = oe-ordl.i-no
                w-ord.cust-part-no = oe-ordl.part-no
                w-ord.cust-po-no   = b-job-hdr.po-no                 
                w-ord.over-pct     = lv-over
                w-ord.qty-before   = oe-rel.qty
                w-ord.ord-qty      = w-ord.qty-before *
                               (1 + (w-ord.over-pct / 100))
                w-ord.po-no        = oe-ordl.po-no-po
                w-ord.ship-code    = oe-rel.ship-id
                w-ord.ship-add1    = oe-rel.ship-add[1]
                w-ord.ship-add2    = oe-rel.ship-add[2]
                w-ord.ship-city    = oe-rel.ship-city
                w-ord.ship-state   = oe-rel.ship-state
                w-ord.ship-zip     = oe-rel.ship-zip
                w-ord.ship-notes   = oe-rel.ship-i
                w-ord.sold-code    = oe-ord.sold-id
                w-ord.sold-name    = oe-ord.sold-name
                w-ord.sold-add1    = oe-ord.sold-add[1]
                w-ord.sold-add2    = oe-ord.sold-add[2]
                w-ord.sold-city    = oe-ord.sold-city
                w-ord.sold-state   = oe-ord.sold-state
                w-ord.sold-zip     = oe-ord.sold-zip
                w-ord.i-name       = oe-ordl.i-name
                w-ord.due-date     = (IF oe-ord.due-date <> ? 
             THEN oe-ord.due-date
             ELSE IF oe-ordl.req-date <> ?  /* 9901 CAH */
             THEN oe-ordl.req-date
             ELSE TODAY)
                w-ord.rel-date     = oe-rel.rel-date
                w-ord.est-no       = oe-ordl.est-no
                w-ord.form-no      = oe-ordl.form-no
                w-ord.vendor       = company.name
                w-ord.tare-wt      = 10
                w-ord.uom          = "EA"
                w-ord.mult         = INTEGER(cbUserPrintCopy) /*IF AVAIL cust AND cust.int-field[1] ne 0 AND NOT glOverrideMult then*/
                /*cust.int-field[1] else v-mult*/
                w-ord.dont-run-set = oe-ordl.is-a-component
                w-ord.ord-desc1    = oe-ordl.part-dscr1
                w-ord.ord-desc2    = oe-ordl.part-dscr2

                /* gdm - 08130804*/
                w-ord.linenum      = oe-ordl.e-num.
            w-ord.ipReturn     = /*tb_ret*/ NO.
            num-rec            = num-rec + 1.


            ASSIGN 
                w-ord.rel-lot# = oe-rel.lot-no.
            IF cb-userfield EQ "Lot" THEN w-ord.lot# = w-ord.rel-lot#.

            IF AVAILABLE itemfg THEN
                ASSIGN
                    w-ord.upc-no  = itemfg.upc-no
                    w-ord.box-len = itemfg.l-score[50]
                    w-ord.box-wid = itemfg.w-score[50]
                    w-ord.box-dep = itemfg.d-score[50]
                    w-ord.flute   = itemfg.flute
                    w-ord.test    = itemfg.test
                    w-ord.pcs     = itemfg.case-count
                    w-ord.bundle  = itemfg.case-pall
                    w-ord.style   = itemfg.style
                    w-ord.zone    = itemfg.spare-char-4.

            IF w-ord.style NE "" THEN
            DO:
                FIND FIRST style WHERE
                    style.company EQ cocode AND
                    style.style EQ w-ord.style
                    NO-LOCK NO-ERROR.

                IF AVAILABLE style THEN
                DO:
                    w-ord.style-desc = style.dscr.
                    RELEASE style.
                END.
            END.

            IF NOT AVAILABLE eb AND AVAILABLE itemfg AND itemfg.est-no NE "" THEN
                FIND FIRST eb
                    WHERE eb.company  EQ itemfg.company
                    AND eb.est-no   EQ itemfg.est-no
                    AND eb.stock-no EQ itemfg.i-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE eb THEN
                ASSIGN
                    w-ord.flute      = eb.flute
                    w-ord.test       = eb.test
                    w-ord.cas-no     = eb.cas-no
                    w-ord.pallt-no   = eb.tr-no
                    w-ord.part-dscr2 = eb.part-dscr2 . 

            RUN oe/custxship.p (oe-rel.company,
                oe-rel.cust-no,
                oe-rel.ship-id,
                BUFFER shipto).

            IF AVAILABLE shipto THEN
                ASSIGN
                    w-ord.ship-code  = shipto.ship-id
                    w-ord.ship-name  = shipto.ship-name
                    w-ord.ship-add1  = shipto.ship-add[1]
                    w-ord.ship-add2  = shipto.ship-add[2]
                    w-ord.ship-city  = shipto.ship-city
                    w-ord.ship-state = shipto.ship-state
                    w-ord.ship-ctry  = shipto.country
                    w-ord.ship-zip   = shipto.ship-zip
                    w-ord.broker     = shipto.broker.

            IF AVAILABLE soldto THEN w-ord.sold-ctry = soldto.country.
        
            ASSIGN
                w-ord.pcs        = ls-qty-per-tag
                w-ord.bundle     = TRUNC(ls-total-run-qty / ls-qty-per-tag, 0)
                w-ord.partial    = ls-num-partial
                w-ord.total-unit = ls-total-run-qty
                w-ord.total-tags = ls-num-tags   .
            IF w-ord.partial EQ ? THEN w-ord.partial = 0 .
        
        END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings W-Win 
PROCEDURE pGetSettings PRIVATE :
    /*------------------------------------------------------------------------------
             Purpose: Returns the key NK1 settings for printing FG Labels
             Notes:
            ------------------------------------------------------------------------------*/  
       
    FIND FIRST company WHERE company.company EQ cocode NO-LOCK.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
    ASSIGN
        tb_16ths = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "LOADTAG"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "LOADTAG"
            sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
            sys-ctrl.char-fld = "ASI".
        MESSAGE "System control record NOT found.  Please enter the load tag option"
            UPDATE sys-ctrl.char-fld.
        FIND CURRENT sys-ctrl NO-LOCK.
    END.

    ASSIGN 
        v-loadtag = sys-ctrl.char-fld
        v-mult    = sys-ctrl.int-fld
        v-cas-lab = sys-ctrl.log-fld
        v-tags    = sys-ctrl.dec-fld.
               
    /* gdm - 09210907 */
    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
    IF AVAILABLE sys-ctrl THEN ASSIGN v-bardir       = sys-ctrl.log-fld
            v-bardir-chr   = sys-ctrl.char-fld
            scr-auto-print = sys-ctrl.log-fld.
    /* gdm - 09210907 end */

    DO TRANSACTION:
        {sys/inc/closejob.i FGPost}
        {sys/inc/fgpostgl.i}   
        {sys/ref/oecount.i}
        {sys/inc/sspostfg.i}
        {sys/inc/bardir.i}     
    END.

    ASSIGN 
        v-fgpostgl = fgpostgl.

    /*if v-loadtag eq "TRIAD" then begin_form = 4.  */

    IF v-mult LE 0 THEN v-mult = 1.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FGRECPT"
        NO-LOCK NO-ERROR.
    ASSIGN
        v-fgrecpt = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".

    /* Override of sys/inc/closejob.i */
    IF SSPostFG-log AND SSPostFG-char EQ "LOADTAG" AND v-close-job EQ 0 THEN 
    DO:


        RUN sys/ref/nk1look.p (cocode, "CLOSEJOB", "I", NO, NO, "", "", 
            OUTPUT lcRtnChar, OUTPUT llRecFound).

        /* Orign: v-close-job = int(sys-ctrl.char-fld eq "{1}") + sys-ctrl.int-fld. */
        /* Calculate v-close-job as though sys-ctrl.char-fld was not FGPOST */
        v-close-job = 0 + INT(lcRtnChar).
    END.
    EMPTY TEMP-TABLE tt-fgrctd-created.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateFgItemList W-Win 
PROCEDURE pUpdateFgItemList :
    /*------------------------------------------------------------------------------
          Purpose:     Gets machine list for Job no and Job no2 combination
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcJobNo            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo2           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormNo           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBlankNo          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFGItemList       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPono     LIKE w-ord.cust-po-no NO-UNDO.
    DEFINE VARIABLE dtRelDate LIKE w-ord.rel-date NO-UNDO.
    DEFINE VARIABLE cRelLot   LIKE w-ord.rel-lot# NO-UNDO.
    DEFINE VARIABLE cShipNote LIKE w-ord.ship-notes NO-UNDO.
    DEFINE VARIABLE iRelQty   AS INTEGER   NO-UNDO.
    
        
    DEFINE VARIABLE iOrderNo  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustNo   AS CHARACTER NO-UNDO.
    
    RUN GetFGItemForJob IN hdJobProcs (
        ipcCompany,
        ipcJobNo,
        INT(ipcJobNo2),
        INT(ipcFormNo),
        INT(ipcBlankNo),
        INPUT-OUTPUT opcFGItemList 
        ).

    IF opcFGItemList EQ "" THEN
        ASSIGN
            opcFGItemList                                 = ""
            cb-FGItem:LIST-ITEMS IN FRAME {&FRAME-NAME}   = opcFGItemList
            cb-FGItem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    ELSE
        cb-FGItem:LIST-ITEMS IN FRAME {&FRAME-NAME} = opcFGItemList.
    cb-FGItem:SCREEN-VALUE = ENTRY(1,opcFGItemList).
    
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.i-no EQ cb-FGItem:SCREEN-VALUE NO-ERROR .
    IF AVAILABLE itemfg THEN 
        ASSIGN
            cItemName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = itemfg.i-name
            cItemDesc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = itemfg.part-dscr1.
    
    RUN GetJobHdrDetails IN hdJobProcs (
        ipcCompany,
        ipcJobNo,
        INT(ipcJobNo2),
        INT(ipcFormNo),        
        OUTPUT iOrderNo,
        OUTPUT cCustNo ,
        OUTPUT cItemNo 
        ).
               
    FIND FIRST cust-part WHERE
        cust-part.company  EQ cocode AND
        cust-part.i-no     EQ cItemNo AND             
        cust-part.cust-no  EQ cCustNo
        NO-LOCK NO-ERROR.
        
    FIND FIRST job-hdr NO-LOCK
        WHERE job-hdr.company EQ cocode           
        AND job-hdr.job-no  EQ ipcJobNo
        AND job-hdr.job-no2 EQ INT(ipcJobNo2)  NO-ERROR .
          
      
    IF AVAILABLE job-hdr AND job-hdr.est-no NE "" THEN 
    DO:
        IF job-hdr.ord-no NE 0 THEN 
        DO:
            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ job-hdr.ord-no
                AND oe-ordl.i-no    EQ cb-FGItem:SCREEN-VALUE IN FRAME {&FRAME-NAME}   NO-ERROR .
             
            IF AVAILABLE oe-ordl THEN 
            DO:
         
                RUN get-rel-info (OUTPUT iPono,
                    OUTPUT dtRelDate,
                    OUTPUT cRelLot,
                    OUTPUT cShipNote,
                    OUTPUT iRelQty,
                    INPUT ROWID(job-hdr)).
                IF iRelQty NE 0 THEN 
                DO:
                    ls-total-run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(iRelQty ) .
                END.
                ls-total-run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(oe-ordl.cas-cnt * oe-ordl.cases-unit ) .
          
                ASSIGN           
                    ls-qty-per-tag:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(oe-ordl.cas-cnt)
                    ls-num-tags:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(trunc(INTEGER(ls-total-run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME}) /  integer(oe-ordl.cas-cnt),0 )) .
          
            END.
             
        END.
        ELSE 
        DO:
            FIND FIRST eb
                WHERE eb.company   EQ cocode
                AND eb.est-no    EQ job-hdr.est-no
                AND eb.form-no   EQ job-hdr.frm
                AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
                NO-LOCK NO-ERROR.
              
            IF AVAILABLE eb THEN
                ASSIGN 
                    ls-total-run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(eb.cas-cnt * eb.cas-pal )
                    ls-qty-per-tag:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(eb.cas-cnt)
                    ls-num-tags:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(eb.cas-pal) .
      
        END.     
    END.
          
     
    IF AVAILABLE cust-part AND cust-part.labelPallet <> "" THEN
        scr-label-file = (IF cust-part.labelPallet <> "" THEN cust-part.labelPallet ELSE v-bardir-chr).
     
    ELSE
        IF INT(iOrderNo) NE 0  THEN
        DO:
           
            FIND FIRST oe-rel WHERE
                oe-rel.company EQ cocode AND
                oe-rel.i-no    EQ cItemNo AND                 
                oe-rel.ord-no  EQ INT(iOrderNo)                 
                NO-LOCK NO-ERROR.

            IF AVAILABLE oe-rel THEN 
                FIND FIRST shipto NO-LOCK 
                    WHERE shipto.company EQ cocode 
                    AND shipto.cust-no EQ oe-rel.cust-no 
                    AND shipto.ship-id EQ oe-rel.ship-id 
                    USE-INDEX ship-id NO-ERROR.
            ELSE
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ cocode 
                    AND shipto.cust-no EQ cCustNo 
                    AND shipto.ship-id EQ cCustNo
                    USE-INDEX ship-id NO-ERROR.

            IF AVAILABLE shipto THEN 
            DO:
                /* IF AVAIL oe-rel THEN
                    v-cust-no = oe-rel.cust-no. */

                FIND FIRST sys-ctrl-shipto NO-LOCK
                    WHERE sys-ctrl-shipto.company      EQ cocode 
                    AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                    AND sys-ctrl-shipto.cust-vend    EQ YES 
                    AND sys-ctrl-shipto.cust-vend-no EQ cCustNo 
                    AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id 
                    AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                IF AVAILABLE sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file = sys-ctrl-shipto.char-fld.
                ELSE 
                DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                        WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ cCustNo 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                    IF AVAILABLE sys-ctrl-shipto AND 
                        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                        scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE 
                    DO:
                        FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                            AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                            AND sys-ctrl-shipto.cust-vend-no EQ ""
                            AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                            scr-label-file = sys-ctrl-shipto.char-fld.
                        ELSE 
                        DO:
                            FIND FIRST sys-ctrl WHERE
                                sys-ctrl.company EQ cocode AND
                                sys-ctrl.name    EQ "BARDIR" 
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE sys-ctrl THEN
                                scr-label-file = sys-ctrl.char-fld.
                            ELSE
                                scr-label-file = "".
                        END.
                    END.
                END.
            END.
            ELSE
            DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK 
                    WHERE sys-ctrl-shipto.company      EQ cocode 
                    AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                    AND sys-ctrl-shipto.cust-vend    EQ YES 
                    AND sys-ctrl-shipto.cust-vend-no EQ cCustNo 
                    AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                IF AVAILABLE sys-ctrl-shipto AND 
                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                    scr-label-file = sys-ctrl-shipto.char-fld.
                ELSE 
                DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                        WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                        AND sys-ctrl-shipto.cust-vend-no EQ ""
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        NO-ERROR.
                    IF AVAILABLE sys-ctrl-shipto AND 
                        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                        scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE 
                    DO:
                        FIND FIRST sys-ctrl WHERE
                            sys-ctrl.company EQ cocode AND
                            sys-ctrl.name    EQ "BARDIR" 
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE sys-ctrl THEN
                            scr-label-file = sys-ctrl.char-fld.
                        ELSE
                            scr-label-file = "".
                    END.
                END.
            END.
        END.
        ELSE
            IF INT(iOrderNo) EQ 0     THEN
            DO:
                FIND FIRST shipto WHERE
                    shipto.company EQ cocode AND
                    shipto.cust-no EQ cCustNo AND
                    shipto.ship-id EQ cCustNo
                    NO-LOCK NO-ERROR.

                IF AVAILABLE shipto THEN 
                DO:

                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company      EQ cocode AND
                        sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                        sys-ctrl-shipto.cust-vend    EQ YES AND
                        sys-ctrl-shipto.cust-vend-no EQ cCustNo AND
                        sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                        sys-ctrl-shipto.char-fld     NE ''
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE sys-ctrl-shipto AND 
                        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                        scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE 
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company      EQ cocode AND
                            sys-ctrl-shipto.NAME         EQ "BARDIR" AND 
                            sys-ctrl-shipto.cust-vend    EQ YES AND
                            sys-ctrl-shipto.cust-vend-no EQ cCustNo AND
                            sys-ctrl-shipto.char-fld     NE ''
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                            scr-label-file = sys-ctrl-shipto.char-fld.
                        ELSE 
                        DO:
                            FIND FIRST sys-ctrl-shipto NO-LOCK 
                                WHERE sys-ctrl-shipto.company      EQ cocode 
                                AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                                AND sys-ctrl-shipto.cust-vend-no EQ ""
                                AND sys-ctrl-shipto.cust-vend    EQ YES 
                                NO-ERROR.
                            IF AVAILABLE sys-ctrl-shipto AND 
                                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                scr-label-file = sys-ctrl-shipto.char-fld.
                            ELSE 
                            DO:
                                FIND FIRST sys-ctrl WHERE
                                    sys-ctrl.company EQ cocode AND
                                    sys-ctrl.name    EQ "BARDIR" 
                                    NO-LOCK NO-ERROR.
                                IF AVAILABLE sys-ctrl THEN
                                    scr-label-file = sys-ctrl.char-fld.
                                ELSE
                                    scr-label-file = "".
                            END.
                        END.
                    END.
                END.
                ELSE
                DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                        WHERE sys-ctrl-shipto.company      EQ cocode 
                        AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                        AND sys-ctrl-shipto.cust-vend    EQ YES 
                        AND sys-ctrl-shipto.cust-vend-no EQ cCustNo 
                        AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                    IF AVAILABLE sys-ctrl-shipto AND 
                        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                        scr-label-file = sys-ctrl-shipto.char-fld.
                    ELSE 
                    DO:
                        FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                            AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                            AND sys-ctrl-shipto.cust-vend-no EQ ""
                            AND sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                            scr-label-file = sys-ctrl-shipto.char-fld.
                        ELSE 
                        DO:
                            FIND FIRST sys-ctrl WHERE
                                sys-ctrl.company EQ cocode AND
                                sys-ctrl.name    EQ "BARDIR" 
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE sys-ctrl THEN
                                scr-label-file = sys-ctrl.char-fld.
                            ELSE
                                scr-label-file = "".
                        END.
                    END.
                END.


            END. /*begin_ord-no and end_ord-no eq 0*/
           
    scr-label-file = IF scr-label-file <> "" THEN scr-label-file ELSE v-bardir-chr.
         
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pXprintTag W-Win 
PROCEDURE pXprintTag :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/    
    IF cBarCodeProgram EQ "" THEN 
    DO:    
        RUN AutoPrint.
    END.
    ELSE IF cBarCodeProgram EQ "xprint" /*AND scr-auto-print*/ THEN 
        DO: 
            PAUSE 1.
            RUN "oerep/LoadtagProcs.p" PERSISTENT SET hLoadtagProcs.
            RUN pPrintView IN hLoadtagProcs ( scr-label-file /*"loadtag.xpr"*/ , YES /*tb_print-view*/ ).
            DELETE OBJECT hLoadtagProcs.
        END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

/* SEND-RECORDS does nothing because there are no External
   Tables specified for this SmartWindow, and there are no
   tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
    /* -----------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateComboBoxes W-Win 
PROCEDURE updateComboBoxes :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/    
    ASSIGN
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListItems = ""
        cMachineListItems = "".
        
    RUN GetSecondaryJobForJob IN hdJobProcs (
        ipcCompany,
        cFormattedJobno,
        INPUT-OUTPUT cJobno2ListItems
        ).
    
    DO iCount = 1 TO NUM-ENTRIES(cJobno2ListItems):
        RUN GetFormnoForJob IN hdJobProcs (
            ipcCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cFormnoListItems
            ).
    
        RUN GetBlanknoForJob IN hdJobProcs (
            ipcCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cBlanknoListItems
            ).

    END.
    
    IF cJobno2ListItems EQ "" THEN
        ASSIGN 
            cJobno2ListItems                              = "00"
            cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cJobno2ListItems 
            cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME} = cJobno2ListItems.
        
    APPLY "VALUE-CHANGED" TO cb-jobno2.
    
    IF cFormnoListItems EQ "" THEN
        ASSIGN
            cFormnoListItems                              = "00"
            cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cFormnoListItems 
            cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cFormnoListItems.

    IF cBlanknoListItems EQ "" THEN
        ASSIGN
            cBlanknoListItems                              = "00"
            cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cBlanknoListItems
            cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cBlanknoListItems.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE write-loadtag-line W-Win 
PROCEDURE write-loadtag-line :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipc-rfid AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipc-totalUnit AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipi-pallet-id AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipi-counter AS INTEGER NO-UNDO.
    DEFINE VARIABLE cLoftString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCtr        AS INTEGER   NO-UNDO.
 
 
 
 

    IF cBarCodeProgram EQ "Loftware" THEN 
    DO:
            
        ASSIGN
            cLoftString           = FILL(",",86)
            ENTRY(1,cLoftString)  = "!" +  scr-label-file
            ENTRY(2,cLoftString)  = '"' + STRING(REPLACE(REPLACE(w-ord.cust-name,","," "),'"',""),"x(30)") + '"'
            ENTRY(3,cLoftString)  = '"' + STRING(REPLACE(REPLACE(w-ord.cust-part-no,","," "),'"',""),"x(30)") + '"'
            ENTRY(4,cLoftString)  = '"' + STRING(REPLACE(REPLACE(w-ord.cust-po-no,","," "),'"',""),"x(15)") + '"'
            ENTRY(5,cLoftString)  = '"' + STRING(REPLACE(REPLACE(w-ord.ship-city,","," "),'"',"") + " " + REPLACE(w-ord.ship-state,'"',""),"x(30)") + '"'
            ENTRY(6,cLoftString)  = '"' + STRING(w-ord.est-no,"9999999") + '"'
            ENTRY(7,cLoftString)  = STRING(w-ord.gross-wt,">>>>9.99")
            ENTRY(9,cLoftString)  = "- -"
            ENTRY(10,cLoftString) = "1"
            ENTRY(11,cLoftString) = "1"
            ENTRY(12,cLoftString) = STRING(w-ord.ord-no,">>>>>>9")
            ENTRY(13,cLoftString) = STRING(w-ord.total-unit,">>>>>9")
            ENTRY(14,cLoftString) = STRING(w-ord.due-date,"99/99/99")
            ENTRY(24,cLoftString) = '"' + STRING(REPLACE(w-ord.cust-no,'"',""),"x(5)") + STRING(REPLACE(w-ord.ship-code,'"',""),"x(3)") + '"'
            ENTRY(30,cLoftString) = '"' + STRING(REPLACE(REPLACE(w-ord.ship-name,","," "),'"',""),"x(60)") + '"'
            ENTRY(31,cLoftString) = '"' + STRING(REPLACE(w-ord.i-no,'"',""),"x(23)") + '"'
            ENTRY(32,cLoftString) = '"' + STRING(REPLACE(REPLACE(w-ord.i-name,","," "),'"',""),"x(30)") + '"'
            ENTRY(37,cLoftString) = '"' + STRING(REPLACE(REPLACE(w-ord.ship-add1,","," "),'"',"") + " " + REPLACE(REPLACE(w-ord.ship-add2,","," "),'"',""),"x(30)") + '"'
            ENTRY(38,cLoftString) = "1"
            ENTRY(39,cLoftString) = STRING(w-ord.pcs,">>>>9")
            ENTRY(41,cLoftString) = TRIM(STRING(loadtag.tag-no,"x(64)"))
            ENTRY(42,cLoftString) = '"' + STRING(REPLACE(REPLACE(w-ord.ord-desc1,","," "),'"',""),"x(64)") + '"'
            ENTRY(44,cLoftString) = '"' + STRING(REPLACE(REPLACE(w-ord.style-desc,","," "),'"',""),"x(15)") + '"'
            ENTRY(45,cLoftString) = STRING(w-ord.box-len,">>>9.99<<<")
            ENTRY(46,cLoftString) = STRING(w-ord.box-wid,">>>9.99<<<")
            ENTRY(47,cLoftString) = STRING(w-ord.box-dep,">>>9.99<<<")
            ENTRY(68,cLoftString) = STRING(w-ord.bundle,">>>9")
            .
    
        PUT UNFORMATTED  
            cLoftString.
    END.
    ELSE 
    DO:

        PUT UNFORMATTED 
            "~""  removeChars(w-ord.cust-name)  "~","
            w-ord.ord-no  ","
            "~""  v-job  "~","
            "~""  CAPS(removeChars(w-ord.i-no))  FORM "x(15)" "~","
            "~""  removeChars(w-ord.cust-part-no) "~","
            "~""  removeChars(w-ord.cust-po-no)  "~","
            w-ord.pcs  ","
            w-ord.bundle  ","
            TRIM(ipc-totalUnit) ","
            "~""  removeChars(w-ord.ship-code)  "~","
            "~""  removeChars(w-ord.ship-name)  "~","
            "~""  removeChars(w-ord.ship-add1)  "~","
            "~""  removeChars(w-ord.ship-add2)  "~","
            "~""  removeChars(w-ord.ship-city)  "~","
            "~""  removeChars(w-ord.ship-state) "~","
            "~""  removeChars(w-ord.ship-ctry)  "~","
            "~""  removeChars(w-ord.ship-zip)   "~","
            "~""  removeChars(w-ord.sold-code)  "~","
            "~""  removeChars(w-ord.sold-name)  "~","
            "~""  removeChars(w-ord.sold-add1)  "~","
            "~""  removeChars(w-ord.sold-add2)  "~","
            "~""  removeChars(w-ord.sold-city)  "~","
            "~""  removeChars(w-ord.sold-state) "~","
            "~""  removeChars(w-ord.sold-ctry)  "~","
            "~""  removeChars(w-ord.sold-zip)   "~","
            "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","
            "~""  w-ord.due-date  "~","
            "~""  w-ord.rel-date  "~","
            "~""  w-ord.upc-no FORMAT "x(20)" "~","
            "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","
            "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","
            "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","
            "~""  w-ord.flute  "~","
            "~""  w-ord.test  "~","
            "~""  w-ord.vendor  "~","
            w-ord.gross-wt  ","
            w-ord.tare-wt  ","
            w-ord.net-wt  ","
            w-ord.sheet-wt  ","
            "~""  w-ord.uom  "~","
            "~""  removeChars(w-ord.style) "~","
            "~""  removeChars(w-ord.style-desc) "~","
            "~""  removeChars(w-ord.rel-lot#) "~","
            "~""  lv-middlesex-job  "~","
            "~""  lv-middlesex-po  "~","
            "~""  loadtag.tag-no "~"," 
            "~""  loadtag.partial "~","
            "~""  w-ord.cas-no  "~","
            "~""  removeChars(v-dept-note[1]) "~","
            "~""  removeChars(v-dept-note[2]) "~","
            "~""  removeChars(v-dept-note[3]) "~","
            "~""  removeChars(v-dept-note[4]) "~","
            "~""  removeChars(v-dept-note[5]) "~","
            "~""  removeChars(v-dept-note[6]) "~","
            "~""  removeChars(v-dept-note[7]) "~","
            "~""  removeChars(v-dept-note[8]) "~","
            w-ord.po-no ","
            "~""  removeChars(v-dept-note[9]) "~","
            "~""  removeChars(v-dept-note[10]) "~","
            "~""  removeChars(v-dept-note[11]) "~","
            "~""  removeChars(v-dept-note[12]) "~","
            "~""  removeChars(v-dept-note[13]) "~","
            "~""  removeChars(v-dept-note[14]) "~","
            "~""  removeChars(v-dept-note[15]) "~","
            "~""  removeChars(v-dept-note[16]) "~","   
            "~""  removeChars(v-dept-note[17]) "~","
            "~""  removeChars(v-dept-note[18]) "~","
            "~""  removeChars(w-ord.est-no) "~","
            "~""  removeChars(w-ord.ord-desc1)    "~","
            "~""  removeChars(w-ord.ord-desc2)    "~","
            .
 
        IF CAN-DO("ASI,SSLABEL",v-loadtag) THEN 
        DO:
            PUT UNFORMATTED 
                "~"" SUBSTR(loadtag.tag-no,16,5) "~"," 
                "~"" ipc-rfid "~"," .
        END.

        PUT UNFORMATTED 
            "~"" w-ord.due-date-jobhdr "~"," 
            "~"" w-ord.due-date-job "~","
            "~"" w-ord.linenum "~","
            "~"" w-ord.unit-wt  "~","
            "~"" w-ord.pallt-wt  "~","          
            "~"" removeChars(v-fgdsc1) "~","
            "~"" removeChars(v-fgdsc2) "~","
            "~"" removeChars(v-fgdsc3) "~","
            "~"" removeChars(w-ord.lot) "~","
            "~"" w-ord.pallt-no "~"," 
            "~"" ipi-pallet-id "~","
            "~"" ipi-counter "~","
            "~"" w-ord.total-tags "~","
            "~"" REPLACE(w-ord.ship-notes[1],'"', '') "~","
            "~"" REPLACE(w-ord.ship-notes[2],'"', '') "~","
            "~"" REPLACE(w-ord.ship-notes[3],'"', '') "~","
            "~"" REPLACE(w-ord.ship-notes[4],'"', '') "~","
            "~"" loadtag.loc "~","
            "~"" loadtag.loc-bin "~","
            "~"" w-ord.job-qty "~","
            "~"" STRING(w-ord.runShip,"R&S/WHSE")  "~"," 
            "~"" STRING(loadtag.pallet-no)  "~"," 
            "~"" STRING(w-ord.zone)  "~","
            "~"" loadtag.createUser "~","
            "~"" STRING(loadtag.tag-date,"99/99/9999") "~","
            "~"" STRING(loadtag.tag-time,"HH:MM AM") "~","
            "~"" STRING(TODAY,"99/99/9999") "~","
            "~"" STRING(TIME,"HH:MM AM") "~" ".
        .
    
        IF lSSCC THEN PUT UNFORMATTED ",~"" w-ord.sscc "~"".
    END.

    PUT UNFORMATTED SKIP.   

    /* temp table for xprint */     
    IF cBarCodeProgram EQ "xprint" THEN 
    DO:
        CREATE tt-word-print .
        BUFFER-COPY w-ord TO tt-word-print .
        ASSIGN 
            tt-word-print.tag-no = loadtag.tag-no .
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE component-qty-check C-Win 
PROCEDURE component-qty-check :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRctdRow AS ROWID NO-UNDO.
    DEFINE VARIABLE li-max-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ll         AS LOG     NO-UNDO.
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.

    FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ iprRctdRow 
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-fg-rctd THEN
        RETURN ERROR.

    IF itemfg.isaset                                                        AND
        (itemfg.alloc EQ NO                OR
        (itemfg.alloc EQ YES      AND
        fgrecpt-char NE "Manual" AND
        TRIM(bf-fg-rctd.job-no) NE "")) THEN
    DO:
        ASSIGN
            bf-fg-rctd.t-qty = bf-fg-rctd.cases *
                 bf-fg-rctd.qty-case +
                 bf-fg-rctd.partial
            li-max-qty       = bf-fg-rctd.t-qty.

        RUN fg/checksetb.p (ROWID(itemfg),
            ROWID(bf-fg-rctd),
            bf-fg-rctd.job-no,
            INT(bf-fg-rctd.job-no2),
            INPUT bf-fg-rctd.loc,
            INPUT-OUTPUT li-max-qty).

        IF li-max-qty LT bf-fg-rctd.t-qty THEN 
        DO:
            ll = NO.

            IF li-max-qty GT 0 AND NOT gvlCreateWithMaxPrompted THEN
                MESSAGE "Create receipt with maximum quantity available?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

            /* Only prompt on the first tag */
            gvlCreateWithMaxPrompted = YES.
            IF ll THEN 
            DO:

                ASSIGN
                    bf-fg-rctd.t-qty   = li-max-qty
                    bf-fg-rctd.cases   = TRUNC((li-max-qty - DEC(bf-fg-rctd.partial)) /
                           DEC(bf-fg-rctd.qty-case),0)
                    bf-fg-rctd.partial = li-max-qty - (DEC(bf-fg-rctd.cases) *
                                   DEC(bf-fg-rctd.qty-case)).
                /* Instead of 0 at 500 with -300 partial, make it */
                /* 1 at -300 with 0 partial 12101418 */
                IF fg-rctd.cases EQ 0 AND fg-rctd.partial NE 0 THEN
                    ASSIGN 
                        fg-rctd.cases    = (IF  fg-rctd.partial LT 0 THEN -1 ELSE 1)
                        fg-rctd.qty-case = (IF fg-rctd.partial LT 0 THEN - fg-rctd.partial ELSE fg-rctd.partial)
                        fg-rctd.partial  = 0
                        .
            END. /* if ll */
            IF NOT ll OR li-max-qty EQ 0 THEN 
            DO:   

                ASSIGN
                    gvcSkippedJob  = bf-fg-rctd.job-no + "-" + STRING(bf-fg-rctd.job-no2)
                    gvcSkippedItem = itemfg.i-no.
                RETURN ERROR. 
            END.

        END. /* if over qty */
    END. /* if isaset */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-ext-cost C-Win 
PROCEDURE calc-ext-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-len       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
    DEFINE VARIABLE v-rec-qty   AS INTEGER NO-UNDO.


    IF NOT AVAILABLE fg-rctd THEN RETURN.  /* no records */


    FIND itemfg WHERE itemfg.company EQ cocode AND itemfg.i-no  EQ fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.

    ASSIGN
        lv-cost-uom = itemfg.prod-uom
        v-bwt       = 0
        v-len       = itemfg.t-len
        v-wid       = itemfg.t-wid
        v-dep       = 0.

    FIND FIRST po-ordl WHERE po-ordl.company = fg-rctd.company
        AND po-ordl.po-no = int(fg-rctd.po-no)
        AND po-ordl.i-no  = fg-rctd.i-no
        AND po-ordl.job-no = fg-rctd.job-no
        AND po-ordl.job-no2 = fg-rctd.job-no2
        AND po-ordl.item-type = NO
        NO-LOCK NO-ERROR.

    IF AVAILABLE po-ordl THEN 
    DO:
        ASSIGN
            v-len = po-ordl.s-len
            v-wid = po-ordl.s-wid.
    END.

    ASSIGN 
        lv-out-qty  = fg-rctd.t-qty
        lv-out-cost = fg-rctd.std-cost.

    IF fg-rctd.cost-uom NE lv-cost-uom THEN
        RUN rm/convcuom.p(fg-rctd.cost-uom, lv-cost-uom,                   
            v-bwt, v-len, v-wid, v-dep,
            fg-rctd.std-cost, OUTPUT lv-out-cost).

    IF lv-cost-uom NE "EA" THEN
        RUN rm/convquom.p("EA", lv-cost-uom,                   
            v-bwt, v-len, v-wid, v-dep,
            lv-out-qty, OUTPUT lv-out-qty).
    ASSIGN 
        fg-rctd.ext-cost = (lv-out-qty * lv-out-cost) + fg-rctd.frt-cost.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars W-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~"".
    DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
    DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE k            AS INTEGER   NO-UNDO.

    /*k = NUM-ENTRIES(invalidChars).
    DO i = 1 TO k: */

    ipField = REPLACE(ipField,ENTRY(1,invalidChars),ENTRY(1,replaceChars)).
    /*END.*/
    RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

