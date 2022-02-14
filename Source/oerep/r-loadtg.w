&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-loadtg.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name     AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE scanAgain     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lv-got-shipto AS LOGICAL   NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom\windows.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
{custom/xprint.i}
ASSIGN
    cocode = gcompany
    locode = gloc.

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
DEFINE VARIABLE lv-rd_print    AS CHARACTER NO-UNDO.

/* 9812 CAH: */
DEFINE VARIABLE v-loadtag      AS CHARACTER NO-UNDO INIT "ASI".  /* sys ctrl option */
DEFINE VARIABLE v-mult         AS INTEGER   NO-UNDO INIT 0.  /* sys ctrl option */
DEFINE VARIABLE v-cas-lab      AS LOG       NO-UNDO.  /* sys ctrl option */
DEFINE VARIABLE v-tags         AS DECIMAL   NO-UNDO INIT 0.  /* sys ctrl option */
DEFINE VARIABLE v-count        AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE v-fgrecpt      AS LOG       NO-UNDO.  /* sys ctrl option */
DEFINE VARIABLE glOverrideMult AS LOGICAL   NO-UNDO.

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
DEFINE VARIABLE lv-text                  AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE UserlabelPath AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE cBarCodeProgram  AS CHARACTER NO-UNDO .
DEFINE VARIABLE i-bardir-int     AS INTEGER   NO-UNDO .
DEFINE VARIABLE i-xprint-int     AS INTEGER   NO-UNDO .
DEFINE VARIABLE hdOutputProcs    AS HANDLE.

DEFINE VARIABLE lFGTagValidation AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGTagValidation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lLoadTagLimit    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLoadtag         AS INTEGER   NO-UNDO.

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

RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "LoadTagLimit",
    INPUT "L",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cResult,
    OUTPUT lFound).
IF lFound THEN
    lLoadTagLimit = LOGICAL(cResult) NO-ERROR. 
    
RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "LoadTag",
    INPUT "I",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cResult,
    OUTPUT lFound).
IF lFound THEN
    iLoadtag = INTEGER(cResult) NO-ERROR.    

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

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbPartSelect loadtagFunction tb_ret ~
tb_reprint-tag v-ord-list v-job-list begin_ord-no end_ord-no begin_job ~
begin_job2 end_job end_job2 begin_i-no end_i-no rd_order-sts rd_print ~
begin_date end_date rd_comps tb_dept-note tb_rel tb_over tb_16ths ~
tb_ship-id scr-auto-print scr-freeze-label scr-label-file begin_labels ~
begin_form btn-ok btn-cancel tb_xfer-lot tb_override-mult begin_ship-to ~
end_ship-to tb_close tb_print-view begin_rel end_rel RECT-7 RECT-8 RECT-11 ~
RECT-12 tbAutoClose 
&Scoped-Define DISPLAYED-OBJECTS tbPartSelect loadtagFunction tb_ret ~
tb_reprint-tag v-ord-list v-job-list begin_ord-no begin_poLine end_ord-no ~
end_poLine begin_job begin_job2 end_job end_job2 begin_i-no end_i-no ~
rd_order-sts rd_print begin_date end_date rd_comps v-dept-list tb_dept-note ~
tb_rel tb_over tb_16ths tb_ship-id v-ship-id scr-auto-print ~
scr-freeze-label scr-label-file begin_labels begin_form begin_filename ~
typeLabel statusLabel lbl_po-no tb_xfer-lot tb_override-mult begin_ship-to ~
end_ship-to tb_close tb_print-view begin_rel end_rel tbAutoClose 

/* Custom List Definitions                                              */
/* jobFields,NonReprint,List-3,List-4,List-5,F1                         */
&Scoped-define jobFields tb_ret v-job-list begin_job begin_job2 end_job ~
end_job2 tb_rel tb_over 
&Scoped-define NonReprint loadtagFunction tb_ret v-ord-list v-job-list ~
begin_ord-no end_ord-no begin_job begin_job2 end_job end_job2 begin_i-no ~
end_i-no rd_order-sts rd_print begin_date end_date rd_comps tb_rel tb_over ~
tb_16ths tb_ship-id v-ship-id begin_filename tb_xfer-lot tb_override-mult ~
begin_ship-to end_ship-to begin_rel end_rel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD win_normalizePath C-Win 
FUNCTION win_normalizePath RETURNS CHARACTER
    ( pcPath AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE v-job-list      AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 42 BY 2.43 NO-UNDO.

DEFINE VARIABLE v-ord-list      AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 42 BY 2.43 NO-UNDO.

DEFINE VARIABLE begin_date      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "From" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_filename  AS CHARACTER FORMAT "X(256)":U INITIAL "ccc" 
    LABEL "Text File Path" 
    VIEW-AS FILL-IN 
    SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE begin_form      AS INTEGER   FORMAT ">>>":U INITIAL 1 
    LABEL "Printer Form#" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no      AS CHARACTER FORMAT "X(15)":U 
    LABEL "From Item#" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job       AS CHARACTER FORMAT "X(9)":U 
    LABEL "From Job#" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2      AS INTEGER   FORMAT "999":U INITIAL 0 
    LABEL "-" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_labels    AS INTEGER   FORMAT ">>>>":U INITIAL 2 
    LABEL "# of Labels per Skid" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "From Order#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE begin_poLine    AS INTEGER   FORMAT ">9":U INITIAL 0 
    LABEL "Ln" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rel       AS INTEGER   FORMAT ">>>>>>9":U INITIAL 0 
    LABEL "From Release" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship-to   AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship To From" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date        AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "To" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no        AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "To Item#" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE end_job         AS CHARACTER FORMAT "X(9)":U 
    LABEL "To Job#" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE end_job2        AS INTEGER   FORMAT "999":U INITIAL 999 
    LABEL "-" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "To Order#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE end_poLine      AS INTEGER   FORMAT ">9":U INITIAL 0 
    LABEL "Ln" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_rel         AS INTEGER   FORMAT ">>>>>>9":U INITIAL 9999999 
    LABEL "To Release" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship-to     AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "To" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cas-lab      AS CHARACTER FORMAT "X(30)":U 
    LABEL "Scan Case Label" 
    VIEW-AS FILL-IN 
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_po-no       AS CHARACTER FORMAT "X(256)":U INITIAL "Print PO from:" 
    VIEW-AS TEXT 
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE scr-label-file  AS CHARACTER FORMAT "X(256)":U 
    LABEL "Print Format" 
    VIEW-AS FILL-IN 
    SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE statusLabel     AS CHARACTER FORMAT "X(256)":U INITIAL "Order/Job Status:" 
    VIEW-AS TEXT 
    SIZE 18 BY .76 NO-UNDO.

DEFINE VARIABLE typeLabel       AS CHARACTER FORMAT "X(256)":U INITIAL " Enter Orders separated by comma" 
    VIEW-AS TEXT 
    SIZE 42 BY .62 NO-UNDO.

DEFINE VARIABLE v-dept-list     AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 25.4 BY 1 NO-UNDO.

DEFINE VARIABLE v-ship-id       AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE loadtagFunction AS CHARACTER INITIAL "Order" 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "Job/Order Receipt", "Order",
    "Purchased Item Receipt", "PO"
    SIZE 29 BY 1.76 NO-UNDO.

DEFINE VARIABLE rd_comps        AS CHARACTER INITIAL "B" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Assembled", "A",
    "Unassembled", "U",
    "Both", "B"
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE rd_order-sts    AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "O",
    "Closed", "C",
    "All", "A"
    SIZE 47 BY .71 NO-UNDO.

DEFINE VARIABLE rd_print        AS CHARACTER INITIAL "H" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Header", "H",
    "Line", "L",
    "Job#", "J",
    "Release", "R"
    SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 99 BY 6.43.

DEFINE RECTANGLE RECT-12
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 99 BY 4.67.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 99 BY 3.1.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 99 BY 9.05
    BGCOLOR 0 FGCOLOR 0 .

DEFINE VARIABLE scr-auto-print   AS LOGICAL INITIAL NO 
    LABEL "Auto Print Label?" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.8 BY .86 NO-UNDO.

DEFINE VARIABLE scr-freeze-label AS LOGICAL INITIAL NO 
    LABEL "Freeze Label File Choice?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY .86 NO-UNDO.

DEFINE VARIABLE tbAutoClose      AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tbPartSelect     AS LOGICAL INITIAL NO 
    LABEL "Select Components" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tb_16ths         AS LOGICAL INITIAL NO 
    LABEL "Show LWD in 16ths?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_close         AS LOGICAL INITIAL NO 
    LABEL "Close?" 
    VIEW-AS TOGGLE-BOX
    SIZE 12 BY .86 NO-UNDO.

DEFINE VARIABLE tb_dept-note     AS LOGICAL INITIAL NO 
    LABEL "Print Department Notes?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_over          AS LOGICAL INITIAL NO 
    LABEL "Include Overrun?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_override-mult AS LOGICAL INITIAL NO 
    LABEL "Ignore Customer Labels/ Skid" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-view    AS LOGICAL INITIAL NO 
    LABEL "Preview?" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.6 BY .86 NO-UNDO.

DEFINE VARIABLE tb_rel           AS LOGICAL INITIAL NO 
    LABEL "Print Posted Release in BOL File?" 
    VIEW-AS TOGGLE-BOX
    SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint-tag   AS LOGICAL INITIAL NO 
    LABEL "&Reprint Tag?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ret           AS LOGICAL INITIAL NO 
    LABEL "Returns?" 
    VIEW-AS TOGGLE-BOX
    SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ship-id       AS LOGICAL INITIAL NO 
    LABEL "Print Ship ID?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tb_xfer-lot      AS LOGICAL INITIAL NO 
    LABEL "Transfer Release Lot#" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tbPartSelect AT ROW 16.81 COL 73.4 WIDGET-ID 32
    loadtagFunction AT ROW 2.19 COL 36 RIGHT-ALIGNED NO-LABELS
    tb_ret AT ROW 2.19 COL 41
    tb_reprint-tag AT ROW 2.19 COL 59
    fi_cas-lab AT ROW 3.14 COL 57 COLON-ALIGNED
    v-ord-list AT ROW 5.95 COL 8 NO-LABELS
    v-job-list AT ROW 5.95 COL 56 NO-LABELS
    begin_ord-no AT ROW 8.62 COL 20 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    begin_poLine AT ROW 8.62 COL 45 COLON-ALIGNED WIDGET-ID 34
    end_ord-no AT ROW 8.62 COL 64 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    end_poLine AT ROW 8.62 COL 89 COLON-ALIGNED WIDGET-ID 34
    begin_job AT ROW 9.76 COL 20 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job2 AT ROW 9.76 COL 36 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job AT ROW 9.76 COL 64 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job2 AT ROW 9.76 COL 80 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    begin_i-no AT ROW 10.91 COL 20 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_i-no AT ROW 10.91 COL 64 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    rd_order-sts AT ROW 13.14 COL 22.6 NO-LABELS
    rd_print AT ROW 14.81 COL 18 NO-LABELS
    begin_date AT ROW 14.76 COL 61.2 COLON-ALIGNED HELP
    "Enter Beginning Release Date"
    end_date AT ROW 14.76 COL 82.6 COLON-ALIGNED HELP
    "Enter Ending Release Date"
    rd_comps AT ROW 16.71 COL 31 NO-LABELS
    v-dept-list AT ROW 18.38 COL 30.6 COLON-ALIGNED NO-LABELS
    tb_dept-note AT ROW 18.48 COL 4
    tb_rel AT ROW 19.43 COL 4
    tb_over AT ROW 19.43 COL 45
    tb_16ths AT ROW 19.43 COL 73
    tb_ship-id AT ROW 18.33 COL 59.4 WIDGET-ID 24
    v-ship-id AT ROW 18.33 COL 75.8 COLON-ALIGNED NO-LABELS WIDGET-ID 26
    scr-auto-print AT ROW 21.33 COL 18.4 WIDGET-ID 2
    scr-freeze-label AT ROW 21.33 COL 39.6 WIDGET-ID 4
    scr-label-file AT ROW 22.29 COL 24 COLON-ALIGNED WIDGET-ID 6
    begin_labels AT ROW 23.43 COL 24 COLON-ALIGNED
    begin_form AT ROW 23.38 COL 84 COLON-ALIGNED
    begin_filename AT ROW 24.57 COL 24 COLON-ALIGNED
    btn-ok AT ROW 26.76 COL 32.4
    btn-cancel AT ROW 26.76 COL 56.6
    typeLabel AT ROW 5.29 COL 6 COLON-ALIGNED NO-LABELS
    statusLabel AT ROW 13.1 COL 4.4 NO-LABELS
    lbl_po-no AT ROW 14.81 COL 4 HELP
    "Print Customer's PO Number from Header, Line item or Release" NO-LABELS
    tb_xfer-lot AT ROW 17.52 COL 4 WIDGET-ID 28
    tb_override-mult AT ROW 23.43 COL 35 WIDGET-ID 34
    begin_ship-to AT ROW 15.67 COL 61.2 COLON-ALIGNED HELP
    "Enter Beginning Release Shipto" WIDGET-ID 36
    end_ship-to AT ROW 15.67 COL 82.6 COLON-ALIGNED HELP
    "Enter Ending Release Shipto" WIDGET-ID 38
    tb_close AT ROW 21.33 COL 69 WIDGET-ID 40
    tb_print-view AT ROW 21.33 COL 81.4 WIDGET-ID 42
    begin_rel AT ROW 12.05 COL 20 COLON-ALIGNED HELP
    "Enter Beginning Release Number" WIDGET-ID 44
    end_rel AT ROW 12.05 COL 64 COLON-ALIGNED HELP
    "Enter Ending Release Number" WIDGET-ID 46
    tbAutoClose AT ROW 25.86 COL 32.6 WIDGET-ID 64
    " Output Options:" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 20.71 COL 4 WIDGET-ID 22
    FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 106.2 BY 30.33
    BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
    "Print Set Components for:" VIEW-AS TEXT
    SIZE 26 BY 1 AT ROW 16.67 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    " Enter Jobs separated by comma" VIEW-AS TEXT
    SIZE 42 BY .62 AT ROW 5.29 COL 56
    " Data Parameters:" VIEW-AS TEXT
    SIZE 20 BY .62 AT ROW 4.57 COL 4 WIDGET-ID 10
    FONT 6
    " Print Options:" VIEW-AS TEXT
    SIZE 16 BY .62 AT ROW 14.1 COL 4 WIDGET-ID 14
    FONT 6
    RECT-7 AT ROW 1.48 COL 3
    RECT-8 AT ROW 5 COL 3 WIDGET-ID 8
    RECT-11 AT ROW 14.33 COL 3 WIDGET-ID 18
    RECT-12 AT ROW 21.1 COL 3 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 106.2 BY 30.33
    BGCOLOR 15 .


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
        TITLE              = "Loadtag Creation"
        HEIGHT             = 27.14
        WIDTH              = 103
        MAX-HEIGHT         = 53.71
        MAX-WIDTH          = 384
        VIRTUAL-HEIGHT     = 53.71
        VIRTUAL-WIDTH      = 384
        MAX-BUTTON         = NO
        RESIZE             = NO
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN begin_date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_filename IN FRAME FRAME-A
   NO-ENABLE 2                                                          */
ASSIGN 
    begin_filename:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_form:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_i-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN begin_job IN FRAME FRAME-A
   1 2                                                                  */
/* SETTINGS FOR FILL-IN begin_job2 IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    begin_labels:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN begin_ord-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN begin_poLine IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN begin_rel IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN begin_ship-to IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    begin_ship-to:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

/* SETTINGS FOR FILL-IN end_date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN end_i-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN end_job IN FRAME FRAME-A
   1 2                                                                  */
/* SETTINGS FOR FILL-IN end_job2 IN FRAME FRAME-A
   1 2                                                                  */
/* SETTINGS FOR FILL-IN end_ord-no IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN end_poLine IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_rel IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN end_ship-to IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    end_ship-to:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fi_cas-lab IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    fi_cas-lab:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lbl_po-no IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
    lbl_po-no:PRIVATE-DATA IN FRAME FRAME-A = "rd_print".

/* SETTINGS FOR RADIO-SET loadtagFunction IN FRAME FRAME-A
   ALIGN-R 2                                                            */
/* SETTINGS FOR RADIO-SET rd_comps IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    rd_comps:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_order-sts IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    rd_order-sts:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd_print IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    rd_print:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    scr-auto-print:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    scr-freeze-label:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    scr-label-file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN statusLabel IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_16ths IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tb_16ths:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_close:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_dept-note:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_over IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    tb_over:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_override-mult IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tb_override-mult:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_print-view:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_rel IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    tb_rel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ret IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    tb_ret:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ship-id IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tb_ship-id:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_xfer-lot IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tb_xfer-lot:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN typeLabel IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-dept-list IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    v-dept-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR EDITOR v-job-list IN FRAME FRAME-A
   1 2                                                                  */
ASSIGN 
    v-job-list:RETURN-INSERTED IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR EDITOR v-ord-list IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    v-ord-list:RETURN-INSERTED IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN v-ship-id IN FRAME FRAME-A
   NO-ENABLE 2                                                          */
ASSIGN 
    v-ship-id:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Loadtag Creation */
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
ON WINDOW-CLOSE OF C-Win /* Loadtag Creation */
    DO:
        /*
        IF INDEX(program-name(4),"asiLogin") <> 0 THEN
            RUN system/userLogOut.p (NO, 0).
         */
        /* This event will close the window and terminate the procedure.  */
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
        
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
    DO:
        DEFINE VARIABLE lv-handle AS HANDLE    NO-UNDO.
        DEFINE VARIABLE char-val  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE rec-val   AS RECID     NO-UNDO.
        DEFINE VARIABLE cCustNo   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE lv-po-no  AS CHARACTER NO-UNDO.

        ASSIGN
            begin_ord-no end_ord-no
            begin_job begin_job2
            end_job end_job2
            begin_i-no     
            end_i-no     

            .

        IF begin_job NE "" AND LENGTH(begin_job) LT 9 THEN
            begin_job = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job)) .
        IF end_job NE "" AND LENGTH(end_job) LT 9 THEN
            end_job = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job)) .

        lv-handle = FOCUS:HANDLE.

        CASE FOCUS:NAME:
            WHEN "begin_ord-no" THEN 
                DO:
                    /* gdm - 06250905 */
                    IF begin_ord-no:LABEL = "From Order#" THEN 
                    DO:
                        RUN windows/l-ordl.w (g_company, 
                            begin_ord-no:screen-value, 
                            OUTPUT char-val, OUTPUT rec-val). 
                        FIND oe-ordl WHERE RECID(oe-ordl) EQ rec-val NO-LOCK NO-ERROR.
                        IF AVAILABLE oe-ordl 
                            THEN 
                        DO:
                            ASSIGN 
                                begin_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                                begin_job:SCREEN-VALUE    = oe-ordl.job-no
                                begin_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                                begin_i-no:SCREEN-VALUE   = oe-ordl.i-no.
                            RUN check-release(0) .
                        END.
                    END.
                    ELSE 
                    DO:
                        ASSIGN 
                            lv-po-no = begin_ord-no:SCREEN-VALUE.

                        RUN windows/l-poopen.w (cocode, lv-po-no, OUTPUT char-val).
                        IF char-val NE "" THEN lv-po-no = ENTRY(1,char-val).

                        ASSIGN 
                            begin_ord-no:SCREEN-VALUE = lv-po-no.

                        APPLY "entry" TO begin_ord-no.
                    END.
                /* gdm - 06250905 end */
                END.
            WHEN "end_ord-no" THEN 
                DO:
                    /* gdm - 06250905 */
                    IF end_ord-no:LABEL = "To Order#" THEN 
                    DO:
                        RUN windows/l-ordl.w (g_company, 
                            begin_ord-no:screen-value, 
                            OUTPUT char-val, OUTPUT rec-val). 
                        FIND oe-ordl WHERE RECID(oe-ordl) EQ rec-val NO-LOCK NO-ERROR.
                        IF AVAILABLE oe-ordl 
                            THEN 
                        DO:
                            ASSIGN 
                                end_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                                end_job:SCREEN-VALUE    = oe-ordl.job-no
                                end_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                                end_i-no:SCREEN-VALUE   = oe-ordl.i-no.
                            RUN check-release(1) .
                        END.
                    END.
                    ELSE 
                    DO:
                        ASSIGN 
                            lv-po-no = end_ord-no:SCREEN-VALUE.

                        RUN windows/l-poopen.w (cocode, lv-po-no, OUTPUT char-val).
                        IF char-val NE "" THEN lv-po-no = ENTRY(1,char-val).

                        ASSIGN 
                            end_ord-no:SCREEN-VALUE = lv-po-no.

                        APPLY "entry" TO end_ord-no.

                    END.
                /* gdm - 06250905 end */
                END.
            WHEN "begin_i-no" THEN 
                DO:
                    RUN windows/l-itemf3.w (g_company,begin_ord-no,begin_job,begin_job2,begin_i-no, OUTPUT char-val, OUTPUT rec-val).
                    IF char-val <> "" THEN begin_i-no:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            WHEN "end_i-no" THEN 
                DO:
                    RUN windows/l-itemf3.w (g_company,end_ord-no,end_job,end_job2,end_i-no, OUTPUT char-val, OUTPUT rec-val).
                    IF char-val <> "" THEN end_i-no:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            WHEN "begin_ship-to" THEN 
                DO:
                    FIND FIRST oe-ord NO-LOCK
                        WHERE oe-ord.company EQ cocode
                        AND oe-ord.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE)
                        NO-ERROR.
                    IF AVAILABLE oe-ord THEN
                        cCustNo = oe-ord.cust-no.

                    RUN windows/l-shipto.w (g_company,g_loc,cCustNo,begin_ship-to:SCREEN-VALUE,OUTPUT char-val).
                    IF char-val <> "" THEN begin_ship-to:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            WHEN "end_ship-to" THEN 
                DO:
                    FIND FIRST oe-ord NO-LOCK
                        WHERE oe-ord.company EQ cocode
                        AND oe-ord.ord-no  EQ INT(end_ord-no:SCREEN-VALUE)
                        NO-ERROR.
                    IF AVAILABLE oe-ord THEN
                        cCustNo = oe-ord.cust-no.

                    RUN windows/l-shipto.w (g_company,g_loc,cCustNo,end_ship-to:SCREEN-VALUE,OUTPUT char-val).
                    IF char-val <> "" THEN end_ship-to:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            WHEN "begin_rel" THEN 
                DO:
                    RUN windows/l-ordrel.w (g_company,begin_ord-no:screen-value,begin_i-no:screen-value, OUTPUT char-val, OUTPUT rec-val).
                    IF char-val <> "" THEN begin_rel:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            WHEN "end_rel" THEN 
                DO:
                    RUN windows/l-ordrel.w (g_company,end_ord-no:screen-value,end_i-no:screen-value, OUTPUT char-val, OUTPUT rec-val).
                    IF char-val <> "" THEN end_rel:SCREEN-VALUE = ENTRY(1,char-val).
                END.
            OTHERWISE 
            DO:
                lv-handle = FOCUS:HANDLE.
                RUN applhelp.p.

                IF g_lookup-var NE "" THEN lv-handle:SCREEN-VALUE = g_lookup-var.
            END.  /* otherwise */
        END CASE.

        APPLY "entry" TO lv-handle.
        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON HELP OF begin_filename IN FRAME FRAME-A /* Text File Path */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-DIR ls-filename 
            TITLE "Select Path to save"
            INITIAL-DIR begin_filename
            UPDATE ll-ok.

        IF ll-ok THEN SELF:screen-value = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON LEAVE OF begin_filename IN FRAME FRAME-A /* Text File Path */
    DO:
        ASSIGN begin_filename.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_form C-Win
ON LEAVE OF begin_form IN FRAME FRAME-A /* Printer Form# */
    DO:
        ASSIGN begin_form.

        begin_filename = "barcode" + string(begin_form) + ".frm".

        DISPLAY begin_filename WITH FRAME FRAME-A IN WINDOW C-Win.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON VALUE-CHANGED OF begin_i-no IN FRAME FRAME-A /* From Item# */
    DO:  
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no EQ INT(begin_ord-no:SCREEN-VALUE)
            AND oe-ordl.i-no EQ begin_i-no:SCREEN-VALUE
            AND oe-ordl.opened EQ YES NO-ERROR .
        IF AVAILABLE oe-ordl 
            THEN 
        DO:
            ASSIGN 
                begin_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                begin_job:SCREEN-VALUE    = oe-ordl.job-no
                begin_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                begin_i-no:SCREEN-VALUE   = oe-ordl.i-no.
            RUN check-release(0) .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_labels
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_labels C-Win
ON LEAVE OF begin_labels IN FRAME FRAME-A /* # of Labels per Skid */
    DO:
        ASSIGN begin_labels
            v-mult = begin_labels.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON VALUE-CHANGED OF begin_ord-no IN FRAME FRAME-A /* From Order# */
    DO:  
    
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no EQ INT(begin_ord-no:SCREEN-VALUE)
            AND (oe-ordl.i-no EQ begin_i-no:SCREEN-VALUE OR begin_i-no:SCREEN-VALUE EQ "" )
            AND (trim(oe-ordl.job-no) EQ trim(begin_job:SCREEN-VALUE) OR begin_job:SCREEN-VALUE EQ "" )
            AND (oe-ordl.job-no2 EQ int(begin_job2:SCREEN-VALUE) OR int(begin_job:SCREEN-VALUE) EQ 0 )
            AND oe-ordl.opened EQ YES NO-ERROR .
        IF AVAILABLE oe-ordl 
            THEN 
        DO:
            ASSIGN 
                begin_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                begin_job:SCREEN-VALUE    = oe-ordl.job-no
                begin_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                begin_i-no:SCREEN-VALUE   = oe-ordl.i-no.
            RUN check-release(0) .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_poLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_poLine C-Win
ON VALUE-CHANGED OF begin_poLine IN FRAME FRAME-A /* Ln */
    DO:  
        RUN get-po-info ("REFRESH").

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        /*    
            DO ictr = 1 TO 15:
                IF INDEX(program-name(ictr),"mainmenu") <> 0 THEN DO:
                    RUN system/userLogOut.p (NO, 0).
                    LEAVE.
                END.
            END.
         */
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
        
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:  
        ASSIGN {&displayed-objects}.

        ASSIGN
            cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
        FOR EACH tt-word-print:
            DELETE tt-word-print .
        END.

        ASSIGN 
            v-auto-print   = scr-auto-print
            glOverrideMult = tb_override-mult.
        IF v-mult LE 0 THEN v-mult = 1.

        IF tb_reprint-tag AND fi_cas-lab:SCREEN-VALUE = "" THEN 
        DO:
            MESSAGE "Enter tag# to reprint loadtag." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_cas-lab.
            RETURN NO-APPLY.
        END.
        IF tb_ship-id AND v-ship-id:SCREEN-VALUE = "" THEN 
        DO:
            MESSAGE "Ship ID field cannot be blank." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO v-ship-id.
            RETURN NO-APPLY.
        END.
        ELSE IF tb_ship-id THEN

                IF scr-auto-print AND scr-label-file = "" THEN
                DO:
                    MESSAGE "Label Matrix Label File cannot be blank."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                    APPLY "ENTRY":U TO scr-label-file IN FRAME {&FRAME-NAME}.
                    RETURN NO-APPLY.
                END.


        IF NOT lv-ok-ran THEN RUN ok-button.
        lv-ok-ran = NO.
        IF fi_cas-lab:SCREEN-VALUE <> ""  THEN 
        DO:
            APPLY "entry" TO fi_cas-lab.
            RETURN NO-APPLY.
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* To Item# */
    DO:
        ASSIGN begin_i-no end_i-no.

        IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
        DO:

            DEFINE VARIABLE v-cust-no AS CHARACTER NO-UNDO.

            ASSIGN
                begin_job
                end_job
                begin_job2
                end_job2
                begin_ord-no END_ord-no.

            FIND FIRST itemfg 
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ END_i-no NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg AND itemfg.alloc EQ YES THEN
                rd_comps:SCREEN-VALUE = "U".
            IF begin_job2 = 0 AND END_job2 = 0 THEN END_job2 = 99.
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ cocode                 
                AND FILL(" ",9 - LENGTH(TRIM(job-hdr.job-no))) + 
                TRIM(job-hdr.job-no) + STRING(job-hdr.job-no2,"999")  
                GE (begin_job + STRING(begin_job2,"999"))
                AND FILL(" ",9 - length(TRIM(job-hdr.job-no))) +
                TRIM(job-hdr.job-no) + STRING(job-hdr.job-no2,"999")  
                LE (end_job + STRING(end_job2,"999"))
                AND job-hdr.job-no2    EQ INT(begin_job2:SCREEN-VALUE) NO-ERROR.

            IF AVAILABLE job-hdr THEN
                v-cust-no = job-hdr.cust-no.
            ELSE 
            DO:
                IF loadtagFunction:SCREEN-VALUE EQ "order" THEN
                DO: 
                    FIND FIRST oe-ord WHERE 
                        oe-ord.company EQ cocode AND
                        oe-ord.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE oe-ord THEN
                        v-cust-no = oe-ord.cust-no.
                END.     
                ELSE 
                DO:
                    IF AVAILABLE itemfg AND end_ord-no:SCREEN-VALUE NE "" THEN 
                        FIND FIRST po-ordl 
                            WHERE po-ordl.company EQ itemfg.company 
                            AND po-ordl.po-no EQ int(END_ord-no:SCREEN-VALUE)
                            AND po-ordl.item-type EQ NO 
                            AND po-ordl.i-no EQ itemfg.i-no
                            NO-LOCK NO-ERROR.
                    IF AVAILABLE po-ordl AND po-ordl.ord-no NE 0 THEN
                        FIND FIRST oe-ord 
                            WHERE oe-ord.company EQ po-ordl.company
                            AND oe-ord.ord-no  EQ po-ordl.ord-no
                            NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-ord THEN
                        v-cust-no = oe-ord.cust-no.
                END.
            END.
            IF v-cust-no NE "" THEN
                FIND FIRST cust-part WHERE
                    cust-part.company  EQ cocode AND
                    cust-part.i-no      GE begin_i-no:SCREEN-VALUE AND
                    cust-part.i-no      LE end_i-no:SCREEN-VALUE AND
                    cust-part.cust-no     EQ v-cust-no
                    NO-LOCK NO-ERROR.
     
            IF AVAILABLE cust-part AND cust-part.labelPallet <> "" THEN
                scr-label-file:SCREEN-VALUE = (IF cust-part.labelPallet <> "" THEN cust-part.labelPallet ELSE v-bardir-chr).
            ELSE
                IF INT(begin_ord-no:SCREEN-VALUE) NE 0 AND
                    INT(end_ord-no:SCREEN-VALUE) NE 0 THEN
                DO:
           
                    FIND FIRST oe-rel WHERE
                        oe-rel.company EQ cocode AND
                        oe-rel.i-no    GE begin_i-no:SCREEN-VALUE AND
                        oe-rel.i-no    LE end_i-no:SCREEN-VALUE AND
                        oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                        oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE)
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
                            AND shipto.cust-no EQ v-cust-no 
                            AND shipto.ship-id EQ v-cust-no
                            USE-INDEX ship-id NO-ERROR.

                    IF AVAILABLE shipto THEN 
                    DO:
                        IF AVAILABLE oe-rel THEN
                            v-cust-no = oe-rel.cust-no.

                        FIND FIRST sys-ctrl-shipto NO-LOCK
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                            AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                            AND sys-ctrl-shipto.cust-vend    EQ YES 
                            AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                            AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id 
                            AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                            scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                        ELSE 
                        DO:
                            FIND FIRST sys-ctrl-shipto NO-LOCK 
                                WHERE sys-ctrl-shipto.company      EQ cocode 
                                AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                                AND sys-ctrl-shipto.cust-vend    EQ YES 
                                AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                                AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                            IF AVAILABLE sys-ctrl-shipto AND 
                                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
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
                                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                                ELSE 
                                DO:
                                    FIND FIRST sys-ctrl WHERE
                                        sys-ctrl.company EQ cocode AND
                                        sys-ctrl.name    EQ "BARDIR" 
                                        NO-LOCK NO-ERROR.
                                    IF AVAILABLE sys-ctrl THEN
                                        scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                                    ELSE
                                        scr-label-file:SCREEN-VALUE = "".
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
                            AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                            AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                            scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
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
                                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                            ELSE 
                            DO:
                                FIND FIRST sys-ctrl WHERE
                                    sys-ctrl.company EQ cocode AND
                                    sys-ctrl.name    EQ "BARDIR" 
                                    NO-LOCK NO-ERROR.
                                IF AVAILABLE sys-ctrl THEN
                                    scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                                ELSE
                                    scr-label-file:SCREEN-VALUE = "".
                            END.
                        END.
                    END.
                END.
                ELSE
                    IF INT(begin_ord-no:SCREEN-VALUE) EQ 0 AND
                        INT(end_ord-no:SCREEN-VALUE) EQ 0 THEN
                    DO:
                        FIND FIRST shipto WHERE
                            shipto.company EQ cocode AND
                            shipto.cust-no EQ v-cust-no AND
                            shipto.ship-id EQ v-cust-no
                            NO-LOCK NO-ERROR.

                        IF AVAILABLE shipto THEN 
                        DO:

                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company      EQ cocode AND
                                sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                                sys-ctrl-shipto.cust-vend    EQ YES AND
                                sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                                sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                                sys-ctrl-shipto.char-fld     NE ''
                                NO-LOCK NO-ERROR.

                            IF AVAILABLE sys-ctrl-shipto AND 
                                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                            ELSE 
                            DO:
                                FIND FIRST sys-ctrl-shipto WHERE
                                    sys-ctrl-shipto.company      EQ cocode AND
                                    sys-ctrl-shipto.NAME         EQ "BARDIR" AND 
                                    sys-ctrl-shipto.cust-vend    EQ YES AND
                                    sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                                    sys-ctrl-shipto.char-fld     NE ''
                                    NO-LOCK NO-ERROR.
                                IF AVAILABLE sys-ctrl-shipto AND 
                                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
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
                                        scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                                    ELSE 
                                    DO:
                                        FIND FIRST sys-ctrl WHERE
                                            sys-ctrl.company EQ cocode AND
                                            sys-ctrl.name    EQ "BARDIR" 
                                            NO-LOCK NO-ERROR.
                                        IF AVAILABLE sys-ctrl THEN
                                            scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                                        ELSE
                                            scr-label-file:SCREEN-VALUE = "".
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
                                AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                                AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                            IF AVAILABLE sys-ctrl-shipto AND 
                                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
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
                                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                                ELSE 
                                DO:
                                    FIND FIRST sys-ctrl WHERE
                                        sys-ctrl.company EQ cocode AND
                                        sys-ctrl.name    EQ "BARDIR" 
                                        NO-LOCK NO-ERROR.
                                    IF AVAILABLE sys-ctrl THEN
                                        scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                                    ELSE
                                        scr-label-file:SCREEN-VALUE = "".
                                END.
                            END.
                        END.


                    END. /*begin_ord-no and end_ord-no eq 0*/
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON VALUE-CHANGED OF end_i-no IN FRAME FRAME-A /* To Item# */
    DO:  
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no EQ INT(end_ord-no:SCREEN-VALUE)
            AND oe-ordl.i-no EQ end_i-no:SCREEN-VALUE
            AND oe-ordl.opened EQ YES NO-ERROR .
        IF AVAILABLE oe-ordl 
            THEN 
        DO:
            ASSIGN 
                end_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                end_job:SCREEN-VALUE    = oe-ordl.job-no
                end_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                end_i-no:SCREEN-VALUE   = oe-ordl.i-no.
            RUN check-release(1) .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job C-Win
ON LEAVE OF end_job IN FRAME FRAME-A /* To Job# */
    DO:
        RUN leave-job-label.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 C-Win
ON LEAVE OF end_job2 IN FRAME FRAME-A /* - */
    DO:  
        RUN leave-job-label.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* To Order# */
    DO:
        DEFINE VARIABLE v-lcnt    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE v-cust-no AS CHARACTER NO-UNDO.

        IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
        DO:

            IF INT(begin_ord-no:SCREEN-VALUE) NE 0 AND 
                INT(end_ord-no:SCREEN-VALUE) NE 0 THEN
            DO:
                v-lcnt = 0.
                FOR EACH oe-rel NO-LOCK 
                    WHERE oe-rel.company EQ cocode 
                    AND oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE)
                    AND oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE):

                    v-lcnt = v-lcnt + 1.
                    IF v-lcnt GT 1 THEN LEAVE.

                END.
                IF v-lcnt GT 1 AND 
                    begin_i-no:SCREEN-VALUE EQ "" AND v-barflg 
                    THEN 
                DO:
                    MESSAGE 
                        "Item # can not be blank. Please enter an Item #."
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                    APPLY "ENTRY" TO begin_i-no IN FRAME {&FRAME-NAME}.
                    RETURN NO-APPLY.
                END.

                FIND FIRST oe-ord NO-LOCK
                    WHERE oe-ord.company EQ cocode 
                    AND oe-ord.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) 
                    AND oe-ord.ord-no  LE INT(end_ord-no:SCREEN-VALUE) NO-ERROR.

                IF AVAILABLE oe-ord THEN
                DO:
                    v-cust-no = oe-ord.cust-no.

                    FIND FIRST cust-part NO-LOCK 
                        WHERE cust-part.company   EQ cocode 
                        AND cust-part.i-no      EQ begin_i-no:SCREEN-VALUE
                        AND cust-part.i-no      EQ end_i-no:SCREEN-VALUE
                        AND cust-part.cust-no   EQ oe-ord.cust-no NO-ERROR.

                    IF AVAILABLE cust-part AND cust-part.labelCase NE "" THEN
                        scr-label-file:SCREEN-VALUE = (IF cust-part.labelPallet <> "" THEN cust-part.labelPallet ELSE v-bardir-chr).
                    ELSE
                    DO:
                        IF begin_i-no:SCREEN-VALUE NE "" AND 
                            end_i-no:SCREEN-VALUE NE "" THEN

                            FIND FIRST oe-rel NO-LOCK 
                                WHERE oe-rel.company EQ cocode 
                                AND oe-rel.i-no    GE begin_i-no:SCREEN-VALUE 
                                AND oe-rel.i-no    LE end_i-no:SCREEN-VALUE 
                                AND oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) 
                                AND oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                                NO-ERROR.
                        ELSE
                            FIND FIRST oe-rel NO-LOCK 
                                WHERE oe-rel.company EQ cocode 
                                AND oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) 
                                AND oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                                NO-ERROR.

                        IF AVAILABLE oe-rel THEN 
                            FIND FIRST shipto NO-LOCK 
                                WHERE shipto.company EQ cocode 
                                AND shipto.cust-no EQ oe-rel.cust-no 
                                AND shipto.ship-id EQ oe-rel.ship-id 
                                USE-INDEX ship-id NO-ERROR.
                        ELSE
                            FIND FIRST shipto NO-LOCK 
                                WHERE shipto.company EQ cocode 
                                AND shipto.cust-no EQ v-cust-no 
                                AND shipto.ship-id EQ v-cust-no
                                USE-INDEX ship-id NO-ERROR.

                        IF AVAILABLE shipto THEN 
                        DO:

                            IF AVAILABLE oe-rel THEN
                                v-cust-no = oe-rel.cust-no.

                            FIND FIRST sys-ctrl-shipto NO-LOCK
                                WHERE sys-ctrl-shipto.company      EQ cocode 
                                AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                                AND sys-ctrl-shipto.cust-vend    EQ YES 
                                AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no
                                AND sys-ctrl-shipto.ship-id      EQ shipto.ship-id
                                AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                            IF AVAILABLE sys-ctrl-shipto AND
                                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                            ELSE 
                                FIND FIRST sys-ctrl-shipto NO-LOCK  
                                    WHERE sys-ctrl-shipto.company      EQ cocode 
                                    AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                                    AND sys-ctrl-shipto.cust-vend    EQ YES 
                                    AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                                    AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                            IF AVAILABLE sys-ctrl-shipto AND 
                                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                            IF scr-label-file:SCREEN-VALUE EQ "" THEN 
                            DO:

                                FIND FIRST sys-ctrl-shipto WHERE
                                    sys-ctrl-shipto.company      EQ cocode AND
                                    sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                                    sys-ctrl-shipto.cust-vend-no EQ "" AND
                                    sys-ctrl-shipto.cust-vend    EQ YES 
                                    NO-LOCK NO-ERROR.

                                IF AVAILABLE sys-ctrl-shipto AND 
                                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                                ELSE
                                DO:
                                    FIND FIRST sys-ctrl NO-LOCK 
                                        WHERE sys-ctrl.company EQ cocode 
                                        AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.

                                    IF AVAILABLE sys-ctrl THEN
                                        scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                                    ELSE scr-label-file:SCREEN-VALUE = "".
                                END.
                            END.
                        END.
                        ELSE
                        DO:

                            FIND FIRST sys-ctrl-shipto NO-LOCK  
                                WHERE sys-ctrl-shipto.company      EQ cocode 
                                AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                                AND sys-ctrl-shipto.cust-vend    EQ YES 
                                AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                                AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                            IF AVAILABLE sys-ctrl-shipto AND 
                                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                            IF scr-label-file:SCREEN-VALUE EQ "" THEN  
                            DO:
                                FIND FIRST sys-ctrl-shipto WHERE
                                    sys-ctrl-shipto.company      EQ cocode AND
                                    sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                                    sys-ctrl-shipto.cust-vend-no EQ "" AND
                                    sys-ctrl-shipto.cust-vend    EQ YES 
                                    NO-LOCK NO-ERROR.

                                IF AVAILABLE sys-ctrl-shipto AND 
                                    TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                                    scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                                ELSE
                                DO:
                                    FIND FIRST sys-ctrl NO-LOCK 
                                        WHERE sys-ctrl.company EQ cocode 
                                        AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.

                                    IF AVAILABLE sys-ctrl THEN
                                        scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                                    ELSE scr-label-file:SCREEN-VALUE = "".
                                END.
                            END.
                        END.
                    END.
                END.
            END.
        END.
        IF end_ord-no:LABEL = "To PO#" THEN 
        DO:

            FIND FIRST bf-po-ord NO-LOCK 
                WHERE bf-po-ord.company EQ cocode 
                AND bf-po-ord.po-no  EQ int(end_ord-no:SCREEN-VALUE) NO-ERROR.
            IF AVAILABLE bf-po-ord THEN 
            DO:
                RUN pLabelPerSkid(bf-po-ord.cust-no).
                FIND FIRST bf-po-ordl NO-LOCK
                    WHERE bf-po-ordl.company EQ bf-po-ord.company
                    AND bf-po-ordl.po-no  EQ bf-po-ord.po-no 
                    AND bf-po-ordl.i-no   EQ begin_i-no:SCREEN-VALUE NO-ERROR.
                IF NOT AVAILABLE bf-po-ordl THEN
                    FIND FIRST bf-po-ordl NO-LOCK
                        WHERE bf-po-ordl.company EQ bf-po-ord.company
                        AND bf-po-ordl.po-no  EQ bf-po-ord.po-no  NO-ERROR.

                IF AVAILABLE bf-po-ordl THEN 
                DO:

                    ASSIGN 
                        begin_i-no:SCREEN-VALUE = bf-po-ordl.i-no
                        end_i-no:SCREEN-VALUE   = bf-po-ordl.i-no
                        v-cust-no               = bf-po-ordl.cust-no.

                    FIND FIRST cust-part NO-LOCK
                        WHERE cust-part.company EQ cocode 
                        AND cust-part.i-no      EQ begin_i-no:SCREEN-VALUE
                        AND cust-part.i-no      EQ end_i-no:SCREEN-VALUE
                        AND cust-part.cust-no   EQ v-cust-no NO-ERROR.

                    IF AVAILABLE cust-part  THEN
                        scr-label-file:SCREEN-VALUE = (IF cust-part.labelPallet <> "" THEN cust-part.labelPallet ELSE v-bardir-chr).


                END.

            END.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON VALUE-CHANGED OF end_ord-no IN FRAME FRAME-A /* To Order# */
    DO:  
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no EQ INT(end_ord-no:SCREEN-VALUE)
            AND (oe-ordl.i-no EQ end_i-no:SCREEN-VALUE OR end_i-no:SCREEN-VALUE EQ "" )
            AND (trim(oe-ordl.job-no) EQ trim(end_job:SCREEN-VALUE) OR end_job:SCREEN-VALUE EQ "" )
            AND (oe-ordl.job-no2 EQ int(end_job2:SCREEN-VALUE) OR int(end_job:SCREEN-VALUE) EQ 0 )
            AND oe-ordl.opened EQ YES NO-ERROR .
        IF AVAILABLE oe-ordl 
            THEN 
        DO:
            ASSIGN 
                end_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
                end_job:SCREEN-VALUE    = oe-ordl.job-no
                end_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
                end_i-no:SCREEN-VALUE   = oe-ordl.i-no.
            RUN check-release(1) .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_poLine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_poLine C-Win
ON VALUE-CHANGED OF end_poLine IN FRAME FRAME-A /* Ln */
    DO:  
        RUN get-po-info ("REFRESH").

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cas-lab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON ENTRY OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
    DO:
    /*   ASSIGN scr-label-file:SCREEN-VALUE = "". */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON HELP OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
    DO:
        DEFINE VARIABLE rec-val  AS RECID     NO-UNDO.
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        IF tb_reprint-tag THEN RUN addon/windows/l-ldtaga.w (cocode, NO, NO, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
        ELSE RUN addon/windows/l-ldtagc.w (cocode, NO, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
        IF char-val NE "" THEN 
        DO:
            fi_cas-lab:SCREEN-VALUE = ENTRY(1,char-val).
            RUN new-cas-lab.
            IF RETURN-VALUE EQ 'ERROR' THEN
                APPLY 'ENTRY':U TO tb_ret.
        END.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON LEAVE OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
    DO:
        IF SELF:SCREEN-VALUE NE "" AND SELF:MODIFIED THEN 
        DO:
            ASSIGN {&displayed-objects}.
            ASSIGN
                cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
     
            FOR EACH tt-word-print:
                DELETE tt-word-print .
            END.

            RUN new-cas-lab.
            IF RETURN-VALUE NE 'ERROR' THEN 
            DO:
                SELF:SCREEN-VALUE = ''.
                APPLY 'ENTRY':U TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO:

                APPLY 'ENTRY':U TO tb_ret.
                RETURN NO-APPLY.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON RETURN OF fi_cas-lab IN FRAME FRAME-A /* Scan Case Label */
    DO:
        lReturn = YES.
        APPLY 'TAB' TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loadtagFunction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadtagFunction C-Win
ON VALUE-CHANGED OF loadtagFunction IN FRAME FRAME-A
    DO:
        ASSIGN {&SELF-NAME}.
        CASE {&SELF-NAME}:
            WHEN 'PO' THEN 
                DO WITH FRAME {&FRAME-NAME}: 
                    ASSIGN
                        typeLabel:SCREEN-VALUE   = REPLACE(typeLabel:SCREEN-VALUE,'Order','PO')
                        begin_ord-no:LABEL       = REPLACE(begin_ord-no:LABEL,'Order','PO')
                        end_ord-no:LABEL         = REPLACE(end_ord-no:LABEL,'Order','PO')
                        statusLabel:SCREEN-VALUE = REPLACE(statusLabel:SCREEN-VALUE,'Order','PO')
                        v-job-list:SCREEN-VALUE  = ''
                        begin_job:SCREEN-VALUE   = ''
                        begin_job2:SCREEN-VALUE  = ''
                        end_job:SCREEN-VALUE     = ''
                        end_job2:SCREEN-VALUE    = ''.
                    DISABLE {&jobFields}.
                    ENABLE begin_poLine end_poLine. 
                    ASSIGN 
                        begin_poline:HIDDEN = FALSE
                        end_poline:HIDDEN   = FALSE 
                        . 
                END.
            WHEN 'Order' THEN 
                DO WITH FRAME {&FRAME-NAME}:
                    ASSIGN
                        typeLabel:SCREEN-VALUE   = REPLACE(typeLabel:SCREEN-VALUE,'PO','Order')
                        begin_ord-no:LABEL       = REPLACE(begin_ord-no:LABEL,'PO','Order')
                        end_ord-no:LABEL         = REPLACE(end_ord-no:LABEL,'PO','Order')
                        statusLabel:SCREEN-VALUE = REPLACE(statusLabel:SCREEN-VALUE,'PO','Order').
                    ENABLE {&jobFields}.
                    DISABLE begin_poLine end_poLine.
                    ASSIGN 
                        begin_poline:HIDDEN = TRUE
                        end_poline:HIDDEN   = TRUE
                        . 
                END.
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
    DO:
        IF {&self-name}:SCREEN-VALUE EQ "R" THEN 
        DO:
            ASSIGN
                begin_date:SENSITIVE    = YES
                end_date:SENSITIVE      = YES
                begin_ship-to:SENSITIVE = YES
                end_ship-to:SENSITIVE   = YES
                tb_xfer-lot:SENSITIVE   = YES.

            APPLY "entry" TO begin_date.
        END.
        ELSE
            ASSIGN
                begin_date:SENSITIVE    = NO
                end_date:SENSITIVE      = NO
                begin_ship-to:SENSITIVE = NO
                end_ship-to:SENSITIVE   = NO
                tb_xfer-lot:SENSITIVE   = NO
                tb_xfer-lot:CHECKED     = NO
                tb_xfer-lot             = NO.  

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-label-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-label-file C-Win
ON HELP OF scr-label-file IN FRAME FRAME-A /* Print Format */
    DO:
        DEFINE VARIABLE chFile AS CHARACTER FORMAT "X(80)" NO-UNDO.
        DEFINE VARIABLE ll-ok  AS LOG       NO-UNDO.

        /* gdm - 11050804 */
        DEFINE VARIABLE v-path AS CHARACTER NO-UNDO.


        ASSIGN 
            v-path = TRIM(scr-label-file:SCREEN-VALUE).

        IF TRIM(v-path) EQ "" THEN 
        DO:
            FIND FIRST sys-ctrl NO-LOCK 
                WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name EQ "CASLABEL" NO-ERROR.
            IF AVAILABLE sys-ctrl THEN
                ASSIGN v-path = TRIM(sys-ctrl.char-fld).

        END.
        RUN sys\ref\char-fld-help.w(INPUT cocode,
            INPUT v-path,
            OUTPUT chFile).


        /* gdm - 11050804 end
     
        DO WITH FRAME {&FRAME-NAME}:
           system-dialog get-file chFile 
                         title "Select Label Matrix Label File"
                         filters "Label Matrix (*.qdf) " "*.qdf"
                         initial-dir v_path
                         MUST-EXIST
                         USE-FILENAME
                         UPDATE ll-ok.
     
           IF ll-ok THEN
        */   
        ASSIGN 
            scr-label-file:SCREEN-VALUE = chFile.


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbPartSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbPartSelect C-Win
ON VALUE-CHANGED OF tbPartSelect IN FRAME FRAME-A /* Select Components */
    DO:
        DEFINE VARIABLE cChoice AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

        IF tbPartSelect:SCREEN-VALUE = "YES" THEN 
        DO:


            ASSIGN 
                cPrevFromItem = begin_i-no:SCREEN-VALUE
                cPrevToItem   = END_i-no:SCREEN-VALUE.  
            RUN windows/l-setcomp.w (INPUT cocode, INPUT begin_i-no:SCREEN-VALUE, OUTPUT cChoice).

            IF NUM-ENTRIES(cChoice) GT 0 THEN 
            DO:
                DO i = 1 TO NUM-ENTRIES(cChoice):
                    CREATE tt-comps.
                    ASSIGN 
                        tt-comps.comp = ENTRY(i, cChoice).
                END.
            END.
            ELSE 
            DO:
                tbPartSelect:SCREEN-VALUE = "NO".
                RETURN.
            END.
        /*     FOR EACH tt-comps BREAK BY tt-comps.comp:    */
        /*       IF FIRST-OF(tt-comps.comp) THEN            */
        /*         begin_i-no:SCREEN-VALUE = tt-comps.comp. */
        /*       IF LAST-OF(tt-comps.comp) THEN             */
        /*         END_i-no:SCREEN-VALUE = tt-comps.comp.   */
        /*     END.                                         */

        END.
        ELSE 
        DO:
            /*     ASSIGN  begin_i-no:SCREEN-VALUE = cPrevFromItem   */
            /*             END_i-no:SCREEN-VALUE   = cPrevToItem   . */
            EMPTY TEMP-TABLE tt-comps.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_16ths
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_16ths C-Win
ON VALUE-CHANGED OF tb_16ths IN FRAME FRAME-A /* Show LWD in 16ths? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_dept-note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dept-note C-Win
ON VALUE-CHANGED OF tb_dept-note IN FRAME FRAME-A /* Print Department Notes? */
    DO:
        IF SELF:SCREEN-VALUE = "Yes" THEN ENABLE v-dept-list WITH FRAME {&FRAME-NAME}.
        ELSE DISABLE v-dept-list WITH FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_over C-Win
ON VALUE-CHANGED OF tb_over IN FRAME FRAME-A /* Include Overrun? */
    DO:
        IF {&self-name}:SCREEN-VALUE EQ "yes" THEN
            rd_print:SCREEN-VALUE = "R".
        ELSE
            IF rd_print:SCREEN-VALUE EQ "R" THEN
                rd_print:SCREEN-VALUE = lv-rd_print.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_override-mult
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_override-mult C-Win
ON VALUE-CHANGED OF tb_override-mult IN FRAME FRAME-A /* Ignore Customer Labels/ Skid */
    DO:
        ASSIGN {&self-name}.
        IF tb_override-mult:SCREEN-VALUE EQ "No" THEN
            begin_labels:SENSITIVE = NO.
        ELSE begin_labels:SENSITIVE = YES . 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rel C-Win
ON VALUE-CHANGED OF tb_rel IN FRAME FRAME-A /* Print Posted Release in BOL File? */
    DO:
    /*IF {&self-name}:SCREEN-VALUE EQ "yes" THEN
      rd_print:SCREEN-VALUE = "R".
    ELSE
    IF rd_print:SCREEN-VALUE EQ "R" THEN
      rd_print:SCREEN-VALUE = lv-rd_print.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint-tag C-Win
ON VALUE-CHANGED OF tb_reprint-tag IN FRAME FRAME-A /* Reprint Tag? */
    DO:
        ASSIGN {&SELF-NAME}.

        IF tb_reprint-tag THEN 
        DO:
            ASSIGN
                fi_cas-lab:HIDDEN    = NO
                fi_cas-lab:SENSITIVE = YES
                fi_cas-lab:LABEL     = "Tag#"
                fi_cas-lab:BGCOLOR   = 14.
            DISABLE {&NonReprint} WITH FRAME {&FRAME-NAME}.
        END.
        ELSE 
        DO:
            ASSIGN
                fi_cas-lab:HIDDEN  = NOT v-cas-lab
                fi_cas-lab:LABEL   = "Scan Case Label"
                fi_cas-lab:BGCOLOR = ?.
            ENABLE {&NonReprint} WITH FRAME {&FRAME-NAME}.
        END.
        APPLY "entry" TO fi_cas-lab.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ret C-Win
ON VALUE-CHANGED OF tb_ret IN FRAME FRAME-A /* Returns? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ship-id C-Win
ON VALUE-CHANGED OF tb_ship-id IN FRAME FRAME-A /* Print Ship ID? */
    DO:
        IF SELF:SCREEN-VALUE = "Yes" THEN ENABLE v-ship-id WITH FRAME {&FRAME-NAME}.
        ELSE DISABLE v-ship-id WITH FRAME {&FRAME-NAME}.
        ASSIGN tb_ship-id.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_xfer-lot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_xfer-lot C-Win
ON VALUE-CHANGED OF tb_xfer-lot IN FRAME FRAME-A /* Transfer Release Lot# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-job-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-job-list C-Win
ON LEAVE OF v-job-list IN FRAME FRAME-A
    DO: 
        IF loadtagFunction:SCREEN-VALUE EQ "order" AND
            NUM-ENTRIES(v-job-list:SCREEN-VALUE) EQ 1 AND v-bardir THEN
            RUN get-jobord-info.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-job-list C-Win
ON RETURN OF v-job-list IN FRAME FRAME-A
    DO:
        APPLY 'tab' TO v-job-list.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ord-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ord-list C-Win
ON LEAVE OF v-ord-list IN FRAME FRAME-A
    DO: 

        IF loadtagFunction:SCREEN-VALUE EQ "order" THEN
        DO:
            IF NUM-ENTRIES(v-ord-list:SCREEN-VALUE) EQ 1 AND v-bardir THEN
                RUN get-ordl-info.
        END.

        ELSE /*loadtagFunction:SCREEN-VALUE ne "order"*/
            IF NUM-ENTRIES(v-ord-list:SCREEN-VALUE) EQ 1 AND v-bardir THEN
                RUN get-po-info ("INITIAL").

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-id C-Win
ON HELP OF v-ship-id IN FRAME FRAME-A
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
        IF (begin_ord-no:SCREEN-VALUE NE end_ord-no:SCREEN-VALUE 
            OR begin_ord-no:SCREEN-VALUE = "" 
            OR end_ord-no:SCREEN-VALUE = "") 
            AND SELF:SCREEN-VALUE NE "" THEN 
        DO:
            MESSAGE "Ship ID can only be overridden when creating a loadtag for one order." 
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        IF loadtagFunction = "Order" THEN 
            FIND FIRST oe-ord WHERE oe-ord.company = g_company 
                AND oe-ord.ord-no = INT(begin_ord-no:SCREEN-VALUE)
                NO-LOCK NO-ERROR.
        ELSE
            FIND FIRST po-ord WHERE po-ord.company = g_company
                AND po-ord.po-no = INT(begin_ord-no:SCREEN-VALUE)
                NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ord THEN 
        DO:
            IF oe-ord.cust-no NE "" THEN RUN windows/l-shipto.w (g_company, g_loc, oe-ord.cust-no, FOCUS:SCREEN-VALUE, OUTPUT char-val).
            IF char-val <> "" THEN SELF:screen-value = ENTRY(1,char-val).
        END.
        ELSE IF AVAILABLE po-ord THEN 
            DO:
                IF po-ord.cust-no NE "" THEN RUN windows/l-shipto.w (g_company, g_loc, po-ord.cust-no, FOCUS:SCREEN-VALUE, OUTPUT char-val).
                IF char-val <> "" THEN SELF:screen-value = ENTRY(1,char-val).
            END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-ship-id C-Win
ON LEAVE OF v-ship-id IN FRAME FRAME-A
    DO:
        IF SELF:SCREEN-VALUE = "" THEN RETURN.
        IF (begin_ord-no:SCREEN-VALUE NE end_ord-no:SCREEN-VALUE 
            OR begin_ord-no:SCREEN-VALUE = "" 
            OR end_ord-no:SCREEN-VALUE = "") 
            AND SELF:SCREEN-VALUE NE "" THEN 
        DO:
            MESSAGE "Ship ID can only be overridden when creating a loadtag for one order or PO." 
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
        IF loadtagFunction = "Order" THEN
            FIND FIRST oe-ord WHERE oe-ord.company = g_company 
                AND oe-ord.ord-no = INT(begin_ord-no:SCREEN-VALUE)
                NO-LOCK NO-ERROR.
        ELSE
            FIND FIRST po-ord WHERE po-ord.company = g_company
                AND po-ord.po-no = INT(begin_ord-no:SCREEN-VALUE)
                NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ord THEN 
        DO:
            FIND FIRST shipto WHERE shipto.company = g_company
                AND shipto.cust-no = oe-ord.cust-no
                AND shipto.ship-id = SELF:SCREEN-VALUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE shipto THEN 
            DO:
                MESSAGE "Invalid Ship ID. Please use F1 to lookup valid values." VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
            ELSE ASSIGN v-ship-id.
        END.
        ELSE IF AVAILABLE po-ord THEN 
            DO:
                FIND FIRST shipto WHERE shipto.company = g_company
                    AND shipto.cust-no = po-ord.cust-no
                    AND shipto.ship-id = SELF:SCREEN-VALUE
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE shipto THEN 
                DO:
                    MESSAGE "Invalid Ship ID. Please use F1 to lookup valid values." VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                END.
                ELSE ASSIGN v-ship-id.
            END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.
    DEFINE INPUT  PARAMETER VisualStyle AS LONG.
    DEFINE RETURN PARAMETER StatusCode  AS LONG.
END PROCEDURE.



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

    FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
    ASSIGN
        tb_16ths = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "LOADTAG"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = gcompany
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
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
    IF AVAILABLE sys-ctrl THEN ASSIGN v-bardir     = sys-ctrl.log-fld
            v-bardir-chr = sys-ctrl.char-fld.
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

    IF v-loadtag EQ "TRIAD" THEN begin_form = 4.

    IF v-mult LE 0 THEN v-mult = 1.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ gcompany
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

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
  
    DO WITH FRAME {&FRAME-NAME}:
        RUN enable_UI.   
        ASSIGN
            v-ord-list:SCREEN-VALUE     = ""
            v-job-list:SCREEN-VALUE     = ""
            begin_ord-no:SCREEN-VALUE   = ""
            end_ord-no:SCREEN-VALUE     = ""
            begin_job:SCREEN-VALUE      = ""
            end_job:SCREEN-VALUE        = ""
            begin_job2:SCREEN-VALUE     = ""
            end_job2:SCREEN-VALUE       = ""
            begin_i-no:SCREEN-VALUE     = ""
            end_i-no:SCREEN-VALUE       = ""
            begin_ship-to:SCREEN-VALUE  = ""
            end_ship-to:SCREEN-VALUE    = "zzzzzzzz"
            lv-rd_print                 = rd_print:SCREEN-VALUE
            tb_ret:SCREEN-VALUE         = "NO"
            tb_reprint-tag:SCREEN-VALUE = "NO" 
            tb_reprint-tag              = NO
            fi_cas-lab:SCREEN-VALUE     = ""
            begin_filename:SCREEN-VALUE = bardir-desc
            userLabelPath               = bardir-desc
            tbPartSelect:SCREEN-VALUE   = "NO"
     
            .

    

        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ gcompany 
            AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
        IF AVAILABLE sys-ctrl 
            THEN ASSIGN v-barflg     = sys-ctrl.log-fld
                v-auto-print = sys-ctrl.log-fld.
        scr-auto-print:SCREEN-VALUE = STRING(sys-ctrl.log-fld).
        /* gdm - 04090909 end */

        {custom/usrprint.i}   
        ASSIGN 
            tb_ship-id:SCREEN-VALUE    = "NO"
            tb_ship-id                 = NO
            v-ship-id:SCREEN-VALUE     = ""
            v-ship-id                  = ""
            tbPartSelect:SCREEN-VALUE  = "NO"
            begin_labels:SCREEN-VALUE  = STRING(v-mult)
            begin_labels               = v-mult
            begin_ship-to:SCREEN-VALUE = ""
            end_ship-to:SCREEN-VALUE   = "zzzzzzzz"
            begin_rel:SCREEN-VALUE     = ""
            end_rel:SCREEN-VALUE       = ""
            .
        IF tb_override-mult:SCREEN-VALUE EQ "No" THEN
            begin_labels:SENSITIVE = NO.
        ELSE begin_labels:SENSITIVE = YES . 
         
        DISABLE v-ship-id.
        ASSIGN 
            v-ord-list:SCREEN-VALUE = "".

        IF v-loadtag EQ "TRIAD" THEN
            begin_form = 4.
        ELSE
            begin_form:VISIBLE = NO.
      
        RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
        THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdOutputProcs).
       
        RUN GetBarDirFilePath(cocode, "", OUTPUT bardir-desc ).
        THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hdOutputProcs). 
       
        ASSIGN 
            begin_filename:SCREEN-VALUE = bardir-desc .
   
    
        IF  PROGRAM-NAME(3) MATCHES "*/b-ordlt.*" THEN 
        DO: 
      
            ASSIGN
                v-ord-list              = ""
                v-job-list              = ""
                v-ord-list:SCREEN-VALUE = ""
                v-job-list:SCREEN-VALUE = ""
                fi_cas-lab:SCREEN-VALUE = "" .
            /* gdm - 06100901 end */

            IF  begin_i-no:SCREEN-VALUE NE '' THEN 
            DO:  
                FIND FIRST po-ordl NO-LOCK
                    WHERE po-ordl.company EQ cocode
                    AND po-ordl.po-no = int(begin_ord-no:SCREEN-VALUE)  NO-ERROR.
                IF AVAILABLE po-ordl THEN 
                DO:
                    FIND FIRST itemfg NO-LOCK
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no EQ begin_i-no:SCREEN-VALUE
                        NO-ERROR.
                END.

                IF (AVAILABLE itemfg AND itemfg.pur-man AND NOT itemfg.isaset)            
                    AND AVAILABLE po-ordl THEN
                    ASSIGN loadtagFunction:SCREEN-VALUE = "Po"
                        begin_job:SCREEN-VALUE       = ""
                        end_job:SCREEN-VALUE         = "".

            END. /* i-no ne '' */
            ELSE
                loadtagFunction:SCREEN-VALUE = "order".

            APPLY 'VALUE-CHANGED':U TO begin_ord-no.
            APPLY 'VALUE-CHANGED':U TO end_ord-no.

        END.  /*   PROGRAM-NAME(3) MATCHES "b-ordlt." */

        IF  (PROGRAM-NAME(3) MATCHES "*/b-trans.*" OR PROGRAM-NAME(3) MATCHES "*/b-ldtag.*" ) THEN 
        DO:
            /*IF SEARCH('IU2-loadtag.txt') NE ? THEN DO:*/
            /* gdm - 06100901 end */
            ASSIGN
                v-ord-list                = ""
                v-job-list                = ""
                v-ord-list:SCREEN-VALUE   = ""
                v-job-list:SCREEN-VALUE   = ""
                begin_ord-no:SCREEN-VALUE = ""
                end_ord-no:SCREEN-VALUE   = "" 
                begin_job:SCREEN-VALUE    = ""
                begin_job:SCREEN-VALUE    = ""
                end_job:SCREEN-VALUE      = ""
                begin_job2:SCREEN-VALUE   = ""
                end_job2:SCREEN-VALUE     = ""  
                begin_i-no:SCREEN-VALUE   = ""  
                end_i-no:SCREEN-VALUE     = "" .
            /* gdm - 06100901 end */

            IF fi_cas-lab:SCREEN-VALUE NE "" THEN
                ASSIGN tb_reprint-tag              = YES
                    tb_reprint-tag:SCREEN-VALUE = "Yes"
                    .
            loadtagFunction:SCREEN-VALUE = "order".
        /* APPLY 'value-changed' TO tb_reprint-tag. */
        END.       /* b-trans  */ 

        APPLY 'VALUE-CHANGED':U TO loadtagFunction.
        APPLY 'VALUE-CHANGED':U TO tb_dept-note.

        IF rd_print:SCREEN-VALUE NE "R" THEN DISABLE begin_date end_date begin_ship-to end_ship-to .

        {methods/nowait.i}    

        APPLY "entry" TO v-ord-list.

        IF v-cas-lab THEN 
        DO:
            ASSIGN
                fi_cas-lab:HIDDEN    = NO
                fi_cas-lab:SENSITIVE = YES.

            APPLY "entry" TO fi_cas-lab.
        END.

        IF end_ord-no:SCREEN-VALUE NE "" 
            THEN APPLY "leave" TO end_ord-no.

        IF end_i-no:SCREEN-VALUE NE "" 
            THEN APPLY "leave" TO end_i-no.

        IF tb_reprint-tag:SCREEN-VALUE = "YES" THEN 
        DO:
            tb_reprint-tag = YES.
            APPLY 'value-changed' TO tb_reprint-tag.
        /* tb_reprint-tag:SCREEN-VALUE = "yes". */
        END.

        /*
            IF begin_filename:SCREEN-VALUE = "" AND userLabelPath <> "" THEN        
                begin_filename:SCREEN-VALUE = userLabelPath.
        */  
        /* gdm - 06100901 */  
        IF  PROGRAM-NAME(3) MATCHES "*/mainmenu.*" 
            /* Next phrase accounts for direct load from icon or combined launcher */
            OR (PROGRAM-NAME(1) MATCHES "*r-loadtg.*"
            AND NOT program-name(2) MATCHES "*persist*") THEN 
        DO:

            /* gdm - 06050908 */
            ASSIGN
                v-ord-list:SCREEN-VALUE     = ""  
                v-job-list:SCREEN-VALUE     = ""  
                begin_ord-no:SCREEN-VALUE   = ""  
                end_ord-no:SCREEN-VALUE     = ""  
                begin_job:SCREEN-VALUE      = ""  
                end_job:SCREEN-VALUE        = ""  
                begin_job2:SCREEN-VALUE     = ""  
                end_job2:SCREEN-VALUE       = ""  
                begin_i-no:SCREEN-VALUE     = ""  
                end_i-no:SCREEN-VALUE       = ""  
                tb_reprint-tag:SCREEN-VALUE = "NO"
                fi_cas-lab:SCREEN-VALUE     = ""  
                begin_rel:SENSITIVE         = NO
                end_rel:SENSITIVE           = NO

                v-ord-list                  = ""
                v-job-list                  = "" 
                begin_ord-no                = 0  
                end_ord-no                  = 0  
                begin_job                   = "" 
                end_job                     = "" 
                begin_job2                  = 0  
                end_job2                    = 0  
                begin_i-no                  = "" 
                end_i-no                    = "" 
                tb_reprint-tag              = NO 
                fi_cas-lab                  = "".
            APPLY 'value-changed' TO tb_reprint-tag.
        /* gdm - 06050908 */
        END.
        /* gdm - 06100901 end */

        IF NOT PROGRAM-NAME(3) MATCHES "*/b-ordlt.*" THEN 
        DO:
            ASSIGN
                begin_rel:SENSITIVE = NO
                end_rel:SENSITIVE   = NO .
        END.
     
    END.
    lForm = NO.
    iForm = 0.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE askNextPalletID C-Win 
PROCEDURE askNextPalletID :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipc-cust AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opl-error AS LOG NO-UNDO.

    MESSAGE "The Pallet ID has reached its limit." SKIP
        "Please reset it for customer " ipc-cust 
        VIEW-AS ALERT-BOX .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPrint C-Win 
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
        AND trim(po-ordl.job-no) = trim(fg-rctd.job-no)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cas-lab-label-mat-file C-Win 
PROCEDURE cas-lab-label-mat-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-lcnt AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF v-auto-print AND
            LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO AND
            INT(begin_ord-no:SCREEN-VALUE) NE 0 AND
            INT(end_ord-no:SCREEN-VALUE) NE 0 THEN
        DO:
            FOR EACH oe-rel WHERE 
                oe-rel.company EQ cocode AND
                oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE)
                NO-LOCK:

                v-lcnt = v-lcnt + 1.
                IF v-lcnt GT 1 THEN LEAVE.

            END.
            IF v-lcnt GT 1 AND 
                begin_i-no:SCREEN-VALUE EQ "" AND v-barflg THEN
            DO:
                MESSAGE 
                    "Item # cannot be blank. Please enter an Item #."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                APPLY "ENTRY" TO begin_i-no IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.

            FIND FIRST oe-ord WHERE
                oe-ord.company EQ cocode AND
                oe-ord.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                oe-ord.ord-no  LE INT(end_ord-no:SCREEN-VALUE)
                NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ord THEN
            DO:
                FIND FIRST cust-part WHERE
                    cust-part.company  EQ cocode AND
                    cust-part.i-no     EQ begin_i-no:SCREEN-VALUE AND
                    cust-part.i-no     EQ end_i-no:SCREEN-VALUE AND
                    cust-part.cust-no  EQ oe-ord.cust-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE cust-part AND cust-part.labelCase NE "" THEN
                    scr-label-file:SCREEN-VALUE = (IF cust-part.labelPallet <> "" THEN cust-part.labelPallet ELSE bardir-chr).
                ELSE 
                DO:
                    IF begin_i-no:SCREEN-VALUE NE "" AND 
                        end_i-no:SCREEN-VALUE NE "" THEN
                        FIND FIRST oe-rel WHERE
                            oe-rel.company EQ cocode AND
                            oe-rel.i-no    GE begin_i-no:SCREEN-VALUE AND
                            oe-rel.i-no    LE end_i-no:SCREEN-VALUE AND
                            oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                            oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                            NO-LOCK NO-ERROR.
                    ELSE
                        FIND FIRST oe-rel WHERE
                            oe-rel.company EQ cocode AND
                            oe-rel.ord-no  GE INT(begin_ord-no:SCREEN-VALUE) AND
                            oe-rel.ord-no  LE INT(end_ord-no:SCREEN-VALUE) 
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE oe-rel THEN
                        FIND FIRST shipto WHERE
                            shipto.company EQ cocode AND
                            shipto.cust-no EQ oe-rel.cust-no AND
                            shipto.ship-id EQ oe-rel.ship-id 
                            USE-INDEX ship-id NO-LOCK NO-ERROR.
                    ELSE
                        FIND FIRST shipto WHERE
                            shipto.company EQ cocode AND
                            shipto.cust-no EQ oe-ord.cust-no AND
                            shipto.ship-id EQ oe-ord.ship-id
                            USE-INDEX ship-id NO-LOCK NO-ERROR.

                    IF AVAILABLE shipto THEN 
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company EQ cocode AND
                            sys-ctrl-shipto.NAME    EQ "BARDIR" AND
                            sys-ctrl-shipto.cust-vend    EQ YES AND
                            sys-ctrl-shipto.cust-vend-no EQ oe-rel.cust-no AND
                            sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                            sys-ctrl-shipto.char-fld     NE ''
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                            scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                        ELSE 
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company      EQ cocode AND
                                sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                                sys-ctrl-shipto.cust-vend    EQ YES AND
                                sys-ctrl-shipto.cust-vend-no EQ oe-rel.cust-no AND
                                sys-ctrl-shipto.char-fld     NE ''
                                NO-LOCK NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                            scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                    END.
                    ELSE
                    DO:
                        FIND FIRST sys-ctrl-shipto NO-LOCK 
                            WHERE sys-ctrl-shipto.company      EQ cocode 
                            AND sys-ctrl-shipto.NAME         EQ "BARDIR" 
                            AND sys-ctrl-shipto.cust-vend    EQ YES 
                            AND sys-ctrl-shipto.cust-vend-no EQ oe-ord.cust-no
                            AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" 
                            THEN scr-label-file:SCREEN-VALUE = 
                                sys-ctrl-shipto.char-fld.
                        ELSE 
                        DO:
                            FIND FIRST sys-ctrl-shipto NO-LOCK 
                                WHERE sys-ctrl-shipto.company      EQ cocode 
                                AND sys-ctrl-shipto.NAME         EQ "BARDIR"
                                AND sys-ctrl-shipto.cust-vend-no EQ ""
                                AND sys-ctrl-shipto.cust-vend    EQ YES 
                                NO-ERROR.
                            IF AVAILABLE sys-ctrl-shipto AND 
                                TRIM(sys-ctrl-shipto.char-fld) NE "" 
                                THEN 
                                scr-label-file:SCREEN-VALUE = 
                                    sys-ctrl-shipto.char-fld.
                            ELSE 
                            DO:
                                FIND FIRST sys-ctrl NO-LOCK 
                                    WHERE sys-ctrl.company EQ cocode 
                                    AND sys-ctrl.name    EQ "BARDIR" 
                                    NO-ERROR.
                                IF AVAILABLE sys-ctrl 
                                    THEN
                                    scr-label-file:SCREEN-VALUE = 
                                        sys-ctrl.char-fld.
                                ELSE scr-label-file:SCREEN-VALUE = "".
                            END.
                        END.
                    END.
                END.
            END.
            IF scr-label-file:SCREEN-VALUE EQ "" THEN 
            DO:
                FIND FIRST sys-ctrl NO-LOCK 
                    WHERE sys-ctrl.company EQ cocode 
                    AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
                IF AVAILABLE sys-ctrl THEN
                    scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-release C-Win 
PROCEDURE check-release :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipicheck AS INTEGER NO-UNDO .
    DEFINE VARIABLE lv-stat   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheckRel AS LOGICAL   NO-UNDO .
    IF AVAILABLE oe-ordl THEN
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            USE-INDEX ord-item

            BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no
            TRANSACTION:

            IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN 
            DO:
              
                FIND FIRST oe-rell
                    WHERE oe-rell.company  EQ oe-rel.company
                    AND oe-rell.r-no     EQ oe-rel.link-no
                    AND oe-rell.ord-no   EQ oe-rel.ord-no
                    AND oe-rell.rel-no   EQ oe-rel.rel-no
                    AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND oe-rell.i-no     EQ oe-rel.i-no
                    AND oe-rell.line     EQ oe-rel.line
                    AND oe-rell.po-no    EQ oe-rel.po-no
                    AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                    USE-INDEX r-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN
                    FIND FIRST oe-rell
                        WHERE oe-rell.company  EQ oe-rel.company
                        AND oe-rell.link-no  EQ oe-rel.r-no
                        AND oe-rell.ord-no   EQ oe-rel.ord-no
                        AND oe-rell.rel-no   EQ oe-rel.rel-no
                        AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                        AND oe-rell.i-no     EQ oe-rel.i-no
                        AND oe-rell.line     EQ oe-rel.line
                        AND oe-rell.po-no    EQ oe-rel.po-no
                        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                        NO-LOCK NO-ERROR.
                /* Needed because line was sometimes different between the two */
                IF NOT AVAILABLE oe-rell THEN
                    FIND FIRST oe-rell
                        WHERE oe-rell.company  EQ oe-rel.company
                        AND oe-rell.ord-no   EQ oe-rel.ord-no              
                        AND oe-rell.i-no     EQ oe-rel.i-no
                        AND oe-rell.link-no  EQ oe-rel.r-no
                        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                        NO-LOCK NO-ERROR.

                IF AVAILABLE oe-rell THEN
                    FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
                
 
                DO WITH FRAME {&FRAME-NAME}:
                    IF AVAILABLE oe-relh AND oe-relh.release# NE 0 THEN 
                    DO:
                        IF ipicheck EQ 0 THEN
                            ASSIGN
                                begin_rel:SCREEN-VALUE = STRING(oe-relh.release#)
                                begin_rel:SENSITIVE    = YES
                                lCheckRel              = YES .
                        ELSE
                            ASSIGN
                                end_rel:SCREEN-VALUE = STRING(oe-relh.release#)
                                end_rel:SENSITIVE    = YES
                                lCheckRel            = YES .
                        LEAVE .
                    END.
                END.
            END.
        END.
    IF NOT lCheckRel THEN 
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            IF ipicheck EQ 0 THEN
                ASSIGN
                    begin_rel:SCREEN-VALUE = ""
                    begin_rel:SENSITIVE    = NO .
            ELSE 
                ASSIGN
                    end_rel:SCREEN-VALUE = ""
                    end_rel:SENSITIVE    = NO .

        END.
    END.
    IF AVAILABLE oe-ordl THEN
        RUN pLabelPerSkid(oe-ordl.cust-no).
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkReturns C-Win 
PROCEDURE checkReturns :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF tb_ret:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "NO" THEN 
    DO:
        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ loadtag.ord-no
            AND oe-ordl.i-no    EQ loadtag.i-no
            AND trim(oe-ordl.job-no)  EQ trim(loadtag.job-no)
            AND oe-ordl.job-no2 EQ loadtag.job-no2
            NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ordl AND
            CAN-FIND(FIRST oe-rel
            WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            AND oe-rel.qty     LT 0) THEN 
        DO:
            /* tb_ret:SCREEN-VALUE = "YES". */
            MESSAGE 'Negative Release / Returns Exist for this Order' SKIP(1)
                'If the New Load Tags are Re-Working the Returns,' SKIP
                'Check the Returns? Parameter' VIEW-AS ALERT-BOX.
            RETURN 'ERROR'.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-fields C-Win 
PROCEDURE clear-fields :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            begin_ord-no:SCREEN-VALUE = ""
            begin_job:SCREEN-VALUE    = ""
            begin_job2:SCREEN-VALUE   = ""
            begin_i-no:SCREEN-VALUE   = ""
            end_ord-no:SCREEN-VALUE   = ""
            end_job:SCREEN-VALUE      = ""
            end_job2:SCREEN-VALUE     = ""
            end_i-no:SCREEN-VALUE     = "".
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
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE li-max-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ll         AS LOG     NO-UNDO.
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.

    FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ iprRctdRow 
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-fg-rctd THEN do:
        oplReturnError = YES.
        RETURN.
    END.    

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
                oplReturnError = YES.    
                RETURN . 
            END.

        END. /* if over qty */
    END. /* if isaset */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr C-Win 
PROCEDURE convert-vend-comp-curr :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-po-no AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DECIMAL DECIMALS 4 NO-UNDO.

    DEFINE BUFFER b-po-ord  FOR po-ord.
    DEFINE BUFFER b-company FOR company.

    FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ cocode AND
        b-po-ord.po-no EQ ip-po-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE b-po-ord THEN
    DO:
        FIND FIRST vend WHERE
            vend.company EQ b-po-ord.company AND
            vend.vend-no EQ b-po-ord.vend-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE vend THEN
        DO:
            FIND FIRST b-company WHERE
                b-company.company EQ cocode
                NO-LOCK.

            IF vend.curr-code NE b-company.curr-code THEN
            DO:
                FIND FIRST currency WHERE
                    currency.company EQ b-po-ord.company AND
                    currency.c-code EQ vend.curr-code
                    NO-LOCK NO-ERROR.

                IF AVAILABLE currency THEN
                DO:
                    ip-cost = ip-cost * currency.ex-rate.
                    RELEASE currency.
                END.
            END.

            RELEASE b-company.
            RELEASE vend.
        END.

        RELEASE b-po-ord.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag C-Win 
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
    DEFINE VARIABLE lReturnError          AS LOGICAL   NO-UNDO.
    /*   DEF BUFFER bf-eb FOR eb. */
    DEFINE BUFFER bf-itemfg FOR itemfg.

    IF tb_reprint-tag THEN 
    DO:
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company   EQ cocode
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ TRIM(fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
            USE-INDEX tag NO-ERROR.
        IF AVAILABLE loadtag THEN
            io-tag-no = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
        RETURN.
    END.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no
        NO-ERROR.
  
    RUN loadtags\GetNextTag.p(cocode, w-ord.i-no, OUTPUT cNextLoadtag).
  //RUN GetNextLoadtagNumber (cocode, w-ord.i-no, OUTPUT io-tag-no).

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
        loadtag.tag-no       = /*string(io-tag-no,"99999") + STRING(w-ord.i-no,"x(15)") */
                          //STRING(CAPS(w-ord.i-no),"x(15)") + STRING(io-tag-no,"99999")
                          cNextLoadtag 
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

    IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.

    IF v-loadtag = "CentBox" THEN 
    DO:
        ASSIGN 
            loadtag.loc     = itemfg.def-loc
            loadtag.loc-bin = itemfg.def-loc-bin.
        FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no
            AND trim(fg-bin.job-no)  EQ trim(w-ord.job-no)
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

    IF v-fgrecpt AND NOT tb_ret THEN 
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
                fg-rctd.cases-unit = loadtag.case-bundle  + INTEGER(DYNAMIC-FUNCTION("fGetOverageQuantitySubUnitsPerUnit" IN hdInventoryProcs,INTEGER(loadtag.partial))) 
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
                    AND trim(job.job-no)  EQ trim(fg-rctd.job-no)
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
                RUN component-qty-check (INPUT ROWID(fg-rctd), OUTPUT lReturnError) NO-ERROR.
            IF lReturnError THEN 
            DO:
                /* User chose not to create the tag with a lower qty */
                /* so remove created record                          */
                IF AVAIL(fg-rctd) THEN
                    DELETE fg-rctd.                
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
                /*            /* Created task 09261318 to be used by receiving screens in addition */*/
                /*             RUN fg/calcRcptCostFromPO.p                                           */
                /*               (INPUT cocode ,                                                     */
                /*               INPUT ROWID(b-po-ordl),                                             */
                /*               INPUT ROWID(fg-rctd),                                               */
                /*               INPUT fg-rctd.qty-case,                                             */
                /*               INPUT fg-rctd.cases,                                                */
                /*               INPUT fg-rctd.partial,                                              */
                /*               INPUT fg-rctd.job-no,                                               */
                /*               INPUT fg-rctd.job-no2,                                              */
                /*               INPUT fg-rctd.cost-uom,                                             */
                /*               INPUT fg-rctd.t-qty,                                                */
                /*               OUTPUT lv-use-full-qty,                                             */
                /*               OUTPUT lv-full-qty,                                                 */
                /*               OUTPUT lvCalcCostUom,                                               */
                /*               OUTPUT lvCalcStdCost,                                               */
                /*               OUTPUT lvCalcExtCost,                                               */
                /*               OUTPUT lvCalcFrtCost,                                               */
                /*               OUTPUT lvSetupPerCostUom).                                          */
                /*                                                                                   */
                /*            ASSIGN                                                                 */
                /*               fg-rctd.cost-uom = lvCalcCostUom                                    */
                /*               fg-rctd.std-cost = lvCalcStdCost.                                   */
                /*               fg-rctd.ext-cost = lvCalcExtCost.                                   */
                /*                                                                                   */
                /*            IF fgpofrt-log THEN                                                    */
                /*              fg-rctd.frt-cost = lvCalcFrtCost.                                    */
                /*                                                                                   */
                /*            ASSIGN                                                                 */
                /*              lv-out-cost = lvCalcStdCost                                          */
                /*              lv-setup-per-cost-uom = lvSetupPerCostUom.                           */

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
            /*       FIND FIRST bf-eb                                       */
            /*         WHERE bf-eb.company  EQ cocode                       */
            /*           AND bf-eb.est-no   EQ w-ord.est-no                 */
            /*           AND bf-eb.stock-no EQ w-ord.i-no NO-LOCK NO-ERROR. */
            /*       IF AVAIL bf-eb THEN DO:                                */
            /*         IF bf-eb.pur-man THEN DO: */
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
                    ELSE IF TRIM(fg-rctd.job-no) NE "" AND TRIM(fg-rctd.po-no) NE "" AND loadtagFunction EQ 'Order' THEN
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

    ELSE IF v-fgrecpt AND tb_ret AND AVAILABLE itemfg THEN 
        DO:
            RUN post-return (RECID(fg-rctd)).
        END.

    /* Update the other tags with this new quantity */
    IF AVAILABLE fg-rctd AND lv-use-full-qty THEN
        RUN get-set-full-qty (INPUT fg-rctd.job-no, INPUT fg-rctd.job-no2, 
            INPUT fg-rctd.i-no, INPUT 0 /* new qty */, 
            INPUT fg-rctd.std-cost /* cost to set */, OUTPUT lv-full-qty).

    FIND CURRENT fg-rctd NO-LOCK NO-ERROR.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-text-file C-Win 
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

    cTmpFile = SESSION:TEMP-DIRECTORY + "/" + USERID("NOSWEAT") + STRING(TIME).
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

            IF tb_dept-note THEN 
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
                        AND trim(job-hdr.job-no)  EQ trim(w-ord.job-no)
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
                                WHERE notes.rec_key EQ job.rec_key
                                AND CAN-DO(v-dept-list,notes.note_code):
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
            END. /* tb_dept-note*/

            ASSIGN
                w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
                v-job          = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', w-ord.job-no, w-ord.job-no2)) .
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
            IF FIRST-OF(w-ord.i-no) THEN
                RUN pRunAPIOutboundTriggerForItem (
                    INPUT g_company,
                    INPUT w-ord.i-no,
                    INPUT w-ord.job-no,
                    INPUT w-ord.job-no2,
                    INPUT w-ord.po-no
                    ).        
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
    IF ssPostFG-log AND SSPostFG-char = "Loadtag"  THEN
        RUN post-all.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-w-ord C-Win 
PROCEDURE create-w-ord :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rel-date AS DATE      NO-UNDO.
    DEFINE VARIABLE cRelStat    AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-job     FOR job.
    DEFINE BUFFER b-job-hdr FOR job-hdr.

    FIND FIRST company WHERE company.company = loadtag.company NO-LOCK NO-ERROR.
    FIND FIRST itemfg WHERE itemfg.company = loadtag.company
        AND itemfg.i-no = loadtag.i-no NO-LOCK NO-ERROR.
    FIND FIRST oe-ord WHERE oe-ord.company = loadtag.company
        AND oe-ord.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord THEN 
    DO:
        FIND FIRST oe-ordl WHERE oe-ordl.company = loadtag.company
            AND oe-ordl.ord-no = loadtag.ord-no
            AND oe-ordl.i-no = loadtag.i-no NO-LOCK NO-ERROR.
        FIND FIRST cust WHERE cust.company = loadtag.company
            AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.

        FIND FIRST b-job NO-LOCK WHERE b-job.company = loadtag.company
            AND trim(b-job.job-no)  = trim(loadtag.job-no)
            AND b-job.job-no2 = loadtag.job-no2  NO-ERROR.
        IF AVAILABLE b-job THEN
            FIND FIRST b-job-hdr WHERE b-job-hdr.company EQ b-job.company
                AND b-job-hdr.job     EQ b-job.job
                AND b-job-hdr.job-no  EQ b-job.job-no
                AND b-job-hdr.job-no2 EQ b-job.job-no2
                AND b-job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.

        CREATE w-ord.
        ASSIGN 
            w-ord.ord-no       = loadtag.ord-no
            w-ord.job-no       = loadtag.job-no
            w-ord.job-no2      = loadtag.job-no2
            w-ord.cust-no      = IF AVAILABLE oe-ord THEN oe-ord.cust-no ELSE ""
            w-ord.cust-name    = IF AVAILABLE oe-ord THEN oe-ord.cust-name ELSE ""
            w-ord.i-no         = loadtag.i-no
            w-ord.cust-part-no = IF AVAILABLE oe-ordl THEN oe-ordl.part-no ELSE ""
            w-ord.ord-qty      = loadtag.qty
            w-ord.po-no        = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po ELSE 0
            w-ord.i-name       = loadtag.i-name
            w-ord.due-date     = IF oe-ord.due-date NE ? THEN
                                   oe-ord.due-date
                                 ELSE
                                 IF AVAILABLE oe-ordl THEN (IF oe-ordl.req-date NE ? THEN
                                   oe-ordl.req-date
                                 ELSE TODAY) ELSE oe-ord.due-date
            w-ord.est-no       = IF AVAILABLE oe-ordl THEN oe-ordl.est-no ELSE ""
            w-ord.form-no      = IF AVAILABLE oe-ordl THEN oe-ordl.form-no ELSE 0
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = IF AVAILABLE cust AND cust.int-field[1] NE 0 AND NOT glOverrideMult THEN
                                   cust.int-field[1] ELSE v-mult
            w-ord.dont-run-set = IF AVAILABLE oe-ordl THEN oe-ordl.is-a-component ELSE NO
            w-ord.ord-desc1    = IF AVAILABLE oe-ordl THEN oe-ordl.part-dscr1 ELSE ""
            w-ord.ord-desc2    = IF AVAILABLE oe-ordl THEN oe-ordl.part-dscr2 ELSE ""
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.linenum      = IF AVAILABLE oe-ordl THEN oe-ordl.e-num ELSE 0
            w-ord.lot          = loadtag.misc-char[2]
            w-ord.runShip      = IF AVAILABLE oe-ordl THEN oe-ordl.whsed ELSE NO 
            w-ord.ipReturn     = tb_ret
            .

        IF AVAILABLE b-job-hdr THEN
            w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
        IF AVAILABLE b-job THEN
            w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
        w-ord.job-qty = IF AVAILABLE b-job AND AVAILABLE b-job-hdr THEN b-job-hdr.qty ELSE 0 . 

        RUN get-rel-info (OUTPUT w-ord.cust-po-no,
            OUTPUT w-ord.rel-date,
            OUTPUT w-ord.rel-lot#,
            OUTPUT w-ord.ship-notes,
            OUTPUT w-ord.rel-qty,
            INPUT ROWID(b-job-hdr)).
        IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

        IF AVAILABLE itemfg THEN
            ASSIGN w-ord.upc-no  = itemfg.upc-no
                w-ord.box-len = itemfg.l-score[50]
                w-ord.box-wid = itemfg.w-score[50]
                w-ord.box-dep = itemfg.d-score[50]
                w-ord.flute   = itemfg.flute
                w-ord.test    = itemfg.test
                w-ord.pcs     = loadtag.qty-case
                w-ord.bundle  = loadtag.case-bundle
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
        FOR EACH w-shipto:
            DELETE w-shipto.
        END.
        lv-got-shipto = NO.
        IF AVAILABLE oe-ordl THEN 
        DO:  
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.line    EQ oe-ordl.line:
                IF NOT tb_ship-id THEN 
                    v-ship-id = oe-rel.ship-id.
                
                RUN oe/custxship.p(
                    INPUT oe-rel.company,
                    INPUT oe-rel.cust-no,
                    INPUT v-ship-id,
                    BUFFER shipto
                    ).
              
                IF AVAILABLE shipto THEN 
                DO:
                    RUN oe/rel-stat.p(
                        INPUT ROWID(oe-rel), 
                        OUTPUT cRelStat
                        ).
              
                    CREATE w-shipto.
                    BUFFER-COPY shipto EXCEPT rec_key TO w-shipto
                        ASSIGN
                        w-shipto.stat   = cRelStat
                        w-shipto.row-id = ROWID(oe-rel).
                END.
            END.
    
            FOR EACH w-shipto,
                FIRST oe-rel NO-LOCK 
                WHERE ROWID(oe-rel) EQ w-shipto.row-id
                BREAK BY oe-rel.rel-date
                BY oe-rel.po-no
                BY oe-rel.ship-no 
                BY oe-rel.qty:
    
                IF LOOKUP(w-shipto.stat , "L,S,I") > 0  OR
                    LAST(oe-rel.rel-date) THEN 
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
        END.
                   
        IF NOT lv-got-shipto THEN 
        DO:
            IF NOT tb_ship-id THEN 
                v-ship-id = oe-ord.cust-no.
              
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ oe-ord.cust-no
                AND shipto.ship-id EQ v-ship-id
                USE-INDEX ship-id  
                NO-ERROR.
            IF AVAILABLE shipto THEN
                ASSIGN
                    w-ord.ship-code  = shipto.ship-id
                    w-ord.ship-name  = shipto.ship-name
                    w-ord.ship-add1  = shipto.ship-add[1]
                    w-ord.ship-add2  = shipto.ship-add[2]
                    w-ord.ship-city  = shipto.ship-city
                    w-ord.ship-state = shipto.ship-state
                    w-ord.ship-zip   = shipto.ship-zip
                    w-ord.broker     = shipto.broker.    
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
                w-ord.pallt-no   = eb.tr-no
                w-ord.part-dscr2 = eb.part-dscr2.

        ASSIGN 
            w-ord.total-tags = 1
            w-ord.ord-qty    = loadtag.qty 
            w-ord.pcs        = loadtag.qty-case
            w-ord.bundle     = loadtag.case-bundle
            w-ord.partial    = loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .    

        IF AVAILABLE b-job AND AVAILABLE eb AND eb.est-type EQ 2 AND AVAILABLE b-job-hdr THEN
            w-ord.job-qty =  b-job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
        ELSE IF AVAILABLE b-job AND AVAILABLE eb AND eb.est-type EQ 6 AND AVAILABLE b-job-hdr THEN
                w-ord.job-qty =  b-job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .

    END.  /* avail oe-ord*/
    ELSE IF loadtag.job-no <> "" THEN 
        DO:
            FIND FIRST job NO-LOCK WHERE job.company = loadtag.company
                AND trim(job.job-no) = trim(loadtag.job-no)
                AND job.job-no2 = loadtag.job-no2  NO-ERROR.
            IF AVAILABLE job THEN
                FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job-hdr AND tb_reprint-tag THEN 
            DO:
                FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    NO-LOCK NO-ERROR.
            END.
            IF AVAILABLE job-hdr THEN 
            DO:

                FIND FIRST cust WHERE cust.company EQ cocode
                    AND cust.cust-no EQ job-hdr.cust-no NO-LOCK NO-ERROR.
                FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.

                CREATE w-ord.
                ASSIGN
                    w-ord.ord-no    = job-hdr.ord-no
                    w-ord.job-no    = job-hdr.job-no
                    w-ord.job-no2   = job-hdr.job-no2
                    w-ord.cust-no   = IF AVAILABLE cust THEN cust.cust-no ELSE ""
                    w-ord.cust-name = IF AVAILABLE cust THEN cust.NAME ELSE ""
                    w-ord.i-no      = loadtag.i-no
                    w-ord.ord-qty   = job-hdr.qty
                    w-ord.due-date  = job.start-date
                    w-ord.est-no    = job.est-no
                    w-ord.form-no   = job-hdr.frm
                    w-ord.vendor    = company.name
                    w-ord.tare-wt   = 10
                    w-ord.uom       = "EA"
                    w-ord.mult      = IF AVAILABLE cust AND cust.int-field[1] NE 0 AND NOT glOverrideMult THEN
                                   cust.int-field[1] ELSE v-mult
                    w-ord.lot       = loadtag.misc-char[2].
                w-ord.job-qty      = job-hdr.qty .
                w-ord.ipReturn     = tb_ret  .

                IF AVAILABLE itemfg THEN
                    ASSIGN
                        w-ord.cust-part-no = itemfg.part-no
                        w-ord.style        = itemfg.style
                        w-ord.i-name       = itemfg.i-name
                        w-ord.upc-no       = itemfg.upc-no
                        w-ord.upc-no       = itemfg.upc-no
                        w-ord.box-len      = itemfg.l-score[50]
                        w-ord.box-wid      = itemfg.w-score[50]
                        w-ord.box-dep      = itemfg.d-score[50]
                        w-ord.zone         = itemfg.spare-char-4.

                FOR EACH cust-part NO-LOCK 
                    WHERE cust-part.company EQ job-hdr.company   
                    AND cust-part.i-no EQ loadtag.i-no 
                    AND cust-part.cust-no EQ job-hdr.cust-no
                    AND cust-part.part-no NE "" :
                    ASSIGN  
                        w-ord.cust-part-no = cust-part.part-no .
                    LEAVE.
                END.

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
                IF v-ship-id EQ "" THEN v-ship-id = job-hdr.cust-no.
                FIND FIRST shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ job-hdr.cust-no
                    AND shipto.ship-id EQ job-hdr.cust-no
                    USE-INDEX ship-id NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN
                    ASSIGN
                        w-ord.ship-code  = shipto.ship-id
                        w-ord.ship-name  = shipto.ship-name
                        w-ord.ship-add1  = shipto.ship-add[1]
                        w-ord.ship-add2  = shipto.ship-add[2]
                        w-ord.ship-city  = shipto.ship-city
                        w-ord.ship-state = shipto.ship-state
                        w-ord.ship-zip   = shipto.ship-zip
                        w-ord.broker     = shipto.broker.

                FIND FIRST est WHERE est.company EQ job.company
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

                ASSIGN 
                    w-ord.total-tags = 1
                    w-ord.ord-qty    = loadtag.qty 
                    w-ord.pcs        = loadtag.qty-case
                    w-ord.bundle     = loadtag.case-bundle
                    w-ord.partial    = loadtag.partial
                    w-ord.total-unit = w-ord.pcs * w-ord.bundle  .   

                IF AVAILABLE eb AND eb.est-type EQ 2 THEN
                    w-ord.job-qty =  job-hdr.qty * (IF eb.cust-% GT 0 THEN eb.cust-% ELSE 1)  .
                ELSE IF AVAILABLE eb AND eb.est-type EQ 6 THEN
                        w-ord.job-qty =  job-hdr.qty * (IF eb.quantityPerSet GT 0 THEN eb.quantityPerSet ELSE 1)  .


            END.  /* avail job*/
        END. /* job-no <> "" */
        ELSE IF loadtag.po-no <> 0 THEN 
            DO:
                FIND FIRST po-ord WHERE po-ord.company = loadtag.company
                    AND po-ord.po-no = loadtag.po-no NO-LOCK NO-ERROR.
                IF AVAILABLE po-ord THEN
                    FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                        AND po-ordl.po-no EQ po-ord.po-no
                        AND po-ordl.i-no = loadtag.i-no
                        USE-INDEX po-no  NO-ERROR.
                IF AVAILABLE po-ordl THEN 
                DO:
                    FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                        AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
                    FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                        AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
                    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                        AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.

                    CREATE w-ord.
                    ASSIGN
                        w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
                        w-ord.cust-no   = po-ord.cust-no
                        w-ord.due-date  = po-ord.due-date
                        w-ord.i-no      = po-ordl.i-no
                        w-ord.i-name    = po-ordl.i-name
                        w-ord.mult      = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                         cust.int-field[1] ELSE v-mult
                        w-ord.ord-qty   = po-ordl.ord-qty
                        w-ord.po-no     = po-ord.po-no
                        w-ord.tare-wt   = 10
                        w-ord.uom       = 'EA'
                        w-ord.vendor    = IF AVAILABLE vend THEN vend.name ELSE ''
                        w-ord.lot       = loadtag.misc-char[2]
                        w-ord.ipReturn  = tb_ret . 
                    IF AVAILABLE itemfg THEN
                        ASSIGN w-ord.est-no       = itemfg.est-no
                            w-ord.upc-no       = itemfg.upc-no
                            w-ord.box-len      = itemfg.l-score[50]
                            w-ord.box-wid      = itemfg.w-score[50]
                            w-ord.box-dep      = itemfg.d-score[50]
                            w-ord.flute        = itemfg.flute
                            w-ord.test         = itemfg.test
                            w-ord.pcs          = itemfg.case-count
                            w-ord.bundle       = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
                            w-ord.style        = itemfg.style
                            w-ord.cust-part-no = itemfg.part-no 
                            w-ord.zone         = itemfg.spare-char-4.
                    IF po-ordl.ord-no > 0 THEN  
                        FOR EACH cust-part NO-LOCK 
                            WHERE cust-part.company EQ po-ord.company   
                            AND cust-part.i-no EQ po-ordl.i-no 
                            AND cust-part.cust-no EQ po-ord.cust-no
                            AND cust-part.part-no NE "" :
                            ASSIGN  
                                w-ord.cust-part-no = cust-part.part-no .
                            LEAVE.
                        END.
          

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

                    IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
                        FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                            AND eb.est-no EQ itemfg.est-no
                            AND eb.stock-no EQ itemfg.i-no NO-ERROR.
                    IF AVAILABLE eb THEN
                        ASSIGN w-ord.flute      = eb.flute
                            w-ord.test       = eb.test
                            w-ord.pcs        = eb.cas-cnt
                            w-ord.bundle     = eb.cas-pal
                            w-ord.cas-no     = eb.cas-no
                            w-ord.pallt-no   = eb.tr-no
                            w-ord.part-dscr2 = eb.part-dscr2.
                    IF v-ship-id EQ "" THEN v-ship-id = po-ord.cust-no.
                    FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ po-ord.cust-no
                        AND shipto.ship-id EQ v-ship-id
                        USE-INDEX ship-id NO-ERROR.
                    IF AVAILABLE shipto THEN
                        ASSIGN  w-ord.ship-code  = shipto.ship-id
                            w-ord.ship-name  = shipto.ship-name
                            w-ord.ship-add1  = shipto.ship-add[1]
                            w-ord.ship-add2  = shipto.ship-add[2]
                            w-ord.ship-city  = shipto.ship-city
                            w-ord.ship-state = shipto.ship-state
                            w-ord.ship-zip   = shipto.ship-zip
                            w-ord.broker     = shipto.broker.

                    ASSIGN 
                        w-ord.total-tags = 1
                        w-ord.ord-qty    = loadtag.qty 
                        w-ord.pcs        = loadtag.qty-case
                        w-ord.bundle     = loadtag.case-bundle
                        w-ord.partial    = loadtag.partial
                        w-ord.total-unit = w-ord.pcs * w-ord.bundle  .      

                END. /* AVAIL PO-ORDL */
            END. /* po-no <> ""*/
            ELSE 
            DO:
                FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                    AND itemfg.i-no EQ loadtag.i-no NO-ERROR.
                IF AVAILABLE itemfg THEN 
                DO:
                    FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                        AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
                    FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                        AND cust.cust-no EQ itemfg.cust-no NO-ERROR.

                    CREATE w-ord.
                    ASSIGN 
                        w-ord.i-no         = itemfg.i-no
                        w-ord.i-name       = itemfg.i-name
                        w-ord.cust-no      = itemfg.cust-no
                        w-ord.cust-name    = itemfg.cust-name
                        w-ord.cust-part-no = itemfg.part-no
                        w-ord.mult         = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                              cust.int-field[1] ELSE v-mult
                        w-ord.box-len      = itemfg.l-score[50]
                        w-ord.box-wid      = itemfg.w-score[50]
                        w-ord.box-dep      = itemfg.d-score[50]
                        w-ord.flute        = itemfg.flute
                        w-ord.upc-no       = itemfg.upc-no
                        w-ord.test         = itemfg.test
                        w-ord.vendor       = IF AVAILABLE vend THEN vend.name ELSE company.name
                        w-ord.tare-wt      = 10
                        w-ord.uom          = "EA"
                        w-ord.pcs          = itemfg.case-count
                        w-ord.bundle       = itemfg.case-pall
                        w-ord.total-tags   = 1
                        w-ord.ord-qty      = loadtag.qty 
                        w-ord.pcs          = loadtag.qty-case
                        w-ord.bundle       = loadtag.case-bundle
                        w-ord.partial      = loadtag.partial
                        w-ord.total-unit   = w-ord.pcs * w-ord.bundle + w-ord.partial
                        w-ord.style        = itemfg.style
                        w-ord.lot          = loadtag.misc-char[2]
                        w-ord.zone         = itemfg.spare-char-4
                        w-ord.ipReturn     = tb_ret .

                    FOR EACH cust-part NO-LOCK 
                        WHERE cust-part.company EQ cocode   
                        AND cust-part.i-no EQ itemfg.i-no 
                        AND cust-part.cust-no EQ itemfg.cust-no
                        AND cust-part.part-no NE "" :
                        ASSIGN  
                            w-ord.cust-part-no = cust-part.part-no .
                        LEAVE.
                    END.

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
                END. /* avail itemfg */
            END.
    /* task 11230523 */
    IF tb_reprint-tag THEN 
    DO:
        FIND FIRST fg-bin WHERE fg-bin.company = loadtag.company
            AND fg-bin.i-no = w-ord.i-no
            AND fg-bin.tag = fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
        IF AVAILABLE fg-bin AND AVAILABLE w-ord THEN
            ASSIGN w-ord.pcs        = fg-bin.case-count
                w-ord.bundle     = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-ord.partial    = fg-bin.partial-count
                w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateWOrdFromItem C-Win 
PROCEDURE CreateWOrdFromItem :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipBeginItem AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipEndItem AS CHARACTER NO-UNDO.

    FOR EACH itemfg NO-LOCK WHERE itemfg.company EQ cocode
        AND itemfg.i-no GE ipBeginItem
        AND itemfg.i-no LE ipEndItem:
        FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
            AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
        FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no NO-ERROR.

        CREATE w-ord.
        ASSIGN
            w-ord.i-no         = itemfg.i-no
            w-ord.i-name       = itemfg.i-name
            w-ord.cust-no      = itemfg.cust-no
            w-ord.cust-name    = itemfg.cust-name
            w-ord.cust-part-no = itemfg.part-no
            w-ord.mult         = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                   cust.int-field[1] ELSE v-mult
            w-ord.box-len      = itemfg.l-score[50]
            w-ord.box-wid      = itemfg.w-score[50]
            w-ord.box-dep      = itemfg.d-score[50]
            w-ord.flute        = itemfg.flute
            w-ord.upc-no       = itemfg.upc-no
            w-ord.test         = itemfg.test
            w-ord.vendor       = IF AVAILABLE vend THEN vend.name ELSE company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.pcs          = itemfg.case-count
            w-ord.bundle       = itemfg.case-pall
            w-ord.style        = itemfg.style
            w-ord.zone         = itemfg.spare-char-4
            w-ord.ipReturn     = tb_ret.
     
        FOR EACH cust-part NO-LOCK 
            WHERE cust-part.company EQ cocode   
            AND cust-part.i-no EQ itemfg.i-no 
            AND cust-part.cust-no EQ itemfg.cust-no
            AND cust-part.part-no NE "" :
            ASSIGN  
                w-ord.cust-part-no = cust-part.part-no .
            LEAVE.
        END.

        /* task 02081202 */
        IF tb_reprint-tag THEN 
        DO:
            FIND FIRST fg-bin WHERE fg-bin.company = itemfg.company
                AND fg-bin.i-no = w-ord.i-no
                AND fg-bin.tag = fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
            IF AVAILABLE fg-bin AND AVAILABLE w-ord THEN
                ASSIGN w-ord.pcs        = fg-bin.case-count
                    w-ord.bundle     = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                    w-ord.partial    = fg-bin.partial-count
                    w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
        END.

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
    END. /* each itemfg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispJobInfo C-Win 
PROCEDURE dispJobInfo :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------ ------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2 AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiForm AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlank AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER iplCheckBar AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iplCheckBarBlank AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCheckBar AS LOGICAL     NO-UNDO.

    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr-2 FOR job-hdr.
    DEFINE VARIABLE v-lncnt       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-frstitem    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-lastitem    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-first-order AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-last-order  AS INTEGER   NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST bf-job WHERE
            bf-job.company EQ cocode AND
            trim(bf-job.job-no) EQ trim(ipcJobNo) AND
            bf-job.job-no2 EQ ipiJobNo2
            NO-LOCK NO-ERROR.

        IF AVAILABLE bf-job THEN
        DO:
            FOR EACH bf-job-hdr-2 FIELDS(i-no frm blank-no) NO-LOCK
                WHERE bf-job-hdr-2.company EQ bf-job.company
                AND bf-job-hdr-2.job-no  EQ bf-job.job-no
                AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
                AND ( bf-job-hdr-2.frm EQ ipiForm OR NOT iplCheckBar )
                AND ( bf-job-hdr-2.blank-no EQ ipiBlank OR NOT iplCheckBarBlank )
                BREAK BY bf-job-hdr-2.i-no:

                v-lncnt = v-lncnt + 1.

                IF FIRST-OF(bf-job-hdr-2.i-no) THEN
                    v-frstitem = bf-job-hdr-2.i-no.
                IF LAST-OF(bf-job-hdr-2.i-no) THEN
                    v-lastitem = bf-job-hdr-2.i-no.
            END.

            FOR EACH bf-job-hdr-2 FIELDS(ord-no frm blank-no) NO-LOCK
                WHERE bf-job-hdr-2.company EQ bf-job.company
                AND bf-job-hdr-2.job-no  EQ bf-job.job-no
                AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
                AND ( bf-job-hdr-2.frm EQ ipiForm OR NOT iplCheckBar )
                AND ( bf-job-hdr-2.blank-no EQ ipiBlank OR NOT iplCheckBarBlank )
                BREAK BY bf-job-hdr-2.ord-no:

                IF FIRST-OF(bf-job-hdr-2.ord-no) THEN
                    v-first-order = bf-job-hdr-2.ord-no.
                IF LAST-OF(bf-job-hdr-2.ord-no) THEN
                    v-last-order = bf-job-hdr-2.ord-no.
            END.

            ASSIGN
                begin_ord-no:SCREEN-VALUE = STRING(v-first-order)
                begin_job:SCREEN-VALUE    = ipcJobNo         
                begin_job2:SCREEN-VALUE   = STRING(ipiJobNo2,"99")
                end_ord-no:SCREEN-VALUE   = STRING(v-last-order)
                end_job:SCREEN-VALUE      = ipcJobNo     
                end_job2:SCREEN-VALUE     = STRING(ipiJobNo2,"99")
                begin_i-no:SCREEN-VALUE   = v-frstitem
                end_i-no:SCREEN-VALUE     = v-lastitem.    
      
            APPLY 'VALUE-CHANGED':U TO begin_ord-no.
            APPLY "LEAVE" TO end_i-no.
            RUN check-release(0) .
            RUN check-release(1) .
            IF v-lncnt EQ 1 THEN
                oplCheckBar = YES . 
            IF v-lncnt GT 1 THEN
                MESSAGE "There are multiple FG Items on this order." SKIP
                    "Please select an FG Item."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
    END.
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
    DISPLAY tbPartSelect loadtagFunction tb_ret tb_reprint-tag v-ord-list 
        v-job-list begin_ord-no begin_poLine end_ord-no end_poLine begin_job 
        begin_job2 end_job end_job2 begin_i-no end_i-no rd_order-sts rd_print 
        begin_date end_date rd_comps v-dept-list tb_dept-note tb_rel tb_over 
        tb_16ths tb_ship-id v-ship-id scr-auto-print scr-freeze-label 
        scr-label-file begin_labels begin_form begin_filename typeLabel 
        statusLabel lbl_po-no tb_xfer-lot tb_override-mult begin_ship-to 
        end_ship-to tb_close tb_print-view begin_rel end_rel tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE tbPartSelect loadtagFunction tb_ret tb_reprint-tag v-ord-list 
        v-job-list begin_ord-no end_ord-no begin_job begin_job2 end_job 
        end_job2 begin_i-no end_i-no rd_order-sts rd_print begin_date end_date 
        rd_comps tb_dept-note tb_rel tb_over tb_16ths tb_ship-id 
        scr-auto-print scr-freeze-label scr-label-file begin_labels begin_form 
        btn-ok btn-cancel tb_xfer-lot tb_override-mult begin_ship-to 
        end_ship-to tb_close tb_print-view begin_rel end_rel RECT-7 RECT-8 
        RECT-11 RECT-12 tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-update C-Win 
PROCEDURE final-update :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FOR EACH w-ord:
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ w-ord.cust-no
            NO-LOCK NO-ERROR.

        IF v-cas-lab THEN 
        DO:
            FIND FIRST loadtag
                WHERE loadtag.company     EQ cocode
                AND loadtag.tag-no      EQ fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                AND loadtag.item-type   EQ NO
                AND loadtag.is-case-tag EQ YES
                NO-LOCK NO-ERROR.
            IF AVAILABLE loadtag AND loadtag.tag-no NE "" THEN
                ASSIGN
                    w-ord.pcs        = loadtag.qty-case
                    w-ord.bundle     = loadtag.case-bundle
                    w-ord.total-unit = w-ord.pcs * w-ord.bundle
                    w-ord.lot        = loadtag.misc-char[2].
        END.

        IF v-tags EQ 0 THEN
            w-ord.total-tags = 1.
        ELSE
            IF v-tags EQ ? THEN w-ord.total-tags = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-job C-Win 
PROCEDURE from-job :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER op-warning AS LOG NO-UNDO.

    DEFINE BUFFER b-job-hdr-2 FOR job-hdr.

    DEFINE VARIABLE lv-rel-date   AS DATE NO-UNDO.
    DEFINE VARIABLE lv-tt-created AS LOG  NO-UNDO.

    FIND FIRST job NO-LOCK
        WHERE ROWID(job) EQ ip-rowid
        AND (v-stat EQ "A" OR
        (v-stat EQ "C" AND job.opened EQ NO) OR
        (v-stat EQ "O" AND job.opened EQ YES))
        NO-ERROR.

    IF NOT AVAILABLE job THEN RETURN.
    IF (v-ord-list NE '' OR begin_ord-no NE 0 OR end_ord-no NE 0) AND
        NOT CAN-FIND(ttblJob WHERE ttblJob.company EQ job.company
        AND ttblJob.job-no EQ job.job-no
        AND ttblJob.job-no2 EQ job.job-no2) THEN 
    DO:

        FOR EACH b-job-hdr-2 FIELDS(company job-no job-no2 ord-no) WHERE
            b-job-hdr-2.company EQ job.company AND
            b-job-hdr-2.job     EQ job.job AND
            b-job-hdr-2.job-no  EQ job.job-no AND
            b-job-hdr-2.job-no2 EQ job.job-no2 AND
            b-job-hdr-2.i-no    GE v-fitem[1] AND
            b-job-hdr-2.i-no    LE v-fitem[2]
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
            AND job-hdr.i-no    GE v-fitem[1]
            AND job-hdr.i-no    LE v-fitem[2]
            /*AND job-hdr.ord-no  EQ 0
           USE-INDEX ord-no*/
            /*ESP - Task 04180703 don't look at order number */

            NO-LOCK,
            FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ job-hdr.cust-no
            NO-LOCK,
            FIRST itemfg
            WHERE itemfg.company EQ cocode
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
                w-ord.over-pct     = IF tb_over THEN cust.over-pct ELSE 0
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
                w-ord.mult         = IF cust.int-field[1] NE 0 AND NOT glOverrideMult THEN
                                   cust.int-field[1] ELSE v-mult
                num-rec            = num-rec + 1
                w-ord.due-date-job = IF job.due-date <> ? THEN STRING(job.due-date, "99/99/9999") ELSE "".
            w-ord.due-date-jobhdr = IF job-hdr.due-date <> ? THEN STRING(job-hdr.due-date, "99/99/9999") ELSE "".
            w-ord.job-qty      = job-hdr.qty  .
            w-ord.zone         = itemfg.spare-char-4. 
            w-ord.ipReturn     = tb_ret.
            FOR EACH cust-part NO-LOCK 
                WHERE cust-part.company EQ cocode   
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
                    style.company EQ cocode AND
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
                    oe-ordl.company EQ cocode AND
                    oe-ordl.ord-no  EQ job-hdr.ord-no AND
                    oe-ordl.i-no    EQ job-hdr.i-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE oe-ordl THEN
                DO:
                    FIND FIRST oe-ord WHERE
                        oe-ord.company EQ cocode AND
                        oe-ord.ord-no  EQ job-hdr.ord-no
                        NO-LOCK NO-ERROR.

                    RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#,
                        OUTPUT w-ord.ship-notes,
                        OUTPUT w-ord.rel-qty,
                        INPUT ROWID(job-hdr)).
                    IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

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
                op-warning = YES.
                IF v-po-no-source = "J"  THEN
                    RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                        OUTPUT w-ord.rel-date,
                        OUTPUT w-ord.rel-lot#,
                        OUTPUT w-ord.ship-notes,
                        OUTPUT w-ord.rel-qty,
                        INPUT ROWID(job-hdr)).
            END.

            IF NOT tb_ship-id THEN v-ship-id = job-hdr.cust-no.
            FOR EACH shipto
                WHERE shipto.company EQ cocode
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


            IF NOT v-oecount THEN
                ASSIGN
                    w-ord.pcs    = w-ord.total-unit
                    w-ord.bundle = 1.

            /* Add .49 to round up and add 1 for extra tag   */
            IF w-ord.rel-qty NE 0 THEN
                w-ord.total-tags = ((w-ord.rel-qty / w-ord.total-unit) + .49) +  IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.
            ELSE
                w-ord.total-tags = ((w-ord.job-qty / w-ord.total-unit) + .49) +  IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-ord C-Win 
PROCEDURE from-ord :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE lv-stat     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-over     LIKE oe-ordl.over-pct NO-UNDO.
    DEFINE VARIABLE lv-rel-date AS DATE      NO-UNDO.
    DEFINE VARIABLE lv-job-no2  LIKE job-hdr.job-no2 NO-UNDO.
    DEFINE VARIABLE lv-job-no   LIKE job.job-no NO-UNDO.
    DEFINE BUFFER b-job-hdr FOR job-hdr. /* rtc */
    DEFINE BUFFER b-job     FOR job.         /* rtc */

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE BUFFER b-loadtag FOR loadtag. 

    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
        AND (v-stat EQ "A"                                    OR
        (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
        (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))
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

          AND (oe-ordl.i-no    ge v-fitem[1]
          AND oe-ordl.i-no    le v-fitem[2]        
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
        AND trim(ttbljob.job-no) = trim(oe-ordl.job-no)
        AND ttbljob.job-no2 = oe-ordl.job-no2
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttbljob THEN
        FIND FIRST ttbljob WHERE ttbljob.company = cocode
            AND ttbljob.ord-no = oe-ordl.ord-no
            NO-LOCK NO-ERROR.
    IF AVAILABLE ttbljob THEN
        FIND FIRST b-job-hdr WHERE b-job-hdr.company = cocode
            AND b-job-hdr.ord-no  = oe-ordl.ord-no
            AND trim(b-job-hdr.job-no) = trim(ttbljob.job-no)
            AND b-job-hdr.job-no2 = ttbljob.job-no2
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
                AND trim(ttbljob.job-no) = trim(b-job-hdr.job-no)
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

    lv-over = IF tb_over THEN oe-ordl.over-pct ELSE 0.

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
                w-ord.mult         = IF AVAILABLE cust AND cust.int-field[1] NE 0 AND NOT glOverrideMult THEN
                                   cust.int-field[1] ELSE v-mult
                w-ord.dont-run-set = oe-ordl.is-a-component
                w-ord.ord-desc1    = oe-ordl.part-dscr1
                w-ord.ord-desc2    = oe-ordl.part-dscr2
                w-ord.runShip      = oe-ordl.whsed

                /* gdm - 08130804*/
                w-ord.linenum      = oe-ordl.e-num
                w-ord.ipReturn     = tb_ret
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
            IF w-ord.job-no EQ "" AND fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
            DO:
                FIND FIRST b-loadtag WHERE
                    b-loadtag.company EQ oe-ordl.company AND
                    b-loadtag.item-type EQ NO AND
                    b-loadtag.is-case-tag EQ YES AND
                    b-loadtag.tag-no EQ fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                    NO-LOCK NO-ERROR.

                IF AVAILABLE b-loadtag THEN
                DO:

                    ASSIGN
                        w-ord.job-no  = b-loadtag.job-no
                        w-ord.job-no2 = b-loadtag.job-no2.

                    RELEASE b-loadtag.
                END.
            END.
            RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                OUTPUT w-ord.rel-date,
                OUTPUT w-ord.rel-lot#,
                OUTPUT w-ord.ship-notes,
                OUTPUT w-ord.rel-qty,
                INPUT ROWID(b-job-hdr)).
            IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

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
                IF NOT tb_ship-id THEN v-ship-id = oe-rel.ship-id.
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
                    IF NOT tb_ship-id THEN v-ship-id = oe-ord.cust-no.
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
                w-ord.total-unit = w-ord.pcs * w-ord.bundle .
            /* Add .49 to round up and add 1 for extra tag   */
            IF w-ord.rel-qty NE 0 THEN
                w-ord.total-tags = ((w-ord.rel-qty / w-ord.total-unit) + .49) +  (IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1).
            ELSE
                w-ord.total-tags = ((oe-ordl.qty / w-ord.total-unit) + .49) +  (IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1).

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
                w-ord.cust-po-no   = IF v-po-no-source EQ "L" THEN oe-ordl.po-no
                                                        ELSE
                               IF v-po-no-source EQ "R" THEN oe-rel.po-no
                                                        ELSE
                               IF v-po-no-source EQ "J" AND AVAILABLE b-job-hdr THEN b-job-hdr.po-no 
                                   ELSE "" 
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
                w-ord.mult         = IF AVAILABLE cust AND cust.int-field[1] NE 0 AND NOT glOverrideMult THEN
                                 cust.int-field[1] ELSE v-mult
                w-ord.dont-run-set = oe-ordl.is-a-component
                w-ord.ord-desc1    = oe-ordl.part-dscr1
                w-ord.ord-desc2    = oe-ordl.part-dscr2

                /* gdm - 08130804*/
                w-ord.linenum      = oe-ordl.e-num.
            w-ord.ipReturn     = tb_ret.
            num-rec            = num-rec + 1.


            ASSIGN 
                w-ord.rel-lot# = oe-rel.lot-no.
            IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.

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
                w-ord.pcs        = oe-rel.qty-case
                w-ord.bundle     = oe-rel.cases
                w-ord.total-unit = w-ord.pcs * w-ord.bundle
                /* Add .49 to round up and add 1 for extra tag   */
                w-ord.total-tags = ((oe-rel.qty / w-ord.total-unit) + .49) +  IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.
        END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-po C-Win 
PROCEDURE from-po :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    FOR EACH po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no EQ po-ord.po-no
        AND po-ordl.item-type EQ NO
        AND po-ordl.i-no GE v-fitem[1]
        AND po-ordl.i-no LE v-fitem[2]
        AND po-ordl.line GE v-ford-line[1]
        AND po-ordl.line LE v-ford-line[2]
        USE-INDEX po-no BREAK BY po-ordl.i-no:

    // IF FIRST-OF(po-ordl.i-no) THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
            AND cust.cust-no EQ po-ordl.cust-no NO-ERROR.
        FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
            AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
        FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
            AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.

        CREATE w-ord.
        ASSIGN
            w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
            w-ord.cust-no   = po-ordl.cust-no
            w-ord.ship-code = po-ord.ship-id
            w-ord.due-date  = po-ord.due-date
            w-ord.i-no      = po-ordl.i-no
            w-ord.i-name    = po-ordl.i-name
            w-ord.mult      = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                     cust.int-field[1] ELSE v-mult
            w-ord.po-no     = po-ord.po-no
            w-ord.po-line   = po-ordl.line
            w-ord.tare-wt   = 10
            w-ord.uom       = 'EA'
            w-ord.vendor    = IF AVAILABLE vend THEN vend.name ELSE ''
            num-rec         = num-rec + 1
            w-ord.ipReturn  = tb_ret.

        RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
            "EA" ,
            10 * itemfg.weight-100 / itemfg.t-sqft, /*convert to lb/MSF*/
            itemfg.t-len,
            itemfg.t-wid,
            itemfg.t-dep,
            po-ordl.ord-qty,
            OUTPUT w-ord.ord-qty).
        w-ord.qty-before = w-ord.ord-qty.

        /*04011307 Added because important information regarding order not included on loadtag*/
        IF po-ordl.ord-no > 0 THEN  
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ po-ordl.company
                AND oe-ordl.ord-no = po-ordl.ord-no
                AND oe-ordl.i-no = po-ordl.i-no 
                NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
        DO:
            ASSIGN 
                w-ord.cust-po-no = oe-ordl.po-no
                w-ord.ord-no     = oe-ordl.ord-no.
            RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                OUTPUT w-ord.rel-date,
                OUTPUT w-ord.rel-lot#,
                OUTPUT w-ord.ship-notes,
                OUTPUT w-ord.rel-qty,
                INPUT "").
            IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.
        END.

        IF AVAILABLE itemfg THEN
            ASSIGN
                w-ord.est-no       = itemfg.est-no
                w-ord.upc-no       = itemfg.upc-no
                w-ord.box-len      = itemfg.l-score[50]
                w-ord.box-wid      = itemfg.w-score[50]
                w-ord.box-dep      = itemfg.d-score[50]
                w-ord.flute        = itemfg.flute
                w-ord.test         = itemfg.test
                w-ord.pcs          = itemfg.case-count
                w-ord.bundle       = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
                w-ord.style        = itemfg.style
                w-ord.cust-part-no = itemfg.part-no 
                w-ord.zone         = itemfg.spare-char-4.
      
        IF w-ord.ord-no > 0 THEN
            FOR EACH cust-part NO-LOCK 
                WHERE cust-part.company EQ po-ord.company   
                AND cust-part.i-no EQ po-ordl.i-no 
                AND cust-part.cust-no EQ po-ordl.cust-no 
                AND cust-part.part-no NE "":
                ASSIGN  
                    w-ord.cust-part-no = cust-part.part-no .
                LEAVE.
            END.


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

        IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
            FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                AND eb.est-no EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no NO-ERROR.

        IF AVAILABLE eb THEN
            ASSIGN
                w-ord.flute      = eb.flute
                w-ord.test       = eb.test
                w-ord.pcs        = eb.cas-cnt
                w-ord.bundle     = eb.cas-pal
                w-ord.cas-no     = eb.cas-no
                w-ord.pallt-no   = eb.tr-no
                w-ord.part-dscr2 = eb.part-dscr2.
        IF NOT tb_ship-id THEN v-ship-id = po-ord.ship-id.
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ po-ord.cust-no
            AND shipto.ship-id EQ v-ship-id
            USE-INDEX ship-id NO-ERROR.
        IF AVAILABLE shipto THEN
            ASSIGN
                w-ord.ship-name  = shipto.ship-name
                w-ord.ship-add1  = shipto.ship-add[1]
                w-ord.ship-add2  = shipto.ship-add[2]
                w-ord.ship-city  = shipto.ship-city
                w-ord.ship-state = shipto.ship-state
                w-ord.ship-ctry  = shipto.country
                w-ord.ship-zip   = shipto.ship-zip
                w-ord.broker     = shipto.broker .

        ASSIGN
            w-ord.total-unit = w-ord.pcs * w-ord.bundle .
        /* Add .49 to round up and add 1 for extra tag   */
        IF w-ord.rel-qty NE 0 THEN
            w-ord.total-tags = ((w-ord.rel-qty / w-ord.total-unit) + .49) +
                (IF CAN-DO("SSLABEL,CentBox",v-loadtag) THEN 0 ELSE 1).
        ELSE
            w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) +
                (IF CAN-DO("SSLABEL,CentBox",v-loadtag) THEN 0 ELSE 1).
    // END. /* first-of */
    END. /* each po-ordl */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-freight-cost C-Win 
PROCEDURE get-freight-cost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-cost LIKE fg-rctd.frt-cost NO-UNDO.

    DEFINE BUFFER b-po-ordl-2 FOR po-ordl.

    FIND FIRST b-po-ordl-2 WHERE
        b-po-ordl-2.company   EQ fg-rctd.company AND
        b-po-ordl-2.po-no     EQ INT(fg-rctd.po-no) AND
        b-po-ordl-2.i-no      EQ fg-rctd.i-no AND
        trim(b-po-ordl-2.job-no) EQ trim(fg-rctd.job-no) AND
        b-po-ordl-2.job-no2   EQ fg-rctd.job-no2 AND
        b-po-ordl-2.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAILABLE b-po-ordl-2 THEN
        RUN po/getfrtcs.p (ROWID(b-po-ordl-2),
            fg-rctd.t-qty,
            OUTPUT op-cost).

    RUN convert-vend-comp-curr(INPUT b-po-ordl-2.po-no, INPUT-OUTPUT op-cost).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-import-ord C-Win 
PROCEDURE get-import-ord :
    /*------------------------------------------------------------------------------
      Purpose:     Get info for imported order # coming from OU1
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER v-order LIKE oe-ord.ord-no NO-UNDO.

    DEFINE VARIABLE v-lncnt      AS INTEGER NO-UNDO.

    DEFINE VARIABLE v-frstitem   LIKE oe-ordl.i-no NO-UNDO.
    DEFINE VARIABLE v-lastitem   LIKE oe-ordl.i-no NO-UNDO.
    DEFINE VARIABLE v-first-job  LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-last-job   LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-first-job2 LIKE job.job-no2 NO-UNDO.
    DEFINE VARIABLE v-last-job2  LIKE job.job-no2 NO-UNDO.

    /* ASSIGN v-order = INT(ENTRY(1,v-ord-list:SCREEN-VALUE IN FRAME {&FRAME-NAME})).  */
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST bf-oe-ord NO-LOCK 
            WHERE bf-oe-ord.company EQ cocode 
            AND bf-oe-ord.ord-no  EQ v-order NO-ERROR.

        IF AVAILABLE bf-oe-ord THEN 
        DO:
            ASSIGN 
                v-lncnt = 0.
            FOR EACH bf-oe-ordl NO-LOCK
                WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                BREAK BY bf-oe-ordl.i-no:
                ASSIGN 
                    v-lncnt = v-lncnt + 1.
                /* gdm - 09290903 */
                IF FIRST-OF(bf-oe-ordl.i-no) THEN ASSIGN v-frstitem = bf-oe-ordl.i-no.
                IF LAST-OF(bf-oe-ordl.i-no)  THEN ASSIGN v-lastitem = bf-oe-ordl.i-no.
            END.

            EMPTY TEMP-TABLE tt-ordjobs.
            FOR EACH bf-oe-ordl NO-LOCK
                WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
                CREATE  tt-ordjobs.
                ASSIGN 
                    tt-ordjobs.job-no  = bf-oe-ordl.job-no
                    tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
            END.
            ASSIGN 
                v-first-job  = ""
                v-first-job2 = 0
                v-last-job   = ""
                v-last-job2  = 0.
            FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
                IF v-first-job = "" THEN
                    v-first-job = tt-ordjobs.job-no.
                v-last-job = tt-ordjobs.job-no.
            END.
            FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
                IF v-first-job2 = 0 THEN
                    v-first-job2 = tt-ordjobs.job-no2.
                v-last-job2 = tt-ordjobs.job-no2.
            END.

            ASSIGN 
                begin_ord-no:SCREEN-VALUE = STRING(bf-oe-ord.ord-no) 
                begin_job:SCREEN-VALUE    = v-first-job
                begin_job2:SCREEN-VALUE   = STRING(v-first-job2)
                end_ord-no:SCREEN-VALUE   = STRING(bf-oe-ord.ord-no)  
                end_job:SCREEN-VALUE      = v-last-job
                end_job2:SCREEN-VALUE     = STRING(v-last-job2).

            FIND FIRST bf-oe-ordl NO-LOCK                             
                WHERE bf-oe-ordl.company EQ bf-oe-ord.company         
                AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no NO-ERROR.

            IF  v-lncnt EQ 1 THEN 
            DO WITH FRAME {&FRAME-NAME}:
                IF AVAILABLE bf-oe-ordl THEN
                    ASSIGN begin_i-no:SCREEN-VALUE = bf-oe-ordl.i-no
                        end_i-no:SCREEN-VALUE   = bf-oe-ordl.i-no.           

                APPLY "LEAVE" TO END_i-no.
            END.
            ELSE 
            DO:

                FOR EACH bf-oe-ordl 
                    WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                    AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                    NO-LOCK
                    BREAK BY bf-oe-ordl.i-no:
                    IF FIRST(bf-oe-ordl.i-no) THEN
                        v-frstitem = bf-oe-ordl.i-no.
                    IF LAST(bf-oe-ordl.i-no) THEN
                        v-lastitem = bf-oe-ordl.i-no.
                END.

                ASSIGN 
                    begin_i-no:SCREEN-VALUE = v-frstitem
                    end_i-no:SCREEN-VALUE   = v-lastitem.

                EMPTY TEMP-TABLE tt-ordjobs.
                FOR EACH bf-oe-ordl NO-LOCK
                    WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                    AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
                    CREATE  tt-ordjobs.
                    ASSIGN 
                        tt-ordjobs.job-no  = bf-oe-ordl.job-no
                        tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
                END.
                ASSIGN 
                    v-first-job  = ""
                    v-first-job2 = 0
                    v-last-job   = ""
                    v-last-job2  = 0.
                FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
                    IF v-first-job = "" THEN
                        v-first-job = tt-ordjobs.job-no.
                    v-last-job = tt-ordjobs.job-no.
                END.
                FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
                    IF v-first-job2 = 0 THEN
                        v-first-job2 = tt-ordjobs.job-no2.
                    v-last-job2 = tt-ordjobs.job-no2.
                END.
                ASSIGN 
                    begin_job:SCREEN-VALUE  = v-first-job
                    begin_job2:SCREEN-VALUE = STRING(v-first-job2)

                    end_job:SCREEN-VALUE    = v-last-job
                    end_job2:SCREEN-VALUE   = STRING(v-last-job2).

                APPLY "LEAVE" TO END_i-no.

                /* gdm - 09290903 */
                MESSAGE 
                    "There are multiple Fg Items in this order." SKIP
                    "     Please select an FG Item."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            END. /* if > 1 items */

        END. /* avail bf-ord */

    END. /* do with frame */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-jobord-info C-Win 
PROCEDURE get-jobord-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-order       LIKE oe-ord.ord-no NO-UNDO.
    DEFINE VARIABLE v-lncnt       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-frstitem    LIKE oe-ordl.i-no NO-UNDO.
    DEFINE VARIABLE v-lastitem    LIKE oe-ordl.i-no NO-UNDO.

    DEFINE VARIABLE v-first-order AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-last-order  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE ll            AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE lv-job-no     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-job-no2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-job         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-job2        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcForm        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iForm         AS CHARACTER NO-UNDO .
    DEFINE VARIABLE iBlank-no     AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lCheckForm    AS LOGICAL   INIT YES NO-UNDO .
    DEFINE VARIABLE lCheckBlank   AS LOGICAL   INIT YES NO-UNDO .
    DEFINE VARIABLE oplCheckForm  AS LOGICAL   INIT NO NO-UNDO .

    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr-2 FOR job-hdr.


    v-job = ENTRY(1,v-job-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    /*    IF SUBSTRING(TRIM(v-job),LENGTH(TRIM(v-job)) - 5,1) = "-" THEN DO: */
    /*        lcForm = SUBSTRING(TRIM(v-job),LENGTH(TRIM(v-job)) - 1,2).     */
    /*        IF INT(lcForm) > 0 THEN DO:                                    */
    /*            iForm = INT(lcForm).                                       */
    /*            lForm = YES.                                               */
    /*        END.                                                           */
    /*        v-job = SUBSTRING(v-job,1,9).                                  */
    /*    END.                                                               */
    /*    ELSE lForm = NO.                                                   */
    /*    MESSAGE lcForm  iForm lForm v-job                                  */
    /*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                             */
    DO li = 1 TO LENGTH(v-job):
        IF INDEX("/:-",SUBSTR(v-job,li,1)) GT 0 THEN
            ll = ll + 1.
        /*ELSE LEAVE.*/
        ELSE 
        DO:
            IF ll EQ 1 THEN lv-job-no = lv-job-no + SUBSTR(v-job,li,1).
            ELSE IF ll EQ 2 THEN lv-job-no2 = lv-job-no2 + SUBSTR(v-job,li,1).
                ELSE IF ll EQ 3 THEN iForm = iForm + SUBSTR(v-job,li,1) NO-ERROR .
                    ELSE IF ll EQ 4 THEN iBlank-no = iBlank-no + SUBSTR(v-job,li,1) NO-ERROR .
        END.
    END.
    IF iForm EQ "" THEN
        lCheckForm = NO .
    IF iBlank-no EQ "" THEN
        lCheckBlank = NO .

    ASSIGN
        lv-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', lv-job-no)) 
        v-job2    = INT(lv-job-no2).
    RUN dispJobInfo (INPUT cocode, INPUT lv-job-no, INPUT v-job2,INPUT iForm, INPUT iBlank-no, INPUT lCheckForm, INPUT lCheckBlank, OUTPUT oplCheckForm ).
    IF lCheckForm AND oplCheckForm THEN
        APPLY "choose" TO btn-ok.
    IF NOT oplCheckForm THEN 
    DO:
        MESSAGE "Please enter correct data..." SKIP
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
/*
FIND FIRST bf-job WHERE
     bf-job.company EQ cocode AND
     trim(bf-job.job-no) EQ trim(lv-job-no) AND
     bf-job.job-no2 EQ v-job2
     NO-LOCK NO-ERROR.

IF AVAIL bf-job THEN
DO:
   FOR EACH bf-job-hdr-2 FIELDS(i-no) NO-LOCK
       WHERE bf-job-hdr-2.company EQ bf-job.company
         AND bf-job-hdr-2.job-no  EQ bf-job.job-no
         AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
        BREAK BY bf-job-hdr-2.i-no:

        v-lncnt = v-lncnt + 1.

        IF FIRST-OF(bf-job-hdr-2.i-no) THEN
           v-frstitem = bf-job-hdr-2.i-no.
        IF LAST-OF(bf-job-hdr-2.i-no) THEN
           v-lastitem = bf-job-hdr-2.i-no.
   END.

   FOR EACH bf-job-hdr-2 FIELDS(ord-no) NO-LOCK
       WHERE bf-job-hdr-2.company EQ bf-job.company
         AND bf-job-hdr-2.job-no  EQ bf-job.job-no
         AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
        BREAK BY bf-job-hdr-2.ord-no:

        IF FIRST-OF(bf-job-hdr-2.ord-no) THEN
           v-first-order = bf-job-hdr-2.ord-no.
        IF LAST-OF(bf-job-hdr-2.ord-no) THEN
           v-last-order = bf-job-hdr-2.ord-no.
   END.

   ASSIGN
      begin_ord-no:SCREEN-VALUE = STRING(v-first-order)
      begin_job:SCREEN-VALUE    = lv-job-no         
      begin_job2:SCREEN-VALUE   = STRING(v-job2,"99")
      end_ord-no:SCREEN-VALUE   = STRING(v-last-order)
      end_job:SCREEN-VALUE      = lv-job-no     
      end_job2:SCREEN-VALUE     = STRING(v-job2,"99")
      begin_i-no:SCREEN-VALUE = v-frstitem
      end_i-no:SCREEN-VALUE   = v-lastitem.           

   APPLY "LEAVE" TO end_i-no.

   IF v-lncnt GT 1 THEN
      MESSAGE "There are multiple FG Items on this order." skip
              "Please select an FG Item."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
*/
/*
FIND FIRST bf-jobhdr WHERE
     bf-jobhdr.company EQ cocode AND
     trim(bf-jobhdr.job-no)  EQ trim(lv-job-no) AND
     bf-jobhdr.job-no2 EQ v-job2 AND
     bf-jobhdr.ord-no NE 0
     NO-LOCK NO-ERROR.

IF NOT AVAIL bf-jobhdr THEN
   FIND FIRST bf-jobhdr WHERE
        bf-jobhdr.company EQ cocode AND
        trim(bf-jobhdr.job-no)  EQ trim(lv-job-no) AND
        bf-jobhdr.job-no2 EQ v-job2
        NO-LOCK NO-ERROR.

IF AVAIL bf-jobhdr THEN DO:
   FIND FIRST bf-oe-ord WHERE
        bf-oe-ord.company EQ cocode AND
        bf-oe-ord.ord-no  EQ bf-jobhdr.ord-no
        NO-LOCK NO-ERROR.

   IF AVAIL bf-oe-ord THEN DO:

      FOR EACH bf-oe-ordl FIELDS(i-no) NO-LOCK
        WHERE bf-oe-ordl.company EQ bf-oe-ord.company
          AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
         BREAK BY bf-oe-ordl.i-no:

         v-lncnt = v-lncnt + 1.

         IF FIRST-OF(bf-oe-ordl.i-no) THEN
            ASSIGN v-frstitem = bf-oe-ordl.i-no.
         IF LAST-OF(bf-oe-ordl.i-no) THEN
            ASSIGN v-lastitem = bf-oe-ordl.i-no.
      END.

      FIND FIRST bf-oe-ordl 
           WHERE bf-oe-ordl.company EQ bf-oe-ord.company
             AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
           NO-LOCK NO-ERROR.

      ASSIGN
         begin_ord-no:SCREEN-VALUE = STRING(bf-oe-ordl.ord-no) 
         begin_job:SCREEN-VALUE    = lv-job-no         
         begin_job2:SCREEN-VALUE   = STRING(v-job2,"99")
         end_ord-no:SCREEN-VALUE   = STRING(bf-oe-ordl.ord-no)  
         end_job:SCREEN-VALUE      = lv-job-no     
         end_job2:SCREEN-VALUE     = STRING(v-job2,"99").

      IF v-lncnt EQ 1 THEN DO WITH FRAME {&FRAME-NAME}:
         ASSIGN
            begin_i-no:SCREEN-VALUE = bf-oe-ordl.i-no
            end_i-no:SCREEN-VALUE   = bf-oe-ordl.i-no.           

         APPLY "LEAVE" TO end_i-no.
      END.
      ELSE DO:

         FIND FIRST bf-oe-ordl WHERE                             
              bf-oe-ordl.company EQ bf-oe-ord.company AND
              bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
              NO-LOCK NO-ERROR.

         v-frstitem = bf-oe-ordl.i-no.

         FIND LAST bf-oe-ordl WHERE
              bf-oe-ordl.company EQ bf-oe-ord.company AND
              bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
              NO-LOCK NO-ERROR.

         ASSIGN
            v-lastitem = bf-oe-ordl.i-no
            begin_i-no:SCREEN-VALUE = v-frstitem
            end_i-no:SCREEN-VALUE   = v-lastitem.

         APPLY "LEAVE" TO end_i-no.

         /* gdm - 09290903 */
         MESSAGE "There are multiple FG Items on this order." skip
                 "Please select an FG Item."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
   END. /* avail bf-oe-ord */
   ELSE /* not avail bf-oe-ord*/
   DO:
      ASSIGN
         begin_ord-no:SCREEN-VALUE = "0" 
         begin_job:SCREEN-VALUE    = lv-job-no         
         begin_job2:SCREEN-VALUE   = STRING(v-job2,"99")
         end_ord-no:SCREEN-VALUE   = "0"  
         end_job:SCREEN-VALUE      = lv-job-no     
         end_job2:SCREEN-VALUE     = STRING(v-job2,"99").

      FOR EACH bf-job-hdr-2 FIELDS(i-no) WHERE
          bf-job-hdr-2.company EQ bf-jobhdr.company AND
          bf-job-hdr-2.job-no  EQ bf-jobhdr.job-no AND
          bf-job-hdr-2.job-no2 EQ bf-jobhdr.job-no2
          NO-LOCK
          BREAK BY bf-job-hdr-2.i-no:

          v-lncnt = v-lncnt + 1.

         IF FIRST-OF(bf-job-hdr-2.i-no) THEN
            ASSIGN v-frstitem = bf-job-hdr-2.i-no.
         IF LAST-OF(bf-job-hdr-2.i-no) THEN
            ASSIGN v-lastitem = bf-job-hdr-2.i-no.
      END.

      DO WITH FRAME {&FRAME-NAME}:
         ASSIGN
            begin_i-no:SCREEN-VALUE = v-frstitem
            end_i-no:SCREEN-VALUE   = v-lastitem.           

         APPLY "LEAVE" TO end_i-no.

         IF v-lncnt GT 1 THEN
            MESSAGE "There are multiple FG Items on this order." skip
                   "Please select an FG Item."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
   END.
END. /* avail bf-jobhdr */
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-ordl-info C-Win 
PROCEDURE get-ordl-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-order      LIKE oe-ord.ord-no NO-UNDO.

    DEFINE VARIABLE v-lncnt      AS INTEGER NO-UNDO.

    DEFINE VARIABLE v-frstitem   LIKE oe-ordl.i-no NO-UNDO.
    DEFINE VARIABLE v-lastitem   LIKE oe-ordl.i-no NO-UNDO.
    DEFINE VARIABLE v-first-job  LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-last-job   LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-first-job2 LIKE job.job-no2 NO-UNDO.
    DEFINE VARIABLE v-last-job2  LIKE job.job-no2 NO-UNDO.

    ASSIGN 
        v-order = INT(ENTRY(1,v-ord-list:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

    FIND FIRST bf-oe-ord NO-LOCK 
        WHERE bf-oe-ord.company EQ cocode 
        AND bf-oe-ord.ord-no  EQ v-order NO-ERROR.
    IF AVAILABLE bf-oe-ord THEN 
    DO:
        ASSIGN 
            v-lncnt = 0.
        FOR EACH bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ bf-oe-ord.company
            AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
            BREAK BY bf-oe-ordl.i-no:
            ASSIGN 
                v-lncnt = v-lncnt + 1.
            /* gdm - 09290903 */
            IF FIRST-OF(bf-oe-ordl.i-no) THEN ASSIGN v-frstitem = bf-oe-ordl.i-no.
            IF LAST-OF(bf-oe-ordl.i-no)  THEN ASSIGN v-lastitem = bf-oe-ordl.i-no.
        END.
        EMPTY TEMP-TABLE tt-ordjobs.
        FOR EACH bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ bf-oe-ord.company
            AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
            CREATE  tt-ordjobs.
            ASSIGN 
                tt-ordjobs.job-no  = bf-oe-ordl.job-no
                tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
        END.
        ASSIGN 
            v-first-job  = ""
            v-first-job2 = 0
            v-last-job   = ""
            v-last-job2  = 0.
        FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
            IF v-first-job = "" THEN
                v-first-job = tt-ordjobs.job-no.
            v-last-job = tt-ordjobs.job-no.
        END.
        FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
            IF v-first-job2 = 0 THEN
                v-first-job2 = tt-ordjobs.job-no2.
            v-last-job2 = tt-ordjobs.job-no2.
        END.

        ASSIGN 
            begin_ord-no:SCREEN-VALUE = STRING(bf-oe-ord.ord-no) 
            begin_job:SCREEN-VALUE    = v-first-job
            begin_job2:SCREEN-VALUE   = STRING(v-first-job2)
            end_ord-no:SCREEN-VALUE   = STRING(bf-oe-ord.ord-no)  
            end_job:SCREEN-VALUE      = v-last-job
            end_job2:SCREEN-VALUE     = STRING(v-last-job2).          
  
        RUN pLabelPerSkid(bf-oe-ord.cust-no).
    
        FIND FIRST bf-oe-ordl NO-LOCK                             
            WHERE bf-oe-ordl.company EQ bf-oe-ord.company         
            AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no NO-ERROR.

        IF v-lncnt EQ 1 THEN 
        DO WITH FRAME {&FRAME-NAME}:
            IF AVAILABLE bf-oe-ordl THEN
                ASSIGN begin_i-no:SCREEN-VALUE = bf-oe-ordl.i-no
                    end_i-no:SCREEN-VALUE   = bf-oe-ordl.i-no.           

            APPLY "LEAVE" TO END_i-no.
        END.
        ELSE 
        DO:

            FOR EACH bf-oe-ordl 
                WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                NO-LOCK
                BREAK BY bf-oe-ordl.i-no:
                IF FIRST(bf-oe-ordl.i-no) THEN
                    v-frstitem = bf-oe-ordl.i-no.
                IF LAST(bf-oe-ordl.i-no) THEN
                    v-lastitem = bf-oe-ordl.i-no.
            END.
            ASSIGN 
                begin_i-no:SCREEN-VALUE = v-frstitem
                end_i-no:SCREEN-VALUE   = v-lastitem.

            EMPTY TEMP-TABLE tt-ordjobs.
            FOR EACH bf-oe-ordl NO-LOCK
                WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no:
                CREATE  tt-ordjobs.
                ASSIGN 
                    tt-ordjobs.job-no  = bf-oe-ordl.job-no
                    tt-ordjobs.job-no2 = bf-oe-ordl.job-no2.
            END.
            ASSIGN 
                v-first-job  = ""
                v-first-job2 = 0
                v-last-job   = ""
                v-last-job2  = 0.
            FOR EACH tt-ordjobs BY tt-ordjobs.job-no:
                IF v-first-job = "" THEN
                    v-first-job = tt-ordjobs.job-no.
                v-last-job = tt-ordjobs.job-no.
            END.
            FOR EACH tt-ordjobs BY tt-ordjobs.job-no2:
                IF v-first-job2 = 0 THEN
                    v-first-job2 = tt-ordjobs.job-no2.
                v-last-job2 = tt-ordjobs.job-no2.
            END.
            ASSIGN 
                begin_job:SCREEN-VALUE  = v-first-job
                begin_job2:SCREEN-VALUE = STRING(v-first-job2)

                end_job:SCREEN-VALUE    = v-last-job
                end_job2:SCREEN-VALUE   = STRING(v-last-job2).

            APPLY "LEAVE" TO END_i-no.

            /* gdm - 09290903 */
            MESSAGE 
                "There are multiple Fg Items in this order." SKIP
                "     Please select an FG Item."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-po-info C-Win 
PROCEDURE get-po-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcState AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-poord    LIKE po-ord.po-no NO-UNDO.

    DEFINE VARIABLE v-lncnt    AS INTEGER NO-UNDO.

    DEFINE VARIABLE v-frstitem LIKE oe-ordl.i-no NO-UNDO INIT "ZZZZZZZZZZZZZZZZZ".
    DEFINE VARIABLE v-lastitem LIKE oe-ordl.i-no NO-UNDO INIT "".
    DEFINE VARIABLE iBeginLine AS INTEGER NO-UNDO INIT 99.
    DEFINE VARIABLE iEndLine   AS INTEGER NO-UNDO INIT 99.
    ASSIGN 
        v-poord    = INT(ENTRY(1,v-ord-list:SCREEN-VALUE IN FRAME {&FRAME-NAME}))
        iBeginLine = INTEGER(begin_poLine:SCREEN-VALUE IN FRAME {&FRAME-NAME})
        iEndLine   = INTEGER(end_poLine:SCREEN-VALUE IN FRAME {&FRAME-NAME})
        .

    FIND FIRST bf-po-ord NO-LOCK 
        WHERE bf-po-ord.company EQ cocode 
        AND bf-po-ord.po-no  EQ v-poord NO-ERROR.
    IF AVAILABLE bf-po-ord THEN 
    DO:
        ASSIGN 
            v-lncnt = 0.
        FOR EACH bf-po-ordl NO-LOCK
            WHERE bf-po-ordl.company EQ bf-po-ord.company
            AND bf-po-ordl.po-no  EQ bf-po-ord.po-no
            AND (bf-po-ordl.line GE iBeginLine OR ipcState EQ "Initial")
            AND (bf-po-ordl.line LE iEndLine OR ipcState EQ "Initial")
      
            BREAK BY bf-po-ordl.i-no BY bf-po-ordl.line :
            IF FIRST-OF(bf-po-ordl.i-no) THEN 
                ASSIGN v-lncnt = v-lncnt + 1.

            IF bf-po-ordl.i-no LT v-frstitem THEN v-frstitem = bf-po-ordl.i-no.
            IF bf-po-ordl.i-no GT v-lastitem THEN v-lastitem = bf-po-ordl.i-no.                                                                           .


        END.

        FIND FIRST bf-po-ordl NO-LOCK
            WHERE bf-po-ordl.company EQ bf-po-ord.company
            AND bf-po-ordl.po-no  EQ bf-po-ord.po-no NO-ERROR.

        ASSIGN 
            begin_ord-no:SCREEN-VALUE = STRING(bf-po-ordl.po-no) 
            end_ord-no:SCREEN-VALUE   = STRING(bf-po-ordl.po-no)
            .
        IF ipcState EQ "Initial" THEN 
            ASSIGN 
                begin_poLine:SCREEN-VALUE = "1"
                end_poLine:SCREEN-VALUE   = "99"
                .

        IF v-lncnt EQ 1 THEN 
        DO WITH FRAME {&FRAME-NAME}:

            ASSIGN 
                begin_i-no:SCREEN-VALUE = bf-po-ordl.i-no
                end_i-no:SCREEN-VALUE   = bf-po-ordl.i-no.           
            IF ipcState EQ "Initial" THEN 
                APPLY "LEAVE" TO END_i-no.

        END.
        ELSE 
        DO:

            /*    FIND FIRST bf-po-ordl NO-LOCK                         */
            /*     WHERE bf-po-ordl.company EQ bf-po-ord.company        */
            /*       AND bf-po-ordl.po-no  EQ bf-po-ord.po-no           */
            /*      BY                                                  */
            /*       NO-ERROR.                                          */
            /*    ASSIGN v-frstitem = bf-po-ordl.i-no.                  */
            /*                                                          */
            /*    FIND LAST bf-po-ordl NO-LOCK                          */
            /*     WHERE bf-po-ordl.company EQ bf-po-ord.company        */
            /*       AND bf-po-ordl.po-no  EQ bf-po-ord.po-no NO-ERROR. */
            /*    ASSIGN v-lastitem = bf-po-ordl.i-no.                  */

            ASSIGN 
                begin_i-no:SCREEN-VALUE = v-frstitem
                end_i-no:SCREEN-VALUE   = v-lastitem.
            IF ipcState EQ "Initial" THEN 
            DO:
                APPLY "LEAVE" TO END_i-no.
    
                MESSAGE 
                    "There are multiple Fg Items in this PO." SKIP
                    "     Please select an FG Item."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            END.
        END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rel-info C-Win 
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
        AND oe-relh.rel-date GE begin_date
        AND oe-relh.rel-date LE end_date
        AND oe-relh.ship-id GE begin_ship-to
        AND oe-relh.ship-id LE end_ship-to
        AND (oe-relh.release# GE begin_rel OR begin_rel EQ 0)
        AND (oe-relh.release# LE end_rel OR end_rel EQ 0)
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
            AND oe-rel.rel-date GE begin_date
            AND oe-rel.rel-date LE end_date
            AND oe-rel.ship-id GE begin_ship-to
            AND oe-rel.ship-id LE end_ship-to
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
    IF v-po-no-source EQ "J" THEN
        FIND FIRST b-job-hdr WHERE ROWID(b-job-hdr) EQ ip-job NO-LOCK NO-ERROR .

    IF v-po-no-source NE "R"                    OR
        (NOT AVAILABLE oe-rel AND NOT AVAILABLE oe-rell) THEN 
    DO:
        IF  v-po-no-source NE "J" THEN
            op-pono = IF v-po-no-source EQ "L" AND AVAILABLE oe-ordl THEN oe-ordl.po-no
            ELSE IF v-po-no-source EQ "H" AND AVAILABLE oe-ord THEN oe-ord.po-no
            ELSE "".
        IF  v-po-no-source EQ "J" THEN 
            op-pono =  IF AVAILABLE b-job-hdr THEN b-job-hdr.po-no 
            ELSE "" .

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-set-full-qty C-Win 
PROCEDURE get-set-full-qty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipc-job-no     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipi-job-no2    AS INTEGER  NO-UNDO.
    DEFINE INPUT PARAMETER ipc-i-no       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipd-current-qty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-cost-to-set AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-out-qty AS DECIMAL NO-UNDO.


    DEFINE VARIABLE v-len          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep          LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-calc-cost   AS DECIMAL.
    DEFINE VARIABLE lv-recalc-cost AS DECIMAL.
    DEFINE VARIABLE lv-ext-cost    AS DECIMAL.
    DEFINE VARIABLE v-rec-qty      AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-ea          AS LOG     NO-UNDO.

    DEFINE BUFFER b-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER b1-fg-rctd FOR fg-rctd.


    cocode = g_company.

    lv-out-qty = 0.
    FOR EACH b-fg-rctd WHERE b-fg-rctd.company EQ g_company AND
        (b-fg-rctd.rita-code EQ "R" OR b-fg-rctd.rita-code EQ "E")
        AND trim(b-fg-rctd.job-no) = trim(ipc-job-no)
        AND b-fg-rctd.job-no2 = INT(ipi-job-no2)
        AND b-fg-rctd.i-no = ipc-i-no 
        NO-LOCK :

        lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
        IF ip-cost-to-set GT 0 THEN 
        DO:

            /* convert cost to b1-fg-rctd uom */

            FIND b1-fg-rctd WHERE ROWID(b1-fg-rctd) EQ ROWID(b-fg-rctd)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE b1-fg-rctd THEN 
            DO WITH FRAME {&FRAME-NAME}:

                FIND itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no  EQ b-fg-rctd.i-no
                    USE-INDEX i-no NO-LOCK NO-ERROR.

                ASSIGN
                    v-bwt = 0
                    v-dep = 0.

                IF AVAILABLE itemfg THEN
                    ASSIGN v-len = itemfg.t-len
                        v-wid = itemfg.t-wid.

                /* Always find just to get quantity */
                FIND FIRST po-ordl WHERE po-ordl.company = cocode
                    AND po-ordl.po-no   = int(b-fg-rctd.po-no)
                    AND po-ordl.LINE   = int(b-fg-rctd.po-line)
                    AND po-ordl.i-no    = b-fg-rctd.i-no
                    AND trim(po-ordl.job-no)  = trim(b-fg-rctd.job-no)
                    AND po-ordl.job-no2 = b-fg-rctd.job-no2
                    AND po-ordl.item-type = NO
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE po-ordl THEN
                    FIND FIRST po-ordl WHERE po-ordl.company = cocode
                        AND po-ordl.po-no   = integer(b-fg-rctd.po-no)
                        AND po-ordl.i-no    = b-fg-rctd.i-no
                        AND po-ordl.item-type = NO
                        NO-LOCK NO-ERROR.


                IF AVAILABLE po-ordl THEN
                    ASSIGN
                        v-len = po-ordl.s-len
                        v-wid = po-ordl.s-wid.
                lv-calc-cost = ip-cost-to-set.
                lv-recalc-cost = lv-calc-cost.
                IF fg-rctd.cost-uom EQ b-fg-rctd.cost-uom               OR
                    (DYNAMIC-FUNCTION("Conv_IsEAUOM",fg-rctd.company, fg-rctd.i-no, fg-rctd.cost-uom) AND
                    DYNAMIC-FUNCTION("Conv_IsEAUOM",b-fg-rctd.company, b-fg-rctd.i-no, b-fg-rctd.cost-uom))   THEN.
                ELSE
                    RUN rm/convcuom.p(fg-rctd.cost-uom, b-fg-rctd.cost-uom, 
                        v-bwt, v-len, v-wid, v-dep,
                        lv-calc-cost, OUTPUT lv-recalc-cost).

                b1-fg-rctd.std-cost = lv-recalc-cost.
                ASSIGN
                    lv-ext-cost         = b1-fg-rctd.t-qty * b1-fg-rctd.std-cost                          
                    b1-fg-rctd.ext-cost = lv-ext-cost + b1-fg-rctd.frt-cost.
            END.

        END.
    END.

    lv-out-qty = lv-out-qty + ipd-current-qty.

    op-out-qty = lv-out-qty.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetNextLoadtagNumber C-Win 
PROCEDURE GetNextLoadtagNumber PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNextTag AS INTEGER NO-UNDO.

    DEFINE VARIABLE iLastFGTag AS INTEGER.
    DEFINE VARIABLE iLastRMTag AS INTEGER.

    FIND LAST loadtag NO-LOCK
        WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcFGItemID 
        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcFGItemID
        USE-INDEX tag NO-ERROR.
    iLastFGTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.

    FIND LAST loadtag NO-LOCK
        WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ YES
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcFGItemID 
        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcFGItemID
        USE-INDEX tag NO-ERROR.
    iLastRMTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
  
    opiNextTag = MAX(iLastFGTag, iLastRMTag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE incrementPalletID C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-job-label C-Win 
PROCEDURE leave-job-label :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-job-no     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-job-no-end AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-cust-no    AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-job-hdr-2 FOR job-hdr.
    DEFINE VARIABLE oplCheckForm AS LOGICAL INIT NO NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF scr-label-file:SCREEN-VALUE EQ "" AND
            begin_job:SCREEN-VALUE NE "" AND 
            end_job:SCREEN-VALUE NE "" AND
            begin_job:SCREEN-VALUE EQ end_job:SCREEN-VALUE AND
            INT(begin_job2:SCREEN-VALUE) EQ INT(end_job2:SCREEN-VALUE) AND
            v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
        DO:
            RELEASE reftable.
            RELEASE oe-ord.
            RELEASE oe-rel.
            RELEASE job-hdr.
            RELEASE shipto.

            v-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job:SCREEN-VALUE)) .

            IF begin_i-no:SCREEN-VALUE EQ end_i-no:SCREEN-VALUE AND
                begin_i-no:SCREEN-VALUE NE "" THEN
                FIND FIRST job-hdr WHERE
                    job-hdr.company EQ cocode AND
                    trim(job-hdr.job-no) EQ trim(v-job-no) AND
                    job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE) AND
                    job-hdr.i-no EQ begin_i-no:SCREEN-VALUE AND
                    job-hdr.ord-no NE 0
                    NO-LOCK NO-ERROR.
            ELSE
            DO:
                FIND FIRST job-hdr WHERE
                    job-hdr.company EQ cocode AND
                    trim(job-hdr.job-no) EQ trim(v-job-no) AND
                    job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE) AND
                    job-hdr.ord-no NE 0
                    NO-LOCK NO-ERROR.

                IF NOT AVAILABLE job-hdr THEN
                    FIND FIRST job-hdr WHERE
                        job-hdr.company EQ cocode AND
                        trim(job-hdr.job-no) EQ trim(v-job-no) AND
                        job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
                        NO-LOCK NO-ERROR.
            END.

            IF AVAILABLE job-hdr THEN 
            DO:
                v-cust-no = job-hdr.cust-no.
                IF begin_i-no:SCREEN-VALUE EQ "" THEN
                    RUN dispJobInfo (INPUT cocode, INPUT v-job-no, INPUT INT(begin_job2:SCREEN-VALUE), 0,0, NO,NO, OUTPUT oplCheckForm).
            END.


            IF job-hdr.ord-no NE 0 THEN
                FIND FIRST oe-ord WHERE
                    oe-ord.company EQ cocode AND
                    oe-ord.ord-no  EQ job-hdr.ord-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ord THEN
                v-cust-no = oe-ord.cust-no.

            FIND FIRST cust-part WHERE
                cust-part.company  EQ cocode AND
                cust-part.i-no     EQ job-hdr.i-no AND
                cust-part.cust-no  EQ v-cust-no
                NO-LOCK NO-ERROR.

            IF AVAILABLE cust-part AND cust-part.labelCase NE "" THEN
                scr-label-file:SCREEN-VALUE = (IF cust-part.labelPallet <> "" THEN
                    cust-part.labelPallet
                    ELSE bardir-chr).
            ELSE 
            DO:
                IF AVAILABLE oe-ord THEN
                    FIND FIRST oe-rel WHERE
                        oe-rel.company EQ cocode AND
                        oe-rel.i-no    EQ job-hdr.i-no AND
                        oe-rel.ord-no  EQ oe-ord.ord-no 
                        NO-LOCK NO-ERROR.

                IF AVAILABLE oe-rel THEN 
                    FIND FIRST shipto WHERE
                        shipto.company EQ cocode AND
                        shipto.cust-no EQ oe-rel.cust-no AND
                        shipto.ship-id EQ oe-rel.ship-id 
                        USE-INDEX ship-id NO-LOCK NO-ERROR.
                ELSE
                    FIND FIRST shipto WHERE
                        shipto.company EQ cocode AND
                        shipto.cust-no EQ v-cust-no AND
                        shipto.ship-id EQ v-cust-no
                        USE-INDEX ship-id NO-LOCK NO-ERROR.

                IF AVAILABLE shipto THEN 
                DO:

                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company EQ cocode AND
                        sys-ctrl-shipto.NAME EQ "BARDIR" AND
                        sys-ctrl-shipto.cust-vend EQ YES AND
                        sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                        sys-ctrl-shipto.ship-id      EQ shipto.ship-id AND
                        sys-ctrl-shipto.char-fld     NE ''
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE sys-ctrl-shipto AND
                        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                        scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                    ELSE 
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company      EQ cocode AND
                            sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                            sys-ctrl-shipto.cust-vend    EQ YES AND
                            sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                            sys-ctrl-shipto.char-fld     NE ''
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE sys-ctrl-shipto AND 
                        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                        scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                    IF scr-label-file:SCREEN-VALUE EQ "" THEN
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company      EQ cocode AND
                            sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                            sys-ctrl-shipto.cust-vend-no EQ "" AND
                            sys-ctrl-shipto.cust-vend    EQ YES 
                            NO-LOCK NO-ERROR.

                        IF AVAILABLE sys-ctrl-shipto AND 
                            TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                            scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                        ELSE 
                        DO:
                            FIND FIRST sys-ctrl NO-LOCK 
                                WHERE sys-ctrl.company EQ cocode 
                                AND sys-ctrl.name    EQ "BARDIR" 
                                NO-ERROR.
                            IF AVAILABLE sys-ctrl THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                            ELSE
                                scr-label-file:SCREEN-VALUE = "".
                        END.
                    END.
                END.
                ELSE
                DO:
                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company      EQ cocode AND
                        sys-ctrl-shipto.NAME         EQ "BARDIR" AND
                        sys-ctrl-shipto.cust-vend    EQ YES AND
                        sys-ctrl-shipto.cust-vend-no EQ v-cust-no AND
                        sys-ctrl-shipto.char-fld     NE ''
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE sys-ctrl-shipto AND 
                        TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                        scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
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
                            scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                        ELSE 
                        DO:
                            FIND FIRST sys-ctrl WHERE
                                sys-ctrl.company EQ cocode AND
                                sys-ctrl.name    EQ "BARDIR" 
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE sys-ctrl THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                            ELSE
                                scr-label-file:SCREEN-VALUE = "".
                        END.
                    END.
                END.
            END.
        END.
    END.

    v-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job:SCREEN-VALUE))  .
    IF begin_i-no:SCREEN-VALUE EQ "" THEN
        RUN dispJobInfo (INPUT cocode, INPUT v-job-no, INPUT INT(begin_job2:SCREEN-VALUE),0,0,NO,NO,OUTPUT oplCheckForm).


    v-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job:SCREEN-VALUE)) .
    v-job-no-end = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job:SCREEN-VALUE)).

    IF INT(end_job2:SCREEN-VALUE) > 0 AND TRIM(v-job-no) > ""  THEN 
    DO:
        FIND FIRST b-job-hdr-2 WHERE trim(b-job-hdr-2.job-no) EQ trim(v-job-no)
            NO-LOCK NO-ERROR.

        FOR EACH b-job-hdr-2  WHERE
            b-job-hdr-2.company EQ cocode AND
            fill(" ",9 - length(TRIM(b-job-hdr-2.job-no))) + trim(b-job-hdr-2.job-no) GE trim(v-job-no) AND
            b-job-hdr-2.job-no2 GE INT(begin_job2:SCREEN-VALUE) AND
            fill(" ",9 - length(TRIM(b-job-hdr-2.job-no))) + trim(b-job-hdr-2.job-no)  LE v-job-no-end AND                
            b-job-hdr-2.job-no2 LE INT(end_job2:SCREEN-VALUE) AND
            b-job-hdr-2.i-no    GE begin_i-no:SCREEN-VALUE AND
            b-job-hdr-2.i-no    LE end_i-no:SCREEN-VALUE
            NO-LOCK:
            IF NOT CAN-FIND(FIRST ttblJob WHERE
                ttblJob.company EQ b-job-hdr-2.company AND
                ttblJob.job-no  EQ b-job-hdr-2.job-no AND
                ttblJob.job-no2 EQ b-job-hdr-2.job-no2 AND
                ttblJob.ord-no  EQ b-job-hdr-2.ord-no) THEN
            DO:
                CREATE ttblJob.
                ASSIGN
                    ttblJob.company = b-job-hdr-2.company
                    ttblJob.job-no  = b-job-hdr-2.job-no
                    ttblJob.job-no2 = b-job-hdr-2.job-no2
                    ttblJob.ord-no  = b-job-hdr-2.ord-no.
                RELEASE ttblJob.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cas-lab C-Win 
PROCEDURE new-cas-lab :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF fi_cas-lab:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST loadtag
                WHERE loadtag.company     EQ cocode
                AND loadtag.tag-no      BEGINS TRIM(fi_cas-lab:SCREEN-VALUE)
                AND loadtag.item-type   EQ NO
                AND loadtag.is-case-tag EQ YES
                NO-LOCK NO-ERROR.
            IF AVAILABLE loadtag THEN 
            DO:

                FIND FIRST oe-ord NO-LOCK
                    WHERE oe-ord.company EQ loadtag.company
                    AND oe-ord.ord-no  EQ loadtag.ord-no
                    AND oe-ord.job-no  EQ loadtag.job-no
                    AND oe-ord.job-no2 EQ loadtag.job-no2
                    AND oe-ord.ord-no GE 0
                    NO-ERROR.

                IF AVAILABLE oe-ord THEN 
                DO:
                    ASSIGN
                        begin_ord-no:SCREEN-VALUE = STRING(loadtag.ord-no)
                        end_ord-no:SCREEN-VALUE   = STRING(loadtag.ord-no).
                END.
                ELSE
                    ASSIGN
                        begin_ord-no:SCREEN-VALUE = ""
                        end_ord-no:SCREEN-VALUE   = ""
                        .

                ASSIGN
                    fi_cas-lab:SCREEN-VALUE = loadtag.tag-no
                    begin_job:SCREEN-VALUE  = loadtag.job-no
                    end_job:SCREEN-VALUE    = loadtag.job-no
                    begin_job2:SCREEN-VALUE = STRING(loadtag.job-no2)
                    end_job2:SCREEN-VALUE   = STRING(loadtag.job-no2)
                    begin_i-no:SCREEN-VALUE = loadtag.i-no
                    end_i-no:SCREEN-VALUE   = loadtag.i-no.


                RUN cas-lab-label-mat-file.

                RUN checkReturns.
                IF RETURN-VALUE EQ 'ERROR' THEN RETURN 'ERROR'.
                IF tb_ret:SCREEN-VALUE EQ "NO" THEN
                    RUN ok-button.

            END.
            ELSE IF tb_reprint-tag THEN 
                DO: /* task# 09200517*/
                    FIND FIRST loadtag WHERE loadtag.company     EQ cocode
                        AND loadtag.item-type   EQ NO
                        AND loadtag.tag-no  EQ TRIM(fi_cas-lab:SCREEN-VALUE) NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE loadtag THEN 
                    DO:
                        MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
                        APPLY "entry" TO fi_cas-lab.
                        RETURN ERROR.
                    END.
                    ASSIGN 
                        fi_cas-lab:SCREEN-VALUE   = loadtag.tag-no
                        begin_ord-no:SCREEN-VALUE = STRING(loadtag.ord-no)
                        end_ord-no:SCREEN-VALUE   = STRING(loadtag.ord-no)
                        begin_job:SCREEN-VALUE    = loadtag.job-no
                        end_job:SCREEN-VALUE      = loadtag.job-no
                        begin_job2:SCREEN-VALUE   = STRING(loadtag.job-no2)
                        end_job2:SCREEN-VALUE     = STRING(loadtag.job-no2)
                        begin_i-no:SCREEN-VALUE   = loadtag.i-no
                        end_i-no:SCREEN-VALUE     = loadtag.i-no.

                    RUN cas-lab-label-mat-file.

                    IF lReturn THEN 
                    DO:
                        APPLY "choose" TO btn-ok.
                        lReturn = NO.
                    END.
                    ELSE
                        RUN ok-button. 
                END.
                ELSE MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextRfidTag C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-button C-Win 
PROCEDURE ok-button :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF begin_filename:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
        userLabelPath <> "" THEN        
        begin_filename:SCREEN-VALUE = userLabelPath.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.
    ASSIGN 
        gvlCreateWithMaxPrompted = NO.
    /* gdm - 04090909 */
    ASSIGN 
        v-out = begin_filename.
    IF v-out EQ ""  THEN 
    DO:

        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ gcompany
            AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN
        DO TRANSACTION:
            CREATE sys-ctrl.
            ASSIGN
                sys-ctrl.company = gcompany
                sys-ctrl.name    = "BARDIR"
                sys-ctrl.descrip = "C:\BA\Label\".
            FIND CURRENT sys-ctrl NO-LOCK.
        END.
        v-out = sys-ctrl.descrip.
    END.
    /* gdm - 04090909 end */

    FILE-INFO:FILE-NAME = begin_filename.
    IF begin_filename <> "" AND FILE-INFO:FILE-TYPE EQ ? THEN 
    DO:
        MESSAGE "Form file/path does not exist. Do you want to create it?" 
            VIEW-AS ALERT-BOX ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
        IF v-ans THEN OS-CREATE-DIR VALUE(begin_filename).
        ELSE 
        DO:
            MESSAGE "Loadtag file path is not valid. Can't create."
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.

    /* gdm - 09290903 */
    IF v-ord-list:SCREEN-VALUE NE "" THEN 
    DO:
        ASSIGN 
            begin_ord-no = 0
            end_ord-no   = 0
            /*            begin_i-no   = "" */
            /*            end_i-no     = "" */
            begin_job    = ""
            end_job      = "".
    END.
    /* gdm - 09290903 end*/
    /*wfk  */
    IF tb_reprint-tag THEN RUN reprint-tag.
    ELSE RUN run-report NO-ERROR. 

    IF NOT ERROR-STATUS:ERROR THEN lv-ok-ran = YES.
    IF gvcSkippedItem NE "" THEN 
        MESSAGE "The finished goods receipt was not created " SKIP
            "for item # " + gvcSkippedItem + ", Job # " + gvcSkippedJob SKIP
            "due to insufficient component inventory."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.



    APPLY "entry" TO fi_cas-lab IN FRAME {&FRAME-NAME}.

    IF lv-ok-ran AND NOT tb_reprint-tag AND tb_close THEN 
    DO:
        IF PROGRAM-NAME(1) MATCHES "*r-loadtg.*"
            AND NOT program-name(2) MATCHES "*persist*" THEN 
        DO:
        /*
        RUN system/userLogOut.p (NO, 0).
        */
        END.
        APPLY "close" TO THIS-PROCEDURE.
    END.
/*
case rd-dest:
     when 1 then run output-to-printer.
     when 2 then run output-to-screen.
     when 3 then run output-to-file.
end case. 

lv-ok-ran = YES.
*/

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
    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    IF init-dir = "" THEN init-dir = "c:\temp" .
    SYSTEM-DIALOG GET-FILE list-name
        TITLE      "Enter Listing Name to SAVE AS ..."
        FILTERS    "Listing Files (*.rpt)" "*.rpt",
        "All Files (*.*)" "*.*"
        INITIAL-DIR init-dir
        ASK-OVERWRITE
        /*     CREATE-TEST-FILE*/
        SAVE-AS
        USE-FILENAME

        UPDATE OKpressed.

    IF NOT OKpressed THEN  RETURN NO-APPLY.


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
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */

    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
  */  
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
    RUN scr-rpt.w (list-name,c-win:TITLE). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCostFromPO C-Win 
PROCEDURE pGetCostFromPO PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPONumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalFreight AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dCostPerEA        AS DECIMAL.
    DEFINE VARIABLE dCostFreight      AS DECIMAL.
    DEFINE VARIABLE dCostFreightPerEA AS DECIMAL.
    DEFINE VARIABLE lFound            AS LOGICAL.
    
    RUN GetCostForPOLine IN hdCostProcs (ipcCompany, ipiPONumber, ipiPOLine, ipcFGItemID, OUTPUT opdCostPerUOM, OUTPUT opcCostUOM, OUTPUT dCostFreight, OUTPUT lFound).
    //dCostPerEA = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, opdCostPerUOM).
    dCostPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
        ipcCompany, 
        ipcFGItemID, 
        "FG", 
        opdCostPerUOM, 
        opcCostUOM, 
        "EA", 
        0, /*BasisWeight*/
        0, /*Length override - leave as 0 if not in UI or on Order/PO*/
        0, /*Width override - leave as 0 if not in UI or on Order/PO*/
        0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
        0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
        ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
        "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
        ).
    //dCostFreightPerEA = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, dCostFreight).
    dCostFreightPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
        ipcCompany, 
        ipcFGItemID, 
        "FG", 
        dCostFreight, 
        opcCostUOM, 
        "EA", 
        0, /*BasisWeight*/
        0, /*Length override - leave as 0 if not in UI or on Order/PO*/
        0, /*Width override - leave as 0 if not in UI or on Order/PO*/
        0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
        0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
        ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
        "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
        ).
    ASSIGN 
        opdCostTotal        = ipdQty * dCostPerEA
        opdCostTotalFreight = ipdQty * dCostFreightPerEA.
    IF fgpofrt-log THEN 
        opdCostTotal = opdCostTotal + opdCostTotalFreight.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLabelPerSkid C-Win 
PROCEDURE pLabelPerSkid PRIVATE :
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllowEdit AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-cust FOR cust.
   
    FIND FIRST bf-cust NO-LOCK
        WHERE bf-cust.company EQ cocode
        AND bf-cust.cust-no EQ ipcCustomer 
        NO-ERROR.
        
    IF (AVAILABLE bf-cust AND bf-cust.int-field[1] EQ 0 AND NOT lLoadTagLimit) 
        OR NOT AVAILABLE bf-cust 
        OR lLoadTagLimit THEN
        lAllowEdit = YES.    
   
    IF lAllowEdit THEN
    DO:
        tb_override-mult:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
        IF tb_override-mult:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "No" THEN
            begin_labels:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        ELSE begin_labels:SENSITIVE IN FRAME {&FRAME-NAME} = YES . 
        begin_labels:SCREEN-VALUE = STRING(iLoadtag).
    END.
    ELSE 
    DO:
        ASSIGN 
            tb_override-mult:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".
        tb_override-mult:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        begin_labels:SENSITIVE IN FRAME {&FRAME-NAME} = NO.  
        IF AVAILABLE bf-cust THEN
            begin_labels:SCREEN-VALUE = STRING(bf-cust.int-field[1]).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-all C-Win 
PROCEDURE post-all :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.


    FOR EACH tt-fgrctd-created:

        FIND fg-rctd WHERE ROWID(fg-rctd) = tt-fgrctd-created.fg-rctd-rowid
            EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAILABLE fg-rctd THEN
            NEXT.
  
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no
            NO-ERROR.
       
        RUN sys/ref/nk1look.p(
            INPUT cocode,
            INPUT "FGTagValidation",
            INPUT "L",
            INPUT YES,
            INPUT YES,
            INPUT IF AVAILABLE itemfg THEN itemfg.cust-no ELSE "",
            INPUT "",
            OUTPUT cReturnValue,
            OUTPUT lRecFound
            ).
      
        lFGTagValidation = LOGICAL(cReturnValue).
   
        RUN sys/ref/nk1look.p(
            INPUT cocode,
            INPUT "FGTagValidation",
            INPUT "C",
            INPUT YES,
            INPUT YES,
            INPUT IF AVAILABLE itemfg THEN itemfg.cust-no ELSE "",
            INPUT "",
            OUTPUT cReturnValue,
            OUTPUT lRecFound
            ).
       
        cFGTagValidation = cReturnValue.
   
        IF lFGTagValidation AND fg-rctd.tag EQ "" THEN 
            NEXT.
        
        IF cFGTagValidation EQ "ItemMatch" AND NOT fg-rctd.tag BEGINS fg-rctd.i-no THEN 
            NEXT. 

        IF AVAILABLE fg-rctd THEN 
        DO:
            IF SSPostFG-log AND
                SSPostFG-char = "Loadtag" THEN 
            DO:
                IF AVAILABLE fg-rctd THEN 
                DO:
                    ASSIGN
                        lv-r-no           = fg-rctd.r-no
                        fg-rctd.r-no      = 0
                        fg-rctd.r-no      = lv-r-no
                        fg-rctd.rita-code = "R"
                        fg-rctd.post-date = TODAY.
                    /** not quite sure what this does
                    fg-rctd.tag2      = w-fg-rctd.tag2. **/

                    FOR EACH fg-rcpts WHERE
                        fg-rcpts.company EQ fg-rctd.company AND
                        fg-rcpts.r-no    EQ fg-rctd.r-no EXCLUSIVE-LOCK:
                        fg-rcpts.rita-code = fg-rctd.rita-code.
                    END.
                END.
            END.
        END.

        FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) = tt-fgrctd-created.fg-rctd-rowid
            NO-LOCK NO-ERROR.

        IF FGSetRec-Int NE 1 THEN 
        DO:

            FOR EACH fg-rcpts WHERE
                fg-rcpts.company EQ bf-fg-rctd.company
                AND  fg-rcpts.linker     = "fg-rctd: " + STRING(bf-fg-rctd.r-no,"9999999999")
                EXCLUSIVE-LOCK:

                FIND FIRST fg-rctd WHERE fg-rctd.r-no = fg-rcpts.r-no
                    EXCLUSIVE-LOCK NO-ERROR.

                IF AVAILABLE fg-rctd THEN 
                DO:


                    RUN oerep/r-ltpost.p (INPUT ROWID(fg-rctd),NO).
                    /*##BL - FGSetAssembly requires the bin to match that of the character*/
                    /*##BL of FGSetAssembly N-K.  If it doesn't, abort posting  */
                    /*             IF lFGSetAssembly AND fg-rctd.loc-bin NE cFGSetAssembly THEN DO:                               */
                    /*                 MESSAGE "The Bin location for Component " fg-rctd.i-no " must be " cFGSetAssembly "." SKIP */
                    /*                     "Please correct on the Set Parts tab of FG Receiving and re-run the posting process."  */
                    /*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                     */
                    /*                 RETURN ERROR.                                                                              */
                    /*             END.                                                                                           */

                    FIND FIRST fg-rctd WHERE fg-rctd.r-no = fg-rcpts.r-no
                        EXCLUSIVE-LOCK NO-ERROR.

                    IF AVAILABLE fg-rctd THEN 
                    DO:

                        RUN oerep/r-ltpost.p (INPUT ROWID(fg-rctd),NO).
                        /*##BL - FGSetAssembly requires the bin to match that of the character*/
                        /*##BL of FGSetAssembly N-K.  If it doesn't, abort posting  */
                        /*             IF lFGSetAssembly AND fg-rctd.loc-bin NE cFGSetAssembly THEN DO:                               */
                        /*                 MESSAGE "The Bin location for Component " fg-rctd.i-no " must be " cFGSetAssembly "." SKIP */
                        /*                     "Please correct on the Set Parts tab of FG Receiving and re-run the posting process."  */
                        /*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                     */
                        /*                 RETURN ERROR.                                                                              */
                        /*             END.                                                                                           */

                        /* These negative receipts should be changed to Rita=A per task 08211305 */
                        FOR EACH fg-rcpth WHERE fg-rcpth.r-no EQ fg-rctd.r-no EXCLUSIVE-LOCK:
                            IF fg-rctd.t-qty LT 0 THEN
                                fg-rcpth.rita-code = "A".
                        END.

                        FOR EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rctd.r-no EXCLUSIVE-LOCK:
                            IF fg-rctd.t-qty LT 0 THEN
                                fg-rdtlh.rita-code = "A".
                        END.

                    END. /* Avail fg-rctd for component */
                END. /* if avail */
            END. /* Each fg-rcpts */
        END. /* If not NoAdjustments */
        FIND fg-rctd WHERE ROWID(fg-rctd) = tt-fgrctd-created.fg-rctd-rowid
            EXCLUSIVE-LOCK NO-ERROR.

        RUN oerep/r-ltpost.p (INPUT ROWID(fg-rctd),NO).

        DELETE tt-fgrctd-created.

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-return C-Win 
PROCEDURE post-return :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-fg-recid AS RECID NO-UNDO.

    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
    DEFINE BUFFER b-fg-bin   FOR fg-bin.
    DEFINE BUFFER b-itemfg   FOR itemfg.

    DEFINE VARIABLE li             AS INTEGER   NO-UNDO.   
    DEFINE VARIABLE v-dec          AS DECIMAL   DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE v-overrun-qty  LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-underrun-qty LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-reduce-qty   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-newhdr       AS LOG       NO-UNDO. 
    DEFINE VARIABLE v-fin-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-est-no       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-one-item     AS LOG       NO-UNDO.
    DEFINE VARIABLE ld-cvt-qty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-cvt-cost    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-binqty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-tagcost      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-cost         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE choice         AS LOG       NO-UNDO.
    DEFINE VARIABLE v-post-date    AS DATE      INIT TODAY NO-UNDO.
    DEFINE VARIABLE li-tag-no      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll-qty-changed AS LOG       NO-UNDO.
    DEFINE VARIABLE ll-whs-item    AS LOG       NO-UNDO.
   
    FOR EACH w-fg-rctd:
        DELETE w-fg-rctd.
    END.

    /* create w/h transfer record*/   
    FIND FIRST itemfg WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ loadtag.i-no NO-ERROR.

    li = 1.
    FOR EACH bf-fg-rctd NO-LOCK BY bf-fg-rctd.r-no DESCENDING:
        LEAVE.
    END.
    IF AVAILABLE bf-fg-rctd THEN li = bf-fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
    FIND FIRST fg-bin WHERE fg-bin.company = cocode
        AND fg-bin.i-no = loadtag.i-no
        AND fg-bin.tag = ""
        AND fg-bin.qty >= loadtag.pallet-count NO-LOCK NO-ERROR.
    IF NOT AVAILABLE fg-bin THEN RETURN.  

    CREATE bf-fg-rctd.
    ASSIGN
        bf-fg-rctd.r-no       = li + 1
        bf-fg-rctd.rct-date   = TODAY
        bf-fg-rctd.trans-time = TIME
        bf-fg-rctd.company    = cocode
        bf-fg-rctd.rita-code  = "I"
        bf-fg-rctd.i-name     = itemfg.i-name
        bf-fg-rctd.i-no       = loadtag.i-no
        bf-fg-rctd.job-no     = loadtag.job-no
        bf-fg-rctd.job-no2    = loadtag.job-no2
        bf-fg-rctd.t-qty      = loadtag.pallet-count /*loadtag.qty*/
        bf-fg-rctd.pur-uom    = itemfg.prod-uom
        bf-fg-rctd.cost-uom   = itemfg.prod-uom
        /*     bf-fg-rctd.std-cost   = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost */
        bf-fg-rctd.ext-cost   = (bf-fg-rctd.t-qty / 1000) * bf-fg-rctd.std-cost
        bf-fg-rctd.qty-case   = loadtag.qty-case

        bf-fg-rctd.partial    = loadtag.partial
        bf-fg-rctd.cases      = TRUNC(bf-fg-rctd.t-qty / bf-fg-rctd.qty-case,0)
        bf-fg-rctd.cases-unit = loadtag.case-bundle + INTEGER(DYNAMIC-FUNCTION("fGetOverageQuantitySubUnitsPerUnit" IN hdInventoryProcs,INTEGER(loadtag.partial)))
        bf-fg-rctd.loc        = IF cFGDefWhse NE "" THEN cFGDefWhse ELSE loadtag.loc
        bf-fg-rctd.loc-bin    = IF cFGDefBin  NE "" THEN cFGDefBin  ELSE loadtag.loc-bin
        bf-fg-rctd.tag        = loadtag.tag-no
        bf-fg-rctd.loc2       = ""
        bf-fg-rctd.loc-bin2   = ""
        bf-fg-rctd.tag2       = ""
        .
    /* post later*/
    CREATE w-fg-rctd.
    BUFFER-COPY bf-fg-rctd TO w-fg-rctd.
    ASSIGN 
        w-fg-rctd.row-id = ROWID(bf-fg-rctd).
    {fg/fg-post.i w-fg-rctd w-fg-rctd}

    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    FIND CURRENT fg-bin NO-LOCK NO-ERROR.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.

    ASSIGN 
        bf-fg-rctd.rita-code = "P"  /* posted */
        bf-fg-rctd.post-date = v-post-date
        bf-fg-rctd.tag2      = w-fg-rctd.tag2. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunAPIOutboundTrigger C-Win 
PROCEDURE pRunAPIOutboundTrigger PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose: Procedure to execute the Outbound APIs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID    AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocationID  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-shipto  FOR shipto.

    ASSIGN
        cAPIID     = "SendFinishedGood"
        cTriggerID = "CreateLoadtag"
        .
    
    IF ipcJobNo NE "" THEN 
    DO:
        FIND FIRST bf-job NO-LOCK
            WHERE bf-job.company EQ ipcCompany
            AND trim(bf-job.job-no)  EQ trim(ipcJobNo)
            AND bf-job.job-no2 EQ ipiJobNo2
            NO-ERROR.
        IF AVAILABLE bf-job THEN 
        DO:
            FOR EACH bf-job-hdr NO-LOCK
                WHERE bf-job-hdr.company EQ bf-job.company
                AND bf-job-hdr.job     EQ bf-job.job
                AND bf-job-hdr.job-no  EQ bf-job.job-no
                AND bf-job-hdr.job-no2 EQ bf-job.job-no2:
                FIND FIRST bf-itemfg NO-LOCK 
                    WHERE bf-itemfg.company EQ bf-job-hdr.company
                    AND bf-itemfg.i-no    EQ bf-job-hdr.i-no
                    NO-ERROR.
                IF AVAILABLE bf-itemfg THEN 
                DO:
                    ASSIGN
                        cPrimaryID   = bf-itemfg.i-no
                        cDescription = cAPIID + " triggered by " + cTriggerID
                                     + " from r-loadtg.w for item " + cPrimaryID
                        .
                      
                    RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                        INPUT  ipcCompany,               /* Company Code (Mandatory) */
                        INPUT  bf-job.loc,               /* Location Code (Mandatory) */
                        INPUT  cAPIID,                   /* API ID (Mandatory) */
                        INPUT  "",                       /* Client ID (Optional) - Pass empty in case to make request for all clients */
                        INPUT  cTriggerID,               /* Trigger ID (Mandatory) */
                        INPUT  "itemfg",                 /* Comma separated list of table names for which data being sent (Mandatory) */
                        INPUT  STRING(ROWID(bf-itemfg)), /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                        INPUT  cPrimaryID,               /* Primary ID for which API is called for (Mandatory) */   
                        INPUT  cDescription,             /* Event's description (Optional) */
                        OUTPUT lSuccess,                 /* Success/Failure flag */
                        OUTPUT cMessage                  /* Status message */
                        ) NO-ERROR.
                END.
            END.
        END. 
    END.
    ELSE IF ipiPOID NE 0 THEN 
        DO:
            FIND FIRST bf-po-ord NO-LOCK
                WHERE bf-po-ord.company EQ ipcCompany
                AND bf-po-ord.po-no   EQ ipiPOID
                NO-ERROR.
            IF AVAILABLE bf-po-ord THEN 
            DO:
                IF bf-po-ord.type EQ "R" THEN
                    cLocationID = bf-po-ord.loc.
                ELSE 
                DO:
                    RUN oe/custxship.p (
                        INPUT  bf-po-ord.company,
                        INPUT  bf-po-ord.cust-no,
                        INPUT  bf-po-ord.ship-id,
                        BUFFER bf-shipto
                        ).
                    IF AVAILABLE bf-shipto THEN
                        cLocationID = bf-shipto.loc. 
                END.
                FOR EACH bf-po-ordl NO-LOCK
                    WHERE bf-po-ordl.company EQ bf-po-ord.company
                    AND bf-po-ordl.po-no   EQ bf-po-ord.po-no:
                    FIND FIRST bf-itemfg NO-LOCK 
                        WHERE bf-itemfg.company EQ bf-po-ordl.company
                        AND bf-itemfg.i-no    EQ bf-po-ordl.i-no
                        NO-ERROR.
                    IF AVAILABLE bf-itemfg THEN 
                    DO:
                        ASSIGN
                            cPrimaryID   = bf-itemfg.i-no
                            cDescription = cAPIID + " triggered by " + cTriggerID
                                     + " from r-loadtg.w for item " + cPrimaryID
                            .

                        RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                            INPUT  ipcCompany,               /* Company Code (Mandatory) */
                            INPUT  cLocationID,              /* Location Code (Mandatory) */
                            INPUT  cAPIID,                   /* API ID (Mandatory) */
                            INPUT  "",                       /* Client ID (Optional) - Pass empty in case to make request for all clients */
                            INPUT  cTriggerID,               /* Trigger ID (Mandatory) */
                            INPUT  "itemfg",                 /* Comma separated list of table names for which data being sent (Mandatory) */
                            INPUT  STRING(ROWID(bf-itemfg)), /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                            INPUT  cPrimaryID,               /* Primary ID for which API is called for (Mandatory) */   
                            INPUT  cDescription,             /* Event's description (Optional) */
                            OUTPUT lSuccess,                 /* Success/Failure flag */
                            OUTPUT cMessage                  /* Status message */
                            ) NO-ERROR.  
                    END.                      
                END.
            END.
        END.
        
    /* Reset context at the end of API calls to clear temp-table 
       data inside OutboundProcs */
    RUN Outbound_ResetContext IN hdOutboundProcs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunAPIOutboundTriggerForItem C-Win 
PROCEDURE pRunAPIOutboundTriggerForItem PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose: Procedure to trigger API Outbound events
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.   
    DEFINE INPUT  PARAMETER ipcItemNo  AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoID    AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocationID  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-shipto  FOR shipto.

    ASSIGN
        cAPIID     = "SendFinishedGood"
        cTriggerID = "CreateLoadtag"
        .

    FIND FIRST bf-itemfg NO-LOCK 
        WHERE bf-itemfg.company EQ ipcCompany
        AND bf-itemfg.i-no    EQ ipcItemNo
        NO-ERROR.
    IF AVAILABLE bf-itemfg THEN 
    DO:
        ASSIGN
            cPrimaryID   = bf-itemfg.i-no
            cDescription = cAPIID + " triggered by " + cTriggerID
                         + " from r-loadtg.w for item " + cPrimaryID
            .    
        IF ipcJobNo NE "" THEN 
        DO:
            FIND FIRST bf-job NO-LOCK
                WHERE bf-job.company EQ ipcCompany
                AND trim(bf-job.job-no)  EQ trim(ipcJobNo)
                AND bf-job.job-no2 EQ ipiJobNo2
                NO-ERROR.
            IF AVAILABLE bf-job THEN
                RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                    INPUT  ipcCompany,               /* Company Code (Mandatory) */
                    INPUT  bf-job.loc,               /* Location Code (Mandatory) */
                    INPUT  cAPIID,                   /* API ID (Mandatory) */
                    INPUT  "",                       /* Client ID (Optional) - Pass empty in case to make request for all clients */
                    INPUT  cTriggerID,               /* Trigger ID (Mandatory) */
                    INPUT  "itemfg",                 /* Comma separated list of table names for which data being sent (Mandatory) */
                    INPUT  STRING(ROWID(bf-itemfg)), /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                    INPUT  cPrimaryID,               /* Primary ID for which API is called for (Mandatory) */   
                    INPUT  cDescription,             /* Event's description (Optional) */
                    OUTPUT lSuccess,                 /* Success/Failure flag */
                    OUTPUT cMessage                  /* Status message */
                    ) NO-ERROR.
        END.
        ELSE IF ipiPOID NE 0 THEN 
            DO:
                FIND FIRST bf-po-ord NO-LOCK
                    WHERE bf-po-ord.company EQ ipcCompany
                    AND bf-po-ord.po-no   EQ ipiPOID
                    NO-ERROR.
                IF AVAILABLE bf-po-ord THEN 
                DO:
                    IF bf-po-ord.type EQ "R" THEN
                        cLocationID = bf-po-ord.loc.
                    ELSE 
                    DO:
                        RUN oe/custxship.p (
                            INPUT  bf-po-ord.company,
                            INPUT  bf-po-ord.cust-no,
                            INPUT  bf-po-ord.ship-id,
                            BUFFER bf-shipto
                            ).
                        IF AVAILABLE bf-shipto THEN
                            cLocationID = bf-shipto.loc. 
                    END.

                    RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                        INPUT  ipcCompany,               /* Company Code (Mandatory) */
                        INPUT  cLocationID,              /* Location Code (Mandatory) */
                        INPUT  cAPIID,                   /* API ID (Mandatory) */
                        INPUT  "",                       /* Client ID (Optional) - Pass empty in case to make request for all clients */
                        INPUT  cTriggerID,               /* Trigger ID (Mandatory) */
                        INPUT  "itemfg",                 /* Comma separated list of table names for which data being sent (Mandatory) */
                        INPUT  STRING(ROWID(bf-itemfg)), /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                        INPUT  cPrimaryID,               /* Primary ID for which API is called for (Mandatory) */   
                        INPUT  cDescription,             /* Event's description (Optional) */
                        OUTPUT lSuccess,                 /* Success/Failure flag */
                        OUTPUT cMessage                  /* Status message */
                        ) NO-ERROR.  
                END.
            END.
    END.
        
    /* Reset context at the end of API calls to clear temp-table 
       data inside OutboundProcs */
    RUN Outbound_ResetContext IN hdOutboundProcs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprint-tag C-Win 
PROCEDURE reprint-tag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLoadtagFile AS CHARACTER NO-UNDO.
  
 
    FIND FIRST loadtag WHERE loadtag.company     EQ cocode
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no  EQ TRIM(fi_cas-lab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE loadtag THEN 
    DO:
        MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO fi_cas-lab.
        RETURN ERROR.
    END.  

    ASSIGN
        cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
    RUN create-w-ord.

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

    RUN create-text-file.

    IF cBarCodeProgram EQ "" THEN 
    DO:    
        RUN AutoPrint.
    END.
    ELSE IF cBarCodeProgram EQ "xprint" AND scr-auto-print THEN 
        DO:
            PAUSE 1.
            RUN "oerep/LoadtagProcs.p" PERSISTENT SET hLoadtagProcs.
            RUN pPrintView IN hLoadtagProcs (scr-label-file:SCREEN-VALUE IN FRAME {&FRAME-NAME}, tb_print-view).
            DELETE OBJECT hLoadtagProcs.
        END.

    IF (NOT is-from-addons() OR SSLoadTag-log = TRUE) THEN 
        MESSAGE "Loadtag reprint is completed." VIEW-AS ALERT-BOX INFORMATION.
    SESSION:SET-WAIT-STATE ("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-barone C-Win 
PROCEDURE run-barone :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-TagText AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iReturnResult AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cProgramName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.

    cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
    cFileName    = "R:\ASI_GUI9\SOURCE\custom\century.lab".


    /* OS-COPY VALUE(ip-tagtext) VALUE("r:\asi_gui9\source\custom\"). */
    RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
        iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-lmw C-Win 
PROCEDURE run-lmw :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-TagText AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iReturnResult AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cProgramName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO.

    /*   cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
    */
    cFileName    = "custom\interpack.qdf".
    FILE-INFO:FILE-NAME = cFileName.
    cFileName = FILE-INFO:FULL-PATHNAME.

    RUN custom/runlmw.p (OUTPUT cprogramname).

    /*   OS-COPY VALUE(ip-tagtext) VALUE("c:\tmp\").*/

    RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
        iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    {oerep/r-loadtg1.i}

    IF cBarCodeProgram EQ "" THEN 
    DO:    
        RUN AutoPrint.
    END.
    ELSE IF cBarCodeProgram EQ "xprint" AND scr-auto-print THEN 
        DO: 
            PAUSE 1.
            RUN "oerep/LoadtagProcs.p" PERSISTENT SET hLoadtagProcs.
            RUN pPrintView IN hLoadtagProcs (scr-label-file:SCREEN-VALUE IN FRAME {&FRAME-NAME}, tb_print-view).
            DELETE OBJECT hLoadtagProcs.
        END.
    SESSION:SET-WAIT-STATE ("").
/*     IF scr-auto-print THEN                                                  */
/*     DO:                                                                     */
/*        DEF VAR v-int AS INT NO-UNDO.                                        */
/*        DEF VAR cFileName AS CHAR NO-UNDO.                                   */
/*        DEF VAR v-path AS CHARACTER NO-UNDO.                                 */
/*                                                                             */
/*        LOAD "SOFTWARE" BASE-KEY "HKEY_LOCAL_MACHINE".                       */
/*        USE "SOFTWARE".                                                      */
/*        GET-KEY-VALUE SECTION "Teklynx\Label Matrix"                         */
/*                      KEY "PATH"                                             */
/*                      VALUE v-path.                                          */
/*        UNLOAD "SOFTWARE".                                                   */
/*                                                                             */
/*        ASSIGN                                                               */
/*           v-path = v-path + "\lmwprint.exe "                                */
/*           cFileName = "/L=" + scr-label-file.                               */
/*                                                                             */
/*           RUN WinExec (INPUT v-path + CHR(32) + cFileName , INPUT 1, OUTPUT */
/*                        v-int).                                              */
/*     END.                                                                    */


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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
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
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-create C-Win 
PROCEDURE temp-create :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    CREATE w-file.
    w-key = ip-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-job C-Win 
PROCEDURE temp-job :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-job-no LIKE job.job-no NO-UNDO.

    FOR EACH job
        WHERE job.company EQ cocode
        AND trim(job.job-no)  EQ trim(SUBSTR(ip-job-no,1,9))
        AND job.job-no2 EQ INT(SUBSTR(ip-job-no,10,3))
        AND (v-stat EQ "A" OR
        (v-stat EQ "C" AND job.opened EQ NO) OR
        (v-stat EQ "O" AND job.opened EQ YES))
        NO-LOCK:

        RUN temp-create (ROWID(job)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-ord C-Win 
PROCEDURE temp-ord :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-ord-no LIKE oe-ord.ord-no NO-UNDO.

    FOR EACH oe-ord
        WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ ip-ord-no
        AND (v-stat EQ "A" OR
        (v-stat EQ "C" AND oe-ord.opened EQ NO) OR
        (v-stat EQ "O" AND oe-ord.opened EQ YES))
        NO-LOCK:
        RUN temp-create (ROWID(oe-ord)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-po C-Win 
PROCEDURE temp-po :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-po-no LIKE po-ord.po-no NO-UNDO.

    FOR EACH po-ord NO-LOCK
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   EQ ip-po-no
        AND (v-stat EQ "A" OR
        (v-stat EQ "C" AND po-ord.opened EQ NO) OR
        (v-stat EQ "O" AND po-ord.opened EQ YES)):
        RUN temp-create (ROWID(po-ord)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE win_breakPath C-Win 
PROCEDURE win_breakPath :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT    PARAMETER pcPath            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT   PARAMETER pcProtocol        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT   PARAMETER pcComputerName    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT   PARAMETER pcSharedFolder    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT   PARAMETER pcDrive           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT   PARAMETER pcDir             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT   PARAMETER pcFile            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT   PARAMETER pcExt             AS CHARACTER NO-UNDO.

    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    ASSIGN
        pcProtocol     = ""
        pcComputerName = ""
        pcSharedFolder = ""
        pcDrive        = ""
        pcDir          = ""
        pcFile         = ""
        pcExt          = "".

    /* assumes that if the call is from another procedure or function with in this library then the path has already been normalized. */

    IF pcPath = ? THEN
        RETURN.

    IF SOURCE-PROCEDURE <> THIS-PROCEDURE THEN
        pcPath = win_normalizePath( pcPath ).

    IF pcPath BEGINS "~\~\" THEN 
    DO:

        ASSIGN
            pcProtocol             = substr( pcPath, 1, 2 )
            substr( pcPath, 1, 2 ) = "".

        i = INDEX( pcPath, "~\", 3 ).
        IF i = 0 THEN i = LENGTH( pcPath ) + 1.

        ASSIGN
            pcComputerName             = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

        i = INDEX( pcPath, "~\", 2 ).
        IF i = 0 THEN i = LENGTH( pcPath ) + 1.

        ASSIGN
            pcSharedFolder             = substr( pcPath, 1, i - 1 )
            substr( pcPath, 1, i - 1 ) = "".

    END. /* pcPath begins "\\" */

    ELSE
        IF  substr( pcPath, 1, 1 ) >= "a"
            AND substr( pcPath, 1, 1 ) <= "z"
            AND substr( pcPath, 2, 1 )  = ":" THEN 
        DO:

            ASSIGN
                pcDrive                = substr( pcPath, 1, 2 )
                substr( pcPath, 1, 2 ) = "".

        END. /* else */

    i = R-INDEX( pcPath, "~\" ). 
    IF i > 0 THEN

        ASSIGN
            pcDir                  = substr( pcPath, 1, i )
            substr( pcPath, 1, i ) = "".

    i = R-INDEX( pcPath, "." ).
    IF i > 0 THEN

        ASSIGN
            pcExt                                = substr( pcPath, i )
            substr( pcPath, i, LENGTH( pcExt ) ) = "".

    pcFile = pcPath.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE write-loadtag-line C-Win 
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
      
        /* HRMS Loftware Map */
        /*01  00000000.lwl = tag template   */
        /*02  Customer Name   30 characters   Customername                        */
        /*03  Customer Part ID or Number  30 characters   custpartid              */
        /*04  Customer PO Number  15 characters   Custpo                          */
        /*05  Customer Ship To City & State   30 characters   custshiptocityst    */
        /*06  File (Estimate) number  7 characters    filenumber                  */
        /*07  Lbs/M Pieces  - cust waste  6 characters    LbsperM                 */
        /*08  Plant Number    2 characters    plantnumber                         */
        /*09  this field contains “- -“   3 characters    Text0098                */
        /*10  Number of tags per unit 1 character **QUANTITY                      */
        /*11  Number of tags to print 5 characters    tagstoprint                 */
        /*12  Order Number    7 characters    ordernumber                         */
        /*13  Quantity per pallet 6 characters    qtypallet                       */
        /*14  Due date in format MM/DD/YY 8 characters    DueDate                 */
        /*15  Order Quantity  9 characters    Orderqty                            */
        /*16  Overrun Percent (% )    2 characters    Overrun                     */
        /*17  Underrun Percent (%)    2 characters    underrun                    */
        /*18  Critical Operation #1   2 characters    C01                         */
        /*19  Critical Operation #2   2 characters    C02                         */
        /*20  Critical Operation #3   2 characters    C03                         */
        /*21  Critical Operation #4   2 characters    C04                         */
        /*22  Critical Operation #5   2 characters    C05                         */
        /*23  Critical Operation #6   2 characters    C06                         */
        /*24  Customer and ShipTo Number in the format NNNNNSSS where 
                NNNNN = Customer Number and SSS = ShipTo Number 8 characters    
                custshiptonumber                                                  */
        /*25  B/L instructions        B/L                                         */
        /*26  RECORD      Record                                                  */
        /*27  Load(Unit)Number – Unitized Inventory not active 5 chars loadnumber */
        /*28  Number of bands (strap pattern) 6 characters    Straps              */
        /*29  Unit type   1 character unittype                                    */
        /*30  Customer ShipTo Name    60 characters   custshiptoname              */
        /*31  Unit ID (Serial number) 23 characters   UnitID                      */
        /*32  Material description    30 characters   matldesc                    */
        /*33  Sheet width 9 characters    shtwdth                                 */
        /*34  Sheet length    9 characters    shtleng.                            */
        /*35  Load Number - Unitized Inventory active   5 characters  Loadnumber  */
        /*36  Broker’s Customer Name  30 characters   brokercust                  */
        /*37  Customer ShipTo Address 30 characters   custshiptoadd               */
        /*38  Number of tags to print 3 characters    numbertagstoprint           */
        /*39  Number per bundle   5 characters    numberperbndl                   */
        /*40  Broker number   5 characters    brokernumber                        */
        /*41  Order entry message line 7  64 characters   msg7                    */
        /*42  Order entry message line 8  64 characters   msg8                    */
        /*43  Order entry message line 9  64 characters   msg9                    */
        /*44  Style description   15 characters   styledesc                       */
        /*45  Box length  9 characters    boxlen                                  */
        /*46  Box width   9 characters    boxwid                                  */
        /*47  Box depth   9 characters    boxdep                                  */
        /*48  Corrugator Scoring 1    6 characters    corrscore1                  */
        /*49  Corrugator Scoring 2    6 characters    corrscore2                  */
        /*50  Corrugator Scoring 3    6 characters    corrscore3                  */
        /*51  Corrugator Scoring 4    6 characters    corrscore4                  */
        /*52  Corrugator Scoring 5    6 characters    corrscore5                  */
        /*53  Corrugator Scoring 6    6 characters    corrscore6                  */
        /*54  Corrugator Scoring 7    6 characters    corrscore7                  */
        /*55  Corrugator Scoring 8    6 characters    corrscore8                  */
        /*56  Corrugator Scoring 9    6 characters    corrscore9                  */
        /*57  Cutoff Scoring 1    6 characters    cutscore1                       */
        /*58  Cutoff Scoring 2    6 characters    cutscore2                       */
        /*59  Cutoff Scoring 3    6 characters    cutscore3                       */
        /*60  Cutoff Scoring 4    6 characters    cutscore4                       */
        /*61  Cutoff Scoring 5    6 characters    cutscore5                       */
        /*62  Cutoff Scoring 6    6 characters    cutscore6                       */
        /*63  Cutoff Scoring 7    6 characters    cutscore7                       */
        /*64  Cutoff Scoring 8    6 characters    cutscore8                       */
        /*65  Cutoff Scoring 9    6 characters    cutscore9                       */
        /*66  Not used at this time   1 character Unused1                         */
        /*67  Number of ties  4 characters    numties                             */
        /*68  Number of bundles per layer 4 characters    numbdllyr               */
        /*69  Stacking pattern    10 characters   stackptn                        */
        /*70  Number of layers    4 characters    Numlayers                       */
        /*71  Dimensions  15 characters   dimensions                              */
        /*72  Cutting die number  10 characters   cuttingdie                      */
        /*73  Cutting die location    10 characters   cuttingdielocn              */
        /*74  Printing die number 10 characters   printingdie                     */
        /*75  Printing die location   10 characters   printdielocn                */
        /*76  1st down color  10 characters   color1                              */
        /*77  2nd down color  10 characters   color2                              */
        /*78  3rd down color  10 characters   color3                              */
        /*79  4th down color  10 characters   color4                              */
        /*80  Critical Operation 1 text   5 characters    LC01                    */
        /*81  Critical Operation 2 text   5 characters    LC02                    */
        /*82  Critical Operation 3 text   5 characters    LC03                    */
        /*83  Critical Operation 4 text   5 characters    LC04                    */
        /*84  Critical Operation 5 text   5 characters    LC05                    */
        /*85  Critical Operation 6 text   5 characters    LC06                    */
        /*86  1st Miscellaneous Billing Message   18 characters   MiscMsg1        */
        /*87  2nd Miscellaneous Billing Message   18 characters   MiscMsg2        */
    
        ASSIGN
            cLoftString           = FILL(",",86)
            ENTRY(1,cLoftString)  = "!" + scr-label-file
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hProc     AS HANDLE NO-UNDO.
    DEFINE VARIABLE lWasFound AS LOG    NO-UNDO.
    lWasFound = NO.
    hProc = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(hProc):
        IF INDEX(hProc:FILE-NAME, "addon") GT 0 THEN 
        DO:
            lWasFound = YES.
            LEAVE. /* found it. */
        END.

        hProc = hProc:NEXT-SIBLING.
    END.

    RETURN lWasFound.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION win_normalizePath C-Win 
FUNCTION win_normalizePath RETURNS CHARACTER
    ( pcPath AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRoot AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDir  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iDir  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE str   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

    pcPath = TRIM( pcPath ).

    IF pcPath = ""
        OR pcPath = ? THEN
        RETURN pcPath.

    pcPath = REPLACE( pcPath, "/", "~\" ).

    DO WHILE INDEX( pcPath, "~\~\", 2 ) <> 0:
        substr( pcPath, 2, LENGTH( pcPath ) - 1 ) = REPLACE( substr( pcPath, 2, LENGTH( pcPath ) - 1 ), "~\~\", "~\" ).
    END.

    DO WHILE INDEX( pcPath, "::" ) <> 0:
        pcPath = REPLACE( pcPath, "::", ":" ).
    END.

    IF LOOKUP( ".", pcPath, "~\" ) > 0 OR lookup( "..", pcPath, "~\" ) > 0 THEN 
    DO:

        ASSIGN
            cRoot = ""
            cPath = "".

        IF pcPath BEGINS "~\~\" THEN 
        DO:

            i = INDEX( pcPath, "~\", 3 ).
            IF i = 0 THEN i = LENGTH( pcPath ).

            ASSIGN
                cRoot                  = substr( pcPath, 1, i )
                substr( pcPath, 1, i ) = "".

            i = INDEX( pcPath, "~\" ). 
            IF i > 0 THEN

                ASSIGN
                    cRoot                  = cRoot + substr( pcPath, 1, i )
                    substr( pcPath, 1, i ) = "".

        END. /* pcPath begins "\\" */

        ELSE
            IF  substr( pcPath, 1, 1 ) >= "a"
                AND substr( pcPath, 1, 1 ) <= "z"
                AND substr( pcPath, 2, 1 )  = ":" THEN 
            DO:

                ASSIGN
                    cRoot                  = substr( pcPath, 1, 2 )
                    substr( pcPath, 1, 2 ) = "".

                IF substr( pcPath, 1, 1 ) = "~\" THEN
                    ASSIGN
                        cRoot                  = cRoot + substr( pcPath, 1, 1 )
                        substr( pcPath, 1, 1 ) = "".

            END. /* substr = ":" */



        DO iDir = 1 TO NUM-ENTRIES( pcPath, "~\" ):

            cDir = ENTRY( iDir, pcPath, "~\" ).

            IF cDir = "." THEN 
            DO:

                IF cPath <> "" OR cRoot <> "" THEN
                    NEXT.

                ELSE
                    cPath = cPath
                        + ( IF cPath <> "" THEN "~\" ELSE "" )
                        + cDir.

            END. /* cDir = "." */

            ELSE
                IF cDir = ".." THEN 
                DO:

                    IF cPath <> "" AND entry( NUM-ENTRIES( cPath, "~\" ), cPath, "~\" ) <> ".." THEN 
                    DO:

                        str = "".

                        DO i = 1 TO NUM-ENTRIES( cPath, "~\" ) - 1:

                            str = str
                                + ( IF str <> "" THEN "~\" ELSE "" )
                                + entry( i, cPath, "~\" ).

                        END. /* 1 to num-entries */

                        cPath = str.

                    END. /* else */

                    ELSE
                        cPath = cPath
                            + ( IF cPath <> "" THEN "~\" ELSE "" )
                            + cDir.

                END. /* cDir = ".." */

                ELSE
                    cPath = cPath
                        + ( IF cPath <> "" THEN "~\" ELSE "" )
                        + cDir.

        END. /* 1 to num-entries */

        pcPath = cPath.

        IF cRoot <> "" THEN
            pcPath = cRoot + pcPath.

    END. /* lookup( ".." ) > 0 */

    RETURN pcPath.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

