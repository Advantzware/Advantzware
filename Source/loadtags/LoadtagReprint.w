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

  File: loadtag/loadtagReprint.w

  Description: Creates a Work In Process tag for an item

  Input Parameters:
    ipcCompany     :Company code
    ipcLocation    :Location code
   
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

DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.  
DEFINE VARIABLE list-name       AS cha       NO-UNDO.
DEFINE VARIABLE init-dir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ship-id       AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_16ths        AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE lv-got-shipto   AS LOGICAL   NO-UNDO.  
DEFINE VARIABLE scr-label-file  AS CHARACTER NO-UNDO.
DEFINE VARIABLE scr-auto-print  AS LOGICAL   INITIAL YES NO-UNDO. 
DEFINE VARIABLE loadtagFunction AS CHARACTER INITIAL "Order" NO-UNDO.
DEFINE VARIABLE g_company       AS CHARACTER NO-UNDO.

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
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

DEFINE VARIABLE v-po-no-source AS CHARACTER FORMAT "!" INIT "R".
DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "O".  
DEFINE VARIABLE v-out          AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE v-job          AS CHARACTER FORMAT "x(9)" NO-UNDO.  
DEFINE VARIABLE v-loadtag      AS CHARACTER NO-UNDO INIT "ASI".  /* sys ctrl option */
DEFINE VARIABLE v-mult         AS INTEGER   NO-UNDO INIT 0.  /* sys ctrl option */
DEFINE VARIABLE v-cas-lab      AS LOG       NO-UNDO.  /* sys ctrl option */
DEFINE VARIABLE v-tags         AS DECIMAL   NO-UNDO INIT 0.  /* sys ctrl option */
DEFINE VARIABLE v-count        AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE v-fgrecpt      AS LOG       NO-UNDO.  /* sys ctrl option */
DEFINE VARIABLE glOverrideMult AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDeptNote      AS LOGICAL   NO-UNDO INIT YES.    
DEFINE VARIABLE lv-r-no        LIKE rm-rctd.r-no NO-UNDO. 
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

DEFINE WORKFILE w-shipto LIKE shipto
    FIELD stat AS CHARACTER
    FIELD row-id AS ROWID.  
    
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

/* gdm - 04090909 */
DEFINE VARIABLE v-barflg      AS LOG NO-UNDO.
DEFINE VARIABLE v-auto-print  AS LOG NO-UNDO.
DEFINE VARIABLE UserlabelPath AS cha NO-UNDO.

/* gdm - 06100901 */
DEFINE VARIABLE v-txtflg      AS LOG NO-UNDO.

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
DEFINE VARIABLE lcRtnChar    AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound   AS LOG       NO-UNDO. 

DEFINE NEW SHARED TEMP-TABLE tt-word-print LIKE w-ord 
    FIELD tag-no AS CHARACTER . 
                          
DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.


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
&Scoped-Define ENABLED-OBJECTS RECT-26 bt-exit btnKeyboard ls-tagno ~
bt-print 
&Scoped-Define DISPLAYED-OBJECTS ls-tagno 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-exit AUTO-END-KEY 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
    LABEL "" 
    SIZE 9.6 BY 2.29 TOOLTIP "Exit".

DEFINE BUTTON bt-print 
    LABEL "Print" 
    SIZE 22.2 BY 3 TOOLTIP "Print"
    FONT 37.

DEFINE BUTTON btnKeyboard 
    IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
    LABEL "Keyboard" 
    SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE VARIABLE ls-tagno AS CHARACTER FORMAT "X(20)":U 
    VIEW-AS FILL-IN 
    SIZE 54 BY 1.38
    FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 105 BY 10.48.

DEFINE RECTANGLE RECT-26
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100.8 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    bt-exit AT ROW 2.19 COL 94.4 WIDGET-ID 84
    btnKeyboard AT ROW 2.62 COL 7 WIDGET-ID 132
    ls-tagno AT ROW 2.71 COL 25 COLON-ALIGNED NO-LABELS WIDGET-ID 10
    bt-print AT ROW 6.91 COL 41.2 WIDGET-ID 108
    "Tag #:" VIEW-AS TEXT
    SIZE 8 BY 1.33 AT ROW 2.67 COL 17 WIDGET-ID 12
    FONT 36
    RECT-26 AT ROW 5.52 COL 2.2 WIDGET-ID 18
    RECT-1 AT ROW 1 COL 1 WIDGET-ID 118
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 106.2 BY 10.62
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
        TITLE              = "Reprint Loadtag"
        HEIGHT             = 10.71
        WIDTH              = 105.8
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
ASSIGN 
    btnKeyboard:HIDDEN IN FRAME F-Main = TRUE.

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

        APPLY "CLOSE":U TO THIS-PROCEDURE.
    
        RETURN.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-print W-Win
ON CHOOSE OF bt-print IN FRAME F-Main /* Print */
    DO: 
        DEFINE VARIABLE cCheckField AS CHARACTER NO-UNDO.         
       
        IF ls-tagno:SCREEN-VALUE = "" THEN 
        DO:
            MESSAGE "Enter tag# to reprint loadtag." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ls-tagno.
            RETURN NO-APPLY.
        END.
        
            
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        
        RUN new-cas-lab. 
               
        ASSIGN
            cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
        
        FOR EACH tt-word-print:
            DELETE tt-word-print .
        END.     
        
        EMPTY TEMP-TABLE w-ord.        
        
        RUN reprint-tag .                     
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard W-Win
ON CHOOSE OF btnKeyboard IN FRAME F-Main /* Keyboard */
    DO:
        APPLY "ENTRY":U TO ls-tagno.
        RUN pKeyboard (ls-tagno:HANDLE, "Qwerty").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-tagno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-tagno W-Win
ON ENTRY OF ls-tagno IN FRAME F-Main
    DO:
        hFocusField = SELF.
        IF lKeyboard THEN
            RUN pKeyboard (SELF, "Qwerty").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-tagno W-Win
ON HELP OF ls-tagno IN FRAME F-Main
    DO:
        DEFINE VARIABLE rec-val  AS RECID NO-UNDO.
        DEFINE VARIABLE char-val AS cha   NO-UNDO.

        RUN addon/windows/l-ldtaga.w (cocode, NO, NO, ls-tagno:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
  
        IF char-val NE "" THEN 
        DO:
            ls-tagno:SCREEN-VALUE = ENTRY(1,char-val).
            RUN new-cas-lab.
            IF RETURN-VALUE EQ 'ERROR' THEN
                APPLY 'ENTRY':U TO ls-tagno.
        END.
        RETURN NO-APPLY.        

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-tagno W-Win
ON LEAVE OF ls-tagno IN FRAME F-Main
    DO:
        DEFINE VARIABLE cJobNo    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cJobNo2   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFormNo   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cBlankNo  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lParse    AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lValidJob AS LOGICAL   NO-UNDO.    
    
        IF VALID-HANDLE(hKeyboard) THEN
            DELETE OBJECT hKeyboard.
        IF ls-tagno:SCREEN-VALUE NE "" THEN
        DO:
        
            FIND FIRST loadtag WHERE loadtag.company     EQ cocode
                AND loadtag.item-type   EQ NO
                AND loadtag.tag-no  EQ TRIM(ls-tagno:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE loadtag THEN 
            DO:
                MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO ls-tagno.
                RETURN NO-APPLY.
            END.               
       
        END.
        RUN new-cas-lab. 
       
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetLoadtag W-Win 
PROCEDURE pGetLoadtag :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER io-tag-no AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-total-unit LIKE w-ord.total-unit NO-UNDO.
        
    FIND FIRST loadtag NO-LOCK  
        WHERE loadtag.company   EQ cocode
        AND loadtag.item-type EQ NO
        AND loadtag.tag-no    EQ TRIM(ls-tagno:SCREEN-VALUE IN FRAME {&FRAME-NAME})
        USE-INDEX tag NO-ERROR.
    IF AVAILABLE loadtag THEN
        io-tag-no = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.      
 
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
                        /*  pGetLoadtag may prompt, so need to close default stream */
                        OUTPUT CLOSE.   
                        RUN pGetLoadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit) NO-ERROR.
                        
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
                                            
                            /* pGetLoadtag may prompt, so need to close default stream */
                            OUTPUT CLOSE.         
                            RUN pGetLoadtag (INPUT-OUTPUT lv-tag-no, 0) NO-ERROR.
                            OUTPUT TO VALUE(cTmpFile) APPEND.

                        END.
                        cTotalUnit = "".      
                        RUN write-loadtag-line (INPUT cRFIDTag, cTotalUnit, INPUT iPalletID, INPUT liTagCounter).
                        iPalletID = iPalletID + 1.
                    END.
                END. /*not SSLABEL, Centbox*/

            END. /*w-ord.total-tags > 0*/
            
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
    DISPLAY ls-tagno 
        WITH FRAME F-Main IN WINDOW W-Win.
    ENABLE RECT-26 bt-exit btnKeyboard ls-tagno bt-print 
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
       
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ ipcCompany
        NO-ERROR .
    IF AVAILABLE company THEN
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
            + " - {&awversion}" + " - " 
            + STRING(company.name) + " - " + ipcLocation.  
             
    RUN pGetSettings.
        
    APPLY "ENTRY" TO ls-tagno IN FRAME {&FRAME-NAME}.       
    

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
            AND b-job.job-no  = loadtag.job-no
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
            w-ord.ipReturn     = /*tb_ret*/ NO
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
        /*IF tb_xfer-lot THEN w-ord.lot# = w-ord.rel-lot#.*/

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
                /*IF NOT tb_ship-id THEN */
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
            /*IF NOT tb_ship-id THEN */
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
                AND job.job-no = loadtag.job-no
                AND job.job-no2 = loadtag.job-no2  NO-ERROR.
            IF AVAILABLE job THEN
                FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job-hdr THEN 
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
                w-ord.ipReturn     = /*tb_ret*/ NO  .

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
                        w-ord.ipReturn  = /*tb_ret*/ NO . 
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
                        w-ord.ipReturn     = /*tb_ret*/ NO .

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
   
    FIND FIRST fg-bin WHERE fg-bin.company = loadtag.company
        AND fg-bin.i-no = w-ord.i-no
        AND fg-bin.tag = ls-tagno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND fg-bin.qty > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE fg-bin AND AVAILABLE w-ord THEN
        ASSIGN w-ord.pcs        = fg-bin.case-count
            w-ord.bundle     = /*fg-bin.cases-unit*/ TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
            w-ord.partial    = fg-bin.partial-count
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial .      
  
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
        AND loadtag.tag-no  EQ TRIM(ls-tagno:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE loadtag THEN 
    DO:
        MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ls-tagno.
        RETURN ERROR.
    END.  
    
    ASSIGN
        cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" 
                        ELSE IF scr-label-file MATCHES "*.lwl" THEN "loftware" 
                        ELSE "".
    RUN create-w-ord.

    SESSION:SET-WAIT-STATE ("general").        
  
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
    ELSE IF cBarCodeProgram EQ "xprint" THEN 
        DO:
            PAUSE 1.                                            
            RUN "oerep/LoadtagProcs.p" PERSISTENT SET hLoadtagProcs.
            RUN pPrintView IN hLoadtagProcs (scr-label-file, YES).
            DELETE OBJECT hLoadtagProcs.
        END.

    IF (NOT is-from-addons() OR SSLoadTag-log = TRUE) THEN 
        MESSAGE "Loadtag reprint is completed." VIEW-AS ALERT-BOX INFORMATION.
    SESSION:SET-WAIT-STATE ("").


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
   
        FIND FIRST loadtag
            WHERE loadtag.company     EQ cocode
            AND loadtag.tag-no      BEGINS TRIM(ls-tagno:SCREEN-VALUE)
            AND loadtag.item-type   EQ NO              
            NO-LOCK NO-ERROR.
          
        IF AVAILABLE loadtag THEN 
        DO:              
            RUN cas-lab-label-mat-file.       
        END.
    END.

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
        IF loadtag.ord-no NE 0 THEN
        DO:
           
            FIND FIRST oe-ord WHERE
                oe-ord.company EQ cocode AND
                oe-ord.ord-no  EQ INT(loadtag.ord-no)               
                NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ord THEN
            DO:
                FIND FIRST cust-part WHERE
                    cust-part.company  EQ cocode AND
                    cust-part.i-no     EQ loadtag.i-no AND                 
                    cust-part.cust-no  EQ oe-ord.cust-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE cust-part AND cust-part.labelCase NE "" THEN
                    scr-label-file = (IF cust-part.labelPallet <> "" THEN cust-part.labelPallet ELSE v-bardir-chr).
                ELSE 
                DO:
                    IF loadtag.i-no NE "" THEN
                        FIND FIRST oe-rel WHERE
                            oe-rel.company EQ cocode AND
                            oe-rel.i-no    GE loadtag.i-no AND                         
                            oe-rel.ord-no  EQ INT(oe-ord.ord-no)                         
                            NO-LOCK NO-ERROR.
                    ELSE
                        FIND FIRST oe-rel WHERE
                            oe-rel.company EQ cocode AND
                            oe-rel.ord-no  EQ INT(oe-ord.ord-no)                        
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
                            scr-label-file = sys-ctrl-shipto.char-fld.
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
                            scr-label-file = sys-ctrl-shipto.char-fld.
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
                            THEN scr-label-file = 
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
                                scr-label-file = 
                                    sys-ctrl-shipto.char-fld.
                            ELSE 
                            DO:
                                FIND FIRST sys-ctrl NO-LOCK 
                                    WHERE sys-ctrl.company EQ cocode 
                                    AND sys-ctrl.name    EQ "BARDIR" 
                                    NO-ERROR.
                                IF AVAILABLE sys-ctrl 
                                    THEN
                                    scr-label-file = sys-ctrl.char-fld.
                                ELSE scr-label-file = "".
                            END.
                        END.
                    END.
                END.
            END.       
        END.
        IF scr-label-file EQ "" THEN 
        DO:
            FIND FIRST sys-ctrl NO-LOCK 
                WHERE sys-ctrl.company EQ cocode 
                AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
            IF AVAILABLE sys-ctrl THEN
                scr-label-file = sys-ctrl.char-fld.
        END.
    END.
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

