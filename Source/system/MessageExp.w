&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-msgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-msgexp 
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
DEFINE INPUT PARAMETER ipiMessageFrom AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiMessageTo   AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}

DEFINE {&NEW} SHARED VARIABLE g_batch       AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS ROWID   NO-UNDO.
DEFINE               VARIABLE v-prgmname    LIKE prgrms.prgmname NO-UNDO.
DEFINE               VARIABLE lActive       AS LOGICAL NO-UNDO .
DEFINE               VARIABLE ou-log        AS LOGICAL NO-UNDO .
{sys/inc/var.i new shared}

v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

ASSIGN
    cocode = g_company
    /*locode = gloc*/ .

DO TRANSACTION:
    {sys/ref/CustList.i NEW}
END.

DEFINE STREAM excel.


DEFINE VARIABLE ldummy             AS LOG NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS cha NO-UNDO.

ASSIGN 
    cTextListToSelect  = "Message ID,Message Name,Msg Type,Module,Hot Key,Security Level,Default Title," +
                         "Default Message,Custom Title,Custom Message,Suppress Display,Default Answer"  
                           
    cFieldListToSelect = "msgID,msgName,msgType,module,hotKey,currSecLevel,defaultTitle," +
                         "defaultMsg,currentTitle,currMessage,userSuppress,rtnValue".
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Message ID,Message Name,Msg Type,Module,Hot Key,Security Level,Default Title," +
                         "Default Message,Custom Title,Custom Message,Suppress Display,Default Answer".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-msgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_msg-id end_msg-id ~
begin_mod end_mod Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_msg-id end_msg-id begin_mod end_mod ~
sl_avail sl_selected tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-msgexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-msgexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-msgexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-msg rd-msgexp 
FUNCTION getValue-msg RETURNS CHARACTER
    ( BUFFER ipb-msg FOR zMessage, ipc-field AS CHARACTER )  FORWARD.

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

DEFINE VARIABLE begin_mod  AS CHARACTER FORMAT "x(3)" 
    LABEL "From Module" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE begin_msg-id AS CHARACTER FORMAT "x(16)" 
    LABEL "From Message" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE end_mod    AS CHARACTER FORMAT "X(3)" INITIAL "zzzzzzzzzzz" 
    LABEL "To Module" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_msg-id   AS CHARACTER FORMAT "X(16)" INITIAL "zzzzzzzzzzz" 
    LABEL "To Message" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE fi_file    AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-msg.csv" 
    LABEL "If Yes, File Name" 
    VIEW-AS FILL-IN 
    SIZE 43 BY 1
    FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 101 BY 8.1.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 101 BY 2.48.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tb_excel    AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL   INITIAL YES 
    LABEL "Auto Run Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-msgexp
    begin_msg-id AT ROW 3.67 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Name" WIDGET-ID 142
    end_msg-id AT ROW 3.67 COL 71 COLON-ALIGNED HELP
    "Enter Ending Name" WIDGET-ID 144
    begin_mod AT ROW 4.62 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Module" WIDGET-ID 150
    end_mod AT ROW 4.62 COL 71 COLON-ALIGNED HELP
    "Enter Ending Module" WIDGET-ID 152
    Btn_Def AT ROW 11.19 COL 44 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_avail AT ROW 11.24 COL 9 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 11.24 COL 64 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 12.33 COL 44 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    Btn_Remove AT ROW 13.52 COL 44 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 14.71 COL 44 WIDGET-ID 136
    btn_down AT ROW 15.91 COL 44 WIDGET-ID 132
    tb_excel AT ROW 17.91 COL 36 WIDGET-ID 32
    tb_runExcel AT ROW 17.91 COL 78 RIGHT-ALIGNED WIDGET-ID 34
    fi_file AT ROW 18.86 COL 34 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    btn-ok AT ROW 20.71 COL 30 WIDGET-ID 14
    btn-cancel AT ROW 20.71 COL 60.2 WIDGET-ID 12
    "Selected Columns" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.52 COL 64.4 WIDGET-ID 138
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.52 COL 9.4 WIDGET-ID 140
    "Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 9.52 COL 3 WIDGET-ID 86
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
    BGCOLOR 2 
    RECT-6 AT ROW 9.76 COL 2 WIDGET-ID 30
    RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
    RECT-8 AT ROW 17.62 COL 2 WIDGET-ID 84
    SPACE(2.39) SKIP(2.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Export Message to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-msgexp
   FRAME-NAME                                                           */
ASSIGN 
    FRAME rd-msgexp:SCROLLABLE = FALSE
    FRAME rd-msgexp:HIDDEN     = TRUE.

ASSIGN 
    begin_mod:PRIVATE-DATA IN FRAME rd-msgexp = "parm".

ASSIGN 
    begin_msg-id:PRIVATE-DATA IN FRAME rd-msgexp = "parm".

ASSIGN 
    end_mod:PRIVATE-DATA IN FRAME rd-msgexp = "parm".

ASSIGN 
    end_msg-id:PRIVATE-DATA IN FRAME rd-msgexp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-msgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-msgexp
   NO-ENABLE                                                            */
ASSIGN 
    tb_excel:PRIVATE-DATA IN FRAME rd-msgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-msgexp
   ALIGN-R                                                              */
ASSIGN 
    tb_runExcel:PRIVATE-DATA IN FRAME rd-msgexp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-msgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-msgexp rd-msgexp
ON HELP OF FRAME rd-msgexp /* Export SysCtrl to Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :

        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-msgexp rd-msgexp
ON WINDOW-CLOSE OF FRAME rd-msgexp /* Export SysCtrl to Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mod rd-msgexp
ON LEAVE OF begin_mod IN FRAME rd-msgexp /* From Module */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_msg-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_msg-id rd-msgexp
ON LEAVE OF begin_msg-id IN FRAME rd-msgexp /* From Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-msgexp
ON CHOOSE OF btn-cancel IN FRAME rd-msgexp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-msgexp
ON CHOOSE OF btn-ok IN FRAME rd-msgexp /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        RUN GetSelectionList.  
  
        RUN run-report.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-msgexp
ON CHOOSE OF Btn_Add IN FRAME rd-msgexp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-msgexp
ON CHOOSE OF Btn_Def IN FRAME rd-msgexp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-msgexp
ON CHOOSE OF btn_down IN FRAME rd-msgexp /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-msgexp
ON CHOOSE OF Btn_Remove IN FRAME rd-msgexp /* << Remove */
    DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-msgexp
ON CHOOSE OF btn_Up IN FRAME rd-msgexp /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mod rd-msgexp
ON LEAVE OF end_mod IN FRAME rd-msgexp /* To Module */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_msg-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_msg-id rd-msgexp
ON LEAVE OF end_msg-id IN FRAME rd-msgexp /* To Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-msgexp
ON LEAVE OF fi_file IN FRAME rd-msgexp /* If Yes, File Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-msgexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-msgexp
    DO:
  
        IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-msgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-msgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-msgexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-msgexp /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-msgexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-msgexp /* Auto Run Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-msgexp 


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
        APPLY "entry" TO begin_msg-id.
    END.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-msgexp  _DEFAULT-DISABLE
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
    HIDE FRAME rd-msgexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-msgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-msgexp 
PROCEDURE DisplaySelectionList :
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
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-msgexp 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
    DEFINE VARIABLE cTmpList      AS cha     NO-UNDO.

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

    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
            ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-msgexp  _DEFAULT-ENABLE
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
    DISPLAY begin_msg-id end_msg-id begin_mod end_mod sl_avail sl_selected tb_excel 
        tb_runExcel fi_file 
        WITH FRAME rd-msgexp.
    ENABLE RECT-6 RECT-7 RECT-8 begin_msg-id end_msg-id begin_mod end_mod Btn_Def 
        sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down tb_runExcel 
        fi_file btn-ok btn-cancel 
        WITH FRAME rd-msgexp.
    VIEW FRAME rd-msgexp.
    {&OPEN-BROWSERS-IN-QUERY-rd-msgexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-msgexp 
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
        ASSIGN 
            ttRptSelected.TextList  = ENTRY(i,cTmpList)
            ttRptSelected.FieldList = ttRptList.FieldList
            /* ttRptSelected.FieldLength */
            .   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-msgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-msgexp 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cexcelheader         AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
    DEFINE BUFFER b-zMessage FOR zMessage.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").
    
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    FIND FIRST users NO-LOCK
        WHERE  users.user_id EQ USERID(LDBNAME(1)) 
        NO-ERROR.
             
    FOR EACH b-zMessage NO-LOCK
        WHERE  integer(b-zMessage.msgID) GE integer(begin_msg-id)
        AND integer(b-zMessage.msgID) LE integer(end_msg-id)
        AND b-zMessage.module GE begin_mod
        AND b-zMessage.module LE end_mod
        AND b-zMessage.currSecLevel LE users.securityLevel:

        v-excel-detail-lines = "".

        FOR EACH ttRptSelected:
            v-excel-detail-lines = v-excel-detail-lines + 
                appendXLLine(getValue-msg(BUFFER b-zMessage,ttRptSelected.FieldList)).
        END.

        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP. 

    END. /* b-utilities */

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-PrepExp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
    ASSIGN 
        begin_msg-id:SCREEN-VALUE = STRING(ipiMessageFrom)
        end_msg-id:SCREEN-VALUE   = STRING(ipiMessageTo)
        .     
END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-msgexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Adds a value to a csv line
        Notes:  Protects agains commans and quotes.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-msgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-msgexp 
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
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-msg rd-msgexp 
FUNCTION getValue-msg RETURNS CHARACTER
    ( BUFFER ipb-izMessage FOR zMessage, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    {custom/getperd.i} 
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
    
    CASE ipc-field :
        WHEN "dfuncTotMSFPTD"  THEN 
            DO:
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-izMessage:BUFFER-FIELD(ipc-field).
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

