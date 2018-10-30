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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}          */

/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/
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
    cTextListToSelect  = "Location,Type,Default Bin,Name,Address1,Address2,Address3,Address4,Address5,Address6," +
                           "City,St Prov,Zip Post,Country,County,Lat,Long,Phone,Ext Code,Fax,Email,Notes,Primary Bin Loc"

    cFieldListToSelect = "loc,type,def-bin,dscr,addr[1],addr[2],addr[3],addr[4],addr[5],addr[6]," +
                           "city,st-prov,zip-post,country,county,lat,long,phone,ext-code,fax,email,notes,prm-bin-loc" .
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Location,Type,Primary Bin Loc" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_location ~
end_location rd_type Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_location end_location lbl_Type ~
rd_type sl_avail sl_selected tb_excel tb_runExcel fi_file 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
    ( BUFFER ipb-itemfg FOR loc, ipc-field AS CHARACTER, ipc-bin-loc AS CHARACTER, ipc-type AS CHARACTER )  FORWARD.

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

DEFINE VARIABLE begin_location AS CHARACTER FORMAT "x(5)" 
     LABEL "From Location" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_location AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "To Location" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-locbin.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_Type AS CHARACTER FORMAT "X(256)":U INITIAL "Type?" 
     VIEW-AS FILL-IN 
     SIZE 7.5 BY 1 NO-UNDO.

DEFINE VARIABLE rd_type AS CHARACTER INITIAL "FG" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG", "FG",
"RM", "RM",
"WIP", "WIP"
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.14.

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
     begin_location AT ROW 3.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Location" WIDGET-ID 142
     end_location AT ROW 3.95 COL 71 COLON-ALIGNED HELP
          "Enter Ending Location" WIDGET-ID 144
     lbl_Type AT ROW 5.71 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     rd_type AT ROW 5.76 COL 34 NO-LABEL WIDGET-ID 148
     Btn_Def AT ROW 10.1 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 10.14 COL 9 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 10.14 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.24 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 12.43 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 13.62 COL 44 WIDGET-ID 136
     btn_down AT ROW 14.81 COL 44 WIDGET-ID 132
     tb_excel AT ROW 16.81 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 16.81 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 17.76 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 19.62 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 19.62 COL 60.2 WIDGET-ID 12
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.43 COL 64.4 WIDGET-ID 138
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 8.43 COL 3 WIDGET-ID 86
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.43 COL 9.4 WIDGET-ID 140
     RECT-6 AT ROW 8.67 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 16.52 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export location/bins to Excel" WIDGET-ID 100.


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
       begin_location:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       end_location:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_Type IN FRAME rd-fgexp
   NO-ENABLE                                                            */
ASSIGN 
       lbl_Type:PRIVATE-DATA IN FRAME rd-fgexp     = 
                "rd_type".

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
ON HELP OF FRAME rd-fgexp /* Export location/bins to Excel */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export location/bins to Excel */
DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_location
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_location rd-fgexp
ON LEAVE OF begin_location IN FRAME rd-fgexp /* From Location */
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
  
        RUN run-report.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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


&Scoped-define SELF-NAME end_location
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_location rd-fgexp
ON LEAVE OF end_location IN FRAME rd-fgexp /* To Location */
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


&Scoped-define SELF-NAME rd_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_type rd-fgexp
ON VALUE-CHANGED OF rd_type IN FRAME rd-fgexp
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
                .                                                      

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgexp /* Export To Excel? */
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
        /* RUN Set-Sort-Data.*/

        APPLY "entry" TO begin_location.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgexp 
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
  
    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
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
  DISPLAY begin_location end_location lbl_Type rd_type sl_avail sl_selected 
          tb_excel tb_runExcel fi_file 
      WITH FRAME rd-fgexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_location end_location rd_type Btn_Def 
         sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down tb_runExcel 
         fi_file btn-ok btn-cancel 
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
        ASSIGN 
            ttRptSelected.TextList  = ENTRY(i,cTmpList)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cexcelheader         AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-loc FOR loc.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    cType = SUBSTR(rd_type,1,3).

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    FOR EACH b-loc NO-LOCK 
      WHERE b-loc.company = cocode
        AND b-loc.loc GE begin_location
        AND b-loc.loc LE end_location :

        v-excel-detail-lines = "".

       /* */

        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.


        i = 0.
        IF cType EQ "FG" THEN DO:
            FOR EACH fg-bin NO-LOCK 
                WHERE  fg-bin.company EQ b-loc.company
                    AND fg-bin.loc EQ b-loc.loc 
                    AND fg-bin.i-no = '':
                i = i + 1.
                v-excel-detail-lines = "".
               
                FOR EACH ttRptSelected:
                    v-excel-detail-lines = v-excel-detail-lines + 
                        appendXLLine(getValue-itemfg(BUFFER b-loc,ttRptSelected.FieldList,fg-bin.loc-bin,cType)).
                END.
                PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

            END. /* for each fg-bin */  
            
        END.
        ELSE IF cType EQ "RM" THEN DO:
            FOR EACH rm-bin NO-LOCK 
                WHERE  rm-bin.company EQ b-loc.company
                    AND rm-bin.loc EQ b-loc.loc
                    AND rm-bin.i-no = '':
                i = i + 1.
                v-excel-detail-lines = "".
                
                FOR EACH ttRptSelected:
                    v-excel-detail-lines = v-excel-detail-lines + 
                        appendXLLine(getValue-itemfg(BUFFER b-loc,ttRptSelected.FieldList,rm-bin.loc-bin,cType)).
                END.
                PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

            END. /* for each rm-bin */
        END.
        ELSE DO:
            FOR EACH wip-bin NO-LOCK 
                WHERE  wip-bin.company EQ b-loc.company
                    AND wip-bin.loc EQ b-loc.loc:
                i = i + 1.
                v-excel-detail-lines = "".
               
                FOR EACH ttRptSelected:
                    v-excel-detail-lines = v-excel-detail-lines + 
                        appendXLLine(getValue-itemfg(BUFFER b-loc,ttRptSelected.FieldList,wip-bin.loc-bin,cType)).
                END.
                PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

            END. /* for each rm-bin */
        END.
        IF i EQ 0 THEN DO:
             FOR EACH ttRptSelected:
                    v-excel-detail-lines = v-excel-detail-lines + 
                        appendXLLine(getValue-itemfg(BUFFER b-loc,ttRptSelected.FieldList,"",cType)).
             END.
             PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
        END.
    END. /* cust */

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
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

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
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

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgexp 
FUNCTION getValue-itemfg RETURNS CHARACTER
    ( BUFFER ipb-itemfg FOR loc, ipc-field AS CHARACTER, ipc-loc-bin AS CHARACTER, ipc-type AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    {custom/getperd.i} 
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
   
    
    FIND FIRST location NO-LOCK
        WHERE location.locationCode = ipb-itemfg.loc 
        AND location.rec_key = ipb-itemfg.addrRecKey NO-ERROR.
   
    CASE ipc-field :
        WHEN "def-bin" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.defaultBin .
                ELSE
                    lc-return = "".
            END.
        WHEN "addr[1]" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.streetAddr[1] .
                ELSE
                    lc-return = "".
            END.
        WHEN "addr[2]" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.streetAddr[2] .
                ELSE
                    lc-return = "".
            END.
        WHEN "addr[3]" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.streetAddr[3] .
                ELSE
                    lc-return = "".
            END.
        WHEN "addr[4]" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.streetAddr[4] .
                ELSE
                    lc-return = "".
            END.
        WHEN "addr[5]" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.streetAddr[5] .
                ELSE
                    lc-return = "".
            END.
        WHEN "addr[6]" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.streetAddr[6] .
                ELSE
                    lc-return = "".
            END.
        WHEN "city" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.subCode3 .
                ELSE
                    lc-return = "".
            END.
        WHEN "st-prov" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.subCode1 .
                ELSE
                    lc-return = "".
            END.
        WHEN "zip-post" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.subCode4 .
                ELSE
                    lc-return = "".
            END.
        WHEN "country" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.countryCode .
                ELSE
                    lc-return = "".
            END.
        WHEN "county" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.subCode2 .
                ELSE
                    lc-return = "".
            END.
        WHEN "lat" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = STRING(location.geoLat).
                ELSE
                    lc-return = "".
            END.
        WHEN "long" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = STRING(location.geoLong) .
                ELSE
                    lc-return = "".
            END.
        WHEN "phone" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.phone .
                ELSE
                    lc-return = "".
            END.
        WHEN "ext-code" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.externalID[1] .
                ELSE
                    lc-return = "".
            END.
        WHEN "fax" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.fax .
                ELSE
                    lc-return = "".
            END.
        WHEN "email" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.email .
                ELSE
                    lc-return = "".
            END.
        WHEN "notes" THEN 
            DO:
                IF AVAILABLE location THEN
                    lc-return = location.notes .
                ELSE
                    lc-return = "".
        END.
        WHEN "prm-bin-loc"  THEN DO:
            lc-return = ipc-loc-bin.
        END.
        WHEN "type"  THEN DO:   
            lc-return = ipc-type .
        END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
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

