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
/*DEFINE INPUT PARAMETER Autofind AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER Broword   AS CHAR NO-UNDO.*/
DEFINE INPUT PARAMETER ipcEmpFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEmpTo   AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
ASSIGN 
    cTextListToSelect  = "Emp.ID,First Name,Last Name,Social Security,Start Date,Start Time,End Date,End Time,Total Time,Machine,Mach Description,Shift" 
    cFieldListToSelect = "empid,first-n,last-n,soc,start-date,start-time,end-date,end-time,tot,mach,mach-desc,shift"  .

{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Emp.ID,First Name,Last Name,Social Security,Start Date,Start Time,End Date,End Time,Total Time,Machine,Mach Description,Shift"   .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_emp end_emp ~
Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_emp end_emp sl_avail sl_selected ~
fi_file tb_OpenCSV tbAutoClose 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-estf rd-fgexp 
FUNCTION getValue-estf RETURNS CHARACTER
    ( BUFFER ipb-estf FOR employee, ipc-field AS CHARACTER , ipc-rowid AS ROWID )  FORWARD.

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

DEFINE VARIABLE begin_emp AS CHARACTER FORMAT "x(6)" 
    LABEL "From Employee" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE end_emp   AS CHARACTER FORMAT "X(6)" INITIAL "zzzzz" 
    LABEL "To Employee" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE fi_file   AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\EmployeeTracking.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 7.86.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 5.43.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 100 BY 2.48.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 6.14 NO-UNDO.

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
    SIZE 15.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
    begin_emp AT ROW 3.52 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Employee Number" WIDGET-ID 142
    end_emp AT ROW 3.52 COL 69.8 COLON-ALIGNED HELP
    "Enter Ending Employee #" WIDGET-ID 144
    Btn_Def AT ROW 8.38 COL 44.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_avail AT ROW 8.43 COL 4.8 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 8.43 COL 68 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 9.52 COL 44.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    Btn_Remove AT ROW 10.67 COL 44.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 11.81 COL 44.8 WIDGET-ID 136
    btn_down AT ROW 13 COL 44.8 WIDGET-ID 132
    tb_excel AT ROW 15.95 COL 36 WIDGET-ID 32
    fi_file AT ROW 16.43 COL 22.6 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 16.52 COL 88.6 RIGHT-ALIGNED WIDGET-ID 34
    tbAutoClose AT ROW 18.38 COL 32.2 WIDGET-ID 64
    btn-ok AT ROW 19.43 COL 32 WIDGET-ID 14
    btn-cancel AT ROW 19.43 COL 58.8 WIDGET-ID 12
    "Selected Columns" VIEW-AS TEXT
    SIZE 23.6 BY .62 AT ROW 7.71 COL 68 WIDGET-ID 138
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 7.71 COL 5.2 WIDGET-ID 140
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 6.95 COL 4 WIDGET-ID 86
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4 WIDGET-ID 36
    RECT-6 AT ROW 7.38 COL 3 WIDGET-ID 30
    RECT-7 AT ROW 1.52 COL 3 WIDGET-ID 38
    RECT-8 AT ROW 15.67 COL 3 WIDGET-ID 84
    SPACE(1.59) SKIP(2.79)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Export Employee Tracking And Data Collection To Excel" WIDGET-ID 100.


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
    FRAME rd-fgexp:SCROLLABLE = FALSE
    FRAME rd-fgexp:HIDDEN     = TRUE.

ASSIGN 
    begin_emp:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    end_emp:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd-fgexp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export Employee Tracking And Data Collection To Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :

        /* when "begin_mach" then do:
             ls-cur-val = lw-focus:screen-value.
             run windows/l-mach.w (cocode, locode, ls-cur-val, output char-val).
             if char-val <> "" then do:
                lw-focus:screen-value =  ENTRY(1,char-val).
             end.
             return no-apply.
         end.  /* itemfg */
         when "end_mach" then do:
             ls-cur-val = lw-focus:screen-value.
             run windows/l-mach.w (cocode, locode, ls-cur-val, output char-val).
             if char-val <> "" then do:
                lw-focus:screen-value =  ENTRY(1,char-val).
             end.
             return no-apply.
         end.  /* itemfg*/  */

        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export Employee Tracking And Data Collection To Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_emp rd-fgexp
ON LEAVE OF begin_emp IN FRAME rd-fgexp /* From Employee */
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
        END.  /* IF NOT tb_OpenCSV THEN */
        ELSE DO:
  	    OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
	END.
  
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

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


&Scoped-define SELF-NAME end_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_emp rd-fgexp
ON LEAVE OF end_emp IN FRAME rd-fgexp /* To Employee */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON HELP OF fi_file IN FRAME rd-fgexp /* Name */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* Name */
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-fgexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-fgexp /* Open CSV? */
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
RUN Set-Sort-Data.

APPLY "entry" TO begin_emp.
fi_file:SCREEN-VALUE = "c:\tmp\EmployeeTracking.csv".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgexp 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTmpList      AS CHARACTER NO-UNDO.

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

    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO iCount = 1 TO sl_selected:NUM-ITEMS: /* task 08191414 */
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
    DISPLAY begin_emp end_emp sl_avail sl_selected fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME rd-fgexp.
    ENABLE RECT-6 RECT-7 RECT-8 begin_emp end_emp Btn_Def sl_avail sl_selected 
        Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV tbAutoClose 
        btn-ok btn-cancel 
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
    DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

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
    DEFINE VARIABLE v-check              AS LOG       INIT NO NO-UNDO .

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    FOR EACH employee WHERE  employee.company = gcompany 
        AND employee.employee GE begin_emp 
        AND employee.employee LE end_emp NO-LOCK :
        ASSIGN  
            v-check = YES .
    
        FOR EACH emplogin OF employee NO-LOCK:
            ASSIGN  
                v-check = NO .
            v-excel-detail-lines = "".
         
            FOR EACH ttRptSelected:
                v-excel-detail-lines = v-excel-detail-lines + 
                    appendXLLine(getValue-estf(BUFFER employee, ttRptSelected.FieldList, ROWID(emplogin) )).
            END.
            PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
        END.
        IF v-check = YES THEN 
        DO:
            v-excel-detail-lines = "".
            FOR EACH ttRptSelected:
                v-excel-detail-lines = v-excel-detail-lines + 
                    appendXLLine(getValue-estf(BUFFER employee, ttRptSelected.FieldList, ROWID(employee) )).
            END.
            PUT STREAM excel UNFORMATTED SKIP v-excel-detail-lines SKIP.
        END.
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
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
        IF ipcEmpFrom <> "" THEN
            ASSIGN 
                begin_emp:SCREEN-VALUE = assignParam(ipcEmpFrom,NO)
                end_emp:SCREEN-VALUE   = assignParam(ipcEmpTo,NO)
                .

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
    
    lc-line = quoter(ipc-append) + ",".
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
        lc-return = ipc-param + "ZZZZZ".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-estf rd-fgexp 
FUNCTION getValue-estf RETURNS CHARACTER
    ( BUFFER ipb-estf FOR employee, ipc-field AS CHARACTER , ipc-rowid AS ROWID ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
    DEFINE VARIABLE j         AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE li-t-time AS INTEGER   NO-UNDO.

    FIND FIRST emplogin WHERE ROWID(emplogin) EQ ipc-rowid NO-LOCK NO-ERROR . 
    
    CASE ipc-field :
        WHEN "empid"  THEN 
            DO:
                lc-return = ipb-estf.employee.
            END.
        WHEN "first-n"  THEN 
            DO:
                lc-return = ipb-estf.first_name .
            END.
        WHEN "last-n"  THEN 
            DO:
                lc-return = ipb-estf.last_name .
            END.
        WHEN "soc"  THEN 
            DO:
                lc-return = IF ipb-estf.soc_sec <> ? THEN STRING(ipb-estf.soc_sec) ELSE "".
            END.
        WHEN "start-date"  THEN 
            DO:
                IF AVAILABLE emplogin THEN
                    lc-return = IF emplogin.start_date <> ? THEN  STRING(emplogin.start_date) ELSE  "".
                ELSE lc-return = "" .
            END.
        WHEN "start-time"  THEN 
            DO:
                IF AVAILABLE emplogin THEN
                    lc-return = IF emplogin.start_time <> ? THEN TRIM(STRING(emplogin.start_time,"HH:MM am")) ELSE "".
                ELSE lc-return = "" .
            END.
        WHEN "end-date"  THEN 
            DO:
                IF AVAILABLE emplogin THEN
                    lc-return = IF emplogin.end_date <> ? THEN STRING(emplogin.end_date) ELSE "".
                ELSE lc-return = "" .
            END.
        WHEN "end-time"  THEN 
            DO:
                IF AVAILABLE emplogin THEN
                    lc-return = IF emplogin.end_date <> ? THEN TRIM(STRING(emplogin.end_time,"HH:MM am")) ELSE "" .
                ELSE lc-return = "" .
            END.  
        WHEN "mach"  THEN 
            DO:
                IF AVAILABLE emplogin THEN
                    lc-return = STRING(emplogin.machine) .
                ELSE lc-return = "" .
            END.
        WHEN "mach-desc"  THEN 
            DO:
                IF AVAILABLE emplogin THEN
                    FIND FIRST mach WHERE mach.company = cocode
                        AND mach.m-code = emplogin.machine NO-LOCK NO-ERROR.
                IF AVAILABLE mach THEN
                    lc-return = STRING(mach.m-dscr)  .
                ELSE lc-return = "" .
            END.
        WHEN "shift"  THEN 
            DO:
                IF AVAILABLE emplogin THEN
                    lc-return = STRING(emplogin.shift)  .
                ELSE lc-return = "" .
            END.
        WHEN "tot"  THEN 
            DO:
                IF AVAILABLE emplogin THEN 
                DO:
                    li-t-time = emplogin.total_time .
                    lc-return = TRIM(STRING(TRUNCATE(li-t-time / 3600,0),">>>>9") + ":" + 
                        STRING(((li-t-time MOD 3600) / 60),"99"))  .
                END.
                ELSE lc-return = "" .
            END.
        
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-estf:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

/* END.*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

