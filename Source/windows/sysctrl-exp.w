&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-sysexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-sysexp 
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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "Company,Config Name,Config Description,Category,Sub-category,Module,Allows Context,Character Value,Character Value - Default,Character Value - Description,Date Value,Date Value - Default,Date Value - Description," +
                          "Decimal Value,Decimal Value - Default,Decimal Value - Description,Integer Value,Integer Value - Default,Integer Value - Description,Logical Value,Logical Value - Default,Logical Value - Description,User Sec Level,User Sec Lev - Default," +
                          "Help Contents"
                           
    cFieldListToSelect = "company,name,descrip,category,subCategory,module,allowsContext,char-fld,char_field_default,char-fld_descrip,date-fld,date-fld_default,date-fld_descrip," + 
                          "dec-fld,dec-fld_default,dec-fld_descrip,int-fld,int-fld_default,int-fld_descrip,log-fld,log-fld_default,log-fld_descrip,securityLevelUser,securityLevelDefault," +
                          "help-cont" .
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Company,Config Name,Config Description,Category,Sub-category,Module,Allows Context,Character Value,Date Value," +
                          "Decimal Value,Integer Value,Logical Value,User Sec Level".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-sysexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_Company ~
end_Company begin_name end_name begin_mod end_mod fi_compare_company ~
Btn_Def sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_Company end_Company begin_name ~
end_name begin_mod end_mod fi_compare_company sl_avail sl_selected fi_file ~
tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-sysexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-sysexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-sysexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetValueExternalToSysCtrl rd-sysexp 
FUNCTION fGetValueExternalToSysCtrl RETURNS CHARACTER
    ( BUFFER ipb-sys-ctrl FOR sys-ctrl, ipc-field AS CHARACTER )  FORWARD.

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

DEFINE VARIABLE begin_Company AS CHARACTER FORMAT "x(8)" 
     LABEL "From Company" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_mod AS CHARACTER FORMAT "x(5)" 
     LABEL "From Module" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_name AS CHARACTER FORMAT "x(8)" 
     LABEL "From Name" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_Company AS CHARACTER FORMAT "X(8)" 
     LABEL "To Company" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_mod AS CHARACTER FORMAT "X(5)" INITIAL "zzzzzzzzzzz" 
     LABEL "To Module" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_name AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzzzzz" 
     LABEL "To Name" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_compare_company AS CHARACTER FORMAT "X(8)" 
     LABEL "Compare with Company" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\NK1List.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 52 BY 1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.38.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 2.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL yes 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-sysexp
     begin_Company AT ROW 2.43 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Company"
     end_Company AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Company"
     begin_name AT ROW 3.38 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Name" WIDGET-ID 142
     end_name AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Name" WIDGET-ID 144
     begin_mod AT ROW 4.33 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Module" WIDGET-ID 150
     end_mod AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Module" WIDGET-ID 152
     fi_compare_company AT ROW 5.76 COL 29 COLON-ALIGNED HELP
          "Compare with Company"
     Btn_Def AT ROW 9.38 COL 42.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_avail AT ROW 9.43 COL 7 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 9.43 COL 62.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.52 COL 42.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 11.71 COL 42.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 12.91 COL 42.2 WIDGET-ID 136
     btn_down AT ROW 14.1 COL 42.2 WIDGET-ID 132
     fi_file AT ROW 17.1 COL 18.8 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     tb_OpenCSV AT ROW 17.19 COL 87.8 RIGHT-ALIGNED WIDGET-ID 34
     tbAutoClose AT ROW 19 COL 43 WIDGET-ID 64
     tb_excel AT ROW 19.1 COL 4 WIDGET-ID 32
     btn-ok AT ROW 19.95 COL 31.8 WIDGET-ID 14
     btn-cancel AT ROW 19.95 COL 51.8 WIDGET-ID 12
     "Selected Columns" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 8.71 COL 69.4 WIDGET-ID 138
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
     " Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 8.05 COL 5 WIDGET-ID 86
     "Available Columns" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 8.71 COL 13.4 WIDGET-ID 140
     RECT-6 AT ROW 8.38 COL 4 WIDGET-ID 30
     RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
     RECT-8 AT ROW 16.48 COL 4 WIDGET-ID 84
     SPACE(2.99) SKIP(3.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Export SysCtrl to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-sysexp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-sysexp:SCROLLABLE       = FALSE
       FRAME rd-sysexp:HIDDEN           = TRUE.

ASSIGN 
       begin_Company:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

ASSIGN 
       begin_mod:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

ASSIGN 
       begin_name:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

ASSIGN 
       end_Company:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

ASSIGN 
       end_mod:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

ASSIGN 
       end_name:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

ASSIGN 
       fi_compare_company:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-sysexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_excel:HIDDEN IN FRAME rd-sysexp           = TRUE
       tb_excel:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-sysexp
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME rd-sysexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-sysexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sysexp rd-sysexp
ON HELP OF FRAME rd-sysexp /* Export SysCtrl to Excel */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sysexp rd-sysexp
ON WINDOW-CLOSE OF FRAME rd-sysexp /* Export SysCtrl to Excel */
DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_Company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_Company rd-sysexp
ON LEAVE OF begin_Company IN FRAME rd-sysexp /* From Company */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mod rd-sysexp
ON LEAVE OF begin_mod IN FRAME rd-sysexp /* From Module */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_name rd-sysexp
ON LEAVE OF begin_name IN FRAME rd-sysexp /* From Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-sysexp
ON CHOOSE OF btn-cancel IN FRAME rd-sysexp /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-sysexp
ON CHOOSE OF btn-ok IN FRAME rd-sysexp /* OK */
DO:
        RUN valid-company NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-sysexp
ON CHOOSE OF Btn_Add IN FRAME rd-sysexp /* Add >> */
DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-sysexp
ON CHOOSE OF Btn_Def IN FRAME rd-sysexp /* Default */
DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-sysexp
ON CHOOSE OF btn_down IN FRAME rd-sysexp /* Move Down */
DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-sysexp
ON CHOOSE OF Btn_Remove IN FRAME rd-sysexp /* << Remove */
DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-sysexp
ON CHOOSE OF btn_Up IN FRAME rd-sysexp /* Move Up */
DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_Company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_Company rd-sysexp
ON LEAVE OF end_Company IN FRAME rd-sysexp /* To Company */
DO:
    ASSIGN {&self-name}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mod rd-sysexp
ON LEAVE OF end_mod IN FRAME rd-sysexp /* To Module */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_name rd-sysexp
ON LEAVE OF end_name IN FRAME rd-sysexp /* To Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_compare_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_compare_company rd-sysexp
ON LEAVE OF fi_compare_company IN FRAME rd-sysexp /* Compare with Company */
DO:
    RUN valid-company NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-sysexp
ON HELP OF fi_file IN FRAME rd-sysexp /* Name */
DO:
   DEF VAR ls-filename AS CHARACTER NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-sysexp
ON LEAVE OF fi_file IN FRAME rd-sysexp /* Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-sysexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-sysexp
DO:
  
        IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-sysexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-sysexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-sysexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-sysexp /* Export To Excel? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-sysexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-sysexp /* Open CSV? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-sysexp 


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
        ASSIGN
        begin_Company:SCREEN-VALUE = cocode 
        end_Company:SCREEN-VALUE   = cocode .
        fi_file:SCREEN-VALUE = "c:~\tmp~\NK1List.csv".
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_Company.
    END.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
    END.
    RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-sysexp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-sysexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-sysexp  _DEFAULT-ENABLE
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
  DISPLAY begin_Company end_Company begin_name end_name begin_mod end_mod 
          fi_compare_company sl_avail sl_selected fi_file tb_OpenCSV 
          tbAutoClose 
      WITH FRAME rd-sysexp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_Company end_Company begin_name end_name 
         begin_mod end_mod fi_compare_company Btn_Def sl_avail sl_selected 
         Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV tbAutoClose 
         btn-ok btn-cancel 
      WITH FRAME rd-sysexp.
  VIEW FRAME rd-sysexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-sysexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-sysexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cexcelheader         AS CHARACTER NO-UNDO .
    DEFINE BUFFER b-sys-ctrl FOR sys-ctrl.
    DEFINE BUFFER bf-sys-ctrl FOR sys-ctrl.

    v-excelheader = buildHeader().
    
    IF fi_compare_company NE "" THEN
    v-excelheader = v-excelheader + buildHeader().
    
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    
        IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

        FIND FIRST users NO-LOCK
                WHERE  users.user_id EQ USERID(LDBNAME(1)) 
                NO-ERROR.

        FOR EACH b-sys-ctrl NO-LOCK
            WHERE b-sys-ctrl.company GE begin_Company 
            AND b-sys-ctrl.company LE end_Company
            AND b-sys-ctrl.NAME GE begin_name
            AND b-sys-ctrl.NAME LE end_name
            AND b-sys-ctrl.module GE begin_mod
            AND b-sys-ctrl.module LE end_mod
            AND b-sys-ctrl.securityLevelUser LE users.securityLevel:
            

            v-excel-detail-lines = "".

            FOR EACH ttRptSelected:
                v-excel-detail-lines = v-excel-detail-lines + 
                    appendXLLine(fGetValueExternalToSysCtrl(BUFFER b-sys-ctrl,ttRptSelected.FieldList)).
            END.
          IF fi_compare_company NE "" AND fi_compare_company NE b-sys-ctrl.company THEN DO:
                FIND FIRST bf-sys-ctrl NO-LOCK
                    WHERE bf-sys-ctrl.company EQ fi_compare_company
                    AND bf-sys-ctrl.NAME EQ b-sys-ctrl.NAME
                    NO-ERROR.
                IF AVAILABLE bf-sys-ctrl THEN DO:
                
                    FOR EACH ttRptSelected:
                       v-excel-detail-lines = v-excel-detail-lines + 
                        appendXLLine(fGetValueExternalToSysCtrl(BUFFER bf-sys-ctrl,ttRptSelected.FieldList)).
                    END.
                 
                END. /*IF AVAILABLE bf-sys-ctrl THEN DO*/
            
          END. /*IF fi_compare_company NE "" */
          
            PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP. 

        END. /* b-sys-ctrl */
   
    
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-company rd-sysexp 
PROCEDURE valid-company :
/*------------------------------------------------------------------------------
  Purpose:    
  Parameters:  <none>
  Notes:      
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR INIT "" NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF fi_compare_company:SCREEN-VALUE NE "" THEN
    DO:
        IF NOT CAN-FIND(FIRST company WHERE company.company EQ fi_compare_company:SCREEN-VALUE) THEN
          lv-msg = "Invalid company".

        IF lv-msg NE "" THEN DO:
          MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO fi_compare_company.
          RETURN ERROR.
        END.
    END. /* IF fi_compare_company:SCREEN-VALUE NE "" */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-sysexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetValueExternalToSysCtrl rd-sysexp 
FUNCTION fGetValueExternalToSysCtrl RETURNS CHARACTER
    ( BUFFER ipb-sys-ctrl FOR sys-ctrl, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    {custom/getperd.i} 
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
    
    CASE ipc-field :
        WHEN "help-cont"  THEN DO:
            FIND FIRST hlp-head NO-LOCK 
                WHERE hlp-head.fld-name EQ ipb-sys-ctrl.name NO-ERROR.
              lc-return = IF AVAIL hlp-head THEN hlp-head.help-txt ELSE "" .
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-sys-ctrl:BUFFER-FIELD(ipc-field).
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

