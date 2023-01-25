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
DEFINE INPUT PARAMETER pcCustFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcCustTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcSalRepFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcSalRepTo   AS CHARACTER NO-UNDO.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_cust-no end_cust-no ~
begin_salrep end_salrep begin_item end_item begin_item-cat end_item-cat ~
fi_file tb_OpenCSV tb_format tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_salrep ~
end_salrep begin_item end_item begin_item-cat end_item-cat fi_file ~
tb_OpenCSV tb_format tbAutoClose 

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


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_item     AS CHARACTER FORMAT "X(15)" 
    LABEL "From FG Item" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_item-cat AS CHARACTER FORMAT "X(15)" 
    LABEL "From FG Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_salrep   AS CHARACTER FORMAT "X(15)" 
    LABEL "From Sales Rep" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_item       AS CHARACTER FORMAT "X(15)" 
    LABEL "To FG Item" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_item-cat   AS CHARACTER FORMAT "X(15)" 
    LABEL "To FG Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_salrep     AS CHARACTER FORMAT "X(15)" 
    LABEL "To Sales Rep" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\CommissionByItem.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.91.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 3.1.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel    AS LOGICAL INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_format   AS LOGICAL INITIAL NO 
    LABEL "Export in import format?" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    begin_cust-no AT ROW 2.33 COL 27.8 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 6
    end_cust-no AT ROW 2.33 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Customer Number" WIDGET-ID 16
    begin_salrep AT ROW 3.43 COL 27.8 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number" WIDGET-ID 104
    end_salrep AT ROW 3.43 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number" WIDGET-ID 106
    begin_item AT ROW 4.48 COL 27.8 COLON-ALIGNED HELP
    "Enter Beginning FG Item Number" WIDGET-ID 100
    end_item AT ROW 4.48 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending FG Item Number" WIDGET-ID 102
    begin_item-cat AT ROW 5.57 COL 27.8 COLON-ALIGNED HELP
    "Enter Beginning FG Product Category" WIDGET-ID 108
    end_item-cat AT ROW 5.57 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending FG Product Category" WIDGET-ID 110
    fi_file AT ROW 8.19 COL 19.2 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 8.29 COL 85.4 RIGHT-ALIGNED WIDGET-ID 34
    tb_format AT ROW 9.43 COL 41 WIDGET-ID 112
    tb_excel AT ROW 10.81 COL 4 WIDGET-ID 32
    tbAutoClose AT ROW 10.91 COL 41 WIDGET-ID 64
    btn-ok AT ROW 11.86 COL 31 WIDGET-ID 14
    btn-cancel AT ROW 11.86 COL 51 WIDGET-ID 12
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
    RECT-8 AT ROW 7.67 COL 4 WIDGET-ID 84
    SPACE(2.99) SKIP(3.60)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Commission Cost By Item Excel Export" WIDGET-ID 100.


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
    begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_item:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    begin_salrep:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_item:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    end_salrep:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

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
ON HELP OF FRAME Dialog-Frame /* Commission Cost By Item Excel Export */
    DO:
        DEFINE VARIABLE lw-focus      AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val    AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val      AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE cFieldsValue  AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE cFoundValue   AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE recFoundRecID AS RECID         NO-UNDO.

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
            WHEN "begin_salrep" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN system/openLookup.p (
                        INPUT  cocode, 
                        INPUT  "",  /* Lookup ID */
                        INPUT  29,  /* Subject ID */
                        INPUT  "",  /* User ID */
                        INPUT  0,   /* Param Value ID */
                        OUTPUT cFieldsValue, 
                        OUTPUT cFoundValue, 
                        OUTPUT recFoundRecID
                        ).
                    IF cFoundValue <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  cFoundValue.
                    END.
                    RETURN NO-APPLY.
                END.  /* salrep*/  
            WHEN "end_salrep" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN system/openLookup.p (
                        INPUT  cocode, 
                        INPUT  "", /* Lookup ID */
                        INPUT  29, /* Subject ID */
                        INPUT  "", /* User ID */
                        INPUT  0,  /* Param Value ID */
                        OUTPUT cFieldsValue, 
                        OUTPUT cFoundValue, 
                        OUTPUT recFoundRecID
                        ).
                    IF cFoundValue <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  cFoundValue.
                    END.
                    RETURN NO-APPLY.
                END.  /* salrep*/
            WHEN "begin_item" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, begin_cust-no, begin_item, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* item-no*/  
            WHEN "end_item" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, begin_cust-no, end_item, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* item-no*/  
            WHEN "begin_item-cat" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-fgcat.w (cocode, begin_item-cat, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* item-cat*/  
            WHEN "end_item-cat" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-fgcat.w (cocode, begin_item-cat, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* item-no*/  
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Commission Cost By Item Excel Export */
    DO:
        APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item Dialog-Frame
ON LEAVE OF begin_item IN FRAME Dialog-Frame /* From FG Item */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item-cat Dialog-Frame
ON LEAVE OF begin_item-cat IN FRAME Dialog-Frame /* From FG Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_salrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_salrep Dialog-Frame
ON LEAVE OF begin_salrep IN FRAME Dialog-Frame /* From Sales Rep */
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
        ELSE DO:
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

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


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item Dialog-Frame
ON LEAVE OF end_item IN FRAME Dialog-Frame /* To FG Item */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item-cat Dialog-Frame
ON LEAVE OF end_item-cat IN FRAME Dialog-Frame /* To FG Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_salrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_salrep Dialog-Frame
ON LEAVE OF end_salrep IN FRAME Dialog-Frame /* To Sales Rep */
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel Dialog-Frame
ON VALUE-CHANGED OF tb_excel IN FRAME Dialog-Frame /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_format Dialog-Frame
ON VALUE-CHANGED OF tb_format IN FRAME Dialog-Frame /* Export in import format? */
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
  
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {methods/nowait.i}
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}

APPLY "entry" TO begin_cust-no.
END.
fi_file:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  "c:~\tmp~\CommissionByItem.csv".
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
    DISPLAY begin_cust-no end_cust-no begin_salrep end_salrep begin_item end_item 
        begin_item-cat end_item-cat fi_file tb_OpenCSV tb_format tbAutoClose 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-7 RECT-8 begin_cust-no end_cust-no begin_salrep end_salrep 
        begin_item end_item begin_item-cat end_item-cat fi_file tb_OpenCSV 
        tb_format tbAutoClose btn-ok btn-cancel 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
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
    DEFINE BUFFER b-item-comm FOR item-comm.

    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.

    IF tb_format THEN
        v-excelheader = "Customer #,Item #,Set Sale Price,Base Cost,Set Sale Price UOM,Base Cost UOM," + 
            "Rebate %,Fixed Gross Profit %,Overhead %,Misc %,Freight %,Industrial %,Warehouse %,Commission %,Locked".
    ELSE
        v-excelheader = "Customer #,Item #,Item Name,Customer Part #,Set Sale Price,UOM,Base Cost,UOM,Locked," + 
            "Rebate %,Fixed Gross Profit %,Overhead %,Warehouse %,Misc %,Freight %,Industrial %,Commission %".

    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN
        OUTPUT STREAM excel TO VALUE(cFileName).
    PUT STREAM excel UNFORMATTED v-excelheader SKIP.
    FOR EACH b-item-comm WHERE b-item-comm.company = cocode 
        AND b-item-comm.cust-no GE begin_cust-no
        AND b-item-comm.cust-no LE end_cust-no 
        AND b-item-comm.i-no GE begin_item 
        AND b-item-comm.i-no LE end_item,
        FIRST cust WHERE cust.company = cocode
        AND cust.cust-no = b-item-comm.cust-no
        AND cust.sman GE begin_salrep
        AND cust.sman LE end_salrep,
        FIRST itemfg WHERE itemfg.company = cocode
        AND itemfg.i-no = b-item-comm.i-no
        AND itemfg.procat GE begin_item-cat
        AND itemfg.procat LE end_item-cat
        NO-LOCK:
        IF tb_format THEN 
        DO:
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.cust-no)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.i-no).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.set-sales-price)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.base-cost)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[2]).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[3]).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.zz-dec[1])).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.fixed-gross-profit)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.overhead-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.misc-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.freight-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.industrial-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.warehouse-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.comm-rate-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[4]).
        END.
        ELSE 
        DO:
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.cust-no)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.i-no).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.i-name).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.part-no).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.set-sales-price)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[2]).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.base-cost)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[3]).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-item-comm.zz-char[4]).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.zz-dec[1])).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.fixed-gross-profit)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.overhead-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.warehouse-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.misc-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.freight-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.industrial-percent)).
            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-item-comm.comm-rate-percent)).
        END. 
        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
        v-excel-detail-lines = "".
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine Dialog-Frame 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.
   
    lc-line = quoter(ipc-append) + ",".
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

