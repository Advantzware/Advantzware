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

DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName            AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_cust-no ~
end_cust-no RS_sort-by TG_cust-no TG_cust-name TG_charge TG_description ~
TG_Amount fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no RS_sort-by ~
TG_cust-no TG_cust-name TG_charge TG_description TG_Amount fi_file ~
tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file       AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SuchargeExport.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE RS_sort-by    AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "Customer Name", 1,
    "Description", 2
    SIZE 21 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 2.86.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.19.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 2.43.

DEFINE VARIABLE tbAutoClose    AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel       AS LOGICAL INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV     AS LOGICAL INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG_Amount      AS LOGICAL INITIAL NO 
    LABEL "Amount" 
    VIEW-AS TOGGLE-BOX
    SIZE 11.2 BY .81 NO-UNDO.

DEFINE VARIABLE TG_charge      AS LOGICAL INITIAL NO 
    LABEL "Charge" 
    VIEW-AS TOGGLE-BOX
    SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE TG_cust-name   AS LOGICAL INITIAL NO 
    LABEL "Customer Name" 
    VIEW-AS TOGGLE-BOX
    SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE TG_cust-no     AS LOGICAL INITIAL NO 
    LABEL "Customer#" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.2 BY .81 NO-UNDO.

DEFINE VARIABLE TG_description AS LOGICAL INITIAL NO 
    LABEL "Description" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    begin_cust-no AT ROW 2.19 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 6
    end_cust-no AT ROW 2.19 COL 71 COLON-ALIGNED HELP
    "Enter Ending Customer Number" WIDGET-ID 16
    RS_sort-by AT ROW 4.57 COL 28 NO-LABELS WIDGET-ID 100
    TG_cust-no AT ROW 8.19 COL 11.4 WIDGET-ID 42
    TG_cust-name AT ROW 8.19 COL 27.4 WIDGET-ID 50
    TG_charge AT ROW 8.19 COL 48.6 WIDGET-ID 104
    TG_description AT ROW 8.19 COL 61.4 WIDGET-ID 106
    TG_Amount AT ROW 8.19 COL 79 WIDGET-ID 108
    fi_file AT ROW 10.91 COL 19.2 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 11 COL 85.6 RIGHT-ALIGNED WIDGET-ID 34
    tb_excel AT ROW 12.67 COL 4 WIDGET-ID 32
    tbAutoClose AT ROW 12.81 COL 31 WIDGET-ID 64
    btn-ok AT ROW 13.81 COL 31 WIDGET-ID 14
    btn-cancel AT ROW 13.81 COL 51 WIDGET-ID 12
    "Sort by:" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 3.62 COL 24 WIDGET-ID 110
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
    " Export Selection" VIEW-AS TEXT
    SIZE 16.8 BY .62 AT ROW 6.86 COL 5 WIDGET-ID 86
    RECT-6 AT ROW 7.19 COL 4 WIDGET-ID 30
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
    RECT-8 AT ROW 10.24 COL 4 WIDGET-ID 84
    SPACE(2.99) SKIP(3.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Surcharge Excel Export" WIDGET-ID 100.


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
    end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

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
ON HELP OF FRAME Dialog-Frame /* Surcharge Excel Export */
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
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Surcharge Excel Export */
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


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        APPLY "go" TO FRAME {&FRAME-NAME}.
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
fi_file:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  "c:~\tmp~\SuchargeExport.csv". 
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
    DISPLAY begin_cust-no end_cust-no RS_sort-by TG_cust-no TG_cust-name TG_charge 
        TG_description TG_Amount fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-6 RECT-7 RECT-8 begin_cust-no end_cust-no RS_sort-by TG_cust-no 
        TG_cust-name TG_charge TG_description TG_Amount fi_file tb_OpenCSV 
        tbAutoClose btn-ok btn-cancel 
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
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE v-fcust       LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].

    DEFINE VARIABLE lv-tmp-string AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-excelheader AS CHARACTER NO-UNDO.

    ASSIGN
        v-fcust[1]           = begin_cust-no
        v-fcust[2]           = end_cust-no
        v-excelheader        = ""
        v-excel-detail-lines = "".


    IF TG_cust-no = YES THEN
        v-excelheader = v-excelheader + "Customer Number,".
    IF TG_cust-name = YES THEN
        v-excelheader = v-excelheader + "Customer Name,".
    IF TG_charge = YES THEN
        v-excelheader = v-excelheader + "Charge,".
    IF TG_description = YES THEN
        v-excelheader = v-excelheader + "Description,".
    IF TG_amount = YES THEN
        v-excelheader = v-excelheader + "Amount,".

       
    SESSION:SET-WAIT-STATE ("general").

    /* {sys/inc/print1.i}                         */
    /* {sys/inc/outprint.i value(lines-per-page)} */
   
    v-excel-detail-lines = "".

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
    

        IF v-excelheader NE "" THEN
            PUT STREAM excel UNFORMATTED v-excelheader SKIP.

        IF RS_sort-by = 1 THEN 
        DO:
            FOR EACH surcharge NO-LOCK WHERE surcharge.company = cocode,
                FIRST cust NO-LOCK WHERE cust.company = surcharge.company
                AND cust.cust-no = surcharge.cust-no
                BY cust.name:
                RUN write-excel-detail-lines.

            END.
      
        END.
        ELSE 
        DO:
            FOR EACH surcharge NO-LOCK WHERE surcharge.company = cocode,                        
                FIRST cust NO-LOCK WHERE cust.company = surcharge.company
                AND cust.cust-no = surcharge.cust-no
                BY surcharge.dscr:
                RUN write-excel-detail-lines.
            END.
        END.

        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE write-excel-detail-lines Dialog-Frame 
PROCEDURE write-excel-detail-lines :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    v-excel-detail-lines = "".

    IF TG_cust-no = YES THEN
        v-excel-detail-lines = v-excel-detail-lines + '"' + surcharge.cust-no + '",'.
    IF TG_cust-name = YES THEN
        v-excel-detail-lines = v-excel-detail-lines + '"' + cust.name + '",'.
    IF TG_charge = YES THEN
        v-excel-detail-lines = v-excel-detail-lines + '"' + surcharge.charge + '",'.
    IF TG_description = YES THEN
        v-excel-detail-lines = v-excel-detail-lines + '"' + surcharge.dscr + '",'.
    IF TG_amount = YES THEN
        v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(surcharge.amt / 1000) + '",'.

    PUT STREAM excel UNFORMATTED 
        v-excel-detail-lines
        SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

