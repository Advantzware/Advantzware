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
DEFINE INPUT PARAMETER pcCustFrom  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcCustTo    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pctypeFrom  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pctypeTo    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pccatfrom   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pccatto     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER auto_find   AS CHARACTER NO-UNDO.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

DEFINE BUFFER m-oe-prmtx FOR oe-prmtx.
 
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
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_date end_date ~
begin_cust-no end_cust-no begin_type end_type begin_item-cat end_item-cat ~
begin_item end_item tb_active fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_cust-no ~
end_cust-no begin_type end_type begin_item-cat end_item-cat begin_item ~
end_item tb_active fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Effective Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item AS CHARACTER FORMAT "X(15)" 
     LABEL "From F.G. Item Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_item-cat AS CHARACTER FORMAT "X(5)" 
     LABEL "From FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Effective Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To F.G. Item Code" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_item-cat AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
     LABEL "To FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\PriceMatrixExport.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 49 BY 1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 7.33.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.14.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_active AS LOGICAL INITIAL no 
     LABEL "Only Active Entries?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL yes 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_date AT ROW 2.29 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Date" WIDGET-ID 108
     end_date AT ROW 2.29 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Date" WIDGET-ID 110
     begin_cust-no AT ROW 3.38 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-no AT ROW 3.38 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 16
     begin_type AT ROW 4.43 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Type" WIDGET-ID 104
     end_type AT ROW 4.43 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Type" WIDGET-ID 106
     begin_item-cat AT ROW 5.52 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning Type" WIDGET-ID 112
     end_item-cat AT ROW 5.52 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending Type" WIDGET-ID 114
     begin_item AT ROW 6.62 COL 27.8 COLON-ALIGNED HELP
          "Enter Beginning FG Item Number" WIDGET-ID 100
     end_item AT ROW 6.62 COL 70.8 COLON-ALIGNED HELP
          "Enter Ending FG Item Number" WIDGET-ID 102
     tb_active AT ROW 7.81 COL 39.6 WIDGET-ID 32
     fi_file AT ROW 9.57 COL 17.4 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     tb_OpenCSV AT ROW 9.67 COL 83.8 RIGHT-ALIGNED WIDGET-ID 34
     tb_excel AT ROW 11.24 COL 4 WIDGET-ID 32
     tbAutoClose AT ROW 11.33 COL 40 WIDGET-ID 64
     btn-ok AT ROW 12.24 COL 31 WIDGET-ID 14
     btn-cancel AT ROW 12.24 COL 51 WIDGET-ID 12
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
     RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
     RECT-8 AT ROW 9.1 COL 4 WIDGET-ID 84
     SPACE(3.79) SKIP(3.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Price Matrix Excel Export" WIDGET-ID 100.


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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       begin_type:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_item-cat:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       end_type:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       tb_active:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_excel:HIDDEN IN FRAME Dialog-Frame           = TRUE
       tb_excel:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME Dialog-Frame
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Price Matrix Excel Export */
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Price Matrix Excel Export */
DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no Dialog-Frame
ON LEAVE OF begin_cust-no IN FRAME Dialog-Frame /* From Customer Code */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date Dialog-Frame
ON LEAVE OF begin_date IN FRAME Dialog-Frame /* From Effective Date */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_item Dialog-Frame
ON LEAVE OF begin_item IN FRAME Dialog-Frame /* From F.G. Item Code */
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


&Scoped-define SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type Dialog-Frame
ON LEAVE OF begin_type IN FRAME Dialog-Frame /* From Customer Type */
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
ON LEAVE OF end_cust-no IN FRAME Dialog-Frame /* To Customer Code */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date Dialog-Frame
ON LEAVE OF end_date IN FRAME Dialog-Frame /* To Effective Date */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_item Dialog-Frame
ON LEAVE OF end_item IN FRAME Dialog-Frame /* To F.G. Item Code */
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


&Scoped-define SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type Dialog-Frame
ON LEAVE OF end_type IN FRAME Dialog-Frame /* To Customer Type */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file Dialog-Frame
ON LEAVE OF fi_file IN FRAME Dialog-Frame /* Name */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_active Dialog-Frame
ON VALUE-CHANGED OF tb_active IN FRAME Dialog-Frame /* Only Active Entries? */
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
        /* {custom/usrprint.i}*/

        IF auto_find NE "" THEN
            ASSIGN
                begin_cust-no:SCREEN-VALUE  = pcCustFrom     
                begin_item-cat:SCREEN-VALUE = pccatfrom       
                begin_item:SCREEN-VALUE     = pcItemFrom        
                begin_type:SCREEN-VALUE     = pctypeFrom     
                end_cust-no:SCREEN-VALUE    = pcCustTo           
                end_item-cat:SCREEN-VALUE   = pccatto           
                end_item:SCREEN-VALUE       = pcItemTo        
                end_type:SCREEN-VALUE       = pctypeTo   .

        FIND FIRST users WHERE users.user_id = USERID("nosweat")  NO-LOCK NO-ERROR.
        IF AVAILABLE users AND users.USER_program[2] <> "" THEN
            ASSIGN fi_file:SCREEN-VALUE = users.USER_program[2] +  "\PriceMatrixExport.csv" .
    
        APPLY "entry" TO begin_date.


    END.
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
  DISPLAY begin_date end_date begin_cust-no end_cust-no begin_type end_type 
          begin_item-cat end_item-cat begin_item end_item tb_active fi_file 
          tb_OpenCSV tbAutoClose 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-7 RECT-8 begin_date end_date begin_cust-no end_cust-no begin_type 
         end_type begin_item-cat end_item-cat begin_item end_item tb_active 
         fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    DEFINE BUFFER b-oe-prmtx FOR oe-prmtx.

    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ino                  AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE pricbas              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIName               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustPart            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIDesc1              AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        v-excelheader = "Eff. Date,Customer,Type,Category,Item Code,Price Basis,Qty1,Price1,Dsc1,UOM1,Qty2,Price2,Dsc2,UOM2,"+
            "Qty3,Price3,Dsc3,UOM3,Qty4,Price4,Dsc4,UOM4,Qty5,Price5,Dsc5,UOM5,Qty6,Price6,Dsc6,UOM6," + 
            "Qty7,Price7,Dsc7,UOM7,Qty8,Price8,Dsc8,UOM8,Qty9,Price9,Dsc9,UOM9,Qty10,Price10,Dsc10,UOM10," +
            "Exp Date,ShipTo,Online,Minimum Order Qty,Quote,Customer Part #,Item Name,Item Description 1,Tax Basis".

        SESSION:SET-WAIT-STATE ("general").

        IF tb_excel THEN
            OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED v-excelheader SKIP.
        FOR EACH b-oe-prmtx WHERE b-oe-prmtx.company = cocode 
            AND b-oe-prmtx.cust-no GE begin_cust-no
            AND b-oe-prmtx.cust-no LE end_cust-no 
            AND b-oe-prmtx.procat GE begin_item-cat
            AND b-oe-prmtx.procat LE end_item-cat 
            AND b-oe-prmtx.i-no GE begin_item 
            AND substring(b-oe-prmtx.i-no,01,15) LE end_item
            AND b-oe-prmtx.custype GE begin_type
            AND b-oe-prmtx.custype LE end_type 
            AND b-oe-prmtx.eff-date GE begin_date
            AND b-oe-prmtx.eff-date LE end_date 
            AND (((tb_active AND b-oe-prmtx.exp-date GE TODAY) 
            OR b-oe-prmtx.exp-date EQ ?) OR NOT tb_active)
            NO-LOCK:
            /*        FIND FIRST reftable                                         */
            /*         WHERE reftable.rec_key  EQ b-oe-prmtx.rec_key              */
            /*           AND reftable.company  EQ "oe-prmtx"                      */
            /*           AND reftable.CODE     GE STRING(begin_date:SCREEN-VALUE) */
            /*           AND reftable.CODE     LE STRING(end_date:SCREEN-VALUE)   */
            /*         NO-LOCK NO-ERROR.                                          */
            /*                                                                    */
            /*        IF AVAIL reftable THEN                                      */
            ASSIGN 
                ino       = SUBSTRING(b-oe-prmtx.i-no,01,15)
                cIName    = ''
                cCustPart = ''
                cIDesc1   = ''
                .
            FIND FIRST itemfg
                WHERE itemfg.company EQ b-oe-prmtx.company
                AND itemfg.i-no EQ ino
                NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN
                ASSIGN
                    cIName    = itemfg.i-name
                    cCustPart = itemfg.part-no
                    cIDesc1   = itemfg.part-dscr1
                    .
            ASSIGN
                pricbas              = (IF b-oe-prmtx.meth EQ YES THEN "Price" ELSE "Discount")
                /*             v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(reftable.CODE)) */
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.eff-date,"99/99/9999"))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.cust-no))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(b-oe-prmtx.custype)
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.procat))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(ino)                      
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(pricbas))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[1]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[1]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[1] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[1]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[2]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[2]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[2] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[2]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[3]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[3]    )) 
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[3] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[3]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[4]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[4]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[4] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[4]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[5]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[5]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[5] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[5]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[6]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[6]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[6] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[6]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[7]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[7]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[7] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[7]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[8]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[8]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[8] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[8]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[9]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[9]    ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[9] ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[9]      ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.qty[10]     ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.price[10]   ))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.discount[10]))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.uom[10]     )) 
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(IF b-oe-prmtx.exp-date NE ? THEN  STRING(b-oe-prmtx.exp-date,"99/99/9999") ELSE "")
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.custShipID))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.online)) 
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.minOrderQty))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.quoteID))
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cCustPart) 
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cIName) 
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cIDesc1)
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(b-oe-prmtx.taxBasis))
                .

            PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
            v-excel-detail-lines = "".
        END.

        IF tb_excel THEN 
        DO:
            OUTPUT STREAM excel CLOSE.
        END.

        /*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).*/

        SESSION:SET-WAIT-STATE ("").
    END.
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

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

