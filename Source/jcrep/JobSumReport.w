&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\JobSumReport.w

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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.

DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoc     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cocode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE locode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE i        AS INTEGER   NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLoc).

cocode =  cCompany.
locode =  cLoc.

{jc\ttJobReport.i}

DEFINE VARIABLE LvOutputSelection AS CHARACTER     INIT "on-Screen" NO-UNDO.
DEFINE VARIABLE ghExcelProcs      AS HANDLE        NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 begin_job-no begin_job-no2 end_job-no ~
end_job-no2 btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR      C-Win             AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_job-no  AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no    AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2   AS CHARACTER FORMAT "-99":U INITIAL "99" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 8.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_job-no AT ROW 4.43 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 4.43 COL 41.2 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 4.43 COL 64.2 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 4.43 COL 76.2 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    btn-ok AT ROW 10.57 COL 31
    btn-cancel AT ROW 10.57 COL 51
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 15.19
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
        TITLE              = "Job Summary Analysis Report New"
        HEIGHT             = 11.81
        WIDTH              = 96
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 204.8
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 204.8
        RESIZE             = yes
        SCROLL-BARS        = no
        STATUS-AREA        = yes
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = yes
        THREE-D            = yes
        MESSAGE-AREA       = no
        SENSITIVE          = yes.
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
   FRAME-NAME                                                           */
ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Summary Analysis Report New */
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
ON WINDOW-CLOSE OF C-Win /* Job Summary Analysis Report New */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.                   
                
        RUN run-report. 
        STATUS DEFAULT "Processing Complete. ". 
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
      
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {methods/nowait.i}
  
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
    
APPLY "entry" TO begin_job-no.
END.
    
IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
    DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 btn-ok 
        btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
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
    {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildXlt C-Win 
PROCEDURE pBuildXlt PRIVATE :
    /*------------------------------------------------------------------------------
         Purpose:  Outputs the data from the temptables to the report
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN system\ExcelProcs.p PERSISTENT SET ghExcelProcs.        
    RUN Excel_InitializeTemplate IN ghExcelProcs ("C:\tmp\JobSummaryN.xltx", OUTPUT lError, OUTPUT cMessage).
    //RUN Excel_InitializeTemplate IN ghExcelProcs ("Template\JobSummaryN.xlt", OUTPUT lError, OUTPUT cMessage).
    
    RUN pFillData.
    
    RUN Excel_Cleanup IN ghExcelProcs.
    
    DELETE OBJECT ghExcelProcs.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFillData C-Win 
PROCEDURE pFillData :
    /*------------------------------------------------------------------------------
             Purpose:    
             Parameters:  <none>
             Notes:      
            ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iColCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRowCount AS INTEGER NO-UNDO.
            
    iRowCount = 1.
    
    FOR EACH ttJob NO-LOCK        
        BREAK BY ttJob.cJobNo
        BY ttJob.iJobNo2: 

        IF FIRST-OF(ttJob.iJobNo2) THEN
        DO:           
            //RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iRowCount), "Job Number:").
            RUN Excel_SetCellValue IN ghExcelProcs ("B" + STRING(iRowCount), STRING(TRIM(ttJob.cJobNo) + "-" + STRING(ttJob.iJobNo2,"99"))).
            //RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iRowCount), "Closing Date: ").
            RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iRowCount), (IF ttJob.dtCloseDate EQ ? THEN " " ELSE STRING(ttJob.dtCloseDate,"99/99/9999"))).
            
            iRowCount = iRowCount + 1. 
            
            //RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iRowCount), "Customer:   ").
            RUN Excel_SetCellValue IN ghExcelProcs ("B" + STRING(iRowCount), ttJob.cCustName).
            
            iRowCount = iRowCount + 2.                  
                    
            RUN pPrintFGItem (INPUT-OUTPUT iRowCount).                           
            
            RUN pPrintDepartment (INPUT-OUTPUT iRowCount).
            
            RUN pPrintMaterial (INPUT-OUTPUT iRowCount).
                        
            RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iRowCount), "Total Standard Machine Cost:"). 
            RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iRowCount),  STRING(dTotStdMachineCost)). 
            RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iRowCount),  "Total Actual Machine Cost:").
            RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iRowCount),  STRING(dTotActMachineCost)).
            iRowCount = iRowCount + 1.
            
            RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iRowCount),  "Total Standard Material Cost:").
            RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iRowCount),  STRING(dTotStdMaterialCost)). 
            RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iRowCount),  "Total Actual Material Cost:").
            RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iRowCount),  STRING(dTotActMaterialCost)).
            iRowCount = iRowCount + 1.
                
            RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iRowCount),  "Total Standard Cost:"). 
            RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iRowCount),  STRING(dTotStdCost)). 
            RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iRowCount),  "Total Actual Cost:").
            RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iRowCount),  STRING(dTotActCost)).
            iRowCount = iRowCount + 3. 
                                       
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintDepartment C-Win 
PROCEDURE pPrintDepartment :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
         
    iopiRowCount = iopiRowCount + 1.  

    FOR EACH ttDepartment NO-LOCK
        WHERE ttDepartment.cJobNo EQ ttJob.cJobNo
        AND ttDepartment.iJobNo2 EQ ttJob.iJobNo2
        AND ttDepartment.cDept NE ""
        BY ttDepartment.iSeq:
        FOR EACH ttOperation NO-LOCK 
            WHERE ttOperation.cJobNo EQ ttDepartment.cJobNo
            AND ttOperation.iJobNo2 EQ ttDepartment.iJobNo2
            AND ttOperation.cDept EQ ttDepartment.cDept
            AND ttOperation.iFormNo EQ ttDepartment.iFormNo
            AND ttOperation.iBlankNo EQ ttDepartment.iBlankNo:
            
            RUN pPrintOperation (INPUT-OUTPUT iopiRowCount, BUFFER ttOperation).
        END.                           
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iopiRowCount),  ttDepartment.cDept).                 */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("B" + STRING(iopiRowCount),  STRING(ttDepartment.dRunQty)).       */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iopiRowCount),  STRING(ttDepartment.dRunQtyVar)).    */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iopiRowCount),  STRING(ttDepartment.dSetupHrs)).     */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("E" + STRING(iopiRowCount),  STRING(ttDepartment.dSetupHrsVar)).  */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iopiRowCount),  STRING(ttDepartment.dRunHrs)).       */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("G" + STRING(iopiRowCount),  STRING(ttDepartment.dRunHrsVar)).    */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("H" + STRING(iopiRowCount),  STRING(ttDepartment.dSpeed)).        */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("I" + STRING(iopiRowCount),  STRING(ttDepartment.dSpeedVar)).     */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("J" + STRING(iopiRowCount),  STRING(ttDepartment.dCost)).         */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("K" + STRING(iopiRowCount),  STRING(ttDepartment.dCostVar)).      */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("L" + STRING(iopiRowCount),  STRING(ttDepartment.dSetupWaste)).   */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("M" + STRING(iopiRowCount),  STRING(ttDepartment.dSetupWasteVar)).*/
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("J" + STRING(iopiRowCount),  STRING(ttDepartment.dRunWaste)).     */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("K" + STRING(iopiRowCount),  STRING(ttDepartment.dRunWasteVar)).  */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("L" + STRING(iopiRowCount),  STRING(ttOperation.cDownTimeCode)).  */
        /*        RUN Excel_SetCellValue IN ghExcelProcs ("M" + STRING(iopiRowCount),  STRING(ttOperation.dDownTimeHrs)).   */
             
    END.  
    
    iopiRowCount = iopiRowCount + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintFGItem C-Win 
PROCEDURE pPrintFGItem :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    
    iopiRowCount = iopiRowCount + 1.
    
    FOR EACH ttItem NO-LOCK
        WHERE ttItem.cJobNo EQ ttJob.cJobNo
        AND ttItem.iJobNo2 EQ ttJob.iJobNo2: 
        
        RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iopiRowCount),  ttItem.cFGItem). 
        RUN Excel_SetCellValue IN ghExcelProcs ("B" + STRING(iopiRowCount),  ttItem.cFGName).
        RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iopiRowCount),  STRING(ttItem.dSellingPrice)). 
        RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iopiRowCount),  STRING(ttItem.cSellingUom)). 
        RUN Excel_SetCellValue IN ghExcelProcs ("E" + STRING(iopiRowCount),  STRING(ttItem.dJobQty)). 
        RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iopiRowCount),  STRING(ttItem.dProduced)).
        RUN Excel_SetCellValue IN ghExcelProcs ("G" + STRING(iopiRowCount),  STRING(ttItem.dOnHand)).
        iopiRowCount = iopiRowCount + 1.
        RUN Excel_InsertRowAbove IN ghExcelProcs (iopiRowCount).                                     
    END.
    
    iopiRowCount = iopiRowCount + 1.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintMaterial C-Win 
PROCEDURE pPrintMaterial :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    
    RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iopiRowCount),  "Material"). 
    RUN Excel_SetCellValue IN ghExcelProcs ("B" + STRING(iopiRowCount),  "Standard Qty").
    RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iopiRowCount),  "Std UOM").
    RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iopiRowCount),  "Actual Qty").
    RUN Excel_SetCellValue IN ghExcelProcs ("E" + STRING(iopiRowCount),  "Act UOM").
    RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iopiRowCount),  "Quantity Variance"). 
    RUN Excel_SetCellValue IN ghExcelProcs ("G" + STRING(iopiRowCount),  "Standard Cost").         
    RUN Excel_SetCellValue IN ghExcelProcs ("H" + STRING(iopiRowCount),  "Actual Cost").          
    RUN Excel_SetCellValue IN ghExcelProcs ("I" + STRING(iopiRowCount),  "Material Cost Variance").             
        
    iopiRowCount = iopiRowCount + 1.  

    FOR EACH ttMaterial NO-LOCK
        WHERE ttMaterial.cJobNo EQ ttJob.cJobNo
        AND ttMaterial.iJobNo2 EQ ttJob.iJobNo2:

        RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iopiRowCount),  STRING(ttMaterial.cMaterial)).
        RUN Excel_SetCellValue IN ghExcelProcs ("B" + STRING(iopiRowCount),  STRING(ttMaterial.dQtyStd)).
        RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iopiRowCount),  STRING(ttMaterial.cStdUom)).
        RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iopiRowCount),  STRING(ttMaterial.dQtyAct)).
        RUN Excel_SetCellValue IN ghExcelProcs ("E" + STRING(iopiRowCount),  STRING(ttMaterial.cActUom)).
        RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iopiRowCount),  STRING(ttMaterial.dQtyVar)). 
        RUN Excel_SetCellValue IN ghExcelProcs ("G" + STRING(iopiRowCount),  STRING(ttMaterial.dCostStd)).         
        RUN Excel_SetCellValue IN ghExcelProcs ("H" + STRING(iopiRowCount),  STRING(ttMaterial.dCostAct)).         
        RUN Excel_SetCellValue IN ghExcelProcs ("I" + STRING(iopiRowCount),  STRING(ttMaterial.dCostVar)).           
            
        iopiRowCount = iopiRowCount + 1.                           
    END. 
    
    iopiRowCount = iopiRowCount + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintOperation C-Win 
PROCEDURE pPrintOperation :
    /*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.          
    DEFINE PARAMETER BUFFER ipbf-ttOperation FOR ttOperation.
          
   
    RUN Excel_SetCellValue IN ghExcelProcs ("A" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.iFormNo,"99") + "|" +  string(ipbf-ttOperation.iBlankNo,"99")).
    RUN Excel_SetCellValue IN ghExcelProcs ("B" + STRING(iopiRowCount),  ipbf-ttOperation.cDept).
    RUN Excel_SetCellValue IN ghExcelProcs ("C" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.cStdAct)).
    RUN Excel_SetCellValue IN ghExcelProcs ("D" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.cMachine)).
    RUN Excel_SetCellValue IN ghExcelProcs ("E" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dRunQty)).
    RUN Excel_SetCellValue IN ghExcelProcs ("F" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dSetupHrs)). 
    RUN Excel_SetCellValue IN ghExcelProcs ("G" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dRunHrs)).       
    RUN Excel_SetCellValue IN ghExcelProcs ("H" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dSpeed)). 
    RUN Excel_SetCellValue IN ghExcelProcs ("I" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dCost)).
    RUN Excel_SetCellValue IN ghExcelProcs ("J" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dSetupWaste)).
    RUN Excel_SetCellValue IN ghExcelProcs ("K" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dRunWaste)).
    RUN Excel_SetCellValue IN ghExcelProcs ("L" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.cDownTimeCode)).
    RUN Excel_SetCellValue IN ghExcelProcs ("M" + STRING(iopiRowCount),  STRING(ipbf-ttOperation.dDownTimeHrs)).        
    iopiRowCount = iopiRowCount + 1.
    RUN Excel_InsertRowAbove IN ghExcelProcs (iopiRowCount).   
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ----------------------------------------------- jc/rep/job-sum.p 08/94 JLF */
    /* Job Summary Report                                                         */
    /* -------------------------------------------------------------------------- */        
   
    SESSION:SET-WAIT-STATE ("general").
       
    EMPTY TEMP-TABLE ttJob.
    EMPTY TEMP-TABLE ttDepartment.
    EMPTY TEMP-TABLE ttOperation.
    EMPTY TEMP-TABLE ttMaterial.
    EMPTY TEMP-TABLE ttItem.    
    
    
    DO WITH FRAME {&FRAME-NAME}:
        RUN jc/jobSumReport.p(cocode, begin_job-no:SCREEN-VALUE, STRING(begin_job-no2:SCREEN-VALUE,"99"), end_job-no:SCREEN-VALUE, STRING(begin_job-no2,"99"),
            OUTPUT table ttJob,
            OUTPUT table ttDepartment,
            OUTPUT table ttOperation,
            OUTPUT table ttMaterial,
            OUTPUT table ttItem
            ).
    END. 
    
    RUN pBuildXlt.                      
      
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

