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
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.

DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoc       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cocode     AS CHARACTER NO-UNDO.
DEFINE VARIABLE locode     AS CHARACTER NO-UNDO.
DEFINE VARIABLE i          AS INTEGER   NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLoc).

cocode =  cCompany.
locode =  cLoc.

{jc\ttJobReport.i}

DEFINE VARIABLE iRowCount          AS INTEGER NO-UNDO.
DEFINE VARIABLE viWorkSheetCount   AS INTEGER          NO-UNDO.   
DEFINE VARIABLE LvOutputSelection  AS CHARACTER        INIT "on-Screen" NO-UNDO.
DEFINE VARIABLE CurActivePrinter   AS CHARACTER        NO-UNDO.
DEFINE VARIABLE AdobePrinter       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE WshNetwork         AS COMPONENT-HANDLE.
DEFINE VARIABLE chFile             AS CHARACTER        NO-UNDO.
DEFINE VARIABLE chWorkBook         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COMPONENT-HANDLE NO-UNDO.

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
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(9)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "000" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "999" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 8.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job-no AT ROW 4.43 COL 29.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.43 COL 42.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.43 COL 64.2 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.43 COL 77.2 COLON-ALIGNED HELP
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
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
        
    RUN pInitializeExcel.
    RUN pMainLoop.
    RUN pCleanup.    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCleanUp C-Win 
PROCEDURE pCleanUp PRIVATE :
/* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.

    /* Reset the Active Printer to the Original Printer. */
    IF CurActivePrinter <> '' THEN
        WshNetwork:SetDefaultPrinter(CurActivePrinter).

    
    /* Release created objects. */
    RELEASE OBJECT WshNetwork         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
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
    
         
    ASSIGN
        viWorkSheetCount = 1.         
   
    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate NO-ERROR.   
    
    ASSIGN
        chWorkSheet      = chExcelApplication:Sheets:item(viWorkSheetCount)
        chWorkSheet:name = "Job Summary Report" .
        
    iRowCount = 1.
    
    FOR EACH ttJob NO-LOCK        
        BREAK BY ttJob.cJobNo
        BY ttJob.iJobNo2: 

        IF FIRST-OF(ttJob.iJobNo2) THEN
        DO:           
            ASSIGN
                chWorkSheet:Range("A" + STRING(iRowCount)):value = "Job Number:" 
                chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(TRIM(ttJob.cJobNo) + "-" + string(ttJob.iJobNo2,"99"))
                chWorkSheet:Range("C" + STRING(iRowCount)):value = "Closing Date: "
                chWorkSheet:Range("D" + STRING(iRowCount)):value = (IF ttJob.dtCloseDate EQ ? THEN " " ELSE STRING(ttJob.dtCloseDate,"99/99/9999")).
                iRowCount = iRowCount + 1. 
            ASSIGN    
                chWorkSheet:Range("A" + STRING(iRowCount)):value = "Customer:   "
                chWorkSheet:Range("B" + STRING(iRowCount)):value = ttJob.cCustName 
                iRowCount = iRowCount + 2.                  
                    
            RUN pPrintFGItem.                           
            
            RUN pPrintOperation.

            RUN pPrintDepartment.
            
            RUN pPrintMaterial.
                        
            ASSIGN
                chWorkSheet:Range("A" + STRING(iRowCount)):value = "Total Standard Machine Cost:" 
                chWorkSheet:Range("C" + STRING(iRowCount)):value = STRING(dTotStdMachineCost) 
                chWorkSheet:Range("D" + STRING(iRowCount)):value = "Total Actual Machine Cost:"
                chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(dTotActMachineCost)
                iRowCount = iRowCount + 1.
                
            ASSIGN
                chWorkSheet:Range("A" + STRING(iRowCount)):value = "Total Standard Material Cost:"
                chWorkSheet:Range("C" + STRING(iRowCount)):value = STRING(dTotStdMaterialCost) 
                chWorkSheet:Range("D" + STRING(iRowCount)):value = "Total Actual Material Cost:"
                chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(dTotActMaterialCost)
                iRowCount = iRowCount + 1.
                
             ASSIGN
                chWorkSheet:Range("A" + STRING(iRowCount)):value = "Total Standard Cost:" 
                chWorkSheet:Range("C" + STRING(iRowCount)):value = STRING(dTotStdCost) 
                chWorkSheet:Range("D" + STRING(iRowCount)):value = "Total Actual Cost:"
                chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(dTotActCost)
                iRowCount = iRowCount + 1. 
                
                 iRowCount = iRowCount + 2.
                           
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitializeExcel C-Win 
PROCEDURE pInitializeExcel PRIVATE :
/* Capture the current active printer. */
    IF LvOutputSelection = "email" THEN
        ASSIGN 
            CurActivePrinter = SESSION:PRINTER-NAME
            AdobePrinter     = "PDFcamp Printer".
  
    RUN sys/ref/getFileFullPathName.p ("Template\JobSummaryN.xlt", OUTPUT chFile).
    IF chFile = ? THEN  
        APPLY 'close' TO THIS-PROCEDURE.

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.

    /* If Excel is running close it. */
    IF VALID-HANDLE (chExcelApplication) THEN
    DO:
        chExcelApplication:Quit()         NO-ERROR.
        RUN pCleanUp.
    END.


    /* Network connection checks. */
    CREATE "WScript.Network" WshNetwork NO-ERROR.
    IF NOT(VALID-HANDLE(WshNetwork)) THEN
    DO :
        MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.    
  
    /* Start a new session of Excel. */
    /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
    /* Check if Excel got initialized. */
    IF NOT (VALID-HANDLE (chExcelApplication)) THEN
    DO :
        MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
  
    /* Make Excel visible. */
    ASSIGN
        chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" OR 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMainLoop C-Win 
PROCEDURE pMainLoop PRIVATE :
/* Open our Excel Template. */  
    ASSIGN 
        chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
    /* Do not display Excel error messages. */
    chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

    /* Disable screen updating so it will go faster */
    chExcelApplication:ScreenUpdating = FALSE.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(1):Activate NO-ERROR.
    chWorkSheet      = chExcelApplication:Sheets:item(1).

    /*Fill in Data*/
    RUN pFillData.

    /* enable screen updating */
    chExcelApplication:ScreenUpdating = TRUE.
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
    
     ASSIGN
        chWorkSheet:Range("A" + STRING(iRowCount)):value = "Department" 
        chWorkSheet:Range("B" + STRING(iRowCount)):value = "Run Qty"
        chWorkSheet:Range("C" + STRING(iRowCount)):value = "Run Qty Var"
        chWorkSheet:Range("D" + STRING(iRowCount)):value = "Setup Hours" 
        chWorkSheet:Range("E" + STRING(iRowCount)):value = "Setup Hours Var"         
        chWorkSheet:Range("F" + STRING(iRowCount)):value = "Run Hours"           
        chWorkSheet:Range("G" + STRING(iRowCount)):value = "Run Hours Var"          
        chWorkSheet:Range("H" + STRING(iRowCount)):value = "Speed" 
        chWorkSheet:Range("I" + STRING(iRowCount)):value = "Speed Var"           
        chWorkSheet:Range("J" + STRING(iRowCount)):value = "Cost"
        chWorkSheet:Range("K" + STRING(iRowCount)):value = "Cost Var" 
        chWorkSheet:Range("L" + STRING(iRowCount)):value = "Setup Waste"
        chWorkSheet:Range("M" + STRING(iRowCount)):value = "Setup Waste Var" 
        chWorkSheet:Range("J" + STRING(iRowCount)):value = "Run Waste"
        chWorkSheet:Range("K" + STRING(iRowCount)):value = "Run Waste Var" 
        chWorkSheet:Range("L" + STRING(iRowCount)):value = "DownTime Code"
        chWorkSheet:Range("M" + STRING(iRowCount)):value = "DownTime Hrs"  
        
        iRowCount = iRowCount + 1.  

    FOR EACH ttDepartment NO-LOCK
        WHERE ttDepartment.cJobNo EQ ttJob.cJobNo
        AND ttDepartment.iJobNo2 EQ ttJob.iJobNo2
        AND ttDepartment.cDept NE ""
        BREAK BY ttDepartment.iJobNo2:
                           
        ASSIGN
            chWorkSheet:Range("A" + STRING(iRowCount)):value = ttDepartment.cDept
            chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(ttDepartment.dRunQty)
            chWorkSheet:Range("C" + STRING(iRowCount)):value = STRING(ttDepartment.dRunQtyVar)
            chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(ttDepartment.dSetupHrs) 
            chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(ttDepartment.dSetupHrsVar)        
            chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(ttDepartment.dRunHrs)           
            chWorkSheet:Range("G" + STRING(iRowCount)):value = STRING(ttDepartment.dRunHrsVar)          
            chWorkSheet:Range("H" + STRING(iRowCount)):value = STRING(ttDepartment.dSpeed) 
            chWorkSheet:Range("I" + STRING(iRowCount)):value = STRING(ttDepartment.dSpeedVar)            
            chWorkSheet:Range("J" + STRING(iRowCount)):value = STRING(ttDepartment.dCost) 
            chWorkSheet:Range("K" + STRING(iRowCount)):value = STRING(ttDepartment.dCostVar) 
            chWorkSheet:Range("L" + STRING(iRowCount)):value = STRING(ttDepartment.dSetupWaste)
            chWorkSheet:Range("M" + STRING(iRowCount)):value = STRING(ttDepartment.dSetupWasteVar) 
            chWorkSheet:Range("J" + STRING(iRowCount)):value = STRING(ttDepartment.dRunWaste)
            chWorkSheet:Range("K" + STRING(iRowCount)):value = STRING(ttDepartment.dRunWasteVar)  
            chWorkSheet:Range("L" + STRING(iRowCount)):value = STRING(ttOperation.cDownTimeCode)
            chWorkSheet:Range("M" + STRING(iRowCount)):value = STRING(ttOperation.dDownTimeHrs) 
            
            iRowCount = iRowCount + 1.         
    END.  
    
    iRowCount = iRowCount + 1.

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
    ASSIGN
        chWorkSheet:Range("A" + STRING(iRowCount)):value = "FG Item" 
        chWorkSheet:Range("B" + STRING(iRowCount)):value = "FG Item Name"
        chWorkSheet:Range("C" + STRING(iRowCount)):value = "Selling Price"
        chWorkSheet:Range("D" + STRING(iRowCount)):value = "Selling UOM"
        chWorkSheet:Range("E" + STRING(iRowCount)):value = "Job Quantity" 
        chWorkSheet:Range("F" + STRING(iRowCount)):value = "Produced"
        chWorkSheet:Range("G" + STRING(iRowCount)):value = "On Hand"
        iRowCount = iRowCount + 1.
    
    FOR EACH ttItem NO-LOCK
        WHERE ttItem.cJobNo EQ ttJob.cJobNo
        AND ttItem.iJobNo2 EQ ttJob.iJobNo2: 
      ASSIGN
        chWorkSheet:Range("A" + STRING(iRowCount)):value = ttItem.cFGItem 
        chWorkSheet:Range("B" + STRING(iRowCount)):value = ttItem.cFGName
        chWorkSheet:Range("C" + STRING(iRowCount)):value = STRING(ttItem.dSellingPrice) 
        chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(ttItem.cSellingUom) 
        chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(ttItem.dJobQty) 
        chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(ttItem.dProduced)
        chWorkSheet:Range("G" + STRING(iRowCount)):value = STRING(ttItem.dOnHand)
        iRowCount = iRowCount + 1.                                     
    END.
    
     iRowCount = iRowCount + 1.

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
   
    ASSIGN
        chWorkSheet:Range("A" + STRING(iRowCount)):value = "Material" 
        chWorkSheet:Range("B" + STRING(iRowCount)):value = "Standard Qty"
        chWorkSheet:Range("C" + STRING(iRowCount)):value = "Std UOM"
        chWorkSheet:Range("D" + STRING(iRowCount)):value = "Actual Qty"
        chWorkSheet:Range("E" + STRING(iRowCount)):value = "Act UOM"
        chWorkSheet:Range("F" + STRING(iRowCount)):value = "Quantity Variance" 
        chWorkSheet:Range("G" + STRING(iRowCount)):value = "Standard Cost"         
        chWorkSheet:Range("H" + STRING(iRowCount)):value = "Actual Cost"           
        chWorkSheet:Range("I" + STRING(iRowCount)):value = "Material Cost Variance"             
        
        iRowCount = iRowCount + 1.  

    FOR EACH ttMaterial NO-LOCK
        WHERE ttMaterial.cJobNo EQ ttJob.cJobNo
        AND ttMaterial.iJobNo2 EQ ttJob.iJobNo2:

        ASSIGN
            chWorkSheet:Range("A" + STRING(iRowCount)):value = STRING(ttMaterial.cMaterial)
            chWorkSheet:Range("B" + STRING(iRowCount)):value = STRING(ttMaterial.dQtyStd)
            chWorkSheet:Range("C" + STRING(iRowCount)):value = STRING(ttMaterial.cStdUom)
            chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(ttMaterial.dQtyAct)
            chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(ttMaterial.cActUom)
            chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(ttMaterial.dQtyVar) 
            chWorkSheet:Range("G" + STRING(iRowCount)):value = STRING(ttMaterial.dCostStd)         
            chWorkSheet:Range("H" + STRING(iRowCount)):value = STRING(ttMaterial.dCostAct)           
            chWorkSheet:Range("I" + STRING(iRowCount)):value = STRING(ttMaterial.dCostVar)             
            
            iRowCount = iRowCount + 1.                           
    END. 
    
    iRowCount = iRowCount + 1.

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
          
    ASSIGN
        chWorkSheet:Range("A" + STRING(iRowCount)):value = "F/B" 
        chWorkSheet:Range("B" + STRING(iRowCount)):value = "Department"
        chWorkSheet:Range("C" + STRING(iRowCount)):value = "Std/Act"
        chWorkSheet:Range("D" + STRING(iRowCount)):value = "Machine" 
        chWorkSheet:Range("E" + STRING(iRowCount)):value = "Run Qty"
        chWorkSheet:Range("F" + STRING(iRowCount)):value = "Setup Hours"
        chWorkSheet:Range("G" + STRING(iRowCount)):value = "Run Hours"          
        chWorkSheet:Range("H" + STRING(iRowCount)):value = "Speed" 
        chWorkSheet:Range("I" + STRING(iRowCount)):value = "Cost"
        chWorkSheet:Range("J" + STRING(iRowCount)):value = "Setup Waste"
        chWorkSheet:Range("K" + STRING(iRowCount)):value = "Run Waste" 
        chWorkSheet:Range("L" + STRING(iRowCount)):value = "Downtime Code"
        chWorkSheet:Range("M" + STRING(iRowCount)):value = "Downtime Hrs"        
        iRowCount = iRowCount + 1.          

    FOR EACH ttOperation NO-LOCK
        WHERE ttOperation.cJobNo EQ ttJob.cJobNo
        AND ttOperation.iJobNo2 EQ ttJob.iJobNo2
        BREAK BY ttOperation.iJobNo2:

        ASSIGN
            chWorkSheet:Range("A" + STRING(iRowCount)):value = STRING(ttOperation.iFormNo,"99") + "|" +  string(ttOperation.iBlankNo,"99")
            chWorkSheet:Range("B" + STRING(iRowCount)):value = ttOperation.cDept
            chWorkSheet:Range("C" + STRING(iRowCount)):value = STRING(ttOperation.cStdAct)
            chWorkSheet:Range("D" + STRING(iRowCount)):value = STRING(ttOperation.cMachine) 
            chWorkSheet:Range("E" + STRING(iRowCount)):value = STRING(ttOperation.dRunQty)
            chWorkSheet:Range("F" + STRING(iRowCount)):value = STRING(ttOperation.dSetupHrs) 
            chWorkSheet:Range("G" + STRING(iRowCount)):value = STRING(ttOperation.dRunHrs)        
            chWorkSheet:Range("H" + STRING(iRowCount)):value = STRING(ttOperation.dSpeed)  
            chWorkSheet:Range("I" + STRING(iRowCount)):value = STRING(ttOperation.dCost)
            chWorkSheet:Range("J" + STRING(iRowCount)):value = STRING(ttOperation.dSetupWaste)
            chWorkSheet:Range("K" + STRING(iRowCount)):value = STRING(ttOperation.dRunWaste) 
            chWorkSheet:Range("L" + STRING(iRowCount)):value = STRING(ttOperation.cDownTimeCode)
            chWorkSheet:Range("M" + STRING(iRowCount)):value = STRING(ttOperation.dDownTimeHrs)        
            iRowCount = iRowCount + 1.            
    END.
    
    iRowCount = iRowCount + 1.

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
        RUN jc/jobSumReport.p(cocode, begin_job-no:SCREEN-VALUE, STRING(begin_job-no2:SCREEN-VALUE,"999"), end_job-no:SCREEN-VALUE, STRING(begin_job-no2,"999"),
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

