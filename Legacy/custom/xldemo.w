&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME BackFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS BackFrame 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR ok AS LOGICAL NO-UNDO.

DEF TEMP-TABLE TTLocation NO-UNDO
    FIELD LocationID AS INT
    FIELD TargetID AS INT
    FIELD PathName AS CHAR
    INDEX LocationID IS UNIQUE PRIMARY
        LocationID
        TargetID.
        
DEF TEMP-TABLE TTClient NO-UNDO
    FIELD ClientID AS INT
    FIELD TargetID AS INT
    FIELD PathName AS CHAR    
    INDEX ClientID IS UNIQUE PRIMARY
        ClientID
        TargetID.

DEF VAR CurrentList AS HANDLE NO-UNDO.

DEF VAR vAllowedClients   AS CHAR INIT "!*" NO-UNDO.
DEF VAR vAllowedLocations AS CHAR INIT "!*" NO-UNDO.

DEF STREAM Data.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME BackFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-31 vSpread SpreadButton OkButton ~
Btn_Cancel HelpButton 
&Scoped-Define DISPLAYED-OBJECTS vSpread 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 vSpread 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON HelpButton 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON OkButton AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON SpreadButton 
     LABEL "..." 
     SIZE 6.2 BY 1.

DEFINE VARIABLE vSpread AS CHARACTER FORMAT "X(256)":U 
     LABEL "&File" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 61.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.2 BY 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME BackFrame
     vSpread AT ROW 1.71 COL 5.8 COLON-ALIGNED
     SpreadButton AT ROW 1.76 COL 70.2
     OkButton AT ROW 3.62 COL 32.8
     Btn_Cancel AT ROW 3.62 COL 47.8
     HelpButton AT ROW 3.62 COL 62.8
     RECT-31 AT ROW 1.24 COL 1.8
     SPACE(0.79) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Debt Status Report"
         DEFAULT-BUTTON OkButton CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX BackFrame
                                                                        */
ASSIGN 
       FRAME BackFrame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN vSpread IN FRAME BackFrame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BackFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BackFrame BackFrame
ON GO OF FRAME BackFrame /* Debt Status Report */
DO:
 ASSIGN {&LIST-1}.
 RUN GoPressed IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BackFrame BackFrame
ON WINDOW-CLOSE OF FRAME BackFrame /* Debt Status Report */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME HelpButton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL HelpButton BackFrame
ON CHOOSE OF HelpButton IN FRAME BackFrame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SpreadButton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SpreadButton BackFrame
ON CHOOSE OF SpreadButton IN FRAME BackFrame /* ... */
DO:
 DEF VAR ok AS LOGICAL NO-UNDO.
 
 SYSTEM-DIALOG GET-FILE vSpread
        TITLE      "Choose Batch Report Spreadsheet"
        FILTERS    "Batch Report SpreadSheets (*.xls)"   "*.xls"
        USE-FILENAME
        SAVE-AS
        UPDATE OK.

 IF NOT ok THEN RETURN.
 
 DISPLAY vSpread WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK BackFrame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK WITH FRAME BackFrame:
  RUN enable_UI.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI BackFrame _DEFAULT-DISABLE
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
  HIDE FRAME BackFrame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI BackFrame _DEFAULT-ENABLE
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
  DISPLAY vSpread 
      WITH FRAME BackFrame.
  ENABLE RECT-31 vSpread SpreadButton OkButton Btn_Cancel HelpButton 
      WITH FRAME BackFrame.
  {&OPEN-BROWSERS-IN-QUERY-BackFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GoPressed BackFrame 
PROCEDURE GoPressed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR chExcelApplication  AS COM-HANDLE NO-UNDO.
 DEF VAR chWorkbook          AS COM-HANDLE NO-UNDO.
 DEF VAR chWorksheet         AS COM-HANDLE NO-UNDO.
 DEF VAR chPrevsheet         AS COM-HANDLE NO-UNDO.
 DEF VAR chPivotTable        AS COM-HANDLE NO-UNDO.
 
 DEF VAR vRow AS DEC NO-UNDO.
 
 DEF VAR vTarget       AS INT NO-UNDO.
 DEF VAR vClientTarget AS INT NO-UNDO.
 DEF VAR vLocation     AS INT NO-UNDO.
 DEF VAR vClient       AS INT NO-UNDO.
 DEF VAR i             AS INT NO-UNDO.
 DEF VAR vMultiplier   AS INT NO-UNDO.
 
 DEF VAR vDate AS DATE NO-UNDO.
 
 DEF VAR vRange    AS CHAR NO-UNDO.
 DEF VAR TxtFile   AS CHAR NO-UNDO.
 DEF VAR vTime     AS CHAR NO-UNDO.
 DEF VAR vOperator AS CHAR NO-UNDO.
 DEF VAR vhead     AS CHAR NO-UNDO.
 
 ASSIGN TxtFile = SESSION:TEMP-DIR + STRING(THIS-PROCEDURE) + ".txt":U.

 OUTPUT STREAM Data TO VALUE(txtFile).

 PUT STREAM Data UNFORMATTED
    
    "Customer~tSales Rep~tState~tCarrier~tItem~tPrice~tQty~t"
     SKIP.

 FOR EACH order-line NO-LOCK: 
  FIND order OF order-line NO-LOCK NO-ERROR.
  
  FIND customer of order NO-LOCK NO-ERROR.
  FIND item of order-line NO-LOCK NO-ERROR.
 
  ASSIGN FRAME {&FRAME-NAME}:TITLE = STRING(order-line.order-num).
  
  PROCESS EVENTS.
  
  EXPORT STREAM Data DELIMITER "~t":U
    sports.Customer.Name 
    sports.Customer.Sales-Rep 
    sports.Customer.State
    sports.Order.Carrier
    sports.Item.Item-Name
    sports.Order-Line.Price 
    sports.Order-Line.Qty.
  ASSIGN vRow = vRow + 1.
 END.          
          
 OUTPUT STREAM Data CLOSE.
 
 ASSIGN FRAME {&FRAME-NAME}:TITLE = "Building SpreadSheet".
            
 CREATE "Excel.Application" chExcelApplication.
    
 ASSIGN chExcelApplication:Visible = true.
 
 chExcelApplication:Workbooks:OpenText(txtFile,2,,,,,TRUE).
 
 ASSIGN chWorkbook = chExcelApplication:WorkBooks:Item(1)
        chWorkSheet = chExcelApplication:Sheets:Item(1).
 
 chWorkSheet:Rows(2):Insert.
 chWorkSheet:Rows(2):Insert.
 
 ASSIGN chWorkSheet:Name                   = "Excel Demo".
  
 ASSIGN chWorkSheet:Rows(1):Font:Bold = TRUE.

 chWorkSheet:Range("A1:G" + STRING(vRow + 3)):Select().
 
 chPivotTable = chWorkSheet:PivotTableWizard(,,"","PivotTable1").

 chPivotTable:AddFields("Data",,"Customer",TRUE).
 chPivotTable:AddFields("Data",,"Item",TRUE).
 chPivotTable:AddFields("Data",,"State",TRUE).
 chPivotTable:AddFields("Data",,"Sales Rep",TRUE).

 ASSIGN chPivotTable:PivotFields:Item("Qty"):Orientation = 4.
 
 DO i = 1 TO chPivotTable:DataFields:Count():
  ASSIGN chPivotTable:DataFields(i):Function = -4112.
 END.
 
 RELEASE OBJECT chWorkSheet.

 ASSIGN chWorkSheet = chExcelApplication:Sheets:Item(1).

 chWorkSheet:Rows(1):Insert.
 chWorkSheet:Rows(1):Insert.
 chWorkSheet:Rows(1):Insert.
 
 ASSIGN chWorksheet:Range("A1"):Value = "Excel Demo"
        chWorkSheet:Rows(1):Font:Bold = TRUE.

 IF chExcelapplication:Version BEGINS "8":U 
    THEN chWorkBook:SaveAs(vSpread,39,,,,,,,TRUE). /* 39 = Excel 95, */    
 
 ELSE chWorkBook:SaveAs(vSpread,-4143).
 
 chWorkBook:Close().
 
 RELEASE OBJECT chWorkSheet  NO-ERROR.
 RELEASE OBJECT chPivotTable NO-ERROR.
 RELEASE OBJECT chWorkBook   NO-ERROR.

 chExcelApplication:Quit().

 RELEASE OBJECT chExcelApplication NO-ERROR.
 
 OS-DELETE VALUE(txtFile) NO-ERROR.
 
 IF NOT SESSION:BATCH THEN MESSAGE "Spreadsheet" vSpread "Generated" VIEW-AS ALERT-BOX INFORMATION.
                        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


