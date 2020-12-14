&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME wReorderTester
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReorderTester 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
DEFINE VARIABLE ghSession AS HANDLE NO-UNDO.

{src/adm2/widgetprto.i}

{fgrep/ttFGReorder.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME wReorderTester
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttFGReorder

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttFGReorder.isSelected ttFGReorder.multiplier ttFGReorder.quantityToOrder ttFGReorder.quantityToOrderSuggested ttFGReorder.itemID ttFGReorder.itemName ttFGReorder.quantityReorderLevel ttFGReorder.quantityOnHand ttFGReorder.quantityOnOrder ttFGReorder.quantityAllocated   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 ttFGReorder.IsSelected ttFGReorder.multiplier ttFGReorder.quantityToOrder   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 ttFGReorder
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 ttFGReorder
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttFGReorder
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttFGReorder.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttFGReorder
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttFGReorder


/* Definitions for DIALOG-BOX wReorderTester                            */
&Scoped-define OPEN-BROWSERS-IN-QUERY-wReorderTester ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS company fi_file tb_excel btnGo BROWSE-1 ~
btnAssess btnDon 
&Scoped-Define DISPLAYED-OBJECTS company fi_file tb_excel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAssess 
     LABEL "Assess Selections" 
     SIZE 46 BY 1.14.

DEFINE BUTTON btnDon AUTO-GO 
     LABEL "Done" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnGo 
     LABEL "Show Results" 
     SIZE 46 BY 1.14.

DEFINE VARIABLE company AS CHARACTER FORMAT "X(6)":U INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\ReorderTester.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttFGReorder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 wReorderTester _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttFGReorder.isSelected COLUMN-LABEL '' WIDTH 6 VIEW-AS TOGGLE-BOX 
    ttFGReorder.multiplier COLUMN-LABEL "#Up" FORMAT ">9"
    ttFGReorder.quantityToOrder COLUMN-LABEL "Quantity To Order" FORMAT "->>>,>>>,>>9"
    ttFGReorder.quantityToOrderSuggested COLUMN-LABEL "Suggested Reorder" FORMAT "->>>,>>>,>>9"
    ttFGReorder.itemID COLUMN-LABEL "FG Item#" WIDTH 20 FORMAT "x(15)"
    ttFGReorder.itemName COLUMN-LABEL "FG Name" WIDTH 30 FORMAT "x(30)"
    ttFGReorder.quantityReorderLevel COLUMN-LABEL "Min Level" FORMAT "->>>,>>>,>>9"
    ttFGReorder.quantityOnHand COLUMN-LABEL "On-hand" FORMAT "->>>,>>>,>>9"
    ttFGReorder.quantityOnOrder COLUMN-LABEL "On-order" FORMAT "->>>,>>>,>>9"
    ttFGReorder.quantityAllocated COLUMN-LABEL "Allocated" FORMAT "->>>,>>>,>>9"
    ENABLE 
        ttFGReorder.IsSelected ttFGReorder.multiplier ttFGReorder.quantityToOrder
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 171 BY 12.14 ROW-HEIGHT-CHARS .75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME wReorderTester
     company AT ROW 2.52 COL 24 COLON-ALIGNED HELP
          "Enter Company" WIDGET-ID 36
     fi_file AT ROW 3.86 COL 64.4 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 40
     tb_excel AT ROW 4 COL 46 RIGHT-ALIGNED WIDGET-ID 42
     btnGo AT ROW 5.29 COL 47 WIDGET-ID 10
     BROWSE-1 AT ROW 6.71 COL 5 WIDGET-ID 200
     btnAssess AT ROW 19.1 COL 47 WIDGET-ID 44
     btnDon AT ROW 21.95 COL 161
     SPACE(2.99) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Reorder Tester"
         DEFAULT-BUTTON btnGo WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReorderTester 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX wReorderTester
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 btnGo wReorderTester */
ASSIGN 
       FRAME wReorderTester:SCROLLABLE       = FALSE
       FRAME wReorderTester:HIDDEN           = TRUE.

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME wReorderTester     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME wReorderTester
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME wReorderTester     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttFGReorder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX wReorderTester
/* Query rebuild information for DIALOG-BOX wReorderTester
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX wReorderTester */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReorderTester
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReorderTester wReorderTester
ON WINDOW-CLOSE OF FRAME wReorderTester /* Reorder Tester */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAssess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAssess wReorderTester
ON CHOOSE OF btnAssess IN FRAME wReorderTester /* Assess Selections */
DO:
        RUN pAssessSelection.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGo wReorderTester
ON CHOOSE OF btnGo IN FRAME wReorderTester /* Show Results */
DO:
        ASSIGN company  
            fi_file .
        RUN pBuildList (company).
        IF tb_excel THEN
            RUN pRunReport (fi_file).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company wReorderTester
ON LEAVE OF company IN FRAME wReorderTester /* Company */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file wReorderTester
ON LEAVE OF fi_file IN FRAME wReorderTester /* If Yes, File Name */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel wReorderTester
ON VALUE-CHANGED OF tb_excel IN FRAME wReorderTester /* Export To Excel? */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReorderTester 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wReorderTester  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wReorderTester  _DEFAULT-DISABLE
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
  HIDE FRAME wReorderTester.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wReorderTester  _DEFAULT-ENABLE
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
  DISPLAY company fi_file tb_excel 
      WITH FRAME wReorderTester.
  ENABLE company fi_file tb_excel btnGo BROWSE-1 btnAssess btnDon 
      WITH FRAME wReorderTester.
  VIEW FRAME wReorderTester.
  {&OPEN-BROWSERS-IN-QUERY-wReorderTester}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssessSelection wReorderTester 
PROCEDURE pAssessSelection PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotalSqin           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCountSelected       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalCyclesRequired AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCyclesRequired      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cMessageDetail       AS CHARACTER NO-UNDO.
    
    FOR EACH ttFgReorder NO-LOCK 
        WHERE ttFGReorder.isSelected:
        ASSIGN 
            iCountSelected       = iCountSelected + 1
            dTotalSqin           = dTotalSqin + ttFGReorder.blankArea * (MAXIMUM(1,ttFGReorder.multiplier))
            dCyclesRequired      = ttFGReorder.quantityToOrder / (MAXIMUM(1,ttFGReorder.multiplier))
            dTotalCyclesRequired = IF dCyclesRequired LT dTotalCyclesRequired OR dTotalCyclesRequired EQ 0 THEN dCyclesRequired ELSE dTotalCyclesRequired            
            .     
    END.
    FOR EACH ttFgReorder NO-LOCK 
        WHERE ttFGReorder.isSelected:
        ASSIGN 
            cMessageDetail       = cMessageDetail + STRING(ttFGReorder.itemID,"x(15)") + " " +  
                STRING(ttFGReorder.quantityToOrder) + " / " + STRING(MAXIMUM(1,ttFGReorder.multiplier)) + " = " + STRING(ttFGReorder.quantityToOrder / (MAXIMUM(1,ttFGReorder.multiplier))) + 
                " Remaining " + STRING(ttFGReorder.quantityToOrder - dTotalCyclesRequired * MAXIMUM(1,ttFGReorder.multiplier)) + CHR(13).
    END.
    
    MESSAGE "Selected: " iCountSelected SKIP 
        "Total Square Inches: " dTotalSqin SKIP 
        "Total Cycles Required: " dTotalCyclesRequired SKIP (2)
        "Details: "  SKIP 
        cMessageDetail
        VIEW-AS ALERT-BOX.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildList wReorderTester 
PROCEDURE pBuildList PRIVATE :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdFGReorder AS HANDLE NO-UNDO.

    RUN fgrep\fgReorder.p PERSISTENT SET hdFGReorder.

    RUN BuildReport IN hdFGReorder (ipcCompany, OUTPUT TABLE ttFGReorder).
    STATUS DEFAULT "".
    {&CLOSE-QUERY-BROWSE-1}   
    {&OPEN-QUERY-BROWSE-1}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunReport wReorderTester 
PROCEDURE pRunReport PRIVATE :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE hdOutput    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTempTable AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
  
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    ASSIGN 
        hdTempTable = TEMP-TABLE ttFgReorder:HANDLE.
    
    RUN Output_TempTableToCSV IN hdOutput (hdTempTable, ipcFile, YES, INPUT TRUE /* Auto increment File name */, OUTPUT lSuccess, OUTPUT cMessage).
  
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

