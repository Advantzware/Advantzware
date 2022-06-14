&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File:             util/ProcessFurnishBatchTester.w
  Description:      Utility to test the creation of Furnish material for 
    Henry Molded Produces 
  Input Parms:      <none>
  Output Parms:     <none>
  Author:           Brad Vigrass
  Created:          00/00/2021
------------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
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
/*{methods/defines/hndldefs.i}*/
/*{methods/prgsecur.i}*/
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cbFormula fiTransDate cbLocation fiTag ~
fiCount tgExportOnly btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS cbFormula fiTransDate cbLocation fiTag ~
fiCount tgExportOnly 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetXRef C-Win 
FUNCTION fGetXRef RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcLookup AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE cbFormula AS CHARACTER FORMAT "X(256)":U 
     LABEL "Formula" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbLocation AS CHARACTER FORMAT "X(256)":U 
     LABEL "Location" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiCount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Batches" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE fiTransDate AS DATE FORMAT "99/99/99":U 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tgExportOnly AS LOGICAL INITIAL no 
     LABEL "Export csv only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1.05 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     cbFormula AT ROW 2 COL 20 COLON-ALIGNED WIDGET-ID 48
     fiTransDate AT ROW 3.52 COL 20 COLON-ALIGNED WIDGET-ID 46
     cbLocation AT ROW 3.52 COL 46.6 COLON-ALIGNED WIDGET-ID 56
     fiTag AT ROW 5 COL 20 COLON-ALIGNED WIDGET-ID 44
     fiCount AT ROW 6.52 COL 20 COLON-ALIGNED WIDGET-ID 50
     tgExportOnly AT ROW 8.29 COL 19 WIDGET-ID 14
     btn-process AT ROW 10.52 COL 20
     btn-cancel AT ROW 10.52 COL 45
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126 BY 13.1.


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
         TITLE              = "Process Batch of Furnish"
         HEIGHT             = 10.86
         WIDTH              = 91.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 127.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 127.2
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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Process Batch of Furnish */
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
ON WINDOW-CLOSE OF C-Win /* Process Batch of Furnish */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
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


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:

        RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgExportOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgExportOnly C-Win
ON VALUE-CHANGED OF tgExportOnly IN FRAME FRAME-A /* Export csv only? */
DO:

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
   
    fiTransDate = TODAY.
    RUN enable_UI.
    DO WITH FRAME {&FRAME-NAME}:
        RUN pBuildFormulaList.
        RUN pBuildLocationList.
        fiTransDate = TODAY.
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
  DISPLAY cbFormula fiTransDate cbLocation fiTag fiCount tgExportOnly 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE cbFormula fiTransDate cbLocation fiTag fiCount tgExportOnly 
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildFormulaList C-Win 
PROCEDURE pBuildFormulaList PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}: 
    FOR EACH reftable NO-LOCK 
    WHERE reftable.company EQ g_company
      AND reftable.reftable EQ 'Xref'
      AND reftable.loc EQ "Formula":
    
        cbFormula:ADD-LAST(reftable.code).
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildLocationList C-Win
PROCEDURE pBuildLocationList PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}: 
    FOR EACH loc NO-LOCK 
    WHERE loc.company EQ g_company:
        cbLocation:ADD-LAST(loc.loc).
    END.
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFolderBrowse C-Win 
PROCEDURE pFolderBrowse :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdFolderEntry AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFolder  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDefault AS CHARACTER NO-UNDO.

    SYSTEM-DIALOG GET-DIR cFolder 
        TITLE "Select Folder for Log File"
        UPDATE lOK.
 
    IF lOK THEN iphdFolderEntry:SCREEN-VALUE = cFolder.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cSuccess AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEstimate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE cTag AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            cbFormula
            cbLocation
            fiTag
            fiTransDate
            tgExportOnly  
            fiCount
            .
    END.
    cEstimate = fGetXRef(g_company, cbFormula).
    RUN util/rjust.p (INPUT-OUTPUT cEstimate, 8).
    
    IF NOT CAN-FIND(FIRST est WHERE est.company EQ g_company AND est.est-no EQ cEstimate) THEN 
        ASSIGN 
            lError = YES
            cMessage = "Invalid formula/estimate " + cbFormula
            .
    IF fiTag EQ "" THEN
        ASSIGN 
            lError = YES
            cMessage = "Tag cannot be blank"
            .
    IF LENGTH(fiTag) GT 16 THEN 
        ASSIGN 
            lError = YES
            cMessage = "Tag cannot be larger than 16 characters"
            .
    IF fiTransDate EQ ? THEN 
        ASSIGN 
            lError = YES
            cMessage = "Date cannot be blank"
            .
    IF cbLocation EQ "" THEN 
        ASSIGN 
            lError = YES
            cMessage = "Location must be selected"
            .            
    IF fiCount LE 0 THEN  
        ASSIGN 
            lError = YES
            cMessage = "Batch count must be greater than 0"
            .
    IF NOT lError THEN DO:
        DO iCount = 1 TO fiCount:
            cTag = fiTag + "-" + STRING(iCount,"999").            
            RUN jc/ProcessFurnishBatch.p (g_company, cEstimate, cTag, fiTransDate, cbLocation, tgExportOnly, OUTPUT lError, OUTPUT cMessage).
        END.
    END.   
    IF lError THEN 
        cSuccess = " with failures".
    MESSAGE STRING(fiCount,">>9") + " Batch process(es) completed" cSuccess SKIP(1) 
        cMessage
        VIEW-AS ALERT-BOX.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetXRef C-Win 
FUNCTION fGetXRef RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcLookup AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose: Lookup a xref value  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/getXref.p (INPUT ipcCompany,
        INPUT "Formula", 
        INPUT ipcLookup, 
        OUTPUT cReturn).

    RETURN cReturn.   /* Function return value. */
        
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

