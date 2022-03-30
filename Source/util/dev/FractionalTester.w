&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
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

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiInput1 Btn_Cancel cbDisplayUnits1 ~
fiResult1 fiInput2 cbDisplayUnits2 fiResult2 
&Scoped-Define DISPLAYED-OBJECTS fiInput1 cbDisplayUnits1 fiResult1 fiUnit1 ~
fiInput2 fiUnit2 cbDisplayUnits2 fiResult2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDisplayDec gDialog 
FUNCTION fDisplayDec RETURNS CHARACTER
  ( INPUT ipcInput AS CHAR , INPUT cbUnits AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDisplayDim gDialog 
FUNCTION fDisplayDim RETURNS CHARACTER
  ( INPUT ipcInput AS CHAR , INPUT cbUnits AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbDisplayUnits1 AS CHARACTER FORMAT "X(256)":U INITIAL "Decimal" 
     LABEL "and the NK1 is" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Decimal","16ths","32nds","64ths" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbDisplayUnits2 AS CHARACTER FORMAT "X(256)":U INITIAL "Decimal" 
     LABEL "and the NK1 is" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Decimal","16ths","32nds","64ths" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiInput1 AS DECIMAL FORMAT "->>,>>9.99<<<<":U INITIAL 0 
     LABEL "If the DB value is" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fiInput2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "If the display is" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fiResult1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "then the display is" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fiResult2 AS DECIMAL FORMAT "->>>>9.99<<<<":U INITIAL 0 
     LABEL "then the DB value is" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fiUnit1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiUnit2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiInput1 AT ROW 1.24 COL 32 COLON-ALIGNED WIDGET-ID 2
     Btn_Cancel AT ROW 1.24 COL 94
     cbDisplayUnits1 AT ROW 2.43 COL 32 COLON-ALIGNED WIDGET-ID 4
     fiResult1 AT ROW 3.62 COL 32 COLON-ALIGNED WIDGET-ID 6
     fiUnit1 AT ROW 3.62 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiInput2 AT ROW 5.76 COL 32 COLON-ALIGNED WIDGET-ID 14
     fiUnit2 AT ROW 5.76 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     cbDisplayUnits2 AT ROW 6.95 COL 32 COLON-ALIGNED WIDGET-ID 10
     fiResult2 AT ROW 8.14 COL 32 COLON-ALIGNED WIDGET-ID 12
     SPACE(55.79) SKIP(8.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fractional Display Tester"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiUnit1 IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUnit2 IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Fractional Display Tester */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiInput1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiInput1 gDialog
ON VALUE-CHANGED OF fiInput1 IN FRAME gDialog /* If the DB value is */
OR VALUE-CHANGED OF cbDisplayUnits1
DO:
    DEF VAR cDisplay AS CHAR NO-UNDO.
    DEF VAR lPrecise AS LOG NO-UNDO.
    RUN pDisplayDim (fiInput1:SCREEN-VALUE,
                     fiUnit1:SCREEN-VALUE,
                     OUTPUT cDisplay,
                     OUTPUT lPrecise).
    ASSIGN 
        fiResult1:SCREEN-VALUE = cDisplay
        fiResult1:BGCOLOR = IF NOT lPrecise THEN 30 ELSE ?  
        fiUnit1:SCREEN-VALUE = IF cbDisplayUnits1:SCREEN-VALUE NE "Decimal" THEN cbDisplayUnits1:SCREEN-VALUE ELSE "in".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiInput2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiInput2 gDialog
ON VALUE-CHANGED OF fiInput2 IN FRAME gDialog /* If the display is */
OR VALUE-CHANGED OF cbDisplayUnits2
DO:
    ASSIGN 
        fiResult2:SCREEN-VALUE = fDisplayDec(fiInput2:SCREEN-VALUE,cbDisplayUnits2:SCREEN-VALUE)
        fiUnit2:SCREEN-VALUE = IF cbDisplayUnits2:SCREEN-VALUE NE "Decimal" THEN cbDisplayUnits2:SCREEN-VALUE ELSE "in".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY fiInput1 cbDisplayUnits1 fiResult1 fiUnit1 fiInput2 fiUnit2 
          cbDisplayUnits2 fiResult2 
      WITH FRAME gDialog.
  ENABLE fiInput1 Btn_Cancel cbDisplayUnits1 fiResult1 fiInput2 cbDisplayUnits2 
         fiResult2 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayDim gDialog
PROCEDURE pDisplayDim:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcInput AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcUnit AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opcDisplay AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplPrecise AS LOG NO-UNDO.
    
    DEF VAR deInput AS DECIMAL NO-UNDO.
    DEF VAR iInput AS INT NO-UNDO.
    DEF VAR deTest1 AS DECIMAL NO-UNDO.
    DEF VAR deTest2 AS DECIMAL NO-UNDO.
    DEF VAR iMultiplier AS INT NO-UNDO.    
    
    ASSIGN 
        deInput = DECIMAL(ipcInput).
    
    CASE ipcUnit:
        WHEN "Decimal" THEN ASSIGN iMultiplier = 1.
        WHEN "16ths" THEN ASSIGN iMultiplier = 16. 
        WHEN "32nds" THEN ASSIGN iMultiplier = 32.
        WHEN "64ths" THEN ASSIGN iMultiplier = 64.
    END CASE.
    
    IF iMultiplier EQ 1 THEN ASSIGN 
        opcDisplay = STRING(DECIMAL(ipcInput),">>>>>9.99<<<")
        oplPrecise = TRUE.
    ELSE DO: 
        ASSIGN 
            iInput = TRUNCATE(deInput,0)
            deTest1 = deInput - iInput
            deTest2 = (deTest1 * iMultiplier)
            opcDisplay = STRING(iInput) + "." + STRING(TRUNCATE(deTest2,0),"99")
            oplPrecise = IF deTest2 EQ TRUNCATE(deTest2,0) THEN TRUE ELSE FALSE. 
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDisplayDec gDialog 
FUNCTION fDisplayDec RETURNS CHARACTER
  ( INPUT ipcInput AS CHAR , INPUT cbUnits AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR opcOutput AS CHAR.
    DEF VAR iInput AS INT NO-UNDO.
    DEF VAR cTest AS CHAR NO-UNDO.
    DEF VAR cTest2 AS CHAR NO-UNDO.
    DEF VAR deInput AS DECIMAL NO-UNDO.
    DEF VAR iMultiplier AS INT NO-UNDO.    
    
    ASSIGN 
        iInput = INTEGER(ENTRY(1,ipcInput,"."))
        cTest = LEFT-TRIM(ipcInput,ENTRY(1,ipcInput,"."))
        cTest = LEFT-TRIM(cTest,".")
        deInput = DECIMAL(cTest).
        
    CASE cbUnits:
        WHEN "Decimal" THEN ASSIGN iMultiplier = 1.
        WHEN "16ths" THEN ASSIGN iMultiplier = 16. 
        WHEN "32nds" THEN ASSIGN iMultiplier = 32.
        WHEN "64ths" THEN ASSIGN iMultiplier = 64.
    END CASE.

    IF iMultiplier EQ 1 THEN ASSIGN 
        opcOutput = STRING(DECIMAL(ipcInput))
        opcOutput = IF SUBSTRING(opcOutput,1,1) EQ "." THEN "0" + opcOutput ELSE opcOutput.

    RETURN opcOutput.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDisplayDim gDialog 
FUNCTION fDisplayDim RETURNS CHARACTER
  ( INPUT ipcInput AS CHAR , INPUT cbUnits AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR opcOutput AS CHAR.
    DEF VAR deInput AS DECIMAL NO-UNDO.
    DEF VAR iInput AS INT NO-UNDO.
    DEF VAR deTest1 AS DECIMAL NO-UNDO.
    DEF VAR deTest2 AS DECIMAL NO-UNDO.
    DEF VAR iMultiplier AS INT NO-UNDO.    
    ASSIGN 
        deInput = DECIMAL(ipcInput).
    
    CASE cbUnits:
        WHEN "Decimal" THEN ASSIGN iMultiplier = 1.
        WHEN "16ths" THEN ASSIGN iMultiplier = 16. 
        WHEN "32nds" THEN ASSIGN iMultiplier = 32.
        WHEN "64ths" THEN ASSIGN iMultiplier = 64.
    END CASE.
    
    IF iMultiplier EQ 1 THEN ASSIGN 
        opcOutput = STRING(DECIMAL(ipcInput))
        opcOutput = IF SUBSTRING(opcOutput,1,1) EQ "." THEN "0" + opcOutput ELSE opcOutput.
    ELSE DO: 
        ASSIGN 
            iInput = TRUNCATE(deInput,0)
            deTest1 = deInput - iInput
            deTest2 = (deTest1 * iMultiplier)
            opcOutput = STRING(iInput) + "." + RIGHT-TRIM(RIGHT-TRIM(TRIM(STRING(deTest2,">>99.99<<")),"0"),"."). 
    END.

    RETURN opcOutput.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

