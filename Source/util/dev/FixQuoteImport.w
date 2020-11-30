&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
{methods/defines/hndldefs.i } 
/*{methods/prgsecur.i}*/
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i NEW SHARED}

DEFINE TEMP-TABLE ttImportQuote
    FIELD CustPart       AS CHARACTER 
    FIELD CustNo         AS CHARACTER
    FIELD Quote          AS INTEGER 
    FIELD ItemNo         AS CHARACTER 
    FIELD Qty            AS INTEGER
    FIELD Price          AS DECIMAL
    .
    
ASSIGN 
    cocode = gcompany
    locode = gloc
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS eInstructions fiFileName bProcess bExit 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiFileName fiSelect ~
fiSelect-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bExit AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bProcess 
     LABEL "Process" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE eInstructions AS CHARACTER INITIAL "Use this function to process Quote Export files that have 'corrupted' customer part numbers.  The 'Process' button will update the Quote's quantity price field ONLY." 
     VIEW-AS EDITOR
     SIZE 90 BY 2.62 NO-UNDO.

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)":U 
     LABEL "File Name" 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE fiSelect AS CHARACTER FORMAT "X(256)":U INITIAL "Enter an exported Quote file to process." 
      VIEW-AS TEXT 
     SIZE 49 BY .62 NO-UNDO.

DEFINE VARIABLE fiSelect-2 AS CHARACTER FORMAT "X(256)":U INITIAL "(F1 to select a file)" 
      VIEW-AS TEXT 
     SIZE 19 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     eInstructions AT ROW 1.95 COL 11 NO-LABEL
     fiFileName AT ROW 5.76 COL 19 COLON-ALIGNED
     bProcess AT ROW 8.62 COL 22
     bExit AT ROW 8.62 COL 86
     fiSelect AT ROW 6.95 COL 19 COLON-ALIGNED NO-LABEL
     fiSelect-2 AT ROW 6.95 COL 80 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.6 BY 10.05
         CANCEL-BUTTON bExit.


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
         TITLE              = "Fix Quote Imports"
         HEIGHT             = 10.05
         WIDTH              = 110.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 126.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 126.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiSelect IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSelect-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix Quote Imports */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix Quote Imports */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess C-Win
ON CHOOSE OF bProcess IN FRAME DEFAULT-FRAME /* Process */
DO:

    DEF VAR cRawLine AS CHAR NO-UNDO.
    DEF VAR cTestField AS CHAR NO-UNDO.
    DEF VAR iCustPartPos AS INT NO-UNDO.
    DEF VAR iCustNoPos AS INT NO-UNDO.
    DEF VAR iQuotePos AS INT NO-UNDO.
    DEF VAR iItemNoPos AS INT NO-UNDO.
    DEF VAR iQtyPos AS INT NO-UNDO.
    DEF VAR iPricePos AS INT NO-UNDO.

    INPUT FROM VALUE(fiFileName:SCREEN-VALUE IN FRAME {&frame-name}).
    IMPORT UNFORMATTED cRawLine.
    INPUT CLOSE.
    ASSIGN 
        iCustPartPos = LOOKUP("Part #",cRawLine)
        iCustNoPos = LOOKUP("Customer",cRawLine)
        iQuotePos = LOOKUP("Quote#",cRawLine)
        iItemNoPos = LOOKUP("FG Item #",cRawLine)
        iQtyPos = LOOKUP("Quantity",cRawLine)
        iPricePos = LOOKUP("Price",cRawLine).
    
    INPUT FROM VALUE(fiFileName:SCREEN-VALUE IN FRAME {&frame-name}).
    IMPORT UNFORMATTED cRawLine.    /* This skips the header line */
    REPEAT:
        IMPORT UNFORMATTED cRawLine.
        CREATE ttImportQuote.
        ASSIGN 
            ttImportQuote.CustPart  = ENTRY(iCustPartPos,cRawLine) 
            ttImportQuote.CustNo    = ENTRY(iCustNoPos,cRawLine)
            ttImportQuote.Quote     = INTEGER(ENTRY(iQuotePos,cRawLine))
            ttImportQuote.ItemNo    = ENTRY(iItemNoPos,cRawLine)
            ttImportQuote.Qty       = INTEGER(ENTRY(iQtyPos,cRawLine))
            ttImportQuote.Price     = DECIMAL(ENTRY(iPricePos,cRawLine))
            .
    END.
    INPUT CLOSE.

    FOR EACH ttImportQuote TRANSACTION:
        /* This section will find the quoteitm if CustPart is "clean" AND there's a quantity match */
        FIND FIRST quoteitm EXCLUSIVE 
            WHERE quoteitm.company EQ cocode
            AND quoteitm.loc EQ locode
            AND quoteitm.part-no EQ ttImportQuote.CustPart
            AND quoteitm.q-no EQ ttImportQuote.Quote
            AND quoteitm.i-no EQ ttImportQuote.ItemNo
            AND quoteitm.qty EQ ttImportQuote.Qty
            NO-ERROR.
        /* This section will find the quoteitm if CustPart was scrambled during export */
        IF NOT AVAIL quoteitm THEN FIND FIRST quoteitm EXCLUSIVE 
            WHERE quoteitm.company EQ cocode
            AND quoteitm.loc EQ locode
            /* AND quoteitm.part-no EQ ttImportQuote.CustPart */
            AND quoteitm.q-no EQ ttImportQuote.Quote
            AND quoteitm.i-no EQ ttImportQuote.ItemNo
            AND quoteitm.qty EQ ttImportQuote.Qty
            NO-ERROR.
        IF AVAIL quoteitm THEN ASSIGN 
            quoteitm.price = ttImportQuote.Price.

        /* Now do the same process for quoteqty records.  There will be ttImportQuote records where we're NOT updating the quoteitm, just the quoteqty */
        /* First, find the quoteitm so we can get the line no */
        FIND FIRST quoteitm NO-LOCK  
            WHERE quoteitm.company EQ cocode
            AND quoteitm.loc EQ locode
            AND quoteitm.part-no EQ ttImportQuote.CustPart
            AND quoteitm.q-no EQ ttImportQuote.Quote
            AND quoteitm.i-no EQ ttImportQuote.ItemNo
            /* Note that we removed the match on qty field */
            NO-ERROR.
        /* This section will find the quoteitm if CustPart was scrambled during export */
        IF NOT AVAIL quoteitm THEN FIND FIRST quoteitm NO-LOCK  
            WHERE quoteitm.company EQ cocode
            AND quoteitm.loc EQ locode
            /* AND quoteitm.part-no EQ ttImportQuote.CustPart */
            AND quoteitm.q-no EQ ttImportQuote.Quote
            AND quoteitm.i-no EQ ttImportQuote.ItemNo
            NO-ERROR.
        IF AVAIL quoteitm THEN DO:
            /* This finds the quoteqty and sets the price */
            FIND FIRST quoteqty EXCLUSIVE 
                WHERE quoteqty.company EQ quoteitm.company
                AND quoteqty.loc EQ quoteitm.loc
                AND quoteqty.q-no EQ quoteitm.q-no
                AND quoteqty.line EQ quoteitm.line
                AND quoteqty.qty EQ ttImportQuote.Qty
                NO-ERROR.
            IF AVAIL quoteqty THEN ASSIGN 
                quoteqty.price = ttImportQuote.Price.
        END.
    END.
    
    MESSAGE "Process complete." VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME fiFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFileName C-Win
ON 'F1' OF fiFileName IN FRAME DEFAULT-FRAME /* File Name */
DO:
    DEF VAR cFileName AS CHAR NO-UNDO.
    DEF VAR lOkPressed AS LOG NO-UNDO.
    
    SYSTEM-DIALOG GET-FILE cFileName
        TITLE   "Choose exported quote file ..."
        FILTERS "CSV (*.csv)"   "*.csv",
        "Text Files (*.txt)"   "*.txt",
        "All Files (*.*)"   "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE lOkPressed.
        
    IF lOkPressed THEN ASSIGN 
            fiFileName:SCREEN-VALUE IN FRAME {&frame-name} = cFileName.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  RUN enable_UI.
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
  DISPLAY eInstructions fiFileName fiSelect fiSelect-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE eInstructions fiFileName bProcess bExit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

