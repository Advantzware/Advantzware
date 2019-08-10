&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/setTaxStat.w

  Description: utility to set globally set taxable flags

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 8.9.2019

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCancel btnOK cStartCustNo cEndCustNo ~
lCustTaxable lShipToTaxable lOrderTaxable lInvoiceTaxable lFGItemTaxable ~
lPrepDieTaxable 
&Scoped-Define DISPLAYED-OBJECTS cStartCustNo cEndCustNo cCust lCustTaxable ~
cShipTo lShipToTaxable cOrder lOrderTaxable cInvoice lInvoiceTaxable ~
cFGItem lFGItemTaxable cPrepDie lPrepDieTaxable 

/* Custom List Definitions                                              */
/* TaxableFields,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define TaxableFields cStartCustNo cEndCustNo lCustTaxable ~
lShipToTaxable lOrderTaxable lInvoiceTaxable lFGItemTaxable lPrepDieTaxable 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 9 BY 1.91 TOOLTIP "Cancel"
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 9 BY 1.9 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE VARIABLE cCust AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cEndCustNo AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE cFGItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cInvoice AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cOrder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cPrepDie AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cShipTo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cStartCustNo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 20 BY 2.38.

DEFINE VARIABLE lCustTaxable AS LOGICAL INITIAL no 
     LABEL "Set Customers Taxable" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE lFGItemTaxable AS LOGICAL INITIAL no 
     LABEL "Set All FG items Taxable" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE lInvoiceTaxable AS LOGICAL INITIAL no 
     LABEL "Set Customer Unposted Invoices Taxable" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE lOrderTaxable AS LOGICAL INITIAL no 
     LABEL "Set Customer Orders Taxable" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE lPrepDieTaxable AS LOGICAL INITIAL no 
     LABEL "Set All Prep and Die Taxable" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE lShipToTaxable AS LOGICAL INITIAL no 
     LABEL "Set Customer Ship Tos Taxable" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCancel AT ROW 14.57 COL 70 HELP
          "Cancel" WIDGET-ID 66
     btnOK AT ROW 14.57 COL 61 HELP
          "Set Selections Taxable" WIDGET-ID 68
     cStartCustNo AT ROW 2.43 COL 28 COLON-ALIGNED HELP
          "Enter Start Customer Number" WIDGET-ID 2
     cEndCustNo AT ROW 3.62 COL 28 COLON-ALIGNED HELP
          "Enter End Customer Number" WIDGET-ID 4
     cCust AT ROW 6 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 82
     lCustTaxable AT ROW 6 COL 30 HELP
          "Toggle to Set Customers Taxable" WIDGET-ID 8
     cShipTo AT ROW 7.19 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     lShipToTaxable AT ROW 7.19 COL 30 HELP
          "Toggle to Set Customer Ship Tos Taxable" WIDGET-ID 10
     cOrder AT ROW 8.38 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     lOrderTaxable AT ROW 8.38 COL 30 HELP
          "Toggle to Set All Orders Taxable" WIDGET-ID 16
     cInvoice AT ROW 9.57 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     lInvoiceTaxable AT ROW 9.57 COL 30 HELP
          "Toggle to Set All Unposted Invoices Taxable" WIDGET-ID 18
     cFGItem AT ROW 10.76 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     lFGItemTaxable AT ROW 10.76 COL 30 HELP
          "Toggle to Set FG items Taxable" WIDGET-ID 12
     cPrepDie AT ROW 11.95 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     lPrepDieTaxable AT ROW 11.95 COL 30 HELP
          "Toggle to Set Prep and Die Taxable" WIDGET-ID 14
     RECT-18 AT ROW 14.33 COL 60 WIDGET-ID 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "Set Tabable Status Globally"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN cCust IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cEndCustNo IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN cFGItem IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cInvoice IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cOrder IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cPrepDie IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cShipTo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cStartCustNo IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lCustTaxable IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lFGItemTaxable IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lInvoiceTaxable IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lOrderTaxable IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lPrepDieTaxable IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX lShipToTaxable IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-18 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Set Tabable Status Globally */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Set Tabable Status Globally */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&TaxableFields}.
    RUN pSetTaxable.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lFGItemTaxable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lFGItemTaxable C-Win
ON VALUE-CHANGED OF lFGItemTaxable IN FRAME DEFAULT-FRAME /* Set All FG items Taxable */
DO:
  
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
  DISPLAY cStartCustNo cEndCustNo cCust lCustTaxable cShipTo lShipToTaxable 
          cOrder lOrderTaxable cInvoice lInvoiceTaxable cFGItem lFGItemTaxable 
          cPrepDie lPrepDieTaxable 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnCancel btnOK cStartCustNo cEndCustNo lCustTaxable lShipToTaxable 
         lOrderTaxable lInvoiceTaxable lFGItemTaxable lPrepDieTaxable 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetTaxable C-Win 
PROCEDURE pSetTaxable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF cust.
    DISABLE TRIGGERS FOR LOAD OF inv-head.
    DISABLE TRIGGERS FOR LOAD OF inv-line.
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    DISABLE TRIGGERS FOR LOAD OF oe-ord.
    DISABLE TRIGGERS FOR LOAD OF oe-ordl.
    DISABLE TRIGGERS FOR LOAD OF oe-ordm.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cCust:SCREEN-VALUE    = ""
            cShipTo:SCREEN-VALUE  = ""
            cOrder:SCREEN-VALUE   = ""
            cInvoice:SCREEN-VALUE = ""
            cFGItem:SCREEN-VALUE  = ""
            cPrepDie:SCREEN-VALUE = ""
            cCust:BGCOLOR         = IF lCustTaxable   THEN 14 ELSE ?
            cShipTo:BGCOLOR       = IF lShipToTaxable THEN 14 ELSE ?
            cOrder:BGCOLOR        = ?
            cInvoice:BGCOLOR      = ?
            cFGItem:BGCOLOR       = ?
            cPrepDie:BGCOLOR      = ?
            .
        DO TRANSACTION:
            IF lCustTaxable OR lShipToTaxable OR lOrderTaxable OR lInvoiceTaxable THEN
            FOR EACH cust
                WHERE cust.company EQ g_company
                  AND cust.cust-no GE cStartCustNo
                  AND cust.cust-no LE cEndCustNo
                :
                cCust:SCREEN-VALUE = cust.cust-no.
                IF lCustTaxable THEN
                cust.sort = "Y".
                FOR EACH shipto
                    WHERE shipto.company EQ cust.company
                      AND shipto.cust-no GE cust.cust-no
                    :
                    cShipTo:SCREEN-VALUE = shipto.cust-no + " - " + STRING(shipto.ship-no).
                    IF lShipToTaxable THEN DO:
                        IF shipto.tax-code EQ "" THEN
                        shipto.tax-code = cust.tax-gr.
                        IF shipto.tax-code NE "" THEN
                        shipto.tax-mandatory = YES.
                    END. /* if lshiptotaxable */
                    IF lOrderTaxable THEN DO:
                        cOrder:BGCOLOR = 14.
                        FOR EACH oe-ord EXCLUSIVE-LOCK
                            WHERE oe-ord.company EQ cust.company
                              AND oe-ord.cust-no EQ cust.cust-no
                              AND oe-ord.ship-id EQ shipto.ship-id
                            :
                            ASSIGN
                                cOrder:SCREEN-VALUE = STRING(oe-ord.ord-no)
                                oe-ord.tax-gr = shipto.tax-code
                                .
                            IF oe-ord.tax-gr NE "" THEN DO:
                                FOR EACH oe-ordl EXCLUSIVE-LOCK
                                    WHERE oe-ordl.company EQ oe-ord.company
                                      AND oe-ordl.ord-no  EQ oe-ord.ord-no
                                    :
                                    ASSIGN
                                        cOrder:SCREEN-VALUE = STRING(oe-ordl.ord-no) + " - " + STRING(oe-ordl.line)
                                        oe-ordl.tax = YES
                                        .
                                END. /* each oe-ordl */
                                FOR EACH oe-ordm EXCLUSIVE-LOCK
                                    WHERE oe-ordm.company EQ oe-ord.company
                                      AND oe-ordm.ord-no  EQ oe-ord.ord-no
                                    :
                                    ASSIGN
                                        cOrder:SCREEN-VALUE = STRING(oe-ordm.ord-no) + " - " + STRING(oe-ordm.line) + " - " + oe-ordm.charge
                                        oe-ordm.spare-char-1 = oe-ord.tax-gr
                                        oe-ordm.tax = YES
                                        .                
                                END. /* each oe-ordm */
                            END. /* if oe-ord.tax-gr */
                        END. /* each oe-ord */
                        ASSIGN
                            cOrder:BGCOLOR = 10
                            cOrder:SCREEN-VALUE = "Done."
                            .
                    END. /* if lordertaxable */
                    IF lInvoiceTaxable THEN DO:
                        cInvoice:BGCOLOR = 14.
                        FOR EACH inv-head EXCLUSIVE-LOCK
                            WHERE inv-head.company EQ cust.company
                              AND inv-head.cust-no EQ cust.cust-no
                              AND inv-head.sold-no EQ shipto.ship-id
                              AND inv-head.posted  EQ NO
                            :
                            ASSIGN
                                cInvoice:SCREEN-VALUE = STRING(inv-head.inv-no)
                                inv-head.tax-gr = shipto.tax-code
                                .
                            IF inv-head.tax-gr NE "" THEN
                            FOR EACH inv-line EXCLUSIVE-LOCK
                                WHERE inv-line.company EQ inv-head.company
                                  AND inv-line.ord-no  EQ inv-head.inv-no
                                :
                                ASSIGN
                                    cInvoice:SCREEN-VALUE = STRING(inv-line.ord-no) + " - " + STRING(inv-line.line)
                                    inv-line.tax = YES
                                    .
                            END. /* each inv-line */
                        END. /* each inv-head */
                        ASSIGN
                            cInvoice:BGCOLOR = 10
                            cInvoice:SCREEN-VALUE = "Done."
                            .
                    END. /* if linvoicetaxable */
                END. /* each shipto */
                IF lShipToTaxable THEN
                ASSIGN
                    cShipTo:BGCOLOR = 10
                    cShipTo:SCREEN-VALUE = "Done."
                    .
                ELSE
                cShipTo:SCREEN-VALUE = "".
            END. /* each cust */
            IF lCustTaxable THEN
            ASSIGN
                cCust:BGCOLOR = 10
                cCust:SCREEN-VALUE = "Done."
                .
                ELSE
                cCust:SCREEN-VALUE = "".
        END. /* do trans */
        
        IF lFGItemTaxable THEN DO WITH TRANSACTION:
            cFGItem:BGCOLOR = 14.
            FOR EACH itemfg EXCLUSIVE-LOCK
                WHERE itemfg.company EQ g_company
                  AND itemfg.stat    EQ "A"
                :
                ASSIGN
                    cFGItem:SCREEN-VALUE = itemfg.i-no
                    itemfg.taxable = YES
                    .
            END. /* each prep */
            ASSIGN
                cFGItem:BGCOLOR = 10
                cFGItem:SCREEN-VALUE = "Done."
                .
        END.

        IF lPrepDieTaxable THEN DO WITH TRANSACTION:
            cPrepDie:BGCOLOR = 14.
            FOR EACH prep EXCLUSIVE-LOCK
                WHERE prep.company EQ g_company
                :
                ASSIGN
                    cPrepDie:SCREEN-VALUE = prep.code
                    prep.taxable = YES
                    .
            END. /* each prep */
            ASSIGN
                cPrepDie:BGCOLOR = 10
                cPrepDie:SCREEN-VALUE = "Done."
                .
        END.
    END. /* do frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

