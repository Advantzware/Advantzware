&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/w-UpdARInv.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Rahul Rawat

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

{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEFINE TEMP-TABLE ttARInv NO-UNDO
    FIELD invoiceNo     AS INTEGER   
    FIELD invoiceDate   AS DATE      
    FIELD invoiceDue    AS DECIMAL   
    FIELD selected      AS LOGICAL   
    FIELD payDate       AS DATE      
    FIELD calculatedDue AS DECIMAL   
    FIELD rowidARInq    AS ROWID 
    FIELD checkDate     AS DATE
    
    INDEX selected selected 
  .
  
    DEFINE VARIABLE dCreditAmt   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDebitAmt    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBalance     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lvalid       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtCheckDate  AS DATE    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brInvoiceData

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttARInv

/* Definitions for BROWSE brInvoiceData                                 */
&Scoped-define FIELDS-IN-QUERY-brInvoiceData ttARInv.selected ttARInv.invoiceNo ttARInv.invoiceDate ttARInv.checkDate ttARInv.payDate ttARInv.due ttARInv.balance   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brInvoiceData ttARInv.selected   
&Scoped-define ENABLED-TABLES-IN-QUERY-brInvoiceData ttARInv
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brInvoiceData ttARInv
&Scoped-define SELF-NAME brInvoiceData
&Scoped-define QUERY-STRING-brInvoiceData FOR EACH ttARInv
&Scoped-define OPEN-QUERY-brInvoiceData OPEN QUERY {&SELF-NAME} FOR EACH ttARInv.
&Scoped-define TABLES-IN-QUERY-brInvoiceData ttARInv
&Scoped-define FIRST-TABLE-IN-QUERY-brInvoiceData ttARInv


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiBeginCust fiEndCust fiBeginDate fiEndDate ~
btnGo btnExit btnSelectAll btnDeselect brInvoiceData RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS fiBeginCust fiEndCust fiBeginDate ~
fiEndDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDeselect 
     LABEL "Deselect All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 12 BY 2.86.

DEFINE BUTTON btnGo 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "Go" 
     SIZE 12 BY 2.86.

DEFINE BUTTON btnSelectAll 
     LABEL "Select All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUpdate 
     LABEL "Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiBeginCust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Begining Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Begining Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndCust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 103.8 BY 3.33.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 49 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brInvoiceData FOR 
      ttARInv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brInvoiceData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brInvoiceData C-Win _FREEFORM
  QUERY brInvoiceData DISPLAY
      ttARInv.selected      FORMAT "YES/NO"         COLUMN-LABEL "Select"           WIDTH 09 VIEW-AS TOGGLE-BOX 
     ttARInv.invoiceNo     FORMAT ">>>>>>9"         COLUMN-LABEL "Invoice#"         WIDTH 11
     ttARInv.invoiceDate   FORMAT "99/99/9999"      COLUMN-LABEL "Invoice Date"     WIDTH 20
     ttARInv.checkDate     FORMAT "99/99/9999"      COLUMN-LABEL "Last Check Date"  WIDTH 20
     ttARInv.payDate       FORMAT "99/99/9999"      COLUMN-LABEL "Pay Date"         WIDTH 20
     ttARInv.invoiceDue    FORMAT "->>>,>>>,>>9.99" COLUMN-LABEL "Inv# Balance Due" WIDTH 20
     ttARInv.CalculatedDue FORMAT "->>>,>>>,>>9.99" COLUMN-LABEL "Calculated Balance Due"

     ENABLE
       ttARInv.selected
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 136 BY 15.48
         FONT 5 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiBeginCust AT ROW 1.71 COL 28 COLON-ALIGNED WIDGET-ID 2
     fiEndCust AT ROW 1.71 COL 68.4 COLON-ALIGNED WIDGET-ID 4
     fiBeginDate AT ROW 3.19 COL 28 COLON-ALIGNED WIDGET-ID 6
     fiEndDate AT ROW 3.14 COL 68.4 COLON-ALIGNED WIDGET-ID 8
     btnGo AT ROW 1.52 COL 90 WIDGET-ID 24
     btnExit AT ROW 1.52 COL 125.2 WIDGET-ID 22
     btnSelectAll AT ROW 21.38 COL 46.6 WIDGET-ID 14
     btnDeselect AT ROW 21.38 COL 62.2 WIDGET-ID 16
     btnUpdate AT ROW 21.38 COL 77.6 WIDGET-ID 18
     brInvoiceData AT ROW 5.05 COL 2 WIDGET-ID 200
     RECT-1 AT ROW 1.24 COL 2.2 WIDGET-ID 12
     RECT-2 AT ROW 20.91 COL 45 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 137 BY 22.57
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


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
         TITLE              = "Update AR Invoice Due"
         HEIGHT             = 22.57
         WIDTH              = 137
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
         RESIZE             = no
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brInvoiceData btnUpdate DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnUpdate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brInvoiceData
/* Query rebuild information for BROWSE brInvoiceData
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttARInv.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brInvoiceData */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update AR Invoice Due */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update AR Invoice Due */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeselect C-Win
ON CHOOSE OF btnDeselect IN FRAME DEFAULT-FRAME /* Deselect All */
DO:
    FOR EACH ttARInv:
        ttARInv.selected = NO. 
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGo C-Win
ON CHOOSE OF btnGo IN FRAME DEFAULT-FRAME /* Go */
DO: 
    RUN pBuildttARInv.
  
    IF TEMP-TABLE ttARInv:HAS-RECORDS THEN DO:
        {&OPEN-QUERY-{&BROWSE-NAME}}
        btnUpdate:SENSITIVE = TRUE.
    END.    
    ELSE DO:
        MESSAGE "No records found"
            VIEW-AS ALERT-BOX INFORMATION.
            btnUpdate:SENSITIVE = FALSE.
    END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAll C-Win
ON CHOOSE OF btnSelectAll IN FRAME DEFAULT-FRAME /* Select All */
DO:
    FOR EACH ttARInv:
        ttARInv.selected = YES.   
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME DEFAULT-FRAME /* Update */
DO:
    IF NOT CAN-FIND(FIRST ttARInv 
                    WHERE ttARInv.selected EQ YES) THEN DO:
        MESSAGE "No rows selected"
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.    
    END.    
    
    MESSAGE "Do you want to update the invoice due amount and pay date?"
        VIEW-AS ALERT-BOX 
        QUESTION BUTTONS YES-NO
        UPDATE lResponse AS LOGICAL.
        
    IF lResponse THEN DO:
        RUN pUpdateARInv.
        MESSAGE "Data updated successfully"
            VIEW-AS ALERT-BOX INFORMATION.
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END.    
                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brInvoiceData
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


ON 'VALUE-CHANGED':U OF ttARInv.selected IN BROWSE {&BROWSE-NAME}
DO:
    ttARInv.selected = LOGICAL(ttARInv.selected:CHECKED IN BROWSE {&BROWSE-NAME}).
END.

/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

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
  DISPLAY fiBeginCust fiEndCust fiBeginDate fiEndDate 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiBeginCust fiEndCust fiBeginDate fiEndDate btnGo btnExit btnSelectAll 
         btnDeselect brInvoiceData RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildttARInv C-Win 
PROCEDURE pBuildttARInv PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: To Calculate te actual balance due 
 Notes:
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttARInv.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.
       
    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company EQ cocode
        AND ar-inv.cust-no   GE fibegincust 
        AND ar-inv.cust-no   LE fiEndCust
        AND ar-inv.posted    EQ YES 
        AND ar-inv.terms     NE "CASH"
        AND ar-inv.inv-date  GE fiBeginDate
        AND ar-inv.inv-date  LE fiEndDate
        BY ar-inv.inv-date
        BY ar-inv.inv-no:
           
        ASSIGN
            dCreditAmt = 0
            dDebitAmt  = 0
            dBalance   = 0
            lValid     = NO
            .

        FOR EACH ar-cashl NO-LOCK
            WHERE ar-cashl.company EQ cocode 
              AND ar-cashl.posted  EQ YES
              AND ar-cashl.cust-no EQ ar-inv.cust-no
              AND ar-cashl.inv-no  EQ ar-inv.inv-no,      
            FIRST ar-cash NO-LOCK
            WHERE ar-cash.c-no EQ ar-cashl.c-no:
            lValid = YES.
            LEAVE.
        END.
        
        IF lValid THEN DO:
            IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
                dDebitAmt = ar-inv.net.
            ELSE
                dDebitAmt = ar-inv.gross.

            dBalance = dBalance + dDebitAmt.
        
            FOR EACH ar-cashl NO-LOCK
                WHERE ar-cashl.company EQ cocode
                  AND ar-cashl.posted  EQ YES
                  AND ar-cashl.cust-no EQ ar-inv.cust-no
                  AND ar-cashl.inv-no  EQ ar-inv.inv-no,      
                FIRST ar-cash NO-LOCK
                WHERE ar-cash.c-no EQ ar-cashl.c-no 
                BY ar-cash.check-date 
                BY ar-cash.c-no:
                    
                IF ar-cashl.amt-disc NE 0 THEN
                    ASSIGN
                        dCreditAmt = ar-cashl.amt-disc
                        dBalance   = dBalance - dCreditAmt
                        .

                IF ar-cashl.amt-paid NE 0 THEN DO:
      
                    IF ar-cashl.memo THEN DO:
                        IF ar-cashl.amt-paid LT 0 THEN
                            dCreditAmt = ar-cashl.amt-paid * -1.
                        ELSE
                            dDebitAmt  = ar-cashl.amt-paid.  
                            
                        IF ar-cashl.amt-paid LT 0 THEN
                            dBalance  = dBalance - dCreditAmt.
                        ELSE
                            dBalance  = dBalance + dDebitAmt.                                                 
                    END.
      
                    ELSE
                        ASSIGN 
                            dCreditAmt = ar-cashl.amt-paid
                            dBalance   = dBalance  - dCreditAmt
                            .
                END.
                dtCheckDate = ar-cash.check-date.
            END. /* ar-cashl */
        END.
        IF lValid AND ar-inv.due NE dbalance THEN DO:                   
            CREATE ttARInv.
            ASSIGN 
                ttARInv.invoiceNo     = ar-inv.inv-no
                ttARInv.invoiceDate   = ar-inv.inv-date
                ttARInv.CalculatedDue = dBalance
                ttARInv.rowidARInq    = ROWID(ar-inv)
                ttARInv.InvoiceDue    = ar-inv.due
                ttARInv.payDate       = ar-inv.pay-date
                ttARInv.checkDate     = dtCheckDate
                .   
        END.    
    END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateARInv C-Win 
PROCEDURE pUpdateARInv :
/*------------------------------------------------------------------------------
 Purpose: TO update the ar-inv due amount and pay-date
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ARInv FOR ar-inv.
    
    FOR EACH ttARInv 
        WHERE ttARInv.selected EQ YES:
        FIND FIRST bf-ARInv EXCLUSIVE-LOCK
             WHERE ROWID(bf-ARInv) EQ ttARInv.rowidARInq
             NO-ERROR.
        
        IF AVAILABLE bf-ARInv THEN
            ASSIGN 
                bf-ARInv.due      = ttARInv.CalculatedDue
                bf-ARInv.pay-date = ttARInv.checkDate
                .  
        DELETE ttARInv.                   
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

