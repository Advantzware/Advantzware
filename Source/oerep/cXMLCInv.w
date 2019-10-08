&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cXMLCustInv.w

  Description: Customer Invoice cXML Generation

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark Task: 05291402

  Created: 7.27.2014

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
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i NEW SHARED}
&SCOPED-DEFINE cXML 
{oe/rep/invoice.i NEW}

DEFINE NEW SHARED VARIABLE s-print-zero-qty AS LOGICAL NO-UNDO.

ASSIGN
  cocode = gcompany
  locode = gloc.

SESSION:SET-WAIT-STATE('').

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLInvoice &Company=cocode}
{XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXMLInvoice &Company=cocode &c=c}

ASSIGN
  lXMLOutput = NO /* prevent XML Invoices from generating */
  clXMLOutput = YES /* force cXML Invoices to generate */
  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cXMLCustomer invStart calendar-1 invEnd ~
calendar-2 btnOK btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cXMLCustomer invStart invEnd 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cXMLCustomer invStart invEnd 
&Scoped-define List-2 calendar-1 calendar-2 btnOK btnCancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOK 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON calendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON calendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05.

DEFINE VARIABLE invEnd AS DATE FORMAT "99/99/9999":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.05 NO-UNDO.

DEFINE VARIABLE invStart AS DATE FORMAT "99/99/9999":U 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.05 NO-UNDO.

DEFINE VARIABLE cXMLCustomer AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 78 BY 11.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cXMLCustomer AT ROW 1.95 COL 2 HELP
          "Select Customer" NO-LABEL WIDGET-ID 4
     invStart AT ROW 13.86 COL 21 COLON-ALIGNED HELP
          "Enter Starting Invoice Date" WIDGET-ID 8
     calendar-1 AT ROW 13.86 COL 39 WIDGET-ID 12
     invEnd AT ROW 13.86 COL 47 COLON-ALIGNED HELP
          "Enter Ending Invoice Date" WIDGET-ID 10
     calendar-2 AT ROW 13.86 COL 65 WIDGET-ID 14
     btnOK AT ROW 15.52 COL 49 WIDGET-ID 16
     btnCancel AT ROW 15.52 COL 65 WIDGET-ID 18
     "Customers" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.24 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16 WIDGET-ID 100.


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
         TITLE              = "cXML Invoice Generation"
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
ASSIGN
       btnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       btnOK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON btnOK IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON calendar-1 IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON calendar-2 IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR SELECTION-LIST cXMLCustomer IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN invEnd IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN invStart IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* cXML Invoice Generation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* cXML Invoice Generation */
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
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.

  ASSIGN {&List-1}
    fcust = cXMLCustomer
    tcust = fcust
    fdate = invStart
    tdate = invEnd
    v-term-id = STRING(YEAR(TODAY),'9999') +
                STRING(MONTH(TODAY),'99') +
                STRING(DAY(TODAY),'99') +
                STRING(TIME,'99999') +
                PROGRAM-NAME(1) +
                USERID('NoSweat')
    .
  IF cXMLCustomer EQ ? THEN DO:
    MESSAGE 'Please Select a Customer' VIEW-AS ALERT-BOX.
    APPLY 'ENTRY':U TO cXMLCustomer.
    RETURN NO-APPLY.
  END.
  FOR EACH inv-head NO-LOCK
      WHERE inv-head.company EQ cocode
        AND inv-head.cust-no EQ cXMLCustomer
        AND inv-head.inv-date GE invStart
        AND inv-head.inv-date LE invEnd
        AND (inv-head.stat EQ "" OR inv-head.stat EQ "X"):
    CREATE report.
    ASSIGN
      report.term-id = v-term-id
      report.key-01 = inv-head.cust-no
      report.key-02 = STRING(inv-head.bol-no,'9999999999')
      report.rec-id = RECID(inv-head)
      .
  END. /* each inv-head */
  DISABLE {&List-1} {&List-2} WITH FRAME {&FRAME-NAME}.
  IF CAN-FIND(FIRST report WHERE report.term-id EQ v-term-id) THEN
  RUN oe/rep/invpremx.p ('',NO).
  iCount = 0.
  FOR EACH report EXCLUSIVE-LOCK WHERE report.term-id EQ v-term-id: 
    iCount = iCount + 1.
    DELETE report.
  END. /* each report */

  IF iCount GT 0 THEN 
      cMessage = "Processed " + STRING(iCount) + " invoices ".
  ELSE
      cMessage = "No invoices available to process ".
  cMessage = cMessage + "for " + cXMLCustomer + " between " + STRING(invStart) + " and " + STRING(invEnd) + ".".

  MESSAGE cMessage
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ENABLE {&List-1} {&List-2} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME calendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calendar-1 C-Win
ON CHOOSE OF calendar-1 IN FRAME DEFAULT-FRAME
DO:
  APPLY 'HELP':U TO invStart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME calendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calendar-2 C-Win
ON CHOOSE OF calendar-2 IN FRAME DEFAULT-FRAME
DO:
  APPLY 'HELP':U TO invEnd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME invEnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL invEnd C-Win
ON HELP OF invEnd IN FRAME DEFAULT-FRAME /* To */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME invStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL invStart C-Win
ON HELP OF invStart IN FRAME DEFAULT-FRAME /* Invoice Date */
DO:
  {methods/calendar.i}
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
  ASSIGN
    invStart = TODAY
    invEnd = TODAY
    .
  RUN enable_UI.

  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ  cocode
         AND sys-ctrl.name    EQ 'cXMLInvoice' NO-ERROR.
  IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
    cXMLCustomer:LIST-ITEM-PAIRS = ?.
    FOR EACH sys-ctrl-shipto OF sys-ctrl NO-LOCK
        WHERE sys-ctrl-shipto.cust-vend EQ YES
          AND sys-ctrl-shipto.log-fld EQ YES,
        FIRST cust NO-LOCK
        WHERE cust.company EQ sys-ctrl-shipto.company
          AND cust.cust-no EQ sys-ctrl-shipto.cust-vend-no:
      cXMLCustomer:ADD-LAST(REPLACE(cust.name,',','') + ' (' + cust.cust-no + ')',cust.cust-no).
    END.
  END. /* avail sys-ctrl */
  ELSE DO:
    MESSAGE 'System Configuration Settings Do Not' SKIP
      'Exist for Generating cXML Invoices'
      VIEW-AS ALERT-BOX.
    APPLY 'CLOSE':U TO THIS-PROCEDURE.
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
  DISPLAY cXMLCustomer invStart invEnd 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cXMLCustomer invStart calendar-1 invEnd calendar-2 btnOK btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

