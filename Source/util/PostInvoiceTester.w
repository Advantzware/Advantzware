&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME wPostInvoiceTester
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wPostInvoiceTester 
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

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME wPostInvoiceTester

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCompany fiPostDate begin_invoice ~
end_invoice begin_cust end_cust tbPost btnGo 
&Scoped-Define DISPLAYED-OBJECTS fiCompany fiPostDate begin_invoice ~
end_invoice begin_cust end_cust tbPost 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGo 
     LABEL "Run" 
     SIZE 46 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_invoice AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Beginning Invoice #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_invoice AS INTEGER FORMAT ">>>>>>>":U INITIAL 9999999 
     LABEL "Ending Invoice #" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiCompany AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiPostDate AS DATE FORMAT "99/99/99":U 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tbPost AS LOGICAL INITIAL no 
     LABEL "Execute Post" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME wPostInvoiceTester
     fiCompany AT ROW 1.24 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 36
     fiPostDate AT ROW 2.43 COL 24 COLON-ALIGNED WIDGET-ID 38
     begin_invoice AT ROW 3.67 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 22
     end_invoice AT ROW 3.67 COL 65 COLON-ALIGNED HELP
          "Enter Ending Order Number" WIDGET-ID 24
     begin_cust AT ROW 4.86 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 28
     end_cust AT ROW 4.86 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 30
     tbPost AT ROW 6.48 COL 25 WIDGET-ID 26
     btnGo AT ROW 8.38 COL 28 WIDGET-ID 10
     SPACE(30.39) SKIP(1.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Post Invoice Tester"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wPostInvoiceTester 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX wPostInvoiceTester
   FRAME-NAME                                                           */
ASSIGN 
       FRAME wPostInvoiceTester:SCROLLABLE       = FALSE
       FRAME wPostInvoiceTester:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX wPostInvoiceTester
/* Query rebuild information for DIALOG-BOX wPostInvoiceTester
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX wPostInvoiceTester */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wPostInvoiceTester
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wPostInvoiceTester wPostInvoiceTester
ON WINDOW-CLOSE OF FRAME wPostInvoiceTester /* Post Invoice Tester */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust wPostInvoiceTester
ON LEAVE OF begin_cust IN FRAME wPostInvoiceTester /* Beginning Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_invoice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_invoice wPostInvoiceTester
ON LEAVE OF begin_invoice IN FRAME wPostInvoiceTester /* Beginning Invoice # */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGo wPostInvoiceTester
ON CHOOSE OF btnGo IN FRAME wPostInvoiceTester /* Run */
DO:
    ASSIGN begin_invoice end_invoice begin_cust end_cust 
          fiPostDate fiCompany tbPost .
    RUN pRunProcess(fiCompany, begin_invoice, end_invoice, 01/01/2019, 12/31/2022, begin_cust, end_cust, fiPostDate, tbPost).    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust wPostInvoiceTester
ON LEAVE OF end_cust IN FRAME wPostInvoiceTester /* Ending Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_invoice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_invoice wPostInvoiceTester
ON LEAVE OF end_invoice IN FRAME wPostInvoiceTester /* Ending Invoice # */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCompany wPostInvoiceTester
ON LEAVE OF fiCompany IN FRAME wPostInvoiceTester /* Company */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbPost wPostInvoiceTester
ON VALUE-CHANGED OF tbPost IN FRAME wPostInvoiceTester /* Execute Post */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wPostInvoiceTester 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wPostInvoiceTester  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wPostInvoiceTester  _DEFAULT-DISABLE
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
  HIDE FRAME wPostInvoiceTester.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wPostInvoiceTester  _DEFAULT-ENABLE
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
  DISPLAY fiCompany fiPostDate begin_invoice end_invoice begin_cust end_cust 
          tbPost 
      WITH FRAME wPostInvoiceTester.
  ENABLE fiCompany fiPostDate begin_invoice end_invoice begin_cust end_cust 
         tbPost btnGo 
      WITH FRAME wPostInvoiceTester.
  VIEW FRAME wPostInvoiceTester.
  {&OPEN-BROWSERS-IN-QUERY-wPostInvoiceTester}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess wPostInvoiceTester 
PROCEDURE pRunProcess PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:  Just process the invoices and export all related temp-tables for review
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvStart AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiInvEnd AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtStart AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipdtEnd AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustEnd AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtPostDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.

    DEFINE VARIABLE lError     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTimer     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iProcessed AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iValid     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosted    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOptions   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdPostInvoices AS HANDLE NO-UNDO.
    
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-misc FOR inv-misc.
    DEFINE BUFFER bf-cust     FOR cust.
    
    cOptions = "Export".
    IF iplPost THEN cOptions = cOptions + ",Post,ExportExceptions".
    RUN oe\PostInvoices.p PERSISTENT SET hdPostInvoices.
    
    FOR EACH inv-head NO-LOCK
        WHERE inv-head.company EQ ipcCompany
        AND inv-head.inv-no GE ipiInvStart
        AND inv-head.inv-no LE ipiInvEnd
        AND inv-head.inv-date GE ipdtStart
        AND inv-head.inv-date LE ipdtEnd
        AND inv-head.cust-no GE ipcCustStart
        AND inv-head.cust-no LE ipcCustEnd
        AND inv-head.printed
        AND inv-head.inv-no   GT 0
        AND (CAN-FIND(FIRST bf-inv-line WHERE bf-inv-line.r-no EQ inv-head.r-no)
        OR CAN-FIND(FIRST bf-inv-misc WHERE bf-inv-misc.r-no = inv-head.r-no )
        OR inv-head.multi-invoice)
        AND inv-head.stat     NE "H"
        USE-INDEX prnt,
        FIRST bf-cust NO-LOCK
        WHERE bf-cust.company EQ inv-head.company
        AND bf-cust.cust-no EQ inv-head.cust-no
        AND ((bf-cust.inv-meth EQ ? AND inv-head.multi-invoice) OR (bf-cust.inv-meth NE ? AND NOT inv-head.multi-invoice))  /*Filter multi-invoices correctly based on customer*/
        :
        iCount = iCount + 1.
    END.
    iTimer = TIME.    
    RUN PostInvoices IN hdPostInvoices (ipcCompany,
        ipiInvStart, ipiInvEnd,
        ipdtStart, ipdtEnd,
        ipcCustStart, ipcCustEnd,
        ipdtPostDate,
        cOptions,
        OUTPUT iProcessed, OUTPUT iValid, OUTPUT iPosted,
        OUTPUT lError, OUTPUT cMessage).

    MESSAGE  "Should be processed = " iCount SKIP
        "Processed = " iProcessed SKIP 
        "Valid = " iValid SKIP
        "Posted = " iPosted SKIP(2)
        "Error" lError cMessage SKIP(2) 
        "Completed in " TIME - iTimer " seconds"
        VIEW-AS ALERT-BOX.
        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

