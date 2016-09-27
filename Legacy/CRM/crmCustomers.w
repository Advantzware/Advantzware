&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: zohoCRM.w

  Description: import contacts from ZOHO CRM

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 6.24.2016

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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcCompany AS CHARACTER NO-UNDO INITIAL "001".
&ENDIF

/* Local Variable Definitions ---                                       */

{XMLOutput/ttNodes.i NEW}

DEFINE TEMP-TABLE ttAccounts NO-UNDO
    FIELD tickerSymbol AS CHARACTER LABEL "CRM Ticker"   FORMAT "x(8)"
    FIELD crmName      AS CHARACTER LABEL "CRM Name"     FORMAT "x(30)"
    FIELD crmPhone     AS CHARACTER LABEL "CRM Phone"    FORMAT "x(20)"
    FIELD crmStreet    AS CHARACTER LABEL "CRM Street"   FORMAT "x(30)"
    FIELD crmStreet2   AS CHARACTER LABEL "CRM Street 2" FORMAT "x(30)"
    FIELD crmCity      AS CHARACTER LABEL "CRM City"     FORMAT "x(15)"
    FIELD crmState     AS CHARACTER LABEL "CRM State"    FORMAT "x(2)"
    FIELD crmCode      AS CHARACTER LABEL "CRM Code"     FORMAT "x(10)"
    FIELD applyAction  AS LOGICAL   LABEL "=>"
    FIELD action       AS CHARACTER LABEL "Action"       FORMAT "x(8)"  INITIAL "Add"
    FIELD custName     AS CHARACTER LABEL "Name"         FORMAT "x(30)"
    FIELD custAreaCode AS CHARACTER LABEL "Area"         FORMAT "(999)"
    FIELD custPhone    AS CHARACTER LABEL "Phone"        FORMAT "999-9999"
    FIELD custStreet   AS CHARACTER LABEL "Street"       FORMAT "x(30)"
    FIELD custStreet2  AS CHARACTER LABEL "Street 2"     FORMAT "x(30)"
    FIELD custCity     AS CHARACTER LABEL "City"         FORMAT "x(15)"
    FIELD custState    AS CHARACTER LABEL "State"        FORMAT "x(2)"
    FIELD custCode     AS CHARACTER LABEL "Code"         FORMAT "x(10)"
    FIELD custRowID    AS ROWID
    FIELD saveAction   AS CHARACTER
        INDEX ttAccounts IS PRIMARY tickerSymbol
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME crmAccounts

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAccounts

/* Definitions for BROWSE crmAccounts                                   */
&Scoped-define FIELDS-IN-QUERY-crmAccounts ttAccounts.tickerSymbol ttAccounts.crmName ttAccounts.crmPhone ttAccounts.applyAction ttAccounts.action ttAccounts.custName ttAccounts.custAreaCode ttAccounts.custPhone ttAccounts.custStreet ttAccounts.custStreet2 ttAccounts.custCity ttAccounts.custState ttAccounts.custCode   
&Scoped-define ENABLED-FIELDS-IN-QUERY-crmAccounts ttAccounts.applyAction   
&Scoped-define ENABLED-TABLES-IN-QUERY-crmAccounts ttAccounts
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-crmAccounts ttAccounts
&Scoped-define SELF-NAME crmAccounts
&Scoped-define QUERY-STRING-crmAccounts FOR EACH ttAccounts
&Scoped-define OPEN-QUERY-crmAccounts OPEN QUERY {&SELF-NAME} FOR EACH ttAccounts.
&Scoped-define TABLES-IN-QUERY-crmAccounts ttAccounts
&Scoped-define FIRST-TABLE-IN-QUERY-crmAccounts ttAccounts


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svSelect crmAccounts btnApply btnReset ~
btnSave btnCancel 
&Scoped-Define DISPLAYED-OBJECTS svSelect svStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 Dialog-Frame btnReset btnSave btnCancel 
&Scoped-define List-4 Dialog-Frame btnReset btnSave btnCancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApply 
     IMAGE-UP FILE "CRM/images/apply.jpg":U
     LABEL "&Apply" 
     SIZE 4.4 BY 1 TOOLTIP "Apply Selected Actions".

DEFINE BUTTON btnCancel AUTO-GO 
     IMAGE-UP FILE "CRM/images/cancel.jpg":U
     LABEL "&Cancel" 
     SIZE 4.4 BY 1 TOOLTIP "Cancel".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "CRM/images/reset.jpg":U
     LABEL "&Reset" 
     SIZE 4.4 BY 1 TOOLTIP "Reset".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "CRM/images/save.jpg":U
     LABEL "&Save" 
     SIZE 4.4 BY 1 TOOLTIP "Save Selected Actions".

DEFINE VARIABLE svStatus AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 49 BY .62 NO-UNDO.

DEFINE VARIABLE svSelect AS LOGICAL INITIAL no 
     LABEL "Select" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY crmAccounts FOR 
      ttAccounts SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE crmAccounts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS crmAccounts Dialog-Frame _FREEFORM
  QUERY crmAccounts DISPLAY
      ttAccounts.tickerSymbol
    ttAccounts.crmName
    ttAccounts.crmPhone
    ttAccounts.applyAction VIEW-AS TOGGLE-BOX
    ttAccounts.action
    ttAccounts.custName
    ttAccounts.custAreaCode
    ttAccounts.custPhone
    ttAccounts.custStreet
    ttAccounts.custStreet2
    ttAccounts.custCity
    ttAccounts.custState
    ttAccounts.custCode
    ENABLE
    ttAccounts.applyAction
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 225 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     svSelect AT ROW 1 COL 67 WIDGET-ID 2
     crmAccounts AT ROW 1.95 COL 2 WIDGET-ID 200
     btnApply AT ROW 7.19 COL 95 HELP
          "Apply Selected Actions" WIDGET-ID 76
     btnReset AT ROW 7.19 COL 100 HELP
          "Reset" WIDGET-ID 16
     btnSave AT ROW 7.19 COL 105 HELP
          "Save Selected Actions" WIDGET-ID 18
     btnCancel AT ROW 7.19 COL 110 HELP
          "Cancel" WIDGET-ID 4
     svStatus AT ROW 7.19 COL 2 NO-LABEL WIDGET-ID 78
     "CRM Accounts" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 26 WIDGET-ID 80
     "Customers" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.24 COL 126 WIDGET-ID 82
     SPACE(90.99) SKIP(6.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "ZOHO CRM (Customers)" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME 3 4                                                       */
/* BROWSE-TAB crmAccounts svSelect Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME Dialog-Frame
   3 4                                                                  */
ASSIGN 
       btnCancel:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "WinKitRibbon".

/* SETTINGS FOR BUTTON btnReset IN FRAME Dialog-Frame
   3 4                                                                  */
ASSIGN 
       btnReset:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "WinKitRibbon".

/* SETTINGS FOR BUTTON btnSave IN FRAME Dialog-Frame
   3 4                                                                  */
ASSIGN 
       btnSave:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "WinKitRibbon".

/* SETTINGS FOR FILL-IN svStatus IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE crmAccounts
/* Query rebuild information for BROWSE crmAccounts
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAccounts.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE crmAccounts */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* ZOHO CRM (Customers) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply Dialog-Frame
ON CHOOSE OF btnApply IN FRAME Dialog-Frame /* Apply */
DO:
    RUN pApplyCRM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset Dialog-Frame
ON CHOOSE OF btnReset IN FRAME Dialog-Frame /* Reset */
DO:
    RUN pGetCRM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
    RUN pApplyCRM.
    RUN pSave.
    RUN pGetCRM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSelect Dialog-Frame
ON VALUE-CHANGED OF svSelect IN FRAME Dialog-Frame /* Select */
DO:
  ASSIGN {&SELF-NAME}.
  FOR EACH ttAccounts:
      ttAccounts.applyAction = {&SELF-NAME}.
      IF ttAccounts.action EQ "" THEN
      ttAccounts.applyAction = NO.
  END. /* each ttAccounts */
  BROWSE crmAccounts:REFRESH() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME crmAccounts
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN pGetCRM.
  IF RETURN-VALUE NE "" THEN DO:
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
      RETURN.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

{CRM/crmProcs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY svSelect svStatus 
      WITH FRAME Dialog-Frame.
  ENABLE svSelect crmAccounts btnApply btnReset btnSave btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pApplyCRM Dialog-Frame 
PROCEDURE pApplyCRM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.

    FOR EACH ttAccounts:
        IF ttAccounts.applyAction EQ NO THEN NEXT.
        IF ttAccounts.action EQ "" THEN NEXT.
        ASSIGN
            cPhone = REPLACE(ttAccounts.crmPhone," ","")
            cPhone = REPLACE(cPhone,"+","")
            cPhone = REPLACE(cPhone,"-","")
            cPhone = REPLACE(cPhone,"(","")
            cPhone = REPLACE(cPhone,")","")
            cPhone = REPLACE(cPhone,"x","")
            cPhone = REPLACE(cPhone,".","")
            ttAccounts.custName     = ttAccounts.crmName
            ttAccounts.custAreaCode = SUBSTR(cPhone,1,3)
            ttAccounts.custPhone    = SUBSTR(cPhone,4,7)
            ttAccounts.custStreet   = ttAccounts.crmStreet
            ttAccounts.custStreet2  = ttAccounts.crmStreet2
            ttAccounts.custCity     = ttAccounts.crmCity
            ttAccounts.custState    = ttAccounts.crmState
            ttAccounts.custCode     = ttAccounts.crmCode
            ttAccounts.saveAction   = ttAccounts.action
            ttAccounts.applyAction  = NO
            ttAccounts.action       = ""
            .
        BROWSE crmAccounts:REFRESH() NO-ERROR.
    END. /* each ttAccounts */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCRM Dialog-Frame 
PROCEDURE pGetCRM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAuthToken  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cConnection AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hWebService AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hSalesSoap  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iHeight     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRows       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcAccounts  AS LONGCHAR  NO-UNDO.

    RUN pGetAuthToken  (ipcCompany, OUTPUT cAuthToken).
    IF cAuthToken EQ "" THEN
    RETURN "Authorization Token Value is Blank".
    
    RUN pGetConnection (OUTPUT cConnection).
    IF cConnection EQ "" THEN
    RETURN "Web Service Connection is Blank".
    
    svStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Connecting to ZOHO CRM ...".

    CREATE SERVER hWebService.
    hWebService:CONNECT(cConnection) NO-ERROR.
    IF NOT hWebService:CONNECTED() THEN DO:
        DELETE OBJECT hWebService.
        RETURN "Web Service Connection Failed".
    END.

    svStatus:SCREEN-VALUE = "Retreiving ZOHO CRM ...".
    
    RUN Service1Soap SET hSalesSoap ON hWebService.
    
    RUN HelpCrmZohoAcc IN hSalesSoap (
        "Accounts",
        "",
        "Accounts(ACCOUNTID,Account%20Name,Ticker%20Symbol,Phone,Billing Street,Billing Street 2,Billing City,Billing State,Billing Code)&fromIndex=1&toIndex=125&sortColumnString=Ticker%20Symbol&sortOrderString=desc",
        "getRecords",
        cAuthToken,
        OUTPUT lcAccounts
        ).

    IF INDEX(STRING(lcAccounts),"<code>4422</code>") NE 0 THEN
    RETURN "No Data Returned".

    OUTPUT TO "c:\tmp\Accounts.xml".
    PUT UNFORMATTED STRING(lcAccounts) SKIP.
    OUTPUT CLOSE.
    EMPTY TEMP-TABLE ttAccounts.
    RUN pXML ("c:\tmp\Accounts.xml", "Accounts").
    
    FOR EACH ttAccounts
        WHERE ttAccounts.tickerSymbol EQ ""
        :
        DELETE ttAccounts.
    END. /* each ttaccounts */
    
    FOR EACH ttAccounts:
        iRows = iRows + 1.
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ipcCompany
               AND cust.cust-no EQ ttAccounts.tickerSymbol
             NO-ERROR.
        IF AVAILABLE cust THEN DO:
            ASSIGN
                ttAccounts.custName     = cust.name
                ttAccounts.custAreaCode = cust.area-code
                ttAccounts.custPhone    = cust.phone
                ttAccounts.custStreet   = cust.addr[1]
                ttAccounts.custStreet2  = cust.addr[2]
                ttAccounts.custCity     = cust.city
                ttAccounts.custState    = cust.state
                ttAccounts.custCode     = cust.zip
                ttAccounts.custRowID    = ROWID(cust)
                ttAccounts.action       = "Update"
                ttAccounts.applyAction  = YES
                .
        END. /* avail cust */
    END. /* each ttaccounts */

    DO WITH FRAME {&FRAME-NAME}:
        IF iRows GT 5 THEN DO:
            ASSIGN
                iHeight = 20 + iRows * 17
                FRAME {&FRAME-NAME}:HEIGHT-PIXELS = 80 + iHeight
                btnApply:Y  = BROWSE crmAccounts:Y + iHeight + 5
                btnReset:Y  = btnApply:Y
                btnSave:Y   = btnApply:Y
                btnCancel:Y = btnApply:Y
                svStatus:Y  = btnApply:Y
                .
            BROWSE crmAccounts:HEIGHT-PIXELS = iHeight.
        END.
    END.
    {&OPEN-QUERY-crmAccounts}
    svStatus:SCREEN-VALUE = "".
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave Dialog-Frame 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ttAccounts:
        IF ttAccounts.saveAction EQ "" THEN NEXT.
        IF ttAccounts.saveAction EQ "Update" THEN
        FIND cust EXCLUSIVE-LOCK WHERE ROWID(cust) EQ ttAccounts.custRowID.
        ELSE DO:
            CREATE cust.
            cust.cust-no = ttAccounts.TickerSymbol.
        END. /* save */
        ASSIGN
            cust.name      = ttAccounts.custName
            cust.area-code = ttAccounts.custAreaCode
            cust.phone     = ttAccounts.custPhone
            cust.addr[1]   = ttAccounts.custStreet
            cust.addr[2]   = ttAccounts.custStreet2
            cust.city      = ttAccounts.custCity
            cust.state     = ttAccounts.custState
            cust.zip       = ttAccounts.custCode
            .
        RELEASE cust.
    END. /* each ttAccounts */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pXML Dialog-Frame 
PROCEDURE pXML :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  input XML file and type (accounts/contacts)
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcXMLFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValue  AS CHARACTER NO-UNDO.

    RUN XMLOutput/XMLParser.p (ipcXMLFile).
    FOR EACH ttNodes:
        ASSIGN
            ttNodes.nodeName   = TRIM(LEFT-TRIM(ttNodes.nodeName))
            ttNodes.nodeValue  = TRIM(LEFT-TRIM(ttNodes.nodeValue))
            ttNodes.parentName = TRIM(LEFT-TRIM(ttNodes.parentName))
            .
        IF ttNodes.nodeName   EQ "no"  AND
           ttNodes.parentName EQ "row" AND
           ttNodes.level      EQ 5     THEN
        CREATE ttAccounts.

        IF ttNodes.nodeName   EQ "val" AND
           ttNodes.parentName EQ "FL"  AND
           ttNodes.level      EQ 6     THEN
        CASE ttNodes.nodeValue:
            WHEN "Account Name" THEN
            ttAccounts.crmName = cValue.
            WHEN "Billing City" THEN
            ttAccounts.crmCity = cValue.
            WHEN "Billing State" THEN
            ttAccounts.crmState = cValue.
            WHEN "Billing Code" THEN
            ttAccounts.crmCode = cValue.
            WHEN "Billing Street" THEN
            ttAccounts.crmStreet = cValue.
            WHEN "Billing Street 2" THEN
            ttAccounts.crmStreet2 = cValue.
            WHEN "Phone" THEN
            ttAccounts.crmPhone = cValue.
            WHEN "Ticker Symbol" THEN
            ttAccounts.tickerSymbol = cValue.
        END CASE.

        IF ttNodes.nodeName   EQ "FL"  AND
           ttNodes.parentName EQ "row" AND
           ttNodes.level      EQ 5     THEN
        cValue = ttNodes.nodeValue.
    END. /* each ttnodes */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

