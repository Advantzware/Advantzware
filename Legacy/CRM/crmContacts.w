&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: crmContacts.w

  Description: import contacts from ZOHO CRM

  Input Parameters: customer reckey

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
DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcRecKey AS CHARACTER NO-UNDO INIT "0108200300150995".
&ENDIF

/* Local Variable Definitions ---                                       */

{XMLOutput/ttNodes.i NEW}

DEFINE TEMP-TABLE ttAccounts NO-UNDO
    FIELD accountName  AS CHARACTER
    FIELD tickerSymbol AS CHARACTER
    .

DEFINE TEMP-TABLE ttContacts NO-UNDO
    FIELD crmFirstName   AS CHARACTER LABEL "CRM First Name" FORMAT "x(20)"
    FIELD crmLastName    AS CHARACTER LABEL "CRM Last Name"  FORMAT "x(20)"
    FIELD crmPhone       AS CHARACTER LABEL "CRM Phone"      FORMAT "x(20)"
    FIELD crmEmail       AS CHARACTER LABEL "CRM Email"      FORMAT "x(30)"
    FIELD applyAction    AS LOGICAL   LABEL "=>"
    FIELD action         AS CHARACTER LABEL "Action"         FORMAT "x(8)"  INITIAL "Add"
    FIELD phoneAttention AS CHARACTER LABEL "Attention"      FORMAT "x(35)"
    FIELD phoneCityCode  AS CHARACTER LABEL "City"           FORMAT "x(5)"
    FIELD phonePhone     AS CHARACTER LABEL "Phone"          FORMAT "x(12)"
    FIELD phoneExt       AS CHARACTER LABEL "Ext"            FORMAT "x(8)"
    FIELD phoneEmail     AS CHARACTER LABEL "Email"          FORMAT "x(30)"
    FIELD phoneRowID     AS ROWID
    FIELD saveAction     AS CHARACTER
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME crmContacts

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttContacts

/* Definitions for BROWSE crmContacts                                   */
&Scoped-define FIELDS-IN-QUERY-crmContacts ttContacts.crmFirstName ttContacts.crmLastName ttContacts.crmPhone ttContacts.crmEmail ttContacts.applyAction ttContacts.action ttContacts.phoneAttention ttContacts.phoneCityCode ttContacts.phonePhone ttContacts.phoneExt ttContacts.phoneEmail   
&Scoped-define ENABLED-FIELDS-IN-QUERY-crmContacts ttContacts.applyAction   
&Scoped-define ENABLED-TABLES-IN-QUERY-crmContacts ttContacts
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-crmContacts ttContacts
&Scoped-define SELF-NAME crmContacts
&Scoped-define QUERY-STRING-crmContacts FOR EACH ttContacts
&Scoped-define OPEN-QUERY-crmContacts OPEN QUERY {&SELF-NAME} FOR EACH ttContacts.
&Scoped-define TABLES-IN-QUERY-crmContacts ttContacts
&Scoped-define FIRST-TABLE-IN-QUERY-crmContacts ttContacts


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svSelect crmContacts btnApply btnReset ~
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
DEFINE QUERY crmContacts FOR 
      ttContacts SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE crmContacts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS crmContacts Dialog-Frame _FREEFORM
  QUERY crmContacts DISPLAY
      ttContacts.crmFirstName
    ttContacts.crmLastName
    ttContacts.crmPhone
    ttContacts.crmEmail
    ttContacts.applyAction VIEW-AS TOGGLE-BOX
    ttContacts.action
    ttContacts.phoneAttention
    ttContacts.phoneCityCode
    ttContacts.phonePhone
    ttContacts.phoneExt
    ttContacts.phoneEmail
    ENABLE
    ttContacts.applyAction
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 204 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     svSelect AT ROW 1 COL 96 WIDGET-ID 2
     crmContacts AT ROW 1.95 COL 2 WIDGET-ID 200
     btnApply AT ROW 7.19 COL 95 HELP
          "Apply Selected Actions" WIDGET-ID 76
     btnReset AT ROW 7.19 COL 100 HELP
          "Reset" WIDGET-ID 16
     btnSave AT ROW 7.19 COL 105 HELP
          "Save Selected Actions" WIDGET-ID 18
     btnCancel AT ROW 7.19 COL 110 HELP
          "Cancel" WIDGET-ID 4
     svStatus AT ROW 7.19 COL 2 NO-LABEL WIDGET-ID 78
     "CRM Contacts" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1.24 COL 41 WIDGET-ID 80
     "Customer Contacts" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.24 COL 149 WIDGET-ID 82
     SPACE(39.00) SKIP(6.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "ZOHO CRM (Customer Contacts)" WIDGET-ID 100.


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
/* BROWSE-TAB crmContacts svSelect Dialog-Frame */
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE crmContacts
/* Query rebuild information for BROWSE crmContacts
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttContacts.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE crmContacts */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* ZOHO CRM (Customer Contacts) */
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
  FOR EACH ttContacts:
      ttContacts.applyAction = {&SELF-NAME}.
      IF ttContacts.action EQ "" THEN
      ttContacts.applyAction = NO.
  END. /* each ttcontacts */
  BROWSE crmContacts:REFRESH() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME crmContacts
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
  ENABLE svSelect crmContacts btnApply btnReset btnSave btnCancel 
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

    FOR EACH ttContacts:
        IF ttContacts.applyAction EQ NO THEN NEXT.
        IF ttContacts.action EQ "" THEN NEXT.
        ASSIGN
            cPhone = REPLACE(ttContacts.crmPhone," ","")
            cPhone = REPLACE(cPhone,"+","")
            cPhone = REPLACE(cPhone,"-","")
            cPhone = REPLACE(cPhone,"(","")
            cPhone = REPLACE(cPhone,")","")
            cPhone = REPLACE(cPhone,"x","")
            ttContacts.phoneAttention = ttContacts.crmFirstName + " " + ttContacts.crmLastName
            ttContacts.phoneCityCode  = SUBSTR(cPhone,1,3)
            ttContacts.phonePhone     = SUBSTR(cPhone,4,7)
            ttContacts.phoneExt       = SUBSTR(cPhone,11)
            ttContacts.phoneEmail     = ttContacts.crmEmail
            ttContacts.saveAction     = ttContacts.action
            ttContacts.applyAction    = NO
            ttContacts.action         = ""
            .
        BROWSE crmContacts:REFRESH() NO-ERROR.
    END. /* each ttcontacts */

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
    DEFINE VARIABLE lcContacts  AS LONGCHAR  NO-UNDO.

    FIND FIRST cust NO-LOCK WHERE cust.rec_key EQ ipcRecKey NO-ERROR.
    IF NOT AVAILABLE cust THEN
    RETURN "Customer Not Available".
    
    RUN pGetAuthToken  (cust.company, OUTPUT cAuthToken).
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
        "Ticker Symbol:" + cust.cust-no,
        "Accounts(ACCOUNTID,Account%20Name,Ticker%20Symbol)",
        "searchRecords",
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
    
    FIND FIRST ttAccounts NO-ERROR.
    IF AVAILABLE ttAccounts THEN DO:
        RUN HelpCrmZohoCont IN hSalesSoap (
            "Contacts",
            "Account%20Name:" + ttAccounts.accountName,
            "Contacts(First%20Name,Last%20Name,Phone,Email)",
            "searchRecords",
            cAuthToken,
            OUTPUT lcContacts
            ).
        OUTPUT TO "c:\tmp\Contacts.xml".
        PUT UNFORMATTED STRING(lcContacts) SKIP.
        OUTPUT CLOSE.
        EMPTY TEMP-TABLE ttContacts.
        RUN pXML ("c:\tmp\Contacts.xml", "Contacts").
    END. /* avail ttaccounts */

    FOR EACH ttContacts:
        iRows = iRows + 1.
        IF ttContacts.crmEmail NE "" THEN
        ttContacts.applyAction = YES.
    END.
    
    FOR EACH phone NO-LOCK
        WHERE phone.table_rec_key EQ ipcRecKey
        :
        RELEASE ttContacts.
        IF phone.e_mail NE "" THEN
        FIND FIRST ttContacts
             WHERE ttContacts.crmEmail EQ phone.e_mail
             NO-ERROR.
        IF NOT AVAILABLE ttContacts THEN DO:
            CREATE ttContacts.
            ASSIGN
                ttContacts.action = ""
                iRows = iRows + 1
                .
        END.
        ELSE
        ASSIGN
            ttContacts.action = "Update"
            ttContacts.applyAction = YES
            .
        ASSIGN
            ttContacts.phoneAttention = phone.attention
            ttContacts.phoneCityCode  = phone.phone_city_code
            ttContacts.phonePhone     = phone.phone
            ttContacts.phoneExt       = phone.phone_ext
            ttContacts.phoneEmail     = phone.e_mail
            ttContacts.phoneRowID     = ROWID(phone)
            .
    END.

    DO WITH FRAME {&FRAME-NAME}:
        IF iRows GT 5 THEN DO:
            ASSIGN
                iHeight = 20 + iRows * 17
                FRAME {&FRAME-NAME}:HEIGHT-PIXELS = 80 + iHeight
                btnApply:Y  = BROWSE crmContacts:Y + iHeight + 5
                btnReset:Y  = btnApply:Y
                btnSave:Y   = btnApply:Y
                btnCancel:Y = btnApply:Y
                svStatus:Y  = btnApply:Y
                .
            BROWSE crmContacts:HEIGHT-PIXELS = iHeight.
        END.
    END.
    {&OPEN-QUERY-crmContacts}
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
    FOR EACH ttContacts:
        IF ttContacts.saveAction EQ "" THEN NEXT.
        IF ttContacts.saveAction EQ "Update" THEN
        FIND phone EXCLUSIVE-LOCK WHERE ROWID(phone) EQ ttContacts.phoneRowID.
        ELSE DO:
            CREATE phone.
            phone.table_rec_key = ipcRecKey.
        END. /* save */
        ASSIGN
            phone.attention       = ttContacts.phoneAttention
            phone.phone_city_code = ttContacts.phoneCityCode
            phone.phone           = ttContacts.phonePhone
            phone.phone_ext       = ttContacts.phoneExt
            phone.e_mail          = ttContacts.phoneEmail
            .
        RELEASE phone.
    END. /* each ttcontacts */

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
           ttNodes.level      EQ 5     THEN DO:
            IF ipcType EQ "Accounts" THEN
            CREATE ttAccounts.
            ELSE IF ipcType EQ "Contacts" THEN
            CREATE ttContacts.
        END.

        IF ttNodes.nodeName   EQ "val" AND
           ttNodes.parentName EQ "FL"  AND
           ttNodes.level      EQ 6     THEN DO:
            IF ipcType EQ "Accounts" THEN
            CASE ttNodes.nodeValue:
                WHEN "Account Name" THEN
                ttAccounts.accountName = cValue.
                WHEN "Ticker Symbol" THEN
                ttAccounts.tickerSymbol = cValue.
            END CASE.
            ELSE IF ipcType EQ "Contacts" THEN
            CASE ttNodes.nodeValue:
                WHEN "First Name" THEN
                ttContacts.crmFirstName = cValue.
                WHEN "Last Name" THEN
                ttContacts.crmLastName = cValue.
                WHEN "Phone" THEN
                ttContacts.crmPhone = cValue.
                WHEN "Email" THEN
                ttContacts.crmEmail = cValue.
            END CASE.
        END.

        IF ttNodes.nodeName   EQ "FL"  AND
           ttNodes.parentName EQ "row" AND
           ttNodes.level      EQ 5     THEN
        cValue = ttNodes.nodeValue.
    END. /* each ttnodes */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

