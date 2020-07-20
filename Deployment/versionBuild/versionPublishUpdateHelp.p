
/*------------------------------------------------------------------------
    File        : pubVersionUpdateHelp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Jul 09 13:03:27 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cConnectString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vhWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE vhSalesSoap AS HANDLE NO-UNDO.
DEFINE VARIABLE parameters1 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE fr-title  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCurrentVersion AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewVersion AS INTEGER NO-UNDO.
DEF VAR cNewVer AS CHAR NO-UNDO.
DEF VAR cNewVer1 AS CHAR NO-UNDO.
ASSIGN 
    cConnectString = "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
CREATE SERVER vhWebService.
vhWebService:CONNECT(cConnectString) NO-ERROR.
    
IF NOT vhWebService:CONNECTED() THEN 
DO:
    MESSAGE 
        "Unable to connect to the Help Service with string " SKIP 
        cConnectString SKIP 
        "Please correct and try again."
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

RUN Service1Soap SET vhSalesSoap ON vhWebService .
RUN HelpVersion IN vhSalesSoap( OUTPUT parameters1).

IF SEARCH("c:\asigui\build\newVer.txt") EQ ? THEN 
DO:
    MESSAGE
        "Unable to locate c:\asigui\build\newVer.txt" SKIP
        "No file updates performed."
        VIEW-AS ALERT-BOX.
    RETURN.
END.
INPUT FROM c:\asigui\build\newVer.txt.
IMPORT cNewVer.
INPUT CLOSE.

MESSAGE 
    "About to update PUBLIC version from " + parameters1 + " to " + cNewVer + "." SKIP 
    "Is this correct?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOG.
    
IF lContinue THEN DO:
    RUN VersionUpdate  IN vhSalesSoap("",INPUT parameters1,INPUT cNewVer,  OUTPUT cNewVer1).
    IF cNewVer1 NE cNewVer THEN MESSAGE 
        "Version did NOT update successfully." SKIP 
        "Try from inside ASI."
        VIEW-AS ALERT-BOX ERROR.
    ELSE MESSAGE 
        "Version update in Help system succeeded."
        VIEW-AS ALERT-BOX INFO.
END.
        
