/*------------------------------------------------------------------------
    File        : api/ResponseHandlerXMLAmazon.p
    Purpose     : Returns the response

    Syntax      :

    Description : Returns the response

    Author(s)   : Mithun Porandla
    Created     : Tue Jan 28 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
       
{XMLOutput/ttNodes.i NEW}

FUNCTION fGetNodeValue RETURNS CHARACTER (
    ipcParentName AS CHARACTER,
    ipcNodeName   AS CHARACTER):
    DEFINE VARIABLE iNodeOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubNode   AS INTEGER NO-UNDO.

    IF NUM-ENTRIES(ipcNodeName,'|') GT 1 THEN
        ASSIGN
            iSubNode    = INT(ENTRY(2,ipcNodeName,'|'))
            ipcNodeName = ENTRY(1,ipcNodeName,'|')
            .
            
    FIND FIRST ttNodes
         WHERE ttNodes.parentName EQ ipcParentName 
         NO-ERROR.
    IF AVAILABLE ttNodes THEN DO:
        iNodeOrder = ttNodes.order.
        FOR EACH ttNodes 
            WHERE ttNodes.order GE iNodeOrder
              AND ttNodes.order LE iNodeOrder + 25:
            IF ttNodes.nodeName EQ ipcNodeName THEN DO:
                IF iSubNode NE 0 THEN
                    ttNodes.nodeName = ttNodes.nodeName + STRING(iSubNode).
                RETURN TRIM(ttNodes.nodeValue).
            END.
        END.
    END.
    
    RETURN ''.
END FUNCTION.

RUN XMLOutput/APIXMLParser.p (
    INPUT iplcResponseData
    ) NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    ASSIGN
        opcMessage = RETURN-VALUE
        oplSuccess = FALSE
        .

ASSIGN
    opcMessage = fGetNodeValue('Response','Status')
    oplSuccess = fGetNodeValue('Status','code') EQ "200"
    .
