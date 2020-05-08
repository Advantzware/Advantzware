/*------------------------------------------------------------------------
    File        : XMLOutput\APIXMLParser.p
    Purpose     : This is an XML Parser

    Syntax      :

    Description : This is an XML Parser

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 26 07:33:22 EDT 2019
    
    Notes       : *** This is copied from XMLOutput/XMLParser.p ***
  ----------------------------------------------------------------------*/
  
DEFINE INPUT PARAMETER ipXMLData AS LONGCHAR NO-UNDO.

{XMLOutput/ttNodes.i}

DEFINE VARIABLE iLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
DEFINE VARIABLE parentName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iParentOrder AS INTEGER   NO-UNDO.

RUN processXML(ipXMLData) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR RETURN-VALUE.

PROCEDURE processXML: 
  DEFINE INPUT PARAMETER XMLData AS LONGCHAR NO-UNDO.

  DEFINE VARIABLE hXML AS HANDLE NO-UNDO. 
  DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
  DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
  
  EMPTY TEMP-TABLE ttNodes.

  CREATE X-DOCUMENT hXML. 
  CREATE X-NODEREF hRoot.

  lSuccess = hXML:LOAD('longchar':U,XMLData,FALSE) NO-ERROR.
  IF NOT lSuccess OR ERROR-STATUS:ERROR THEN
      RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).

  /*Get the root element handle*/ 
  hXML:GET-DOCUMENT-ELEMENT(hRoot) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).
              
  ASSIGN
      parentName   = 'ROOT'
      iParentOrder = 0
      . 

  RUN processChildren(hRoot) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).
        
  DELETE OBJECT hXML. 
  DELETE OBJECT hRoot. 
END. /* processXML */

PROCEDURE processChildren: 
  DEFINE INPUT PARAMETER hParent AS HANDLE NO-UNDO.

  DEFINE VARIABLE hChild AS HANDLE NO-UNDO. 
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  iLevel = iLevel + 1.

  CREATE X-NODEREF hChild.
  
  CREATE ttNodes. 
  ASSIGN
    iOrder              = iOrder + 1
    ttNodes.order       = iOrder
    ttNodes.nodeName    = hParent:NAME 
    ttNodes.parentName  = parentName
    ttNodes.parentOrder = iParentOrder
    ttNodes.nodeType    = "parent" 
    ttNodes.level       = iLevel
    iParentOrder        = ttNodes.parentOrder
    .
  
  RUN processAttributes(hParent).
  
  DO i = 1 TO hParent:NUM-CHILDREN :
    hParent:GET-CHILD(hChild,i) NO-ERROR.
    IF hChild:NAME EQ '#text' OR hChild:NAME EQ '#cdata-section' THEN DO:
      IF i GT 1 THEN DO:
        CREATE ttNodes.
        ASSIGN
          iOrder              = iOrder + 1
          ttNodes.order       = iOrder
          ttNodes.nodeName    = hParent:NAME
          ttNodes.parentName  = parentName
          ttNodes.parentOrder = iParentOrder
          ttNodes.level       = iLevel
          .
      END.
      ASSIGN
          ttNodes.nodeValue = hChild:NODE-VALUE.
          ttNodes.nodeType  = "child"
          .
    END.
    ELSE DO:
      parentName = hParent:NAME.
      RUN processChildren(hChild) NO-ERROR.
    END.
  END.

  iLevel = iLevel - 1. 
  DELETE OBJECT hChild. 
END. /* process-child */

PROCEDURE processAttributes: 
  DEFINE INPUT PARAMETER hParent AS HANDLE NO-UNDO.

  DEFINE VARIABLE cAttribute AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE i AS INTEGER NO-UNDO. 
  DEFINE BUFFER ttNodes FOR ttNodes.

  DO i = 1 TO NUM-ENTRIES(hParent:ATTRIBUTE-NAMES) TRANSACTION: 
    cAttribute = ENTRY(i,hParent:ATTRIBUTE-NAMES). 
    CREATE ttNodes. 
    ASSIGN 
      iOrder              = iOrder + 1
      ttNodes.order       = iOrder
      ttNodes.nodeName    = cAttribute 
      ttNodes.parentName  = hParent:NAME
      ttNodes.parentOrder = iParentOrder 
      ttNodes.nodeValue   = hParent:GET-ATTRIBUTE(cAttribute) 
      ttNodes.nodeType    = "attribute"
      ttNodes.level       = iLevel + 1
      NO-ERROR.
  END. 
END. /* processAttributes */
