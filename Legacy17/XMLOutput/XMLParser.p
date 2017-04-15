/* XMLParser.p */

DEFINE INPUT PARAMETER ipXMLFile AS CHARACTER NO-UNDO.

{XMLOutput/ttNodes.i}

DEFINE VARIABLE iLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
DEFINE VARIABLE parentName AS CHARACTER NO-UNDO.

RUN processXML(ipXMLFile).

PROCEDURE processXML: 
  DEFINE INPUT PARAMETER XMLFileName AS CHARACTER NO-UNDO.

  DEFINE VARIABLE hXML AS HANDLE NO-UNDO. 
  DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.

  EMPTY TEMP-TABLE ttNodes.

  CREATE X-DOCUMENT hXML. 
  CREATE X-NODEREF hRoot.

  hXML:LOAD('FILE':U,XMLFileName,FALSE).

  /*Get the root element handle*/ 
  hXML:GET-DOCUMENT-ELEMENT(hRoot).

  parentName = 'ROOT'. 
  RUN processChildren(hRoot).

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
    iOrder = iOrder + 1
    ttNodes.order = iOrder
    ttNodes.nodeName = hParent:NAME 
    ttNodes.parentName = parentName 
    ttNodes.level = iLevel
    .
  
  RUN processAttributes(hParent).

  DO i = 1 TO hParent:NUM-CHILDREN : 
    hParent:GET-CHILD(hChild,i). 
    IF hChild:NAME EQ '#text' OR hChild:NAME EQ '#cdata-section' THEN DO:
      IF i GT 1 THEN DO:
        CREATE ttNodes. 
        ASSIGN 
          iOrder = iOrder + 1
          ttNodes.order = iOrder
          ttNodes.nodeName = hParent:NAME 
          ttNodes.parentName = parentName 
          ttNodes.level = iLevel
          .
      END.
      ttNodes.nodeValue = hChild:NODE-VALUE.
    END.
    ELSE DO:
      parentName = hParent:NAME.
      RUN processChildren(hChild). 
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
      iOrder = iOrder + 1
      ttNodes.order = iOrder
      ttNodes.nodeName = cAttribute 
      ttNodes.parentName = hParent:NAME 
      ttNodes.nodeValue = hParent:GET-ATTRIBUTE(cAttribute) 
      ttNodes.level = iLevel + 1
      .
  END. 
END. /* processAttributes */
