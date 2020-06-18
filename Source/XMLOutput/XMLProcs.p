
/*------------------------------------------------------------------------
    File        : XMLOutput/XMLProcs.p
    Purpose     : 

    Syntax      :

    Description : Procedure to read XML into temp-table		

    Author(s)   : Mithun Porandla
    Created     : Thu Jun 11 02:07:26 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

{XMLOutput/ttNodes.i}

/* ************************  Function Prototypes ********************** */
FUNCTION fReplaceExceptionCharacters RETURNS CHARACTER 
	( INPUT ipcValue AS CHARACTER ) FORWARD.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetChildren PRIVATE: 
    /*------------------------------------------------------------------------------
     Purpose: Creates a ttNodes record for each child of an element
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER iphdParent     AS HANDLE    NO-UNDO.
    DEFINE INPUT        PARAMETER ipiParentOrder AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcParentName  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiLevel       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiChildLevel  AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiOrder      AS INTEGER   NO-UNDO.
  
    DEFINE VARIABLE hdChild      AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE iNumChild    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParentOrder AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cParentName  AS CHARACTER NO-UNDO.
    
    CREATE X-NODEREF hdChild.

    DEFINE BUFFER ttNodes FOR ttNodes.
    
    CREATE ttNodes. 
    ASSIGN
        ttNodes.order       = iopiOrder + 1
        ttNodes.nodeName    = iphdParent:NAME 
        ttNodes.parentName  = ipcParentName
        ttNodes.parentOrder = ipiParentOrder
        ttNodes.nodeType    = iphdParent:SUBTYPE 
        ttNodes.level       = ipiLevel
        ttNodes.childLevel  = ipiChildLevel
        iopiOrder           = ttNodes.order
        iParentOrder        = ttNodes.order
        cParentName         = ttNodes.nodeName
        .

    RUN pGetAttributes (
        INPUT        iphdParent,
        INPUT        iParentOrder,      /* Parent Order */
        INPUT        cParentName,       /* Parent Name */
        INPUT        ipiLevel + 1,      /* Level */
        INPUT-OUTPUT iopiOrder          /* Order */
        ) NO-ERROR.

    DO iNumChild = 1 TO iphdParent:NUM-CHILDREN :
        iphdParent:GET-CHILD(hdChild, iNumChild) NO-ERROR.

        IF hdChild:SUBTYPE EQ "TEXT" THEN
             ttNodes.nodeValue = hdChild:NODE-VALUE.

        IF hdChild:SUBTYPE NE "ELEMENT" THEN
            NEXT.
    
        RUN pGetChildren(
            INPUT        hdChild,
            INPUT        iParentOrder,      /* Parent Order */
            INPUT        cParentName,       /* Parent Name */
            INPUT        ipiLevel + 1,      /* Level */
            INPUT        iNumChild / 2,     /* Child level. Divide by 2 because an element has two childs the element itself and text(value) */
            INPUT-OUTPUT iopiOrder          /* Order */
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).       
    END.

    DELETE OBJECT hdChild. 
    
    RELEASE ttNodes.
END PROCEDURE.

PROCEDURE pGetAttributes PRIVATE: 
    /*------------------------------------------------------------------------------
     Purpose: Created a ttNodes record for each attribute of an element
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER iphdParent     AS HANDLE    NO-UNDO.
    DEFINE INPUT        PARAMETER ipiParentOrder AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcParentName  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiLevel       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiOrder      AS INTEGER   NO-UNDO.
  
    DEFINE VARIABLE cAttribute    AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iNumAttribute AS INTEGER   NO-UNDO. 
    
    DEFINE BUFFER ttNodes FOR ttNodes.

    DO iNumAttribute = 1 TO NUM-ENTRIES(iphdParent:ATTRIBUTE-NAMES) TRANSACTION: 
        cAttribute = ENTRY(iNumAttribute, iphdParent:ATTRIBUTE-NAMES). 

        CREATE ttNodes. 
        ASSIGN
            ttNodes.order       = iopiOrder + 1
            ttNodes.nodeName    = cAttribute 
            ttNodes.nodeValue   = iphdParent:GET-ATTRIBUTE(cAttribute)
            ttNodes.parentName  = ipcParentName
            ttNodes.parentOrder = ipiParentOrder
            ttNodes.nodeType    = "ATTRIBUTE" 
            ttNodes.level       = ipiLevel
            ttNodes.childLevel  = iNumAttribute
            iopiOrder           = ttNodes.order
            .
    END. 
    
    RELEASE ttNodes.
END PROCEDURE.

PROCEDURE XML_ReadToTT:
    /*------------------------------------------------------------------------------
     Purpose: Reads xml and creates ttNodes records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcXMLData AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE hdXML    AS HANDLE  NO-UNDO. 
    DEFINE VARIABLE hdRoot   AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iOrder   AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttNodes.

    CREATE X-DOCUMENT hdXML. 
    CREATE X-NODEREF  hdRoot.

    lSuccess = hdXML:LOAD('LONGCHAR':U, iplcXMLData, FALSE) NO-ERROR.
    IF NOT lSuccess OR ERROR-STATUS:ERROR THEN
        RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).

    /*Get the root element handle*/ 
    hdXML:GET-DOCUMENT-ELEMENT(hdRoot) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).

    RUN pGetChildren(
        INPUT        hdRoot,
        INPUT        0,       /* Parent Order */
        INPUT        "ROOT",  /* Parent Name */
        INPUT        1,       /* Level */
        INPUT        1,       /* Child Level */
        INPUT-OUTPUT iOrder   /* Order */
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).
        
    DELETE OBJECT hdXML. 
    DELETE OBJECT hdRoot. 

END PROCEDURE.

PROCEDURE XML_WriteFromTT:
    /*------------------------------------------------------------------------------
     Purpose: Writes an xml from temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplcXMLData AS LONGCHAR NO-UNDO.
       
    RUN pWriteChildsFromTT (
        INPUT        0,
        INPUT-OUTPUT oplcXMLData
        ).
END PROCEDURE.

PROCEDURE pWriteAttributesFromTT:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipiParentOrder AS INTEGER  NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcXMLData   AS LONGCHAR NO-UNDO.
    
    DEFINE BUFFER ttNodes FOR ttNodes.
    
    FOR EACH ttNodes 
        WHERE ttNodes.parentOrder EQ ipiParentOrder
          AND ttNodes.nodeType    EQ "ATTRIBUTE":
        ioplcXMLData = ioplcXMLData + " " + ttNodes.nodeName + "=" 
                     + '"' + fReplaceExceptionCharacters(ttNodes.nodeValue) + '"'.
    END.
    ioplcXMLData = ioplcXMLData + ">".
    
    RELEASE ttNodes.
END PROCEDURE.

PROCEDURE pWriteChildsFromTT:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipiParentOrder AS INTEGER  NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcXMLData   AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE cNodeName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNodeValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttNodes FOR ttNodes.
    
    FOR EACH ttNodes 
        WHERE ttNodes.parentOrder EQ ipiParentOrder
          AND ttNodes.nodeType    EQ "ELEMENT":
        ASSIGN
            cNodeName  = ttNodes.nodeName
            cNodeValue = ttNodes.nodeValue
            .
            
        ioplcXMLData = ioplcXMLData + "<" + cNodeName.
        RUN pWriteAttributesFromTT (
            INPUT        ttNodes.order,
            INPUT-OUTPUT ioplcXMLData
            ).
            
        RUN pWriteChildsFromTT (
            INPUT        ttNodes.order,
            INPUT-OUTPUT ioplcXMLData            
            ).

        ioplcXMLData = ioplcXMLData + fReplaceExceptionCharacters(cNodeValue) + "</" + cNodeName + ">".              
    END.
    
    RELEASE ttNodes.
END PROCEDURE.        

PROCEDURE XML_GetFieldValueByName:    
    /*------------------------------------------------------------------------------
     Purpose: Use this procedure only if you are sure that there will only be a single
              record for a given node name
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcNodeName   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.

    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.nodeName    EQ ipcNodeName
           AND bf-ttNodes.parentOrder EQ 0
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN
        ASSIGN
            oplRecFound   = TRUE
            opcFieldValue = bf-ttNodes.nodeValue
            .
        
    RELEASE bf-ttNodes.
END PROCEDURE.

PROCEDURE XML_GetFieldOrderByName:    
    /*------------------------------------------------------------------------------
     Purpose: Use this procedure only if you are sure that there will only be a single
              record for a given node name
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcNodeName   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiFieldOrder AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.

    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.nodeName EQ ipcNodeName
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN
        ASSIGN
            oplRecFound   = TRUE
            opiFieldOrder = bf-ttNodes.order
            .
        
    RELEASE bf-ttNodes.
END PROCEDURE.

PROCEDURE XML_GetFieldValueByNameAndParent:    
    /*------------------------------------------------------------------------------
     Purpose: Returns the value of an element given the name and parent id
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcNodeName   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiParentID   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound   AS LOGICAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcFieldValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.

    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.nodeName    EQ ipcNodeName 
           AND bf-ttNodes.parentOrder EQ ipiParentID
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN
        ASSIGN
            oplRecFound   = TRUE
            opcFieldValue = bf-ttNodes.nodeValue
            .

    RELEASE bf-ttNodes.        
END PROCEDURE.

PROCEDURE XML_GetFieldOrderListByNameAndParent:  
    /*------------------------------------------------------------------------------
     Purpose: Fetches the list of field order values for a given parent 
     Notes:  
    ------------------------------------------------------------------------------*/      
    DEFINE INPUT  PARAMETER ipcNodeName       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiParentID       AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFieldOrderList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.

    FOR EACH bf-ttNodes
        WHERE bf-ttNodes.nodeName    BEGINS ipcNodeName
          AND bf-ttNodes.parentOrder EQ     ipiParentID:
        opcFieldOrderList = opcFieldOrderList + "," + STRING(bf-ttNodes.order).
    END.
    
    opcFieldOrderList = TRIM(opcFieldOrderList,",").
    
    RELEASE bf-ttNodes.        
END PROCEDURE.

PROCEDURE XML_GetNameAndValueByFieldOrder:  
    /*------------------------------------------------------------------------------
     Purpose: Fetches the name and value of a given field order id
     Notes:  
    ------------------------------------------------------------------------------*/      
    DEFINE INPUT  PARAMETER ipiFieldOrderID  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcName          AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcValue         AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.

    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.order EQ ipiFieldOrderID
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN DO:
        ASSIGN
            oplRecFound = TRUE
            opcName     = bf-ttNodes.nodeName
            opcValue    = bf-ttNodes.nodeValue
            .
    END.   
    
    RELEASE bf-ttNodes.        
END PROCEDURE.

PROCEDURE XML_GetFieldOrderByNameAndParent:    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcNodeName   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiParentID   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound   AS LOGICAL   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opiFieldOrder AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.

    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.nodeName    EQ ipcNodeName 
           AND bf-ttNodes.parentOrder EQ ipiParentID
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN
        ASSIGN
            oplRecFound   = TRUE
            opiFieldOrder = bf-ttNodes.order
            .

    RELEASE bf-ttNodes.        
END PROCEDURE.
    
PROCEDURE XML_GetRecordCountByNameAndParent:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcNodeName    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiParentID    AS INTEGER   NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcRecordCount AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.
    
    FOR EACH bf-ttNodes
        WHERE bf-ttNodes.nodeName    EQ ipcNodeName 
          AND bf-ttNodes.parentOrder EQ ipiParentID:
        opcRecordCount = opcRecordCount + 1.  
    END.    

    RELEASE bf-ttNodes.    
END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fReplaceExceptionCharacters RETURNS CHARACTER 
	( INPUT ipcValue AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Replaces exception characters in xml element values. Writing these 
          characters as it is, may cause xml errors while readin
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
    
    cValue = ipcValue.
    
    ASSIGN
        cValue = REPLACE(cValue, '&', "&amp;")
        cValue = REPLACE(cValue, '"', "&quot;")
        cValue = REPLACE(cValue, "'", "&apos;")
        cValue = REPLACE(cValue, '<', "&lt;")
        cValue = REPLACE(cValue, '>', "&gt;")
        .
    
    RETURN cValue.
END FUNCTION.
