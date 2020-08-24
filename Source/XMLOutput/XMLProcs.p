
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
        ttNodes.nodeType    = "ELEMENT" 
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

        IF (hdChild:SUBTYPE EQ "TEXT" AND hdChild:NAME EQ "#text") OR
           (hdChild:SUBTYPE EQ "CDATA-SECTION" AND hdChild:NAME EQ "#cdata-section") THEN DO:
            IF iNumChild GT 1 THEN DO:
                CREATE ttNodes. 
                ASSIGN
                    ttNodes.order       = iopiOrder + 1
                    ttNodes.nodeName    = hdChild:NAME 
                    ttNodes.parentName  = cParentName
                    ttNodes.parentOrder = iParentOrder
                    ttNodes.nodeType    = "ELEMENT"
                    ttNodes.valueType   = hdChild:SUBTYPE 
                    ttNodes.level       = ipiLevel + 1
                    ttNodes.childLevel  = ipiChildLevel + 1
                    iopiOrder           = ttNodes.order
                    .
            END.
            ASSIGN
                ttNodes.nodeValue = hdChild:NODE-VALUE
                ttNodes.valueType = hdChild:SUBTYPE
                .            
        END.
        ELSE DO:    
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
            ttNodes.valueType   = "TEXT" 
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
    
    DEFINE VARIABLE cXMLDocumentMetaData AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cXMLDTD              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iXMLDTDRootCount     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iXMLDTDRootPosition  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRootElementPosition AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMatchPosition       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cXMLRootName         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex               AS INTEGER   NO-UNDO.
    
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
    
    /* SYSTEM-ID stores the XML dtd information. The root name is root tag name
       of the xml */
    ASSIGN
        cXMLDTD      = hdXML:SYSTEM-ID
        cXMLRootName = hdRoot:NAME
        .
    
    /* Root name can be included with xsl tag */
    IF INDEX(cXMLRootName, ":") GT 0 THEN
        cXMLRootName = ENTRY(NUM-ENTRIES(cXMLRootName, ":"), cXMLRootName, ":"). 
        
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
    
    /* The below process to extract the document information like xml version
       and document type that cannot be read to construct the xml from temp-table */
    /* Get the position of root name in doctype */
    iMatchPosition = INDEX(iplcXMLData, cXMLRootName, 1).

    /* DOCTYPE stores the DTD information and XML root name */
    IF INDEX(iplcXMLData, "DOCTYPE", 1) GT 0 THEN DO:
        /* Get the position of root element in xml */
        iMatchPosition = INDEX(iplcXMLData, cXMLRootName, iMatchPosition + LENGTH(cXMLRootName)).
        
        /* Verify if root name is available in the dtd. Get the number of time root name is
           repeated in DTD */
        IF LENGTH(cXMLDTD) GE LENGTH(cXMLRootName) THEN DO:
            iXMLDTDRootPosition = 1.
            DO WHILE(iXMLDTDRootPosition LT LENGTH(cXMLDTD)):
                IF INDEX(cXMLDTD, cXMLRootName, iXMLDTDRootPosition) GT 0 THEN
                    ASSIGN
                        iXMLDTDRootCount    = iXMLDTDRootCount + 1
                        iXMLDTDRootPosition = INDEX(cXMLDTD, cXMLRootName, iXMLDTDRootPosition) + LENGTH(cXMLRootName)
                        .
                ELSE
                    LEAVE.
            END.
        END.
        
        /* Forward the position of the root name with number of time it is available in DTD */
        IF iXMLDTDRootCount GT 0 THEN DO:
            DO iIndex = 1 TO iXMLDTDRootCount:
                iMatchPosition = INDEX(iplcXMLData, cXMLRootName, iMatchPosition + LENGTH(cXMLRootName)).
            END.
        END.
    END.
    
    /* Once we get an exact position of the root element, get the position
       where it started */
    iRootElementPosition = R-INDEX(iplcXMLData, "<", iMatchPosition).

    /* Get the complete XML metadata from starting of the xml to beginning of root element tag */
    cXMLDocumentMetaData = SUBSTRING(iplcXMLData, 1, iRootElementPosition - 1).

    IF cXMLDocumentMetaData NE "" THEN DO:
        CREATE ttNodes. 
        ASSIGN
            ttNodes.order       = iOrder + 1
            ttNodes.nodeName    = "XMLMETADATA"
            ttNodes.nodeValue   = cXMLDocumentMetaData 
            ttNodes.parentName  = "X-DOCUMENT"
            ttNodes.parentOrder = 0
            ttNodes.nodeType    = "SYSTEM" 
            ttNodes.level       = 0
            ttNodes.childLevel  = 0
            .
    END.
    
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
    
    /* Append the XML meta data. This includes the xml version, encoding and dtd
       information */ 
    FIND FIRST ttNodes
         WHERE ttNodes.nodeName   = "XMLMETADATA"
           AND ttNodes.parentName = "X-DOCUMENT"
           AND ttNodes.nodeType   = "SYSTEM"
         NO-ERROR.
    IF AVAILABLE ttNodes THEN
        oplcXMLData = ttNodes.nodeValue + oplcXMLData.

END PROCEDURE.

PROCEDURE pWriteAttributesFromTT:
    /*------------------------------------------------------------------------------
     Purpose: Writes attributes of an elements to the xml data
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
    
    /* Close the xml start tag */
    ioplcXMLData = ioplcXMLData + ">".
    
    RELEASE ttNodes.
END PROCEDURE.

PROCEDURE pWriteChildsFromTT:
    /*------------------------------------------------------------------------------
     Purpose: Writes the XML children data
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
        
        /* Do not create an element if the value type of the node is either "TEXT"
           or "CDATA-SECTION". These value types won't have any attributes */
        IF NOT ((ttNodes.valueType EQ "TEXT" AND ttNodes.nodeName EQ "#text") OR
                (ttNodes.valueType EQ "CDATA-SECTION" AND ttNodes.nodeName EQ "#cdata-section")) THEN DO:
            ioplcXMLData = ioplcXMLData + "<" + cNodeName.
            RUN pWriteAttributesFromTT (
                INPUT        ttNodes.order,
                INPUT-OUTPUT ioplcXMLData
                ).
        END.

        /* If value type is "CDATA-SECTION" then wrap the value inside character data definition.
           Else escape the special characters that interfere with xml validation. */
        IF ttNodes.valueType EQ "CDATA-SECTION" AND ttNodes.nodeName EQ "#cdata-section" THEN
            ioplcXMLData = ioplcXMLData + "<![CDATA[" + cNodeValue + "]]>".
        ELSE
            ioplcXMLData = ioplcXMLData + fReplaceExceptionCharacters(cNodeValue).
        
        RUN pWriteChildsFromTT (
            INPUT        ttNodes.order,
            INPUT-OUTPUT ioplcXMLData            
            ).

        /* Write the closing tags only for elements */
        IF NOT ((ttNodes.valueType EQ "TEXT" AND ttNodes.nodeName EQ "#text") OR
                (ttNodes.valueType EQ "CDATA-SECTION" AND ttNodes.nodeName EQ "#cdata-section")) THEN
            ioplcXMLData = ioplcXMLData + "</" + cNodeName + ">".              
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
    
    DEFINE BUFFER bf-ttNodes       FOR ttNodes.
    DEFINE BUFFER bf-value-ttNodes FOR ttNodes.
    
    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.nodeName    EQ ipcNodeName
           AND bf-ttNodes.parentOrder EQ 0
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN DO:
        opcFieldValue = bf-ttNodes.nodeValue.
        /* The below code is to fetch any additional value that is saved in "#text"
           and "#cdata-section" of the element */
        FOR EACH bf-value-ttNodes
            WHERE bf-value-ttNodes.parentOrder EQ bf-ttNodes.order
              AND bf-value-ttNodes.parentName  EQ bf-ttNodes.nodeName
              AND ((bf-value-ttNodes.valueType EQ "TEXT" AND bf-value-ttNodes.nodeName EQ "#text") OR
                   (bf-value-ttNodes.valueType EQ "CDATA-SECTION" AND bf-value-ttNodes.nodeName EQ "#cdata-section")):
            opcFieldValue = opcFieldValue + bf-value-ttNodes.nodeValue.
        END. 

        oplRecFound = TRUE.
    END.
            
    RELEASE bf-ttNodes.
    RELEASE bf-value-ttNodes.
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
    
    DEFINE BUFFER bf-ttNodes       FOR ttNodes.
    DEFINE BUFFER bf-value-ttNodes FOR ttNodes.
    
    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.nodeName    EQ ipcNodeName 
           AND bf-ttNodes.parentOrder EQ ipiParentID
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN DO:
        opcFieldValue = bf-ttNodes.nodeValue.

        /* The below code is to fetch any additional value that is saved in "#text"
           and "#cdata-section" of the element */        
        FOR EACH bf-value-ttNodes
            WHERE bf-value-ttNodes.parentOrder EQ bf-ttNodes.order
              AND bf-value-ttNodes.parentName  EQ bf-ttNodes.nodeName
              AND ((bf-value-ttNodes.valueType EQ "TEXT" AND bf-value-ttNodes.nodeName EQ "#text") OR
                   (bf-value-ttNodes.valueType EQ "CDATA-SECTION" AND bf-value-ttNodes.nodeName EQ "#cdata-section")):
            opcFieldValue = opcFieldValue + bf-value-ttNodes.nodeValue.
        END. 

        oplRecFound = TRUE.
    END.

    RELEASE bf-ttNodes. 
    RELEASE bf-value-ttNodes.       
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
    
    DEFINE BUFFER bf-ttNodes       FOR ttNodes.
    DEFINE BUFFER bf-value-ttNodes FOR ttNodes.

    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.order EQ ipiFieldOrderID
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN DO:
        ASSIGN
            opcName  = bf-ttNodes.nodeName
            opcValue = bf-ttNodes.nodeValue
            .
        /* The below code is to fetch any additional value that is saved in "#text"
           and "#cdata-section" of the element */            
        FOR EACH bf-value-ttNodes
            WHERE bf-value-ttNodes.parentOrder EQ bf-ttNodes.order
              AND bf-value-ttNodes.parentName  EQ bf-ttNodes.nodeName
              AND ((bf-value-ttNodes.valueType EQ "TEXT" AND bf-value-ttNodes.nodeName EQ "#text") OR
                   (bf-value-ttNodes.valueType EQ "CDATA-SECTION" AND bf-value-ttNodes.nodeName EQ "#cdata-section")):
            opcValue = opcValue + bf-value-ttNodes.nodeValue.
        END. 

        oplRecFound = TRUE.
    END.
    
    RELEASE bf-ttNodes.  
    RELEASE bf-value-ttNodes.      
END PROCEDURE.

PROCEDURE XML_GetFieldValueByFieldOrder:  
    /*------------------------------------------------------------------------------
     Purpose: Fetches the value of a given field order id
     Notes:  
    ------------------------------------------------------------------------------*/      
    DEFINE INPUT  PARAMETER ipiFieldOrderID  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRecFound      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValue         AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes       FOR ttNodes.
    DEFINE BUFFER bf-value-ttNodes FOR ttNodes.
    
    FIND FIRST bf-ttNodes
         WHERE bf-ttNodes.order EQ ipiFieldOrderID
         NO-ERROR.
    IF AVAILABLE bf-ttNodes THEN DO:
        opcValue = bf-ttNodes.nodeValue.

        /* The below code is to fetch any additional value that is saved in "#text"
           and "#cdata-section" of the element */        
        FOR EACH bf-value-ttNodes
            WHERE bf-value-ttNodes.parentOrder EQ bf-ttNodes.order
              AND bf-value-ttNodes.parentName  EQ bf-ttNodes.nodeName
              AND ((bf-value-ttNodes.valueType EQ "TEXT" AND bf-value-ttNodes.nodeName EQ "#text") OR
                   (bf-value-ttNodes.valueType EQ "CDATA-SECTION" AND bf-value-ttNodes.nodeName EQ "#cdata-section")):
            opcValue = opcValue + bf-value-ttNodes.nodeValue.
        END. 

        oplRecFound = TRUE.
    END.   
    
    RELEASE bf-ttNodes.
    RELEASE bf-value-ttNodes.        
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
    DEFINE OUTPUT PARAMETER opiRecordCount AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttNodes FOR ttNodes.
    
    FOR EACH bf-ttNodes
        WHERE bf-ttNodes.nodeName    EQ ipcNodeName 
          AND bf-ttNodes.parentOrder EQ ipiParentID:
        opiRecordCount = opiRecordCount + 1.  
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
