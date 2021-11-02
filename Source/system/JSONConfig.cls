 
 /*------------------------------------------------------------------------
    File        : system.JSONConfig.cls
    Purpose     : Reads a JSON config file
    Syntax      : 
    Description : 
    Author(s)   : Rajesh
    Created     : Mon Jan 11 02:23:46 EST 2020
    Notes       : This programs can fetch information accurately only 
                  upto 2 levels of nesting in a json
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.JSONConfig IMPLEMENTS system.Config:
    DEFINE PRIVATE TEMP-TABLE ttConfig NO-UNDO
        FIELD configID   AS INTEGER
        FIELD parentID   AS INTEGER
        FIELD configName AS CHARACTER
        INDEX configID IS PRIMARY UNIQUE configID
        .
    
    DEFINE PRIVATE TEMP-TABLE ttAttribute NO-UNDO
        FIELD configID       AS INTEGER
        FIELD attributeID    AS INTEGER
        FIELD attributeName  AS CHARACTER
        FIELD attributeValue AS CHARACTER
        FIELD attributeType  AS CHARACTER
        INDEX attributeID IS PRIMARY UNIQUE configID attributeID
        .
    
    DEFINE PRIVATE VARIABLE iConfigIDIndex AS INTEGER   NO-UNDO.
    DEFINE PRIVATE VARIABLE cDataTypeList  AS CHARACTER NO-UNDO INITIAL "String,Number,Boolean,Object,Array,Null".
    
    DESTRUCTOR PUBLIC JSONConfig():
        EMPTY TEMP-TABLE ttConfig.
        EMPTY TEMP-TABLE ttAttribute.
    END.
    
    CONSTRUCTOR PUBLIC JSONConfig(ipcJSONFile AS CHARACTER):
        pLoadJSON(INPUT ipcJSONFile).
    END CONSTRUCTOR.

    METHOD PRIVATE LOGICAL pLoadJSON (ipcJSONFile AS CHARACTER):
        DEFINE VARIABLE oModelParser    AS ObjectModelParser NO-UNDO.
        DEFINE VARIABLE oJSONObject     AS JsonObject        NO-UNDO. 
                
        oModelParser = NEW ObjectModelParser().
        
        oJSONObject = CAST(oModelParser:ParseFile(ipcJSONFile), JsonObject).
        
        EMPTY TEMP-TABLE ttConfig.
        EMPTY TEMP-TABLE ttAttribute.
        
        IF TYPE-OF(oJSONObject, "jsonObject") THEN
            pCreateConfig (INPUT oJSONObject, INPUT "root", INPUT 0).
        
        IF VALID-OBJECT(oJSONObject) THEN
            DELETE OBJECT oJSONObject.
        
        IF VALID-OBJECT(oModelParser) THEN
            DELETE OBJECT oModelParser.
    END METHOD.
    
    METHOD PRIVATE VOID pCreateConfig (ipoJSONObject AS JSONObject, ipcConfigName AS CHARACTER, ipiParentConfigID AS INTEGER):
        DEFINE VARIABLE cPropertyNames AS CHARACTER NO-UNDO EXTENT.
        DEFINE VARIABLE iPropertyCount AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
        
        DEFINE VARIABLE oJSONObject     AS JsonObject NO-UNDO. 
        DEFINE VARIABLE iParentConfigID AS INTEGER    NO-UNDO.
        
        CREATE ttConfig.
        ASSIGN
            iConfigIDIndex      = iConfigIDIndex + 1
            ttConfig.configID   = iConfigIDIndex
            ttConfig.parentID   = ipiParentConfigID
            ttConfig.configName = ipcConfigName
            iParentConfigID     = ttConfig.configID
            .
        
        cPropertyNames = ipoJSONObject:GetNames().
        
        iPropertyCount = EXTENT(cPropertyNames).
        
        DO iIndex = 1 TO iPropertyCount:
            IF ENTRY(ipoJSONObject:GetType(cPropertyNames[iIndex]),cDataTypeList) EQ "Object" THEN DO:
                oJSONObject = ipoJSONObject:GetJsonObject(cPropertyNames[iIndex]).
                
                pCreateConfig (oJSONObject, cPropertyNames[iIndex], iParentConfigID).
                
                IF VALID-OBJECT (oJSONObject) THEN
                    DELETE OBJECT oJSONObject.
            END.
            ELSE DO:
                pCreateAttribute (iConfigIDIndex, iIndex, cPropertyNames[iIndex], STRING(ipoJSONObject:GetJsonText(cPropertyNames[iIndex])), ENTRY(ipoJSONObject:GetType(cPropertyNames[iIndex]), cDataTypeList)).
            END.
        END.

    END METHOD.

    METHOD PRIVATE VOID pCreateAttribute (ipiConfigID AS INTEGER, ipiAttributeID AS INTEGER, ipcAttributeName AS CHARACTER, ipcAttributeValue AS CHARACTER, ipcAttributeType AS CHARACTER):
        CREATE ttAttribute.
        ASSIGN
            ttAttribute.attributeID    = ipiAttributeID
            ttAttribute.configID       = ipiConfigID
            ttAttribute.attributeName  = ipcAttributeName
            ttAttribute.attributeValue = ipcAttributeValue 
            ttAttribute.attributeType  = ipcAttributeType
            .
    END METHOD.
    
    METHOD PRIVATE CHARACTER pGetAttributeValue(ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER, ipcSubAttribute AS CHARACTER, ipiParentID AS INTEGER):
        DEFINE VARIABLE lIsAttributeAvailable AS LOGICAL NO-UNDO.
        
        RETURN pGetAttributeValue(ipcConfig, ipcAttribute, ipcSubAttribute, ipiParentID, OUTPUT lIsAttributeAvailable).
    END METHOD.

    METHOD PRIVATE LOGICAL pIsAttributeAvailable(ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER, ipcSubAttribute AS CHARACTER, ipiParentID AS INTEGER):
        DEFINE VARIABLE lIsAttributeAvailable AS LOGICAL NO-UNDO.
        
        pGetAttributeValue(ipcConfig, ipcAttribute, ipcSubAttribute, ipiParentID, OUTPUT lIsAttributeAvailable).
        
        RETURN lIsAttributeAvailable.
    END METHOD.
        
	METHOD PRIVATE CHARACTER pGetAttributeValue(ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER, ipcSubAttribute AS CHARACTER, ipiParentID AS INTEGER, OUTPUT oplIsAttributeAvailable AS LOGICAL):	    
        DEFINE VARIABLE cAttributeValue AS CHARACTER NO-UNDO.

	    DEFINE BUFFER bf-ttConfig    FOR ttConfig.
	    
	    DEFINE BUFFER bf-ttAttribute FOR ttAttribute.	    
	    
	    FIND FIRST bf-ttConfig
	         WHERE bf-ttConfig.configName EQ ipcConfig
	           AND bf-ttConfig.parentID   EQ ipiParentID
	         NO-ERROR.
	    IF AVAILABLE bf-ttConfig THEN
	        FIND FIRST bf-ttAttribute
	             WHERE bf-ttAttribute.configID      EQ bf-ttConfig.configID
	               AND bf-ttAttribute.attributeName EQ ipcAttribute
	             NO-ERROR.
	    
	    IF AVAILABLE bf-ttAttribute THEN
	        ASSIGN
	           cAttributeValue         = bf-ttAttribute.attributeValue
	           oplIsAttributeAvailable = TRUE
	           .
	    ELSE IF AVAILABLE bf-ttConfig AND ipcSubAttribute NE "" THEN
	        cAttributeValue = pGetAttributeValue(ipcAttribute, ipcSubAttribute, "" , bf-ttConfig.configID, OUTPUT oplIsAttributeAvailable).
	    
	    RETURN cAttributeValue.
	END METHOD.

	METHOD PUBLIC CHARACTER GetAttributeValue (ipcAttribute AS CHARACTER):
        RETURN pGetAttributeValue("root", ipcAttribute, "", 1).
	END METHOD.

    METHOD PUBLIC CHARACTER GetAttributeValue (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER):
        RETURN pGetAttributeValue(ipcConfig, ipcAttribute, "", 1).
    END METHOD.    

    METHOD PUBLIC CHARACTER GetAttributeValue (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER, ipcSubAttribute AS CHARACTER):
        RETURN pGetAttributeValue(ipcConfig, ipcAttribute, ipcSubAttribute, 1).
    END METHOD.
    
    METHOD PUBLIC LOGICAL IsAttributeAvailable (ipcAttribute AS CHARACTER):
        RETURN pIsAttributeAvailable("root", ipcAttribute, "", 1).
    END METHOD.

    METHOD PUBLIC LOGICAL IsAttributeAvailable (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER):
        RETURN pIsAttributeAvailable(ipcConfig, ipcAttribute, "", 1).
    END METHOD.    

    METHOD PUBLIC LOGICAL IsAttributeAvailable (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER, ipcSubAttribute AS CHARACTER):
        RETURN pIsAttributeAvailable(ipcConfig, ipcAttribute, ipcSubAttribute, 1).
    END METHOD.    
END CLASS.
