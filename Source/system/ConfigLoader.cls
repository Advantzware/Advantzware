 
 /*------------------------------------------------------------------------
    File        : system.ConfigLoader.cls
    Purpose     : Class to maintain JSONConfig classes
    Syntax      : 
    Description : 
    Author(s)   : Rajesh
    Created     : Mon Jan 11 02:23:46 EST 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS system.ConfigLoader:
    DEFINE PRIVATE VARIABLE cCONFIG_TYPE_JSON        AS CHARACTER NO-UNDO INITIAL "JSON".
    DEFINE PRIVATE VARIABLE cCONFIG_TYPE_SOURCE_DB   AS CHARACTER NO-UNDO INITIAL "DB".
    DEFINE PRIVATE VARIABLE cCONFIG_TYPE_SOURCE_FILE AS CHARACTER NO-UNDO INITIAL "FILE".
    
    DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS system.ConfigLoader NO-UNDO
    GET ():
        /* Returns reference to the single allowed instance. Actually instantiates
        the class if no instance is active */
        IF Instance = ? THEN 
            Instance = NEW system.ConfigLoader( ).
        RETURN Instance.
    END GET.
    PRIVATE SET.   
    
    DESTRUCTOR PUBLIC ConfigLoader():
        Instance = ?.
    END DESTRUCTOR.   
    
    METHOD PRIVATE system.Config pGetConfig (ipcConfig AS CHARACTER):       
        DEFINE VARIABLE cConfigType   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cConfigSource AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcConfig      AS LONGCHAR  NO-UNDO.
        
        DEFINE VARIABLE oSetting AS system.Setting NO-UNDO.
        DEFINE VARIABLE oObject  AS Object         NO-UNDO.
        
        cConfigType = pGetConfigType(ipcConfig).
        cConfigSource = pGetConfigSource(ipcConfig).
        
        IF pGetConfigType(ipcConfig) EQ cCONFIG_TYPE_JSON THEN DO:
            IF cConfigSource EQ cCONFIG_TYPE_SOURCE_FILE THEN
                oObject = NEW system.JSONConfig(pGetJSONConfigFile(ipcConfig)).
            ELSE IF cConfigSource EQ cCONFIG_TYPE_SOURCE_DB THEN DO:
                oSetting = NEW system.Setting().
                
                lcConfig = oSetting:GetByName(ipcConfig).

                oObject = NEW system.JSONConfig(lcConfig).

                DELETE OBJECT oSetting.            
            END.
        END.
        
        RETURN CAST(oObject, system.Config).
    END METHOD. 
    
    METHOD PUBLIC system.Config GetConfig (ipcConfig AS CHARACTER):
        RETURN pGetConfig(ipcConfig).
    END METHOD.
    
    METHOD PRIVATE CHARACTER pGetJSONConfigFile (ipcConfig AS CHARACTER):
        CASE ipcConfig:
            WHEN "SSLoadTag" THEN
                RETURN SEARCH("system/SSLoadTag.json").
            WHEN "SSLoadTagJobDesign" THEN
                RETURN SEARCH("system/SSLoadTagJobDesign.json").
            WHEN "SSLoadTagBOLDesign" THEN
                RETURN SEARCH("system/SSLoadTagBOLDesign.json").
            WHEN "SSLoadTagPODesign" THEN
                RETURN SEARCH("system/SSLoadTagPODesign.json").
            WHEN "SSLoadTagReprintDesign" THEN
                RETURN SEARCH("system/SSLoadTagReprintDesign.json").
        END CASE.
    END METHOD.     

    METHOD PRIVATE CHARACTER pGetConfigType (ipcConfig AS CHARACTER):
        CASE ipcConfig:
            WHEN "SSLoadTag" OR
            WHEN "SSLoadTagJobDesign" OR
            WHEN "SSLoadTagBOLDesign" OR
            WHEN "SSLoadTagPODesign" OR
            WHEN "SSLoadTagReprintDesign" THEN
                RETURN cCONFIG_TYPE_JSON.
        END CASE.
    END METHOD. 

    METHOD PRIVATE CHARACTER pGetConfigSource (ipcConfig AS CHARACTER):
        CASE ipcConfig:
            WHEN "SSLoadTagJobDesign" THEN
                RETURN cCONFIG_TYPE_SOURCE_DB. 
            WHEN "SSLoadTag" OR 
            WHEN "SSLoadTagBOLDesign" OR
            WHEN "SSLoadTagPODesign" OR 
            WHEN "SSLoadTagReprintDesign" THEN
                RETURN cCONFIG_TYPE_SOURCE_FILE.
        END CASE.
    END METHOD. 
END CLASS.
