 
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
    DEFINE PRIVATE VARIABLE cCONFIG_TYPE_JSON AS CHARACTER NO-UNDO INITIAL "JSON".
    
    DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS system.ConfigLoader NO-UNDO
    GET ():
        /* Returns reference to the single allowed instance. Actually instantiates
        the class if no instance is active */
        IF Instance = ? THEN 
            Instance = NEW system.ConfigLoader( ).
        RETURN Instance.
    END GET.
    PRIVATE SET.   
    
    METHOD PRIVATE system.Config pGetConfig (ipcConfig AS CHARACTER):       
        DEFINE VARIABLE oObject AS Object NO-UNDO.
        
        DEFINE VARIABLE cConfigType AS CHARACTER NO-UNDO.
        
        cConfigType = pGetConfigType(ipcConfig).

        IF pGetConfigType(ipcConfig) EQ cCONFIG_TYPE_JSON THEN
            oObject = NEW system.JSONConfig(pGetJSONConfigData(ipcConfig)).
        
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

    METHOD PRIVATE LONGCHAR pGetJSONConfigData (ipcConfig AS CHARACTER):
        DEFINE VARIABLE lcJSONConfigData AS LONGCHAR NO-UNDO.
        
        FIX-CODEPAGE(lcJSONConfigData) = "utf-8".
        
        RUN spGetSettingByName(ipcConfig, OUTPUT lcJSONConfigData).
            
        RETURN lcJSONConfigData.
    END METHOD.     

    METHOD PRIVATE CHARACTER pGetConfigType (ipcConfig AS CHARACTER):
        CASE ipcConfig:
            WHEN "SSLoadTag" THEN
                RETURN cCONFIG_TYPE_JSON.
            WHEN "SSLoadTagJobDesign" THEN
                RETURN cCONFIG_TYPE_JSON.
            WHEN "SSLoadTagBOLDesign" THEN
                RETURN cCONFIG_TYPE_JSON.
            WHEN "SSLoadTagPODesign" THEN
                RETURN cCONFIG_TYPE_JSON.
            WHEN "SSLoadTagReprintDesign" THEN
                RETURN cCONFIG_TYPE_JSON.
        END CASE.
    END METHOD. 
END CLASS.
