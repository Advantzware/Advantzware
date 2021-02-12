 
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
    DEFINE PRIVATE TEMP-TABLE ttClass
       FIELD className     AS CHARACTER 
       FIELD classInstance AS CLASS Object
       FIELD configType    AS CHARACTER
       INDEX className IS PRIMARY UNIQUE className configType
       .
    
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
    
    DESTRUCTOR PUBLIC ConfigLoader():
        Instance = ?.
        FOR EACH ttClass:
            IF VALID-OBJECT (ttClass.classInstance) THEN
                DELETE OBJECT ttClass.classInstance.
        END.
        EMPTY TEMP-TABLE ttClass.
    END DESTRUCTOR.   
    
    METHOD PRIVATE system.Config pGetConfig (ipcConfig AS CHARACTER):       
        DEFINE VARIABLE cConfigType AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER bf-ttClass FOR ttClass.
 
        cConfigType = pGetConfigType(ipcConfig).

        FIND FIRST bf-ttClass
             WHERE bf-ttClass.className  EQ ipcConfig
               AND bf-ttClass.configType EQ cConfigType
             NO-ERROR.
        IF NOT AVAILABLE bf-ttClass THEN DO:
            CREATE bf-ttClass.
            ASSIGN
                bf-ttClass.className  = ipcConfig
                bf-ttClass.configType = cCONFIG_TYPE_JSON
                .
            
            IF pGetConfigType(ipcConfig) EQ cCONFIG_TYPE_JSON THEN
                bf-ttClass.classInstance = NEW system.JSONConfig(pGetJSONConfigFile(ipcConfig)).
        END.
        
        RETURN CAST(bf-ttClass.classInstance, system.Config).
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
        END CASE.
    END METHOD.     

    METHOD PRIVATE CHARACTER pGetConfigType (ipcConfig AS CHARACTER):
        CASE ipcConfig:
            WHEN "SSLoadTag" THEN
                RETURN cCONFIG_TYPE_JSON.
            WHEN "SSLoadTagJobDesign" THEN
                RETURN cCONFIG_TYPE_JSON.
        END CASE.
    END METHOD. 
END CLASS.
