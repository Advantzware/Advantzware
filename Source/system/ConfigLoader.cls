 
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
    DEFINE TEMP-TABLE ttClass
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
    
    METHOD PUBLIC Object GetConfig (ipcConfig AS CHARACTER):
        FIND FIRST ttClass
             WHERE ttClass.className  EQ ipcConfig
               AND ttClass.configType EQ cCONFIG_TYPE_JSON
             NO-ERROR.
        IF NOT AVAILABLE ttClass THEN DO:
            CREATE ttClass.
            ASSIGN
                ttClass.className  = ipcConfig
                ttClass.configType = cCONFIG_TYPE_JSON
                .
            
            IF pGetConfigType(ipcConfig) EQ cCONFIG_TYPE_JSON THEN
                ttClass.classInstance = NEW system.JSONConfig(pGetJSONConfigFile(ipcConfig)).
        END.
        
        RETURN ttClass.classInstance.
    END METHOD. 
    
    METHOD PRIVATE CHARACTER pGetJSONConfigFile (ipcConfig AS CHARACTER):
        CASE ipcConfig:
            WHEN "SSLoadTagJob" THEN
                RETURN SEARCH("system/SSLoadTagJob.json").
        END CASE.
    END METHOD.     

    METHOD PRIVATE CHARACTER pGetConfigType (ipcConfig AS CHARACTER):
        CASE ipcConfig:
            WHEN "SSLoadTagJob" THEN
                RETURN cCONFIG_TYPE_JSON.
        END CASE.
    END METHOD. 
END CLASS.
