 
 /*------------------------------------------------------------------------
    File        : system/SharedConfig
    Purpose     : 
    Syntax      : 
    Description : Fetch and store the shared variables in temp-table based 
                  on key & value
    Author(s)   : Rahul Rawat
    Created     : Thu Sep 03 02:54:56 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING system.SessionInterface.

CLASS system.SharedConfig IMPLEMENTS SessionInterface: 
        
    DEFINE TEMP-TABLE ttKeyValue NO-UNDO
       FIELD cKey   AS CHARACTER 
       FIELD cValue AS CHARACTER
       INDEX cKey   IS  PRIMARY UNIQUE cKey.
    
    DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS system.SharedConfig NO-UNDO
    GET ():
        /* Returns reference to the single allowed instance. Actually instantiates
        the class if no instance is active */
        IF Instance = ? THEN 
            Instance = NEW system.SharedConfig( ).
        RETURN Instance.
    END GET.
    PRIVATE SET.


    /*------------------------------------------------------------------------------
     Purpose: Set the value for a given key. Overwrites value if already available
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetValue(INPUT ipcKey AS CHARACTER, INPUT ipcValue AS CHARACTER):
        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cKey EQ ipcKey
             NO-ERROR.
        IF NOT AVAILABLE ttKeyValue THEN DO: 
            CREATE ttKeyValue.
            ttKeyValue.cKey = ipcKey.
        END.

        ttKeyValue.cValue = ipcValue.
    END.     

    /*------------------------------------------------------------------------------
     Purpose: Returns value for a given key & deletes the key
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ConsumeValue(INPUT ipcKey AS CHARACTER):
        
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        
        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cKey EQ ipcKey
             NO-ERROR.
        IF AVAILABLE ttKeyValue THEN DO:
            cValue = ttKeyValue.cValue.
            DELETE ttKeyValue.
        END.  
        RETURN cValue.         
    END. 
    
    /*------------------------------------------------------------------------------
     Purpose: Returns the value for a given key (key value referece is kept alive)
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcKey AS CHARACTER):
        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cKey EQ ipcKey
             NO-ERROR.
        IF AVAILABLE ttKeyValue THEN 
            RETURN ttKeyValue.cValue.
        ELSE 
            RETURN "".              
    END.
    
    /*------------------------------------------------------------------------------
     Purpose: Creates a value for a given key, if already available then appends the value 
              (Values are '|' pipe separated)
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetValueAppend(INPUT ipcKey AS CHARACTER, INPUT ipcValue AS CHARACTER):
        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cKey EQ ipcKey
             NO-ERROR.
        IF AVAILABLE ttKeyValue THEN 
            ttKeyValue.cValue = ttKeyValue.cValue + "|" + ipcValue.  
        
        ELSE 
            SetValue(INPUT ipcKey, INPUT ipcValue).    
    END.                   


    /*------------------------------------------------------------------------------
     Purpose: Deletes the value for a given key
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteValue(INPUT ipcKey AS CHARACTER):
        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cKey EQ ipcKey
             NO-ERROR.
             
        IF AVAILABLE ttKeyValue THEN 
            DELETE ttKeyValue.     
    END.    

	/*------------------------------------------------------------------------------
	 Purpose: Called when object is deleted
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC SharedConfig ( ):
	   instance = ?.
	   EMPTY TEMP-TABLE ttKeyValue.
	END DESTRUCTOR.


END CLASS.