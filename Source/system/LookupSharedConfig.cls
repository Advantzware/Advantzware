 
 /*------------------------------------------------------------------------
    File        : system/LookupSharedConfig
    Purpose     : 
    Syntax      : 
    Description : Fetch and store the shared variables in temp-table based 
                  on key & value
    Author(s)   : DEVA$!
    Created     : Thu Apr 01 05:43:56 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS system.LookupSharedConfig: 

    DEFINE TEMP-TABLE ttKeyValue NO-UNDO
        FIELD cProgram AS CHARACTER
        FIELD cKey     AS CHARACTER 
        FIELD cValue   AS CHARACTER
        INDEX cProgramKey IS  PRIMARY UNIQUE cProgram cKey.
    
    DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS system.LookupSharedConfig NO-UNDO
    GET ():
        /* Returns reference to the single allowed instance. Actually instantiates
        the class if no instance is active */
        IF Instance = ? THEN 
            Instance = NEW system.LookupSharedConfig( ).
        RETURN Instance.
    END GET.
    PRIVATE SET.


    /*------------------------------------------------------------------------------
     Purpose: Set the value for a given key. Overwrites value if already available
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetValue(INPUT ipcKey AS CHARACTER, INPUT ipcValue AS CHARACTER):
        DEFINE VARIABLE cPrgmName AS CHARACTER NO-UNDO.
       
        ASSIGN
            cPrgmName = PROGRAM-NAME(2)
            cPrgmName = SUBSTRING(cPrgmName,1,INDEX(cPrgmName,"."))
            cPrgmName = ENTRY(NUM-ENTRIES(cPrgmName," "),cPrgmName," ")
            .
        
        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cProgram EQ cPrgmName
               AND ttKeyValue.cKey     EQ ipcKey
             NO-ERROR.
        IF NOT AVAILABLE ttKeyValue THEN DO: 
            CREATE ttKeyValue.
            ASSIGN
                ttKeyValue.cKey     = ipcKey
                ttKeyValue.cProgram = cPrgmName
                .
        END.

        ttKeyValue.cValue = ipcValue.
    END.     

    /*------------------------------------------------------------------------------
     Purpose: Returns the value for a given key (key value referece is kept alive)
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetValue(INPUT ipcProgram AS CHARACTER, INPUT ipcKey AS CHARACTER):

        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cProgram EQ ipcProgram
               AND LOOKUP(ipcKey, ttKeyValue.cKey) GT 0
             NO-ERROR.
        IF AVAILABLE ttKeyValue THEN 
            RETURN ttKeyValue.cValue.
        ELSE 
            RETURN "".              
    END.

    /*------------------------------------------------------------------------------
     Purpose: Deletes the value for a given key
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteValue(INPUT ipcProgram AS CHARACTER, INPUT ipcKey AS CHARACTER):
        FIND FIRST ttKeyValue 
             WHERE ttKeyValue.cProgram EQ ipcProgram
               AND ttKeyValue.cKey     EQ ipcKey
             NO-ERROR.             
        IF AVAILABLE ttKeyValue THEN 
            DELETE ttKeyValue.     
    END.    

    /*------------------------------------------------------------------------------
     Purpose: Deletes all the records for a given program
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteByProgram(INPUT ipcProgram AS CHARACTER):
        FOR EACH ttKeyValue 
            WHERE ttKeyValue.cProgram EQ ipcProgram:
            DELETE ttKeyValue.     
        END.
    END.  
    
    /*------------------------------------------------------------------------------
     Purpose: Deletes all records
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteAll():
        EMPTY TEMP-TABLE ttKeyValue.     
    END. 
    
	/*------------------------------------------------------------------------------
	 Purpose: Called when object is deleted
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC LookupSharedConfig ( ):
	   instance = ?.
	   EMPTY TEMP-TABLE ttKeyValue.
	END DESTRUCTOR.


END CLASS.