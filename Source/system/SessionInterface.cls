
/*------------------------------------------------------------------------
    File        : system\SessionInterfcae
    Purpose     : 
    Syntax      : 
    Description : Common interface with get and set methods
    Author(s)   : Rahul Rawat
    Created     : Thu Sep 03 01:22:01 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE system.SessionInterface:
     
	/*------------------------------------------------------------------------------
	 Purpose:Set the value for a given key. Overwrites value if already available
	 Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetValue(INPUT ipcKey AS CHARACTER, INPUT ipcValue AS CHARACTER).


	/*------------------------------------------------------------------------------
	 Purpose: Returns value for a given key & deletes the key
	 Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER ConsumeValue(INPUT ipcKey AS CHARACTER).
	
	

	/*------------------------------------------------------------------------------
	 Purpose: Returns the value for a given key (key value referece is kept alive)
	 Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetValue(INPUT ipcKey AS CHARACTER ).
	
	
	
	/*------------------------------------------------------------------------------
     Purpose: Creates a value for a given key, if already available then appends the value 
              (Values are '|' pipe separated)
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetValueAppend(INPUT ipcKey AS CHARACTER, INPUT ipcValue AS CHARACTER).
    
    
    
    /*------------------------------------------------------------------------------
     Purpose: Deletes the value for a given key
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeleteValue(INPUT ipcKey AS CHARACTER).
    
END INTERFACE.