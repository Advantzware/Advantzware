
/*------------------------------------------------------------------------
    File        : system.Config
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Rajesh
    Created     : Mon Jan 11 13:05:27 EST 2021
    Notes       : Config Interface
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE system.Config:       
    METHOD PUBLIC CHARACTER GetAttributeValue (ipcKey AS CHARACTER ).
    
    METHOD PUBLIC CHARACTER GetAttributeValue (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER).
    
    METHOD PUBLIC CHARACTER GetAttributeValue (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER, ipcSubAttribute AS CHARACTER).
    
    METHOD PUBLIC LOGICAL IsAttributeAvailable (ipcKey AS CHARACTER ).
    
    METHOD PUBLIC LOGICAL IsAttributeAvailable (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER).
    
    METHOD PUBLIC LOGICAL IsAttributeAvailable (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER, ipcSubAttribute AS CHARACTER).    
END INTERFACE.