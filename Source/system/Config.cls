
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

INTERFACE System.Config:       
    METHOD PUBLIC CHARACTER GetAttributeValue (ipcKey AS CHARACTER ).
    
    METHOD PUBLIC CHARACTER GetAttributeValue (ipcConfig AS CHARACTER, ipcAttribute AS CHARACTER).
END INTERFACE.