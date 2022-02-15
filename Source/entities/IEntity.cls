
/*------------------------------------------------------------------------
    File        : IEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : gshde
    Created     : Sat Aug 28 23:50:03 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE entities.IEntity:  
      
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL Find( cFilter AS CHARACTER ).

END INTERFACE.