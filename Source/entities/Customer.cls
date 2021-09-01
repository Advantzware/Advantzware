 
 /*------------------------------------------------------------------------
    File        : Customer
    Syntax      : 
    Author(s)   : gshde
    Created     : Sat Aug 28 15:49:07 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/


USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS entities.Customer INHERITS entities.Entity:
    {entities/customer.i}
      
	/*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Customer(dsEntity AS HANDLE, srcEntity AS HANDLE):  
        SUPER (dsEntity,  srcEntity). 
        
        IF SEARCH ("C:\temp\customer.json") NE ? THEN
            LoadAttributes ("C:\temp\customer.json").
    END CONSTRUCTOR.
    
END CLASS.
