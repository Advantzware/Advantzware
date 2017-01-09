
/*------------------------------------------------------------------------
    File        : ISupportsScrollup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 25 14:46:23 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISupportsScrollup:  
  
    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the BatchSize used when reading data from the Business 
                 Entity. Batching is used for the EntityTable, not the view 
                 tables!
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BatchSize AS INTEGER NO-UNDO 
    GET.
  
    /*------------------------------------------------------------------------------
        Purpose: Get/Set the BindingSource of the current Class. The BindingSource 
                 can be set to any class derived from Progress.Data.BindingSource.
                 This Property is set internaly if the SmartDataAdapter is attached
                 to a Browser where a DesignTime BindingSource is attached or if
                 the developer calls RetrieveData (). 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns true if a previous batch is available.                                                                       
        Notes:              
        @return Logical value indicating if a previous batch is available                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL PreviousBatchAvailable () .
      
    /*------------------------------------------------------------------------------
        Purpose: Returns a Value of RecordPositionEnum representing the current condition.
                 The Property is read in the Method PositionChangedHandler () of a
                 Class implementing the Interface ISmartNavigationTarget.
                 Enforced by Interface ISmartNavigationTarget.
        Notes:   Possible Values (RecordPositionEnum):
                     NoRecordAvailable
                     OnlyRecord
                     FirstRecord
                     LastRecord
                     NotFirstOrLast
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RecordPosition AS CHARACTER NO-UNDO 
    GET.  
  
    /*------------------------------------------------------------------------------
        Purpose: Fetches the Previous Batch of records                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OffHome ().
  
END INTERFACE.