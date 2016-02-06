/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : IDataSource
    Purpose     : Interface for components providing data to other components 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 17 17:02:07 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .
USING System.* FROM ASSEMBLY . 

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartDataSource:  

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the QueryPosition has changed (i.e. Add or 
                 Delete and Grids etc. need to update the current position)
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterQueryPositionChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Event fired when a DataTarget of this SmartDataSource enters 
                 Update State
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The DataSourceUpdateStateEventArgs instance with the data for this event                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeginUpdateState SIGNATURE VOID (sender AS System.Object,
                                                         e AS DataSourceUpdateStateEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired whenever the SmartDataAdapters BindingSource property 
                 changes
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The BindingSourceChangedEventArgs instance with the data for this event                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BindingSourceChanged SIGNATURE VOID (sender AS System.Object,
                                                             e AS BindingSourceChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired when a DataTarget of this SmartDataSource enters 
                 Update State
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The DataSourceUpdateStateEventArgs instance with the data for this event                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT EndUpdateState SIGNATURE VOID (sender AS System.Object,
                                                       e AS DataSourceUpdateStateEventArgs).
                                                             
    /*------------------------------------------------------------------------------
        Purpose: Returns if the DataSource currently has a record available 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Available AS LOGICAL NO-UNDO 
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
        Purpose: Get/Set the SmartDataSourceState of the current Class. 
                 This Property to Updating if the Method BeginUpdateState () of the
                 SmartDataAdapter instance is called and it is set to Navigatable
                 during initialization and if the Property SmartTableIOState of a
                 Viewer is set to anything but ModifyingData. 
        Notes:   Possible Values (DataSourceStateEnum):
                     Navigatable
                     Updating
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSourceState AS CHARACTER NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: The Message to for instance Disable a Browser is send to all 
                 SmartDataTargets except the calling one
        Notes:   This Method is called from the SmartViewerControl derived Class when
                 starting to Modify or Add data.
                 Enforced by Interface ISmartDataSource.
        @param poSmartDataTarget The reference to the ISmartDataTarget which is in UpdateState now   
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID BeginUpdateState (poSmartDataTarget AS ISmartDataTarget).

    /*------------------------------------------------------------------------------
        Purpose: Invoked from the SmartDataTarget to create a new row row                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID CreateRecord ().
  
    /*------------------------------------------------------------------------------
        Purpose: Invoked from the SmartDataTarget to cancel the creation of a new 
                 row                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID CancelCreateRecord ().

      /*------------------------------------------------------------------------------
        Purpose: The DeleteRow is an abstract method - that means that it needs 
                 to be overridden in a class that inherits from this base class.
        Notes:   Do NOT call SUPER:DeleteRow() in the overriding method.
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID DeleteRow ().

    /*------------------------------------------------------------------------------
        Purpose: The Message to for instance enable a Browser is send to all 
                 SmartDataTargets except the calling one.
        Notes:   This Method is called from the SmartViewerControl derived Class when
                 starting to Modify or Add data.
                 Enforced by Interface ISmartDataSource.
        @param poSmartDataTarget The reference to the ISmartDataTarget that ends the UpdateState    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EndUpdateState (poSmartDataTarget AS ISmartDataTarget).

    /*------------------------------------------------------------------------------
        Purpose: Returns a CHR(1) delimited list of field values from the 
                 DataSource                                                                        
        Notes:    
        @param pcFields The comma delimited list of fields to return
        @return The CHR(1) delimited list of field values                                                                    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC CHARACTER GetFieldValues (pcFields AS CHARACTER).

     /*------------------------------------------------------------------------------
        Purpose: Register a SmartDataTarget with the SmartDataSource.
                 This Method is called by Classes implementing the ISmartDataTarget
                 Interface when a new SmartDataSource is set.
        Notes:   After a new SmartDataTargetCollection Row is created this routine 
                 tests if there is a valid BindingSource inside the new SmartDataTarget.
                 If there is one, the BindingSource of the SmartDataAdapter is disposed
                 and replaced by the one of the SmartDataTarget. A new BindingSource is
                 then populated to all SmartDataTargets of this SmartDataAdapter.
        @param poSmartDataTarget The reference to the ISmartDataTarget to add to this SmartDataAdapter                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartDataTarget (poSmartDataTarget AS ISmartDataTarget).
    
     /*------------------------------------------------------------------------------
        Purpose: Deregister a SmartDataTarget with the SmartDataSource. Usually done 
                 when a SmartDataTarget is deleted and the SmartDataSource is still
                 running
        Notes:   
        @param poSmartDataTarget The reference to the ISmartDataTarget to deregister   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartDataTarget (poSmartDataTarget AS ISmartDataTarget).
        
     /*------------------------------------------------------------------------------
        Purpose: The UpdateRow is an abstract method - that means that it needs 
                 to be overridden in a class that inherits from this base class.
        Notes:   Do NOT call SUPER:UpdateRow() in the overriding method.
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC VOID UpdateRow () .
    
     /*------------------------------------------------------------------------------
        Purpose: RetrieveData is an abstract method - that means that it needs 
                 to be overridden in a class that inherits from the base class.
        Notes:   Do NOT call SUPER:RetrieveData() in the overriding method.
                 Remember to test if a BindingSource exists. If not call 
                 CreateBindingSource from the base class.
        @return Logical value indicating the success of the method, not always used
    ------------------------------------------------------------------------------*/        
    METHOD PUBLIC LOGICAL RetrieveData ().
    
END INTERFACE.
