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
    File        : INavigationTarget
    Purpose     : 
    Syntax      : 
    Description : An Interface declaring the members of a nativation target 
                  (data source) component. 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 17 14:38:02 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartNavigationTarget:  
  
    /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSourceState of the current Class. 
                 This Property to Updating if the Method BeginUpdateState () of the
                 SmartDataAdapter instance is called and it is set to Navigatable
                 during initialization and if the Property SmartTableIOState of a
                 Viewer is set to anything but ModifyingData. 
                 Enforced by Interface ISmartDataSource.
        Notes:   Possible Values (DataSourceStateEnum):
                     Navigatable
                     Updating
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSourceState AS CHARACTER NO-UNDO 
    GET.
  
    /*------------------------------------------------------------------------------
        Purpose: Get and Set the current SmartNavigationSource of the SmartDataAdapter.
                 Every SmartNavigationTarget is registered with handshake and kept in a
                 collection.
                 This Property is set by the developer when initializing a Control 
                 implementing the Interface ISmartDataSource.
        Notes:   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY SmartNavigationSource AS ISmartNavigationSource NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Returns a Value of RecordPositionEnum representing the current condition.
                 The Property is read in the Method PositionChangedHandler () of a
                 Class implementing the Interface ISmartNavigationTarget.
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
        Purpose: This Method moves to the first DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchFirst () .
    
    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the previous DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.   
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchPrev () .
    
    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the next DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchNext () .
    
    /*------------------------------------------------------------------------------
        Purpose: This Method moves to the last DataRow inside the BindingSource.
        Notes:   It is called by The Navigation Tools of the Toolbars.
                 Enforced by Interface ISmartNavigationTarget.
        @return Logical value, currently not used   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchLast () .
 
END INTERFACE.
