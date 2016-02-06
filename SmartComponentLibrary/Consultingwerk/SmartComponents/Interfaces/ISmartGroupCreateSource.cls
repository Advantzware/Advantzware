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
    File        : ISmartGroupCreateSource
    Purpose     : Interface to support the GroupCreate Link
    Syntax      : 
    Description : Implemented by the SmartBusinessEntityAdapter and the 
                  SmartDatasetChildAdapter (to support chaining) using the
                  common base class SmartGroupCreateSource
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 02 11:05:25 CET 2012
    Notes       : The GroupCreate Link is only supported from 
                  SmartBusinessEntityAdapter to SmartDatasetChildAdapter and
                  SmartDatasetChildAdapter to SmartDatasetChildAdapter
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.*       FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartGroupCreateSource:  

    /*------------------------------------------------------------------------------
        Purpose: Event raised when a ISmartGroupCreateSource needs to create a 
                 record to allow creation of a child record in a ISmartGroupCreateTarget
                 instance. 
        Notes:   The purpose of this event is that an event handler can create 
                 meaningful key values. When done an event handler is supposed to 
                 ASSIGN e:Assigned = TRUE . 
        @param sender The object that raised the AssignKeyFields event
        @param e The AssignKeyFieldsEventArgs instance with the event data
    ------------------------------------------------------------------------------*/     
    DEFINE PUBLIC EVENT AssignKeyFields SIGNATURE VOID (sender AS System.Object, 
                                                        e AS AssignKeyFieldsEventArgs) .

    /*------------------------------------------------------------------------------
        Purpose: Returns if this SmartDatasetAdapter is currently creating a new
                 record
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreatingRecord AS LOGICAL NO-UNDO 
    GET.

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
        Purpose: Creates a parent record when needed for creating a record in a
                 SmartGroupCreateTarget                                                                      
        Notes:   Creates a record and attempts to assign the primary key values
        @param pcKeyFields The Key Fields to assign                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateParentRecord (pcKeyFields AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Deregister a SmartGroupCreateTarget with the SmartGroupCreateSource
                 
        Notes:   Enforced by Interface ISmartGroupCreateSource.
        @param poSmartGroupCreateTarget The ISmartGroupCreateTarget to deregister    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartGroupCreateTarget (poSmartGroupCreateTarget AS ISmartGroupCreateTarget) .  
  
    /*------------------------------------------------------------------------------
        Purpose: Register a SmartGroupCreateTarget with the SmartGroupCreateSource
                 This Method is executed every time a new SmartGroupCreateSource is
                 registered to the corresponding Property of a Class implementing the
                 Interface ISmartGroupCreateSource.
        Notes:   Enforced by Interface ISmartGroupCreateSource.
        @param poSmartGroupCreateTarget The ISmartGroupCreateTarget to register    
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID RegisterSmartGroupCreateTarget (poSmartGroupCreateTarget AS ISmartGroupCreateTarget) .  

    /*------------------------------------------------------------------------------
        Purpose: Saves the data in this ISmartGroupCreateSource related updating 
                 ISmartDataTarget so that before saving a new child record, that 
                 parent record is created      
        Notes:  
        @param poSmartGroupCreateTarget The ISmartGroupCreateTarget that invoked this Save operation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveFromGroupCreateTarget (poSmartGroupCreateTarget AS ISmartGroupCreateTarget).

END INTERFACE.
