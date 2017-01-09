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
    File        : ISmartBusinessEntityAdapterEvents
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Mar 10 11:02:22 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* .
USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Implementation.ISmartBusinessEntityAdapterEvents:  
  
    /*------------------------------------------------------------------------------
        Purpose: Event fired after the creation of a record has been cancelled
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AfterCancelCreateRecordEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterCancelCreateRecord SIGNATURE VOID (sender AS System.Object, 
                                                                e AS AfterCancelCreateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event fired after a record has been created
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AfterCreateRecordEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterCreateRecord SIGNATURE VOID (sender AS System.Object, 
                                                          e AS AfterCreateRecordEventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Event raised after a record has been deleted
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AfterDeleteRecordEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterDeleteRecord SIGNATURE VOID (sender AS System.Object, 
                                                          e AS AfterDeleteRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised after a record has been updated
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The AfterUpdateRecordEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterUpdateRecord SIGNATURE VOID (sender AS System.Object, 
                                                          e AS AfterUpdateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised before the creation of the record will be cancelled
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The BeforeCancelCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCancelCreateRecord SIGNATURE VOID (sender AS System.Object, 
                                                                 e AS BeforeCancelCreateRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised before the a record is created
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The BeforeCreateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeCreateRecord SIGNATURE VOID (sender AS System.Object, 
                                                           e AS BeforeCreateRecordEventArgs).
                                                           
    /*------------------------------------------------------------------------------
        Purpose: Raised before the a record is deleted
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The BeforeDeleteRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeDeleteRecord SIGNATURE VOID (sender AS System.Object, 
                                                           e AS BeforeDeleteRecordEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised before the a record is updated
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The BeforeUpdateRecordEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT BeforeUpdateRecord SIGNATURE VOID (sender AS System.Object, 
                                                           e AS BeforeUpdateRecordEventArgs).
  
END INTERFACE.
