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
    File        : IDatasetController
    Purpose     : Allows client side business logic that is executed 
                  independently from the actual part of the UI. Typically 
                  one IDatasetController instance would be used per Form.
    Syntax      : 
    Description : This Interface defines the required public members 
                  for classes that control a Dataset within the SmartComponent 
                  Library SmartBusinessEntityAdapter. Typically a class 
                  implementing this Interface would be the owner of a 
                  staticly defined ProDataset.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 14 19:35:52 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.IDatasetController:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the primary Dataset owned by the DatasetController
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.  

    /*------------------------------------------------------------------------------
        Purpose: Deregisters a Consumer with this DatasetController                                                                       
        Notes:   May be used to unsubscribe from events of the Consumer etc.
                 The consumer is of type Progress.Lang.Object and needs to be casted
                 to a useful type/Interface.                                                                        
        @param poConsumer The reference of the Object detaching from the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object).

    /*------------------------------------------------------------------------------
        Purpose: Registers a Consumer with this DatasetController                                                                     
        Notes:   May be used to subscribe to events from the Consumer etc.      
                 The consumer is of type Progress.Lang.Object and needs to be casted
                 to a useful type/Interface.                                                                 
        @param poConsumer The reference of the Object attaching to the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object).
      
END INTERFACE.
