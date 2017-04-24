/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartSchedulerJobTableModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : 25/05/2016 19:55:57,570+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Scheduler.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Scheduler.SchedulerJobBusinessEntity", type="TableModel") .

CLASS Consultingwerk.SmartFramework.Scheduler.SmartSchedulerJobTableModel 
    INHERITS SmartSchedulerJobTableModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobTableModel class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSchedulerJobTableModel (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSchedulerJobTableModel class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartSchedulerJobTableModel on which this SmartSchedulerJobTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSchedulerJobTableModel (poTableModel AS SmartSchedulerJobTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

END CLASS.
