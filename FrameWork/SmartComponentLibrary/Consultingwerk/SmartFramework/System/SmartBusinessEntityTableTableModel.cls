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
    File        : SmartBusinessEntityTableTableModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17/10/2013 19:01:56,377+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartBusinessEntityTableTableModel 
    INHERITS SmartBusinessEntityTableTableModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityTableTableModel class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityTableTableModel (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityTableTableModel class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartBusinessEntityTableTableModel on which this SmartBusinessEntityTableTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityTableTableModel (poTableModel AS SmartBusinessEntityTableTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

END CLASS.
