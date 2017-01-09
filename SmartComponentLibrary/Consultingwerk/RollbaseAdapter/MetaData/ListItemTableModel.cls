/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ListItemTableModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05/09/2014 11:54:55,844+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.MetaData.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.MetaData.ListItemTableModel 
    INHERITS ListItemTableModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListItemTableModel class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListItemTableModel (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ListItemTableModel class
        Notes:   Creates a view model
        @param poTableModel The reference to ListItemTableModel on which this ListItemTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ListItemTableModel (poTableModel AS ListItemTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

END CLASS.
