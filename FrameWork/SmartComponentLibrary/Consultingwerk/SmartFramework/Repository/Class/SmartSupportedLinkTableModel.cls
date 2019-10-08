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
    File        : SmartSupportedLinkTableModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03/05/2016 23:06:51,138+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.SupportedLinkBusinessEntity", type="TableModel") .

CLASS Consultingwerk.SmartFramework.Repository.Class.SmartSupportedLinkTableModel 
    INHERITS SmartSupportedLinkTableModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSupportedLinkTableModel class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSupportedLinkTableModel (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartSupportedLinkTableModel class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartSupportedLinkTableModel on which this SmartSupportedLinkTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartSupportedLinkTableModel (poTableModel AS SmartSupportedLinkTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

END CLASS.
