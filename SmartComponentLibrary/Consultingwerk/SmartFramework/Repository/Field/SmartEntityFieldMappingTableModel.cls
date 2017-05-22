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
    File        : SmartEntityFieldMappingTableModel
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05/11/2016 08:47:49,656+01:00
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Field.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingBusinessEntity", type="TableModel") .

CLASS Consultingwerk.SmartFramework.Repository.Field.SmartEntityFieldMappingTableModel
    INHERITS SmartEntityFieldMappingTableModel_Generated:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartEntityFieldMappingTableModel class
        Notes:
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartEntityFieldMappingTableModel (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):

        SUPER (poDatasetModel, phBufferHandle).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartEntityFieldMappingTableModel class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartEntityFieldMappingTableModel on which this SmartEntityFieldMappingTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartEntityFieldMappingTableModel (poTableModel AS SmartEntityFieldMappingTableModel, phBufferHandle AS HANDLE):

        SUPER (poTableModel, phBufferHandle).

    END CONSTRUCTOR.

END CLASS.
