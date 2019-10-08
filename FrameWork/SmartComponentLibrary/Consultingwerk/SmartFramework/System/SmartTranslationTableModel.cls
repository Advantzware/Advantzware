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
    File        : SmartTranslationTableModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:50:27,385+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.SmartTranslationTableModel 
    INHERITS SmartTranslationTableModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTranslationTableModel class
        Notes:   
        @param poDatasetModel The reference to the owning DatasetModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTranslationTableModel (poDatasetModel AS DatasetModel, phBufferHandle AS HANDLE):
        
        SUPER (poDatasetModel, phBufferHandle).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTranslationTableModel class
        Notes:   Creates a view model
        @param poTableModel The reference to SmartTranslationTableModel on which this SmartTranslationTableModel servers as a ViewModel
        @param phBufferHandle The handle of the ProDataset member buffer this TableModel represents
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartTranslationTableModel (poTableModel AS SmartTranslationTableModel, phBufferHandle AS HANDLE):
        
        SUPER (poTableModel, phBufferHandle).
        
    END CONSTRUCTOR.

END CLASS.
