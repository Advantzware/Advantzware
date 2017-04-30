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
    File        : ValueList
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:52:48,798+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.ValueListDatasetModel
    INHERITS ValueListDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValueListDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValueListDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValueListDatasetModel class
        Notes:   
        @param pValueListGuid The filter value for the eSmartValueList.ValueListGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValueListDatasetModel (pValueListGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartValueList:Filter:ValueListGuid:EQ (pValueListGuid) .

        THIS-OBJECT:SmartValueList:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
