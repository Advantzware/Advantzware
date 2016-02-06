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
    File        : Group
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:36:03,638+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.GroupDatasetModel
    INHERITS GroupDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GroupDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GroupDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the GroupDatasetModel class
        Notes:   
        @param pGroupGuid The filter value for the eSmartGroup.GroupGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC GroupDatasetModel (pGroupGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartGroup:Filter:GroupGuid:EQ (pGroupGuid) .

        THIS-OBJECT:SmartGroup:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
