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
    File        : LinkType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21/05/2014 10:28:00,067+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.Class.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Repository.Class.LinkTypeDatasetModel
    INHERITS LinkTypeDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LinkTypeDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LinkTypeDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LinkTypeDatasetModel class
        Notes:   
        @param pLinkTypeGuid The filter value for the eSmartLinkType.LinkTypeGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LinkTypeDatasetModel (pLinkTypeGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartLinkType:Filter:LinkTypeGuid:EQ (pLinkTypeGuid) .

        THIS-OBJECT:SmartLinkType:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
