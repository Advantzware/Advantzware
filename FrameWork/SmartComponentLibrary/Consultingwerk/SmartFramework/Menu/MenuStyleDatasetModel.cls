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
    File        : MenuStyle
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 09/08/2013 08:55:32,974+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.MenuStyleDatasetModel
    INHERITS MenuStyleDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuStyleDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuStyleDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuStyleDatasetModel class
        Notes:   
        @param pMenuStyleGuid The filter value for the eSmartMenuStyle.MenuStyleGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuStyleDatasetModel (pMenuStyleGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartMenuStyle:Filter:MenuStyleGuid:EQ (pMenuStyleGuid) .

        THIS-OBJECT:SmartMenuStyle:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
