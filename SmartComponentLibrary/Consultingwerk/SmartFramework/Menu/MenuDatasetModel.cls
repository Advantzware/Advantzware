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
    File        : Menu
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 09/08/2013 09:13:00,452+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.MenuDatasetModel
    INHERITS MenuDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuDatasetModel class
        Notes:   
        @param pMenuGuid The filter value for the eSmartMenu.MenuGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuDatasetModel (pMenuGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartMenu:Filter:MenuGuid:EQ (PMenuGuid) .

        THIS-OBJECT:SmartMenu:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
