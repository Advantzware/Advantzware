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
    File        : Module
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:46:10,368+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.ModuleDatasetModel
    INHERITS ModuleDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModuleDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModuleDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ModuleDatasetModel class
        Notes:   
        @param pModuleGuid The filter value for the eSmartModule.ModuleGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ModuleDatasetModel (pModuleGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartModule:Filter:ModuleGuid:EQ (pModuleGuid) .

        THIS-OBJECT:SmartModule:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
