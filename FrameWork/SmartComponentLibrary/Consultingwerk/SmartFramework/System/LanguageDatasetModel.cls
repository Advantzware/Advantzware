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
    File        : Language
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 20/09/2013 09:09:55,873+02:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.LanguageDatasetModel
    INHERITS LanguageDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LanguageDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LanguageDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LanguageDatasetModel class
        Notes:   
        @param pLanguageGuid The filter value for the eSmartLanguage.LanguageGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LanguageDatasetModel (pLanguageGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartLanguage:Filter:LanguageGuid:EQ (pLanguageGuid) .

        THIS-OBJECT:SmartLanguage:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
