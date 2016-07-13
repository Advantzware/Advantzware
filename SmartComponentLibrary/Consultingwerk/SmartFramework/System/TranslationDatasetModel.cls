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
    File        : Translation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:50:27,387+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.TranslationDatasetModel
    INHERITS TranslationDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TranslationDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TranslationDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TranslationDatasetModel class
        Notes:   
        @param pTranslationScope The filter value for the eSmartTranslation.TranslationScope field
        @param pOriginalString The filter value for the eSmartTranslation.OriginalString field
        @param pLanguageGuid The filter value for the eSmartTranslation.LanguageGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TranslationDatasetModel (pTranslationScope AS CHARACTER, pOriginalString AS CHARACTER, pLanguageGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartTranslation:Filter:TranslationScope:EQ (pTranslationScope) .
        THIS-OBJECT:SmartTranslation:Filter:OriginalString:EQ (pOriginalString) .
        THIS-OBJECT:SmartTranslation:Filter:LanguageGuid:EQ (pLanguageGuid) .

        THIS-OBJECT:SmartTranslation:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TranslationDatasetModel class
        Notes:   
        @param pTranslationGuid The filter value for the eSmartTranslation.TranslationGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TranslationDatasetModel (pTranslationGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartTranslation:Filter:TranslationGuid:EQ (pTranslationGuid) .

        THIS-OBJECT:SmartTranslation:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
