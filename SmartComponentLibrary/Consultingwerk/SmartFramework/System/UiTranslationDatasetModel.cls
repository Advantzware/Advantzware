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
    File        : UiTranslation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:50:53,526+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.UiTranslationDatasetModel
    INHERITS UiTranslationDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UiTranslationDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UiTranslationDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UiTranslationDatasetModel class
        Notes:   
        @param pLanguageGuid The filter value for the eSmartUiTranslation.LanguageGuid field
        @param pObjectName The filter value for the eSmartUiTranslation.ObjectName field
        @param pOriginalString The filter value for the eSmartUiTranslation.OriginalString field
        @param pResourceKey The filter value for the eSmartUiTranslation.ResourceKey field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UiTranslationDatasetModel (pLanguageGuid AS CHARACTER, pObjectName AS CHARACTER, pOriginalString AS CHARACTER, pResourceKey AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartUiTranslation:Filter:LanguageGuid:EQ (pLanguageGuid) .
        THIS-OBJECT:SmartUiTranslation:Filter:ObjectName:EQ (pObjectName) .
        THIS-OBJECT:SmartUiTranslation:Filter:OriginalString:EQ (pOriginalString) .
        THIS-OBJECT:SmartUiTranslation:Filter:ResourceKey:EQ (pResourceKey) .

        THIS-OBJECT:SmartUiTranslation:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UiTranslationDatasetModel class
        Notes:   
        @param pLanguageGuid The filter value for the eSmartUiTranslation.LanguageGuid field
        @param pObjectName The filter value for the eSmartUiTranslation.ObjectName field
        @param pResourceKey The filter value for the eSmartUiTranslation.ResourceKey field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UiTranslationDatasetModel (pLanguageGuid AS CHARACTER, pObjectName AS CHARACTER, pResourceKey AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartUiTranslation:Filter:LanguageGuid:EQ (pLanguageGuid) .
        THIS-OBJECT:SmartUiTranslation:Filter:ObjectName:EQ (pObjectName) .
        THIS-OBJECT:SmartUiTranslation:Filter:ResourceKey:EQ (pResourceKey) .

        THIS-OBJECT:SmartUiTranslation:Filter:Run () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UiTranslationDatasetModel class
        Notes:   
        @param pUiTranslationGuid The filter value for the eSmartUiTranslation.UiTranslationGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UiTranslationDatasetModel (pUiTranslationGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartUiTranslation:Filter:UiTranslationGuid:EQ (pUiTranslationGuid) .

        THIS-OBJECT:SmartUiTranslation:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.
