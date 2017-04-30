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
    File        : ObtainTranslationEventArgs
    Purpose     : Event argument class for the ObtainTranslation event of the 
                  IUserInterfaceTranslationService  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 14 06:34:06 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                             FROM PROPATH .
USING Consultingwerk.Assertion.*                   FROM PROPATH .
USING Consultingwerk.SmartFramework.Localization.* FROM PROPATH .  
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Localization.ObtainTranslationEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the translation was handled
        Notes:   Set this to TRUE when you provided a translation
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Handled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the user control or form that is being processed
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the original string of the element that the event is raised
                 for 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OriginalString AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Resource Key for the element that the event is raised 
                 for 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResourceKey AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Get and sets the partial match of the Resource Key that was used to
                 obtain the current translation 
        Notes:   Set this when a (global) translation was obtained through a partial 
                 match, e.g. when numbers where replaced with *
                 The IUserinterfaceTranslationService is expected to replace numbers
                 in the resource key with * here
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResourceKeyPartial AS CHARACTER NO-UNDO INIT ?
    GET.
    SET.     

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the translated string for the element that the event 
                 is raised for
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TranslatedString AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the TranslationType that was used to obtain the 
                 translation
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TranslationType AS CHARACTER NO-UNDO 
    GET.
    SET.     

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObtainTranslationEventArgs class
        Notes:   
        @param pcObjectName The name of the user control or form that is being processed
        @param pcResourceKey The Resource Key for the element that the event is raised for 
        @param pcResourceKeyPartial The partial match of the Resource Key that was used to obtain the current translation 
        @param pcOriginalString The original string of the element that the event is raised for
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObtainTranslationEventArgs (pcObjectName AS CHARACTER,
                                                   pcResourceKey AS CHARACTER,
                                                   pcResourceKeyPartial AS CHARACTER,
                                                   pcOriginalString AS CHARACTER):
        SUPER ().
        
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcObjectName} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcResourceKey} .

        ASSIGN THIS-OBJECT:ObjectName         = pcObjectName
               THIS-OBJECT:ResourceKey        = pcResourceKey
               THIS-OBJECT:ResourceKeyPartial = pcResourceKeyPartial
               THIS-OBJECT:OriginalString     = pcOriginalString .
        
    END CONSTRUCTOR.

END CLASS.
