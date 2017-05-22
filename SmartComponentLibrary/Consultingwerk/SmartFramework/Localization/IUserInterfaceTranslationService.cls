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
    File        : IUserInterfaceTranslationService
    Purpose     : Provides connection to a translation repository
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 10 10:00:14 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.*       FROM PROPATH . 
USING Consultingwerk.SmartFramework.Localization.* FROM PROPATH .  
USING Progress.Lang.*                              FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.Localization.IUserInterfaceTranslationService:  
  
    { Consultingwerk/SmartFramework/Localization/eStringResources.i }
  
    /*------------------------------------------------------------------------------
        Purpose: Event raised to allow filtering out a resource string for translation 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The FilterStringResourceEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT FilterStringResource SIGNATURE VOID (sender AS Progress.Lang.Object, 
                                                             e AS FilterStringResourceEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Event raised to allow obtaining customized translation 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ObtainTranslationEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT ObtainTranslation SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                          e AS ObtainTranslationEventArgs).


    /*------------------------------------------------------------------------------
        Purpose: Event raised to allow customizing how a translation is stored 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The StoreTranslationEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT StoreTranslation SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                         e AS StoreTranslationEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Returns the Partial match key for the given resource key
        Notes:   
        @param pcResourceKey The original resource key 
        @return The partial match key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetPartialMatchKey (pcResourceKey AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the String Resources Table for the named UI object 
        Notes:   
        @param pcObjectName The name of the object to translate (class name)
        @param poOriginalStrings The dictionary with the original UI strings
        @param eStringResources OUTPUT The table with the string resources
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslationsForObject (pcObjectName AS CHARACTER,
                                                 poOriginalStrings AS CharacterDictionary,
                                                 OUTPUT TABLE eStringResources).

    /*------------------------------------------------------------------------------
        Purpose: Returns the translated String Resources Table for the named UI object 
        Notes:   
        @param pcObjectName The name of the object to translate (class name)
        @param eStringResources INPUT-OUTPUT The table with the string resources (input the original strings, output the translated)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslationsForObject (pcObjectName AS CHARACTER,
                                                 INPUT-OUTPUT TABLE eStringResources).

    /*------------------------------------------------------------------------------
        Purpose: Stores the String Resources Table for the named UI object 
        Notes:   
        @param pcObjectName The name of the object to translate (class name)
        @param eStringResources INPUT-OUTPUT The table with the string resources
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreTranslationsForObject (pcObjectName AS CHARACTER,
                                                   INPUT-OUTPUT TABLE eStringResources).

END INTERFACE.
