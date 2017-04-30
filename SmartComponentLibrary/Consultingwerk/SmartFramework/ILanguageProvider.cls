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
    File        : ILanguageProvider
    Purpose     : Interface for services that provide the list of available languages
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 22:59:51 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.SmartFramework.ILanguageProvider:  
  
    {Consultingwerk/SmartFramework/ttLanguage.i &NO-BEFORE}  
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ttLanguage Temp-Table with the list of available 
                 languages
        Notes:   
        @param ttLanguage OUTPUT Temp-Table with the list of available languages
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetAvailableLanguages (OUTPUT TABLE ttLanguage).

    /*------------------------------------------------------------------------------
        Purpose: Returns the code of the session default language
        Notes:   
        @return The character key of the session default language or ? when unknown
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSessionDefaultLanguage ().

END INTERFACE.