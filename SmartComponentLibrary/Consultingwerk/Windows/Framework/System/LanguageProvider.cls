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
    File        : LanguageProvider
    Purpose     : SmartFramework dependent ILanguageProvider implementation
                  Retrieves languages from the SmartDB and the current 
                  language from the Windows locale
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 23:07:10 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                      FROM PROPATH . 
USING Consultingwerk.OERA.*                           FROM PROPATH . 
USING Consultingwerk.SmartFramework.ILanguageProvider FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*       FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Threading.*                              FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Framework.System.LanguageProvider 
    IMPLEMENTS ILanguageProvider: 

    {Consultingwerk/SmartFramework/ttLanguage.i &NO-BEFORE}  
    {Consultingwerk/SmartFramework/System/dsLanguage.i}
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ttLanguage Temp-Table with the list of available 
                 languages
        Notes:   
        @param ttLanguage OUTPUT Temp-Table with the list of available languages
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetAvailableLanguages (OUTPUT TABLE ttLanguage):
       
        EMPTY TEMP-TABLE ttLanguage . 
       
        FrameworkSettings:ServiceAdapter:RetrieveData ("":U, 
                                                       "Consultingwerk.SmartFramework.System.LanguageBusinessEntity":U,
                                                       NEW FetchDataRequest ("eSmartLanguage":U),
                                                       OUTPUT DATASET dsLanguage) .
        
        FOR EACH eSmartLanguage:
        
            CREATE ttLanguage .
            BUFFER-COPY eSmartLanguage TO ttLanguage
                ASSIGN ttLanguage.LanguageKey = eSmartLanguage.LanguageGuid .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the code of the session default language
        Notes:   
        @return The character key of the session default language or ? when unknown
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSessionDefaultLanguage ():
        
        IF NOT AVAILABLE ttLanguage THEN 
            GetAvailableLanguages (OUTPUT TABLE ttLanguage) .
        
        FIND FIRST ttLanguage WHERE ttLanguage.LanguageIsoCode = Thread:CurrentThread:CurrentUICulture:Name NO-ERROR .
        
        IF AVAILABLE ttLanguage THEN 
            RETURN ttLanguage.LanguageKey .

        FIND FIRST ttLanguage WHERE ttLanguage.LanguageIsoCode = Thread:CurrentThread:CurrentUICulture:TwoLetterISOLanguageName NO-ERROR .

        IF AVAILABLE ttLanguage THEN 
            RETURN ttLanguage.LanguageKey .

        RETURN ? .
        
    END METHOD . 

END CLASS.
