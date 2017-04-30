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
    File        : ITranslationProvider
    Purpose     : Interface for services that return translations for
                  strings based on an optional scope, the original string
                  or a key value
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 10 23:02:58 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.* FROM PROPATH .
USING Progress.Lang.*                 FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.ITranslationProvider:

    /**
     * Purpose: Returns if all translations have been cached
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AllTranslationsCached AS LOGICAL NO-UNDO
    GET.

    /**
     * Purpose: Caches all translations for the current language
     * Notes:   Useful to avoid AppServer calls when multiple translations are
     *          required
     */
    METHOD PUBLIC VOID CacheAllTranslations ().

    /**
     * Purpose: Caches all translations by a given scope
     * Notes:   Useful to avoid AppServer calls when multiple translations are
     *          required in a row
     * @param pcScope The scope to retrieve from the back end
     */
    METHOD PUBLIC VOID CacheTranslationsByScope (pcScope AS CHARACTER).

    /**
     * Purpose: Returns a translation based on a key value and an optional scope
     * Notes:   Returns ? when no translation is available
     * @param pcScope The optional translation scope
     * @param pcKey The translation key value to return
     * @return The translated string
     */
    METHOD PUBLIC CHARACTER GetTranslationByKey (pcScope AS CHARACTER,
                                                 pcKey AS CHARACTER).

    /**
     * Purpose: Returns a translation based on a key value and an optional scope
     * Notes:   Returns ? when no translation is available
     * @param pcScope The optional translation scope
     * @param pcKey The translation key value to return
     * @param pcDefaultString The optional default string to return when no translation is available
     * @return The translated string
     */
    METHOD PUBLIC CHARACTER GetTranslationByKey (pcScope AS CHARACTER,
                                                 pcKey AS CHARACTER,
                                                 pcDefaultString AS CHARACTER).

    /**
     * Purpose: Returns a translation for a string value and an optional scope
     * Notes:   Returns the original string when no translation is available
     * @param pcScope The optional translation scope
     * @param pcOriginalString The original string to translate
     * @return The translated string
     */
    METHOD PUBLIC CHARACTER GetTranslationByString (pcScope AS CHARACTER,
                                                    pcOriginalString AS CHARACTER).

END INTERFACE.
