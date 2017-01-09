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
    File        : TranslationProvider
    Purpose     : Default ITranslationProvider implementation that returns
                  translations for strings based on an optional scope,
                  the original string or a key value
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 10 23:09:45 CET 2013
    Notes       : The TranslationProvider is primarily intended to be used from
                  client side code. It can also be used on the AppServer,
                  however in those cases the caching (by the current language)
                  of translation records that are disposed when the language
                  changes may cause some overhead
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Framework.Session.*     FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.System.TranslationProvider
    IMPLEMENTS ITranslationProvider:

    DEFINE VARIABLE oCachedScopes AS CharacterList NO-UNDO .

    { Consultingwerk/SmartFramework/System/dsTranslation.i }

    DEFINE TEMP-TABLE ttTranslationCache NO-UNDO LIKE eSmartTranslation
        FIELD TranslationAvailable AS LOGICAL INIT TRUE .

    /**
     * Purpose: Returns if all translations have been cached
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AllTranslationsCached AS LOGICAL NO-UNDO INIT FALSE
    GET.
    PROTECTED SET.

    /**
     * Purpose: Controls if the GetTranslationsByKey and ...ByString methods
     *          will attempt to fetch translations from the Backend even after
     *          all translations are chached. TRUE will result in faster
     *          performance but may require a user adding translations to the
     *          system to restart the application more frequently
     * Notes:   Initialized by the "DontFetchTranslationsWhenAllTranslationsCached"
     *          value from the IConfigurationProvider service
     */
    DEFINE PUBLIC PROPERTY DontFetchTranslationsWhenAllTranslationsCached AS LOGICAL NO-UNDO INITIAL FALSE
    GET.
    SET.

    /**
     * Purpose: Constructor for the TranslationProvider class
     * Notes:
     */
    CONSTRUCTOR PUBLIC TranslationProvider ():
        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .
        DEFINE VARIABLE lValue                 AS LOGICAL                NO-UNDO .

        SUPER ().

        SessionManager:LanguageKeyChanged:Subscribe (LanguageKeyChangedHandler) .

        ASSIGN oCachedScopes = NEW CharacterList ()

               oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider}.

        IF VALID-OBJECT (oConfigurationProvider) THEN DO:
            ASSIGN lValue = DataTypeHelper:ToLogical(oConfigurationProvider:GetValue("DontFetchTranslationsWhenAllTranslationsCached":U)) .

            IF lValue <> ? THEN
                ASSIGN THIS-OBJECT:DontFetchTranslationsWhenAllTranslationsCached = lValue .
        END .

    END CONSTRUCTOR.

    /**
     * Purpose: Caches all translations for the current language
     * Notes:   Useful to avoid AppServer calls when multiple translations are
     *          required
     */
    METHOD PUBLIC VOID CacheAllTranslations ():

        DEFINE VARIABLE oCacheTranslationParameter AS CacheTranslationParameter NO-UNDO .

        EMPTY TEMP-TABLE ttTranslationCache .

        oCacheTranslationParameter = NEW CacheTranslationParameter (?,
                                                                    SessionManager:LanguageKey) .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                           "CacheAllTranslations":U,
                                           INPUT-OUTPUT DATASET dsTranslation,
                                           oCacheTranslationParameter) .
        ELSE
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                                           "CacheAllTranslations":U,
                                                           INPUT-OUTPUT DATASET dsTranslation,
                                                           oCacheTranslationParameter) .

        TEMP-TABLE ttTranslationCache:COPY-TEMP-TABLE (TEMP-TABLE eSmartTranslation:HANDLE, TRUE, FALSE, TRUE) .

        ASSIGN THIS-OBJECT:AllTranslationsCached = TRUE .

    END METHOD .

    /**
     * Purpose: Caches all translations by a given scope
     * Notes:   Useful to avoid AppServer calls when multiple translations are
     *          required in a row
     * @param pcScope The scope to retrieve from the back end
     */
    METHOD PUBLIC VOID CacheTranslationsByScope (pcScope AS CHARACTER):

        DEFINE VARIABLE oCacheTranslationParameter AS CacheTranslationParameter NO-UNDO .

        IF THIS-OBJECT:AllTranslationsCached = TRUE OR oCachedScopes:ContainsValue (pcScope) THEN
            RETURN .

        oCacheTranslationParameter = NEW CacheTranslationParameter (pcScope,
                                                                    SessionManager:LanguageKey) .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                           "CacheTranslationsByScope":U,
                                           INPUT-OUTPUT DATASET dsTranslation,
                                           oCacheTranslationParameter) .
        ELSE
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                                           "CacheTranslationsByScope":U,
                                                           INPUT-OUTPUT DATASET dsTranslation,
                                                           oCacheTranslationParameter) .

        oCachedScopes:Add (pcScope) .

        FOR EACH ttTranslationCache WHERE ttTranslationCache.TranslationScope = pcScope:
            DELETE ttTranslationCache .
        END.

        TEMP-TABLE ttTranslationCache:COPY-TEMP-TABLE (TEMP-TABLE eSmartTranslation:HANDLE, TRUE, FALSE, TRUE) .

    END METHOD .

    /**
     * Purpose: Returns a translation based on a key value and an optional scope
     * Notes:   Returns ? when no translation is available
     * @param pcScope The optional translation scope
     * @param pcKey The translation key value to return
     * @return The translated string
     */
    METHOD PUBLIC CHARACTER GetTranslationByKey (pcScope AS CHARACTER,
                                                 pcKey AS CHARACTER):

        RETURN THIS-OBJECT:GetTranslationByKey(pcScope, pcKey, ?) .

    END METHOD .

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

        DEFINE VARIABLE oParameter AS GetTranslationByKeyParameter NO-UNDO .

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcKey "'Translation Key':U"} .

        FIND FIRST ttTranslationCache
             WHERE ttTranslationCache.TranslationScope = pcScope
               AND ttTranslationCache.TranslationKey   = pcKey NO-ERROR .

        IF AVAILABLE ttTranslationCache THEN DO:
            IF ttTranslationCache.TranslationAvailable THEN
                RETURN ttTranslationCache.TranslatedString .
            ELSE
                RETURN pcDefaultString .     /* we already know there is no translation */
        END.

        /* Mike Fechner, Consultingwerk Ltd. 29.08.2016
           SCL-1421 : When all translations are cached, we don't go to the backend
                      anymore */
        IF THIS-OBJECT:DontFetchTranslationsWhenAllTranslationsCached = TRUE AND
           THIS-OBJECT:AllTranslationsCached = TRUE THEN
            RETURN pcDefaultString .

        oParameter = NEW GetTranslationByKeyParameter (pcScope,
                                                       pcKey,
                                                       SessionManager:LanguageKey) .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                           "GetTranslationByKey":U,
                                           INPUT-OUTPUT DATASET dsTranslation,
                                           oParameter) .
        ELSE
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                                           "GetTranslationByKey":U,
                                                           INPUT-OUTPUT DATASET dsTranslation,
                                                           oParameter) .

        CREATE ttTranslationCache .

        FIND FIRST eSmartTranslation NO-ERROR .

        IF NOT AVAILABLE eSmartTranslation THEN DO:
            ASSIGN ttTranslationCache.TranslationGuid      = GUID
                   ttTranslationCache.TranslationScope     = pcScope
                   ttTranslationCache.TranslationKey       = pcKey
                   ttTranslationCache.OriginalString       = ?
                   ttTranslationCache.TranslationAvailable = FALSE .

            RETURN pcDefaultString .
        END.

        BUFFER-COPY eSmartTranslation TO ttTranslationCache .

        RETURN eSmartTranslation.TranslatedString .

    END METHOD .

    /**
     * Purpose: Returns a translation for a string value and an optional scope
     * Notes:   Returns the original string when no translation is available
     * @param pcScope The optional translation scope
     * @param pcOriginalString The original string to translate
     * @return The translated string
     */
    METHOD PUBLIC CHARACTER GetTranslationByString (pcScope AS CHARACTER,
                                                    pcOriginalString AS CHARACTER):

        DEFINE VARIABLE oParameter AS GetTranslationByStringParameter NO-UNDO .

        IF pcOriginalString > "":U THEN .
        ELSE
            RETURN "":U .

        FIND FIRST ttTranslationCache
             WHERE ttTranslationCache.TranslationScope = pcScope
               AND ttTranslationCache.OriginalString   = pcOriginalString NO-ERROR .

        IF AVAILABLE ttTranslationCache THEN DO:
            IF ttTranslationCache.TranslationAvailable THEN
                RETURN ttTranslationCache.TranslatedString .
            ELSE
                RETURN pcOriginalString .     /* we already know there is no translation */
        END.

        /* Mike Fechner, Consultingwerk Ltd. 29.08.2016
           SCL-1421 : When all translations are cached, we don't go to the backend
                      anymore */
        IF THIS-OBJECT:DontFetchTranslationsWhenAllTranslationsCached = TRUE AND
           THIS-OBJECT:AllTranslationsCached = TRUE THEN
            RETURN pcOriginalString .

        oParameter = NEW GetTranslationByStringParameter (pcScope,
                                                          pcOriginalString,
                                                          SessionManager:LanguageKey) .

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                           "GetTranslationByString":U,
                                           INPUT-OUTPUT DATASET dsTranslation,
                                           oParameter) .
        ELSE
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.System.TranslationBusinessEntity":U,
                                                           "GetTranslationByString":U,
                                                           INPUT-OUTPUT DATASET dsTranslation,
                                                           oParameter) .

        CREATE ttTranslationCache .

        FIND FIRST eSmartTranslation NO-ERROR .

        IF NOT AVAILABLE eSmartTranslation THEN DO:
            ASSIGN ttTranslationCache.TranslationGuid      = GUID
                   ttTranslationCache.TranslationScope     = pcScope
                   ttTranslationCache.TranslationKey       = ?
                   ttTranslationCache.OriginalString       = pcOriginalString
                   ttTranslationCache.TranslationAvailable = FALSE .

            RETURN pcOriginalString .
        END.

        BUFFER-COPY eSmartTranslation TO ttTranslationCache .

        RETURN eSmartTranslation.TranslatedString .

    END METHOD .

    /**
     * Purpose: Event handler for the LanguageKeyChanged handler of the SessionManager
     * Notes:   Cleares all translation cache
     */
    METHOD PRIVATE VOID LanguageKeyChangedHandler ():

        EMPTY TEMP-TABLE ttTranslationCache .

        oCachedScopes:Clear () .

        THIS-OBJECT:AllTranslationsCached = FALSE .

    END METHOD .

END CLASS.
