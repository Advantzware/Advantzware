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
    File        : UserInterfaceTranslationService
    Purpose     : UI Translation Service of the SmartFramework
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 10 10:03:45 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                             FROM PROPATH .
USING Consultingwerk.Framework.*                   FROM PROPATH .
USING Consultingwerk.Framework.Collections.*       FROM PROPATH .
USING Consultingwerk.Framework.Session.*           FROM PROPATH .
USING Consultingwerk.OERA.*                        FROM PROPATH .
USING Consultingwerk.Util.*                        FROM PROPATH .
USING Consultingwerk.SmartFramework.Localization.* FROM PROPATH .
USING Progress.Lang.*                              FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Localization.UserInterfaceTranslationService
    IMPLEMENTS IUserInterfaceTranslationService:

    { Consultingwerk/SmartFramework/System/dsUiTranslation.i }
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
        @param sender The object that raised the ObtainTranslation event
        @param e The ObtainTranslationEventArgs with the data for the event
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
        Purpose: Retrieves stored translations from the back end
        Notes:
        @param pcObjectName The name of the UI object to retrieve translations for
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FetchTranslationsForObject (pcObjectName AS CHARACTER):

        DEFINE VARIABLE oFetchDataRequest AS IFetchDataRequest NO-UNDO .

        oFetchDataRequest = NEW FetchDataRequest ("eSmartUiTranslation":U,
                                                  SUBSTITUTE ("FOR EACH eSmartUiTranslation WHERE eSmartUiTranslation.ObjectName = &1 AND eSmartUiTranslation.LanguageGuid = &2":U,
                                                              QUOTER (pcObjectName),
                                                              QUOTER (SessionManager:LanguageKey))) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.System.UiTranslationBusinessEntity":U,
                                                       oFetchDataRequest,
                                                       OUTPUT DATASET dsUiTranslation) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Partial match key for the given resource key
        Notes:   Replaces all numbers with *, eliminated double **
        @param pcResourceKey The original resource key
        @return The partial match key
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetPartialMatchKey (pcResourceKey AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 0 TO 9 ON ERROR UNDO, THROW:
            ASSIGN pcResourceKey = REPLACE (pcResourceKey, STRING (i), "*":U) .
        END.

        DO WHILE INDEX (pcResourceKey, "**":U) > 0 ON ERROR UNDO, THROW:
            ASSIGN pcResourceKey = REPLACE (pcResourceKey, "**":U, "*":U) .
        END.

        RETURN pcResourceKey .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the String Resources Table for the named UI object
        Notes:
        @param pcObjectName The name of the object to translate (class name)
        @param poOriginalStrings The dictionary with the original UI strings
        @param eStringResources OUTPUT The table with the string resources
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslationsForObject (pcObjectName AS CHARACTER,
                                                 poOriginalStrings AS CharacterDictionary,
                                                 OUTPUT TABLE eStringResources):

        DEFINE VARIABLE cKeys       AS LONGCHAR                      NO-UNDO .
        DEFINE VARIABLE cKey        AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE cPartialKey AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOriginal   AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE cDelimiter  AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                       NO-UNDO .
        DEFINE VARIABLE e           AS FilterStringResourceEventArgs NO-UNDO .
        DEFINE VARIABLE e2          AS ObtainTranslationEventArgs    NO-UNDO .

        EMPTY TEMP-TABLE eStringResources .

        ASSIGN cKeys      = poOriginalStrings:Keys
               cDelimiter = poOriginalStrings:KeyDelimiter .

        THIS-OBJECT:FetchTranslationsForObject (pcObjectName) .

        DO i = 1 TO NUM-ENTRIES (cKeys, cDelimiter) ON ERROR UNDO, THROW:

            ASSIGN cKey        = ENTRY (i, cKeys, cDelimiter)
                   cPartialKey = THIS-OBJECT:GetPartialMatchKey (cKey)
                   cOriginal   = poOriginalStrings:GetValue (cKey) .

            e = NEW FilterStringResourceEventArgs (cKey, cOriginal) .

            THIS-OBJECT:OnFilterStringResource (e) .

            IF e:Filtered = TRUE THEN
                NEXT .

            CREATE eStringResources.
            ASSIGN eStringResources.ResourceKey     = cKey
                   eStringResources.OriginalString  = cOriginal
                   eStringResources.TranslationType = TranslationTypeEnum:LocalByKey
                    .

            e2 = NEW ObtainTranslationEventArgs (pcObjectName,
                                                 cKey,
                                                 cPartialKey,
                                                 cOriginal) .

            THIS-OBJECT:OnObtainTranslation (e2) .

            IF e2:Handled THEN DO:

                ASSIGN eStringResources.TranslatedString   = e2:TranslatedString
                       eStringResources.TranslationType    = e2:TranslationType
                       eStringResources.ResourceKeyPartial = e2:ResourceKeyPartial
                       .

                NEXT .
            END.

            /* Mike Fechner, Consultingwerk Ltd. 12.05.2013
               Local translation ? */
            FIND FIRST eSmartUiTranslation WHERE eSmartUiTranslation.LanguageGuid = SessionManager:LanguageKey
                                             AND eSmartUiTranslation.ObjectName   = pcObjectName
                                             AND eSmartUiTranslation.ResourceKey  = cKey
                NO-ERROR .

            IF AVAILABLE eSmartUiTranslation THEN
                ASSIGN eStringResources.TranslatedString = eSmartUiTranslation.TranslatedString
                       eStringResources.TranslationType  = TranslationTypeEnum:LocalByKey .

            FINALLY:
                RELEASE eStringResources .
            END FINALLY.
        END .

        FINALLY:
            DATASET dsUiTranslation:EMPTY-DATASET () .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the translated String Resources Table for the named UI object
        Notes:
        @param pcObjectName The name of the object to translate (class name)
        @param eStringResources INPUT-OUTPUT The table with the string resources (input the original strings, output the translated)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslationsForObject (pcObjectName AS CHARACTER,
                                                 INPUT-OUTPUT TABLE eStringResources):

        DEFINE VARIABLE cKey        AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE cPartialKey AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cOriginal   AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                       NO-UNDO .
        DEFINE VARIABLE e           AS FilterStringResourceEventArgs NO-UNDO .
        DEFINE VARIABLE e2          AS ObtainTranslationEventArgs    NO-UNDO .

        THIS-OBJECT:FetchTranslationsForObject (pcObjectName) .

        FOR EACH eStringResources ON ERROR UNDO, THROW:
            ASSIGN eStringResources.TranslatedString = "":U .

            ASSIGN cKey        = eStringResources.ResourceKey
                   cPartialKey = THIS-OBJECT:GetPartialMatchKey (cKey)
                   cOriginal   = eStringResources.OriginalString .

            e = NEW FilterStringResourceEventArgs (cKey, cOriginal) .

            THIS-OBJECT:OnFilterStringResource (e) .

            IF e:Filtered = TRUE THEN
                NEXT .

            ASSIGN eStringResources.TranslationType = TranslationTypeEnum:LocalByKey .

            e2 = NEW ObtainTranslationEventArgs (pcObjectName,
                                                 cKey,
                                                 cPartialKey,
                                                 cOriginal) .

            THIS-OBJECT:OnObtainTranslation (e2) .

            IF e2:Handled THEN DO:

                ASSIGN eStringResources.TranslatedString   = e2:TranslatedString
                       eStringResources.TranslationType    = e2:TranslationType
                       eStringResources.ResourceKeyPartial = e2:ResourceKeyPartial
                       .

                NEXT .
            END.

            /* Mike Fechner, Consultingwerk Ltd. 12.05.2013
               Local translation ? */
            FIND FIRST eSmartUiTranslation WHERE eSmartUiTranslation.LanguageGuid = SessionManager:LanguageKey
                                             AND eSmartUiTranslation.ObjectName   = pcObjectName
                                             AND eSmartUiTranslation.ResourceKey  = cKey
                NO-ERROR .

            IF AVAILABLE eSmartUiTranslation THEN
                ASSIGN eStringResources.TranslatedString = eSmartUiTranslation.TranslatedString
                       eStringResources.TranslationType  = TranslationTypeEnum:LocalByKey .

            FINALLY:
                RELEASE eStringResources .
            END FINALLY.
        END .

        FINALLY:
            DATASET dsUiTranslation:EMPTY-DATASET () .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the FilterStringResource event
        Notes:
        @param e The FilterStringResourceEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnFilterStringResource (e AS FilterStringResourceEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "FilterStringResource":U) .

        THIS-OBJECT:FilterStringResource:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the ObtainTranslation event
        Notes:
        @param e The ObtainTranslationEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnObtainTranslation (e AS ObtainTranslationEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "ObtainTranslation":U) .

        THIS-OBJECT:ObtainTranslation:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the StoreTranslation event
        Notes:
        @param e The ObtainTranslationEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnStoreTranslation (e AS StoreTranslationEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "StoreTranslation":U) .

        THIS-OBJECT:StoreTranslation:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the String Resources Table for the named UI object
        Notes:
        @param pcObjectName The name of the object to translate (class name)
        @param eStringResources INPUT-OUTPUT The table with the string resources
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreTranslationsForObject (pcObjectName AS CHARACTER,
                                                   INPUT-OUTPUT TABLE eStringResources):

        DEFINE VARIABLE cContext AS CHARACTER                 NO-UNDO.
        DEFINE VARIABLE e        AS StoreTranslationEventArgs NO-UNDO .

        THIS-OBJECT:FetchTranslationsForObject (pcObjectName) .

        DatasetHelper:SetTrackingChanges (DATASET dsUiTranslation:HANDLE, TRUE) .

        FOR EACH eStringResources WHERE eStringResources.TranslationType  = TranslationTypeEnum:LocalByKey
                                    AND eStringResources.TranslatedString > "":U
            ON ERROR UNDO, THROW:

            e = NEW StoreTranslationEventArgs (pcObjectName,
                                               eStringResources.ResourceKey,
                                               eStringResources.ResourceKeyPartial,
                                               eStringResources.OriginalString,
                                               eStringResources.TranslatedString,
                                               eStringResources.TranslationType) .

            THIS-OBJECT:OnStoreTranslation (e) .

            IF e:Handled THEN
                NEXT .

            FIND FIRST eSmartUiTranslation WHERE eSmartUiTranslation.LanguageGuid = SessionManager:LanguageKey
                                             AND eSmartUiTranslation.ObjectName   = pcObjectName
                                             AND eSmartUiTranslation.ResourceKey  = eStringResources.ResourceKey
                NO-ERROR .

            IF NOT AVAILABLE eSmartUiTranslation THEN DO:
                CREATE eSmartUiTranslation .
                ASSIGN eSmartUiTranslation.UiTranslationGuid = GUID
                       eSmartUiTranslation.LanguageGuid      = SessionManager:LanguageKey
                       eSmartUiTranslation.ObjectName        = pcObjectName
                       eSmartUiTranslation.ResourceKey       = eStringResources.ResourceKey .
            END.

            ASSIGN eSmartUiTranslation.OriginalString   = eStringResources.OriginalString
                   eSmartUiTranslation.TranslatedString = eStringResources.TranslatedString .

            FINALLY:
                RELEASE eSmartUiTranslation .
            END FINALLY.
        END.

        DatasetHelper:SetTrackingChanges (DATASET dsUiTranslation:HANDLE, FALSE) .

        FrameworkSettings:ServiceAdapter:SubmitData ("":U,
                                                     "Consultingwerk.SmartFramework.System.UiTranslationBusinessEntity":U,
                                                     INPUT-OUTPUT DATASET dsUiTranslation,
                                                     INPUT-OUTPUT cContext) .

        FINALLY:
            DatasetHelper:SetTrackingChanges (DATASET dsUiTranslation:HANDLE, FALSE) .

            DATASET dsUiTranslation:EMPTY-DATASET () .
        END FINALLY.

    END METHOD .

END CLASS.
