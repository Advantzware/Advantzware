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
    File        : TranslationBusinessEntity
    Purpose     : Business Entity for Translation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 10.01.2013 14:40:05
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

@BusinessEntityTable (name="eSmartTranslation", MandatoryColumns="", DefaultSort="BY eSmartTranslation.TranslationKey BY eSmartTranslation.OriginalString") .

CLASS Consultingwerk.SmartFramework.System.TranslationBusinessEntity
    INHERITS SmartBusinessEntity:

    { Consultingwerk/SmartFramework/System/dsTranslation.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the TranslationBusinessEntity class
        Notes:   Passes the handle of the dataset instance to the base class and
                 sets the default DataAccessName
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TranslationBusinessEntity ():
        SUPER (DATASET dsTranslation:HANDLE).

        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.TranslationDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Caches all translations
        Notes:
        @param hDataset INPUT-OUTPUT Dataset with the translation
        @param poParameter The CacheTranslationParameter value with the request data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CacheAllTranslations (INPUT-OUTPUT DATASET-HANDLE hDataset,
                                             poParameter AS CacheTranslationParameter):

        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .
        DEFINE VARIABLE hBuffer  AS HANDLE            NO-UNDO .

        hDataset:CREATE-LIKE (DATASET {&DATASET-NAME}:HANDLE) .

        ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (1) .

        /* first retrieve all translations for the given scope and the current language */
        IF poParameter:SessionLanguageKey > "":U THEN DO:
            oRequest = NEW FetchDataRequest
                                ("eSmartTranslation":U,
                                 SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.LanguageGuid = &1":U,
                                             QUOTER (poParameter:SessionLanguageKey)),
                                 0) .

            THIS-OBJECT:FetchData (oRequest) .

            FOR EACH eSmartTranslation:
                hBuffer:BUFFER-CREATE () .
                hBuffer:BUFFER-COPY (BUFFER eSmartTranslation:HANDLE) .
                hBuffer:BUFFER-RELEASE () .
            END.
        END.

        /* now retrieve all default translations and put them in the
           dynamic dataset when the related record is not yet there
           in the session language */
        oRequest = NEW FetchDataRequest
                            ("eSmartTranslation":U,
                             SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.LanguageGuid = &1":U,
                                         QUOTER ("":U)),
                             0) .

        THIS-OBJECT:FetchData (oRequest) .

        FOR EACH eSmartTranslation ON ERROR UNDO, THROW:
            /* Based on Key or original string */
            IF eSmartTranslation.TranslationKey > "":U THEN
                hBuffer:FIND-FIRST (SUBSTITUTE ("WHERE TranslationScope = &1 AND TranslationKey = &2":U,
                                                QUOTER (eSmartTranslation.TranslationScope),
                                                QUOTER (eSmartTranslation.TranslationKey))) NO-ERROR .
            ELSE
                hBuffer:FIND-FIRST (SUBSTITUTE ("WHERE TranslationScope = &1 AND OriginalString = &2":U,
                                                QUOTER (eSmartTranslation.TranslationScope),
                                                QUOTER (eSmartTranslation.OriginalString))) NO-ERROR .

            IF NOT hBuffer:AVAILABLE THEN DO:
                hBuffer:BUFFER-CREATE () .
                hBuffer:BUFFER-COPY (BUFFER eSmartTranslation:HANDLE) .
                hBuffer:BUFFER-RELEASE () .
            END.
        END.

        DELETE OBJECT hDataset .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Caches all translations by a given scope
        Notes:
        @param hDataset INPUT-OUTPUT Dataset with the translation
        @param poParameter The CacheTranslationParameter value with the request data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CacheTranslationsByScope (INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                 poParameter AS CacheTranslationParameter):

        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .
        DEFINE VARIABLE hBuffer  AS HANDLE            NO-UNDO .

        hDataset:CREATE-LIKE (DATASET {&DATASET-NAME}:HANDLE) .

        ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (1) .

        /* first retrieve all translations for the given scope and the current language */
        IF poParameter:SessionLanguageKey > "":U THEN DO:
            oRequest = NEW FetchDataRequest
                                ("eSmartTranslation":U,
                                 SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.TranslationScope = &1 AND eSmartTranslation.LanguageGuid = &2":U,
                                             QUOTER (poParameter:TranslationScope),
                                             QUOTER (poParameter:SessionLanguageKey)),
                                 0) .

            THIS-OBJECT:FetchData (oRequest) .

            FOR EACH eSmartTranslation:
                hBuffer:BUFFER-CREATE () .
                hBuffer:BUFFER-COPY (BUFFER eSmartTranslation:HANDLE) .
                hBuffer:BUFFER-RELEASE () .
            END.
        END.

        /* now retrieve all default translations and put them in the
           dynamic dataset when the related record is not yet there
           in the session language */
        oRequest = NEW FetchDataRequest
                            ("eSmartTranslation":U,
                             SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.TranslationScope = &1 AND eSmartTranslation.LanguageGuid = &2":U,
                                         QUOTER (poParameter:TranslationScope),
                                         QUOTER ("":U)),
                             0) .

        THIS-OBJECT:FetchData (oRequest) .

        FOR EACH eSmartTranslation ON ERROR UNDO, THROW:
            /* Based on Key or original string */
            IF eSmartTranslation.TranslationKey > "":U THEN
                hBuffer:FIND-FIRST (SUBSTITUTE ("WHERE TranslationScope = &1 AND TranslationKey = &2":U,
                                                QUOTER (eSmartTranslation.TranslationScope),
                                                QUOTER (eSmartTranslation.TranslationKey))) NO-ERROR .
            ELSE
                hBuffer:FIND-FIRST (SUBSTITUTE ("WHERE TranslationScope = &1 AND OriginalString = &2":U,
                                                QUOTER (eSmartTranslation.TranslationScope),
                                                QUOTER (eSmartTranslation.OriginalString))) NO-ERROR .

            IF NOT hBuffer:AVAILABLE THEN DO:
                hBuffer:BUFFER-CREATE () .
                hBuffer:BUFFER-COPY (BUFFER eSmartTranslation:HANDLE) .
                hBuffer:BUFFER-RELEASE () .
            END.
        END.

        DELETE OBJECT hDataset .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a translation based on a key value and an optional scope
        Notes:   Returns ? whne no translation is available
        @param dsTranslation INPUT-OUTPUT Dataset with the translation
        @param poParameter The GetTranslationByKeyParameter value with the request data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslationByKey (INPUT-OUTPUT DATASET {&DATASET-NAME},
                                            poParameter AS GetTranslationByKeyParameter):

        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poParameter,
                                                       "GetTranslationByKeyParameter":U) .

        /* First try the client session language - if specified */
        IF poParameter:SessionLanguageKey > "":U THEN DO:
            oRequest = NEW FetchDataRequest
                                ("eSmartTranslation":U,
                                 SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.TranslationScope = &1 AND eSmartTranslation.TranslationKey = &2 AND eSmartTranslation.LanguageGuid = &3":U,
                                             QUOTER (poParameter:TranslationScope),
                                             QUOTER (poParameter:TranslationKey),
                                             QUOTER (poParameter:SessionLanguageKey)),
                                 1) .

            THIS-OBJECT:FetchData (oRequest) .

            IF CAN-FIND (FIRST eSmartTranslation) THEN
                RETURN .
        END.

        /* now try the unspecified (default) language */
        oRequest = NEW FetchDataRequest
                            ("eSmartTranslation":U,
                             SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.TranslationScope = &1 AND eSmartTranslation.TranslationKey = &2 AND eSmartTranslation.LanguageGuid = &3":U,
                                         QUOTER (poParameter:TranslationScope),
                                         QUOTER (poParameter:TranslationKey),
                                         QUOTER ("":U)),
                             1) .

        THIS-OBJECT:FetchData (oRequest) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a translation based on an original string and an optional scope
        Notes:   Returns ? whne no translation is available
        @param dsTranslation INPUT-OUTPUT Dataset with the translation
        @param poParameter The GetTranslationByKeyParameter value with the request data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTranslationByString (INPUT-OUTPUT DATASET {&DATASET-NAME},
                                               poParameter AS GetTranslationByStringParameter):


        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poParameter,
                                                       "GetTranslationByKeyParameter":U) .

        /* First try the client session language - if specified */
        IF poParameter:SessionLanguageKey > "":U THEN DO:
            oRequest = NEW FetchDataRequest
                                ("eSmartTranslation":U,
                                 SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.TranslationScope = &1 AND eSmartTranslation.OriginalString = &2 AND eSmartTranslation.LanguageGuid = &3":U,
                                             QUOTER (poParameter:TranslationScope),
                                             QUOTER (poParameter:OriginalString),
                                             QUOTER (poParameter:SessionLanguageKey)),
                                 1) .

            THIS-OBJECT:FetchData (oRequest) .

            IF CAN-FIND (FIRST eSmartTranslation) THEN
                RETURN .
        END.

        /* now try the unspecified (default) language */
        oRequest = NEW FetchDataRequest
                            ("eSmartTranslation":U,
                             SUBSTITUTE ("FOR EACH eSmartTranslation WHERE eSmartTranslation.TranslationScope = &1 AND eSmartTranslation.OriginalString = &2 AND eSmartTranslation.LanguageGuid = &3":U,
                                         QUOTER (poParameter:TranslationScope),
                                         QUOTER (poParameter:OriginalString),
                                         QUOTER ("":U)),
                             1) .

        THIS-OBJECT:FetchData (oRequest) .

    END METHOD .
    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and
                 Update operations (i.e. population of aggregated values)
        Notes:   Invoked during FetchData () and SaveChanges ()
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update
                 operations
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():

    END METHOD.

END CLASS.
