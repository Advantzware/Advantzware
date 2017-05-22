/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ContextDatasetStore
    Purpose     : Service that provides persistent storage of the Context
                  Dataset between REST requests
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 25 08:10:34 CET 2016
    Notes       : Supports the ContextDatasetStore custom log entry type
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                        FROM PROPATH .
USING Consultingwerk.Assertion.*              FROM PROPATH .
USING Consultingwerk.Framework.Server.*       FROM PROPATH .
USING Consultingwerk.Framework.Session.*      FROM PROPATH .
USING Consultingwerk.SmartFramework.Context.* FROM PROPATH .
USING Consultingwerk.OERA.*                   FROM PROPATH.
USING Consultingwerk.Util.*                   FROM PROPATH .
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.Framework.Server.ContextDatasetStore
&IF DEFINED (SmartFramework) NE 0 &THEN
    IMPLEMENTS IContextDatasetStore
&ENDIF
            :
&IF DEFINED (SmartFramework) NE 0 &THEN
    DEFINE VARIABLE lcPreviousContextDataset AS LONGCHAR NO-UNDO.

    /**
     * Purpose: Constructor of the ContextDatasetStore class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ContextDatasetStore ():

        FIX-CODEPAGE (lcPreviousContextDataset) = Codepages:UTF-8 .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Restores the Session Context
        Notes:
        @param phPrincipal The CLIENT-PRINCIPAL valid for the current request
        @return Logical value indicating if the session context was restored
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RestoreSessionContext (phPrincipal AS HANDLE):

        DEFINE VARIABLE oContextStore AS ContextStoreDatasetModel NO-UNDO .
        DEFINE VARIABLE lcDataset     AS LONGCHAR                 NO-UNDO .

        HandleAssert:ValidHandle (phPrincipal, "phPrincipal":U) .

        ASSIGN lcPreviousContextDataset = ? .

        FIX-CODEPAGE (lcDataset) = Codepages:UTF-8 .

        oContextStore = ContextStoreDatasetModel:FromSessionID (phPrincipal:SESSION-ID) .

        LogManager:WriteMessage (SUBSTITUTE ("[RestoreSessionContext] Stored context dataset available: &1":U,
                                             oContextStore:SmartContextStore:Available), "ContextDatasetStore":U) .

        IF NOT VALID-HANDLE (SessionManager:ContextDataset) THEN DO:
            LogManager:WriteMessage ("[RestoreSessionContext] Creating NEW context dataset using IContextDatasetFactory service.":U, "ContextDatasetStore":U) .

            SessionManager:ContextDataset = {Consultingwerk/get-service.i Consultingwerk.OERA.Context.IContextDatasetFactory}:CreateContextDataset ().
        END.

        IF oContextStore:SmartContextStore:Available THEN DO:
            LogManager:WriteMessage ("[RestoreSessionContext] Restoring Context Dataset from persistent store.":U, "ContextDatasetStore":U) .

            ASSIGN lcDataset = oContextStore:SmartContextStore:ContextDataset .

            SessionManager:ContextDataset:READ-XML ("longchar":U,
                                                    lcDataset,
                                                    "EMPTY":U, ?, ?) .

            ASSIGN
                SessionManager:ClientPrincipal              = ContextWrapper:ClientPrincipal
                SessionManager:LanguageDisplayName          = ContextWrapper:LanguageDisplayName
                SessionManager:LanguageIsoCode              = ContextWrapper:LanguageIsoCode
                SessionManager:LanguageKey                  = ContextWrapper:LanguageKey
                SessionManager:LoginCompanyKey              = ContextWrapper:LoginCompanyKey
                SessionManager:LoginCompanyName             = ContextWrapper:LoginCompanyName
                SessionManager:LoginCompanyReferenceChar    = ContextWrapper:LoginCompanyReferenceChar
                SessionManager:LoginCompanyReferenceDecimal = ContextWrapper:LoginCompanyReferenceDecimal
                SessionManager:LoginCompanyReferenceInteger = ContextWrapper:LoginCompanyReferenceInteger
                SessionManager:LoginCompanyShortName        = ContextWrapper:LoginCompanyShortName
                SessionManager:ServerLogEntryTypes          = ContextWrapper:ServerLogEntryTypes
                SessionManager:ServerDebugMode              = ContextWrapper:ServerDebugMode
                SessionManager:ServerCustomLogEntries       = ContextWrapper:ServerCustomLogEntries
                SessionManager:UserName                     = ContextWrapper:UserName
                SessionManager:UserKey                      = ContextWrapper:UserKey
                SessionManager:UserGroupKeys                = ContextWrapper:UserGroupKeys
                SessionManager:UserGroupNames               = ContextWrapper:UserGroupNames
                .

            lcPreviousContextDataset = lcDataset .

            RETURN TRUE .
        END.
        ELSE DO:
            lcPreviousContextDataset = ? .

            RETURN FALSE .
        END.

        CATCH err AS Progress.Lang.Error:
            LogManager:WriteMessage (SUBSTITUTE ("[RestoreSessionContext] Error:&1":U, err:GetMessage (1)), "ContextDatasetStore":U) .
            LogManager:WriteError (err) .

            RETURN FALSE .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the Session Context
        Notes:
        @param phPrincipal The CLIENT-PRINCIPAL valid for the current request
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreSessionContext (phPrincipal AS HANDLE):

        DEFINE VARIABLE oContextStore AS ContextStoreDatasetModel NO-UNDO .
        DEFINE VARIABLE lcDataset     AS LONGCHAR                 NO-UNDO .

        HandleAssert:ValidHandle (phPrincipal, "phPrincipal":U) .

        IF NOT VALID-HANDLE (SessionManager:ContextDataset) THEN DO:
            LogManager:WriteMessage ("[StoreSessionContext] No context dataset available in persistent store.":U, "ContextDatasetStore":U) .

            RETURN .
        END.

        ASSIGN
            ContextWrapper:ClientPrincipal              = SessionManager:ClientPrincipal
            ContextWrapper:LanguageDisplayName          = SessionManager:LanguageDisplayName
            ContextWrapper:LanguageIsoCode              = SessionManager:LanguageIsoCode
            ContextWrapper:LanguageKey                  = SessionManager:LanguageKey
            ContextWrapper:LoginCompanyKey              = SessionManager:LoginCompanyKey
            ContextWrapper:LoginCompanyName             = SessionManager:LoginCompanyName
            ContextWrapper:LoginCompanyReferenceChar    = SessionManager:LoginCompanyReferenceChar
            ContextWrapper:LoginCompanyReferenceDecimal = SessionManager:LoginCompanyReferenceDecimal
            ContextWrapper:LoginCompanyReferenceInteger = SessionManager:LoginCompanyReferenceInteger
            ContextWrapper:LoginCompanyShortName        = SessionManager:LoginCompanyShortName
            ContextWrapper:ServerLogEntryTypes          = SessionManager:ServerLogEntryTypes
            ContextWrapper:ServerDebugMode              = SessionManager:ServerDebugMode
            ContextWrapper:ServerCustomLogEntries       = SessionManager:ServerCustomLogEntries
            ContextWrapper:UserName                     = SessionManager:UserName
            ContextWrapper:UserKey                      = SessionManager:UserKey
            ContextWrapper:UserGroupKeys                = SessionManager:UserGroupKeys
            ContextWrapper:UserGroupNames               = SessionManager:UserGroupNames
            .

        FIX-CODEPAGE (lcDataset) = Codepages:UTF-8 .

        /* Did we change the context dataset? */
        SessionManager:ContextDataset:WRITE-XML ("longchar":U,
                                                 lcDataset,
                                                 FALSE,
                                                 Codepages:UTF-8) .

        IF lcPreviousContextDataset = lcDataset THEN DO:
            LogManager:WriteMessage ("[StoreSessionContext] No change in context dataset detected.":U, "ContextDatasetStore":U) .

            RETURN .
        END.

        oContextStore = ContextStoreDatasetModel:FromSessionID (phPrincipal:SESSION-ID) .

        oContextStore:TrackingChanges = TRUE .

        IF NOT oContextStore:SmartContextStore:Available THEN DO:
            LogManager:WriteMessage ("[StoreSessionContext] Creating new record in persistent store.":U, "ContextDatasetStore":U) .

            oContextStore:SmartContextStore:Create() .

            ASSIGN oContextStore:SmartContextStore:DomainName = phPrincipal:DOMAIN-NAME
                   oContextStore:SmartContextStore:SessionID  = phPrincipal:SESSION-ID
                   oContextStore:SmartContextStore:UserName   = phPrincipal:USER-ID .
        END.
        ELSE
            LogManager:WriteMessage ("[StoreSessionContext] Storing context dataset in persistent store.":U, "ContextDatasetStore":U) .

        oContextStore:SmartContextStore:ContextDataset = lcDataset .

        oContextStore:SaveChanges() .

        CATCH err AS Progress.Lang.Error:
            LogManager:WriteMessage (SUBSTITUTE ("[StoreSessionContext] Error:&1":U, err:GetMessage (1)), "ContextDatasetStore":U) .
            LogManager:WriteError (err) .
        END CATCH.

        FINALLY:
            ASSIGN lcPreviousContextDataset = ? .
        END FINALLY.

    END METHOD .
&ENDIF

END CLASS.
