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
    File        : SessionContextSerializer
    Purpose     : Serializes the client side context for a server request
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 16 12:50:36 CET 2013
    Notes       : Uses the Context Dataset for context serialization
                  Optionally restores modified context from the AppServer
                  on the client
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*         FROM PROPATH .
USING Consultingwerk.Framework.Session.* FROM PROPATH .
USING Consultingwerk.Framework.Server.*  FROM PROPATH .
USING Consultingwerk.OERA.*              FROM PROPATH .
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Framework.Server.SessionContextSerializer:

    /**
     * Purpose: Constructor for the SessionContextSerializer class
     * Notes:   Defaults to NOT collect context from the AppServer
     */
    CONSTRUCTOR PUBLIC SessionContextSerializer ():
        THIS-OBJECT (FALSE).

    END CONSTRUCTOR .

    /**
     * Purpose: Constructor for the SessionContextSerializer class
     * Notes:
     * @param plCollectContextFromServer Logical value indicating if the context from the server should be assigned back to the client side SessionManager
     */
    CONSTRUCTOR PUBLIC SessionContextSerializer (plCollectContextFromServer AS LOGICAL):
        DEFINE VARIABLE oAdapter AS IServiceAdapterWithContextEvents NO-UNDO .

        SUPER ().

        IF NOT TYPE-OF (FrameworkSettings:ServiceAdapter, IServiceAdapterWithContextEvents) THEN
            UNDO, THROW NEW AppError ("The SessionContextSerializer is not supported when the Service Adapter does not provide the context events"{&TRAN}, 0) .

        ASSIGN oAdapter = CAST (FrameworkSettings:ServiceAdapter, IServiceAdapterWithContextEvents) .

        IF plCollectContextFromServer THEN
            oAdapter:CollectContextFromServer:Subscribe (CollectContextFromServerHandler) .

        oAdapter:PrepareContextForServer:Subscribe (PrepareContextForServerHandler) .

    END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the CollectContextFromServer event of the Service Adapter
     * Notes:   Assigns updated context from the server to the client side Session Manager
     * @param sender The object that raised the CollectContextFromServer event
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID CollectContextFromServerHandler (sender AS Progress.Lang.Object,
                                                           e AS Consultingwerk.EventArgs):

        IF VALID-HANDLE (SessionManager:ContextDataset) THEN
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

    END METHOD .

    /**
     * Purpose: Event handler for the PrepareContextForServer event of the Service Adapter
     * Notes:   Assigns client side Session Manager values to the context dataset
     * @param sender The object that raised the PrepareContextForServer event
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID PrepareContextForServerHandler (sender AS Progress.Lang.Object,
                                                          e AS Consultingwerk.EventArgs):

        IF VALID-HANDLE (SessionManager:ContextDataset) THEN
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

    END METHOD .

END CLASS.
