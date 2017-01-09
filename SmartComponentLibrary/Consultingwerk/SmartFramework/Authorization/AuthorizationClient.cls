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
    File        : AuthorizationClient
    Purpose     : Provides authorization information about tokens etc.
                  Service for Client and AppServer and Client/Server
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 18 21:36:27 CEST 2014
    Notes       : Caches information when used on the client
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Session.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Authorization.AuthorizationClient
    IMPLEMENTS ITokenSecurityService, IToolbarItemSecurityService:

    DEFINE TEMP-TABLE ttTokenSecurityCache NO-UNDO
        FIELD SecurityTokenCode AS CHARACTER
        FIELD IsRestricted      AS LOGICAL
        INDEX SecurityTokenCode IS UNIQUE PRIMARY SecurityTokenCode .

    DEFINE TEMP-TABLE ttToolbarItemSecurityCache NO-UNDO
        FIELD ObjectName              AS CHARACTER
        FIELD ToolbarItemKey          AS CHARACTER
        FIELD ToolbarSecurityItemGuid AS CHARACTER
        FIELD IsRestricted            AS LOGICAL INIT ?
        INDEX ObjectNameToolbarItemKey IS UNIQUE PRIMARY ObjectName ToolbarItemKey .

    DEFINE VARIABLE lToolbarItemDefaultRestricted AS LOGICAL NO-UNDO INIT ? .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AuthorizationClient class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AuthorizationClient ():

        DEFINE VARIABLE oToolbarSecurityItemDatasetModel AS ToolbarSecurityItemDatasetModel NO-UNDO .
        DEFINE VARIABLE oSecurityRealmModel              AS SecurityRealmDatasetModel       NO-UNDO .

        /* preload all Toolbar Items */
        oToolbarSecurityItemDatasetModel = NEW ToolbarSecurityItemDatasetModel () .
        oToolbarSecurityItemDatasetModel:BatchSize = 0 .
        oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:Fill () .

        DO WHILE oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:Available:

            CREATE ttToolbarItemSecurityCache.
            ASSIGN ttToolbarItemSecurityCache.ObjectName              = oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:ObjectName
                   ttToolbarItemSecurityCache.ToolbarItemKey          = oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:ToolbarItemKey
                   ttToolbarItemSecurityCache.ToolbarSecurityItemGuid = oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:ToolbarSecurityItemGuid .

            oToolbarSecurityItemDatasetModel:SmartToolbarSecurityItem:GetNext () .
        END.

        /* Default for Toolbar Item Security */
        oSecurityRealmModel = NEW SecurityRealmDatasetModel () .
        oSecurityRealmModel:SmartSecurityRealm:Filter:SecurityRealmCode:EQ ("STI":U):Run() .

        IF oSecurityRealmModel:SmartSecurityRealm:Available THEN
            ASSIGN lToolbarItemDefaultRestricted = oSecurityRealmModel:SmartSecurityRealm:DefaultRestricted .

        SessionManager:UserNameChanged:Subscribe (UserNameChangedHandler) .
        SessionManager:LoginCompanyKeyChanged:Subscribe (LoginCompanyKeyChangedHandler) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Clears the cache of the SecurityServiceClient
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearCache ():

        EMPTY TEMP-TABLE ttTokenSecurityCache .
        EMPTY TEMP-TABLE ttToolbarItemSecurityCache .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given security token is restricted
        Notes:   When the token is unknown on the current system, the ? is returned
        @param pcSecurityTokenCode The code of the security token
        @return Logical value indicating if the given token is restricted for the current user, TRUE means the user does not have access to the token
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsTokenRestricted (INPUT pcSecurityTokenCode AS CHARACTER):

        DEFINE VARIABLE oToken        AS SecurityTokenDatasetModel NO-UNDO .
        DEFINE VARIABLE lIsRestricted AS LOGICAL                   NO-UNDO INIT ? .

        IF NOT CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN DO:
            FIND ttTokenSecurityCache WHERE ttTokenSecurityCache.SecurityTokenCode = pcSecurityTokenCode NO-ERROR .

            ERROR-STATUS:ERROR = FALSE NO-ERROR .

            IF AVAILABLE ttTokenSecurityCache THEN
                RETURN ttTokenSecurityCache.IsRestricted .
        END.

        oToken = NEW SecurityTokenDatasetModel () .
        oToken:SmartSecurityToken:Filter:SecurityTokenCode:EQ (pcSecurityTokenCode):Run() .

        IF NOT oToken:SmartSecurityToken:Available THEN
            ASSIGN lIsRestricted = ? .
        ELSE
            ASSIGN lIsRestricted = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService}:IsRestricted
                    ("SST":U,
                     oToken:SmartSecurityToken:SecurityTokenGuid) .

        IF NOT CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN DO:
            CREATE ttTokenSecurityCache.
            ASSIGN ttTokenSecurityCache.SecurityTokenCode = pcSecurityTokenCode
                   ttTokenSecurityCache.IsRestricted      = lIsRestricted .
        END.

        RETURN lIsRestricted .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given toolbar item is restricted
        Notes:   When the toolbar item is unknown on the current system, the security realm default will be returned
        @param pcObjectName The name of the container or SmartTableIOTarget
        @param pcToolbarItemKey The name of the toolbar item
        @return Logical value indicating if the given toolbar item is restricted for the current user, TRUE means the user does not have access to the toolbar item
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsToolbarItemRestricted (pcObjectName AS CHARACTER,
                                                   pcToolbarItemKey AS CHARACTER):

        /* Exact match */
        FIND ttToolbarItemSecurityCache WHERE ttToolbarItemSecurityCache.ObjectName     = pcObjectName
                                          AND ttToolbarItemSecurityCache.ToolbarItemKey = pcToolbarItemKey NO-ERROR .

        IF AVAILABLE ttToolbarItemSecurityCache THEN DO:
            IF ttToolbarItemSecurityCache.IsRestricted <> ? THEN
                RETURN ttToolbarItemSecurityCache.IsRestricted .

            ASSIGN ttToolbarItemSecurityCache.IsRestricted = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService}:IsRestricted
                    ("STI":U,
                     ttToolbarItemSecurityCache.ToolbarSecurityItemGuid) .

            RETURN ttToolbarItemSecurityCache.IsRestricted .
        END.

        /* Only for the Toolbar Item */
        FIND ttToolbarItemSecurityCache WHERE ttToolbarItemSecurityCache.ObjectName     = "":U
                                          AND ttToolbarItemSecurityCache.ToolbarItemKey = pcToolbarItemKey NO-ERROR .

        IF AVAILABLE ttToolbarItemSecurityCache THEN DO:
            IF ttToolbarItemSecurityCache.IsRestricted <> ? THEN
                RETURN ttToolbarItemSecurityCache.IsRestricted .

            ASSIGN ttToolbarItemSecurityCache.IsRestricted = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService}:IsRestricted
                    ("STI":U,
                     ttToolbarItemSecurityCache.ToolbarSecurityItemGuid) .

            RETURN ttToolbarItemSecurityCache.IsRestricted .
        END.

        RETURN lToolbarItemDefaultRestricted .

        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LoginCompanyKeyChanged event of the SessionManager class
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LoginCompanyKeyChangedHandler ():

        THIS-OBJECT:ClearCache() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the UserNameChanged event of the SessionManager class
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UserNameChangedHandler ():

        THIS-OBJECT:ClearCache() .

    END METHOD .

END CLASS.
