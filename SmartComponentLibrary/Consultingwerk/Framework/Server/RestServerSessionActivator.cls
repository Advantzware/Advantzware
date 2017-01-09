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
    File        : RestServerSessionActivator
    Purpose     : Performs session activation for REST requests
    Syntax      :
    Description : Requires SmartFramework
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 16 11:32:03 CET 2016
    Notes       : Although in our default setup this class is loaded as a
                  service using services.xml there is no need to implement
                  an interface. This class subscribes itself to events of
                  the service interface and does not have any public methods
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                    FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Session.*             FROM PROPATH .
USING Consultingwerk.Framework.Server.*              FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*         FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.Framework.Server.RestServerSessionActivator:

    &IF DEFINED (SmartFramework) NE 0 &THEN

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ServerSessionActivator class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RestServerSessionActivator ():

        SUPER ().

        ServiceInterface:Activated:Subscribe (ActivateServerSession) .
        ServiceInterface:Deactivated:Subscribe (DeactivateServerSession) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates a server session
        Notes:
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ActivateServerSession (e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE hPrincipal           AS HANDLE                           NO-UNDO.
        DEFINE VARIABLE cName                AS CHARACTER                        NO-UNDO.
        DEFINE VARIABLE cDomain              AS CHARACTER                        NO-UNDO.

        DEFINE VARIABLE oLoginCompany        AS LoginCompanyDatasetModel         NO-UNDO .
        DEFINE VARIABLE oUser                AS UserDatasetModel                 NO-UNDO .
        DEFINE VARIABLE oGetGroupsParameter  AS GetUserGroupAssignmentsParameter NO-UNDO .
        DEFINE VARIABLE hDataset             AS HANDLE                           NO-UNDO .
        DEFINE VARIABLE oContextDatasetStore AS IContextDatasetStore             NO-UNDO .

/*        LogManager:WriteSeparator() .                                               */
/*        LogManager:WriteObjectProperties (SESSION:CURRENT-REQUEST-INFO, "REQUEST") .*/

        IF VALID-OBJECT (SESSION:CURRENT-REQUEST-INFO) THEN
            ASSIGN hPrincipal = SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal().

/*        LogManager:WriteSeparator() .                                      */
/*        LogManager:WriteWidgetProperties (hPrincipal, "CLIENT-PRINCIPAL") .*/
/*        LogManager:WriteSeparator() .                                      */

        IF VALID-HANDLE (hPrincipal) THEN DO:

            hPrincipal:VALIDATE-SEAL ({Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider}:GetValue ("clientPrincipalSeal":U)) .

            IF hPrincipal:SESSION-ID > "":U THEN
                ASSIGN oContextDatasetStore = {Consultingwerk/get-service.i Consultingwerk.Framework.Server.IContextDatasetStore} .

            IF VALID-OBJECT (oContextDatasetStore) THEN
                IF oContextDatasetStore:RestoreSessionContext (hPrincipal) THEN
                    RETURN .

            ASSIGN cName   = hPrincipal:USER-ID
                   cDomain = hPrincipal:DOMAIN-NAME .

            oUser = NEW UserDatasetModel () .

            IF cDomain > "":U THEN DO:
                oLoginCompany = NEW LoginCompanyDatasetModel () .
                oLoginCompany:SmartLoginCompany:Filter:LoginCompanyTenantDomain:EQ (cDomain):Run () .

                IF NOT oLoginCompany:SmartLoginCompany:Available THEN
                    UNDO, THROW NEW Exception (SUBSTITUTE ("Unable to validate user because no login company exists with the provided domain name: &1"{&TRAN}, cDomain), 0) .

                /* locate Domain specific user */
                oUser:SmartUser:Filter:LoginCompanyGuid:EQ (oLoginCompany:SmartLoginCompany:LoginCompanyGuid) .
                oUser:SmartUser:Filter:UserName:EQ (cName):Run () .

            END.

            IF NOT oUser:SmartUser:Available THEN DO:
                oUser = NEW UserDatasetModel () .
                oUser:SmartUser:Filter:LoginCompanyGuid:EQ ("":U) .
                oUser:SmartUser:Filter:UserName:EQ (cName):Run () .
            END.

            IF NOT oUser:SmartUser:Available THEN
                UNDO, THROW NEW Exception (SUBSTITUTE ("Invalid user."{&TRAN}, cDomain), 0) .

            IF VALID-OBJECT (oLoginCompany) THEN
                ASSIGN SessionManager:LoginCompanyKey              =  oLoginCompany:SmartLoginCompany:LoginCompanyGuid
                       SessionManager:LoginCompanyName             =  oLoginCompany:SmartLoginCompany:LoginCompanyName
                       SessionManager:LoginCompanyReferenceChar    =  oLoginCompany:SmartLoginCompany:LoginCompanyReferenceChar
                       SessionManager:LoginCompanyReferenceDecimal =  oLoginCompany:SmartLoginCompany:LoginCompanyReferenceDecimal
                       SessionManager:LoginCompanyReferenceInteger =  oLoginCompany:SmartLoginCompany:LoginCompanyReferenceInteger
                       SessionManager:LoginCompanyShortName        =  oLoginCompany:SmartLoginCompany:LoginCompanyShort .
            ELSE
                ASSIGN SessionManager:LoginCompanyKey                         =  oLoginCompany:SmartLoginCompany:LoginCompanyGuid
                       SessionManager:LoginCompanyName                        =  oLoginCompany:SmartLoginCompany:LoginCompanyName
                       SessionManager:LoginCompanyReferenceChar               =  oLoginCompany:SmartLoginCompany:LoginCompanyReferenceChar
                       SessionManager:LoginCompanyReferenceDecimal            =  oLoginCompany:SmartLoginCompany:LoginCompanyReferenceDecimal
                       SessionManager:LoginCompanyReferenceInteger            =  oLoginCompany:SmartLoginCompany:LoginCompanyReferenceInteger
                       SessionManager:LoginCompanyShortName                   =  oLoginCompany:SmartLoginCompany:LoginCompanyShort .

            ASSIGN SessionManager:UserName            = oUser:SmartUser:UserName
                   SessionManager:UserKey             = oUser:SmartUser:UserGuid
                   SessionManager:LanguageDisplayName = oUser:SmartUser:LanguageName
                   SessionManager:LanguageIsoCode     = oUser:SmartUser:LanguageIsoCode
                   SessionManager:LanguageKey         = oUser:SmartUser:LanguageGuid .

            oGetGroupsParameter = NEW GetUserGroupAssignmentsParameter (SessionManager:UserName,
                                                                        SessionManager:LoginCompanyKey) .

            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                           "GetUserGroupAssignments":U,
                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                           oGetGroupsParameter) .

            ASSIGN
                SessionManager:UserGroupKeys  = oGetGroupsParameter:GroupGuids
                SessionManager:UserGroupNames = oGetGroupsParameter:GroupNames .
        END.
        ELSE
            SessionManager:Reset() .

        IF FrameworkSettings:DebugMode AND NOT ErrorHelper:CurrentStacktrace MATCHES "*as_startup*":U THEN
            SessionManager:LogSessionManagerProperties () .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deactivates a server session
        Notes:
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DeactivateServerSession (e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE hPrincipal           AS HANDLE                           NO-UNDO.
        DEFINE VARIABLE oContextDatasetStore AS IContextDatasetStore             NO-UNDO .

        IF VALID-OBJECT (SESSION:CURRENT-REQUEST-INFO) THEN
            ASSIGN hPrincipal = SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal().

        IF VALID-HANDLE (hPrincipal) AND hPrincipal:SESSION-ID > "":U AND VALID-HANDLE (SessionManager:ContextDataset) THEN
            ASSIGN oContextDatasetStore = {Consultingwerk/get-service.i Consultingwerk.Framework.Server.IContextDatasetStore} .

        IF VALID-OBJECT (oContextDatasetStore) THEN
            oContextDatasetStore:StoreSessionContext (hPrincipal) .

        SessionManager:Reset() .

    END METHOD .

    &ENDIF

END CLASS.
