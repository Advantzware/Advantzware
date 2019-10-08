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
    File        : SecurityRealmInfoService
    Purpose     : Service class that provides access to security realm
                  information
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 28 18:01:04 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Assertion.*                    FROM PROPATH .
USING Consultingwerk.Framework.*                    FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityRealmInfoService
    IMPLEMENTS IBusinessService:

    { Consultingwerk/SmartFramework/Authorization/dsSecurityRealm.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityRealmInfoService class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityRealmInfoService ():
        SUPER ().

        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO .

        oRequest = NEW FetchDataRequest ("eSmartSecurityRealm":U,
                                         "FOR EACH eSmartSecurityRealm":U) .

        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.SecurityRealmBusinessEntity":U,
                                    oRequest,
                                    OUTPUT DATASET dsSecurityRealm) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the description of the specified security item
        Notes:
        @param pcRealmGuid The Guid of the requested security realm
        @param pcItemGuid The Guid of the requested security item
        @return The description of the specified security item
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSecurityItemDescription (pcRealmGuid AS CHARACTER,
                                                        pcItemGuid AS CHARACTER):

        DEFINE VARIABLE oService AS ISecurityRealmService NO-UNDO .

        oService = THIS-OBJECT:GetSecurityRealmService (pcRealmGuid) .

        RETURN oService:GetDescription (pcItemGuid) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the code of the specified security realm
        Notes:
        @param pcRealmGuid The Guid of the requested security realm
        @return The code of the specified security realm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSecurityRealmCode (pcRealmGuid AS CHARACTER):

        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmGuid = pcRealmGuid NO-ERROR .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .

        RETURN eSmartSecurityRealm.SecurityRealmCode .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the security assignment for the given security realm is
                 dependent on the login company
        Notes:
        @param pcRealmGuid The Guid of the requested security realm
        @return Logical value indicating if the security realm is dependent on the login company
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GetSecurityRealmCompanyDependent (pcRealmGuid AS CHARACTER):

        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmGuid = pcRealmGuid NO-ERROR .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .

        RETURN eSmartSecurityRealm.CompanyDependent .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the description of the specified security realm
        Notes:
        @param pcRealmGuid The Guid of the requested security realm
        @return The description of the specified security realm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSecurityRealmDescription (pcRealmGuid AS CHARACTER):

        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmGuid = pcRealmGuid NO-ERROR .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .

        RETURN eSmartSecurityRealm.SecurityRealmDescription .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Security Realm supports empty key values
        Notes:
        @param pcRealmGuid The Guid of the requested security realm
        @return Logical value indicating if the realm supports empty key values
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GetSecurityRealmEmptyKeyAllowed (pcRealmGuid AS CHARACTER):

        RETURN THIS-OBJECT:GetSecurityRealmService (pcRealmGuid):EmptyKeyAllowed .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Guid of the specified security realm
        Notes:
        @param pcRealmCode The code of the requested security realm
        @return The Guid of the specified security realm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetSecurityRealmGuid (pcRealmCode AS CHARACTER):

        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmCode = pcRealmCode NO-ERROR .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .

        RETURN eSmartSecurityRealm.SecurityRealmGuid .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the SmartBusinessEntityLookupDescriptor instance for the specified security realm
        Notes:
        @param poParameter The SecurityRealmLookupParameter instance with the data for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetSecurityRealmLookupParameter (poParameter AS SecurityRealmLookupParameter):

        DEFINE VARIABLE oRealmService AS ISecurityRealmService NO-UNDO .

        ObjectAssert:IsValid (poParameter, "SecurityRealmLookupParameter":U) .

        oRealmService = THIS-OBJECT:GetSecurityRealmService (poParameter:SecurityRealmGuid) .

        IF NOT VALID-OBJECT (poParameter:LookupDescriptor) THEN
            poParameter:LookupDescriptor = NEW SmartBusinessEntityLookupDescriptor () .

        oRealmService:AssignLookupDescriptorProperties (poParameter:LookupDescriptor) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the ISecurityRealmService instance for the specified security realm
        Notes:
        @param pcRealmGuid The Guid of the requested security realm
        @return The ISecurityRealmService instance for the specified security realm
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ISecurityRealmService GetSecurityRealmService (pcRealmGuid AS CHARACTER):

        DEFINE VARIABLE oService AS ISecurityRealmService NO-UNDO .
        DEFINE VARIABLE oClass   AS Progress.Lang.Class   NO-UNDO .

        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmGuid = pcRealmGuid NO-ERROR .

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .

        ASSIGN oClass   = Progress.Lang.Class:GetClass (eSmartSecurityRealm.SecurityRealmService)
               oService = CAST (FrameworkSettings:ServiceContainer:GetService (oClass),
                                ISecurityRealmService) .

        IF VALID-OBJECT (oService) THEN
            RETURN oService .

        oService = CAST (oClass:New(), ISecurityRealmService) .

        FrameworkSettings:ServiceContainer:AddService (oClass, oService) .

        RETURN oService .

    END METHOD .

END CLASS.
