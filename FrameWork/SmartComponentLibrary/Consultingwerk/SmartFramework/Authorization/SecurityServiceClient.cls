/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SecurityServiceClient
    Purpose     : ISecurityService implementation for the AppServer
                  client
    Syntax      :
    Description : Provides chaching for IsRestricted enquiries
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 18 20:57:07 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Session.*             FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.*  FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartFramework.Authorization.SecurityServiceClient
    IMPLEMENTS ISecurityService:

    DEFINE TEMP-TABLE ttIsRestricted NO-UNDO
        FIELD RealmCode        AS CHARACTER
        FIELD SecurityItemGuid AS CHARACTER
        FIELD IsRestricted     AS LOGICAL
        INDEX Idx IS UNIQUE PRIMARY RealmCode SecurityItemGuid .

    DEFINE TEMP-TABLE ttGetUserRestrictedDefaultLevel NO-UNDO
        FIELD SecurityRealmGuid AS CHARACTER
        FIELD SecurityItemGuid  AS CHARACTER
        FIELD UserGuid          AS CHARACTER
        FIELD LoginCompanyKey   AS CHARACTER
        FIELD DefaultLevel      AS CHARACTER
        INDEX Idx IS UNIQUE PRIMARY SecurityRealmGuid SecurityItemGuid UserGuid LoginCompanyKey.

    DEFINE TEMP-TABLE ttGetGroupRestrictedDefaultLevel NO-UNDO
        FIELD SecurityRealmGuid AS CHARACTER
        FIELD SecurityItemGuid  AS CHARACTER
        FIELD GroupGuid         AS CHARACTER
        FIELD LoginCompanyKey   AS CHARACTER
        FIELD DefaultLevel      AS CHARACTER
        INDEX Idx IS UNIQUE PRIMARY SecurityRealmGuid SecurityItemGuid GroupGuid LoginCompanyKey.

    {Consultingwerk/SmartFramework/Authorization/ttSecurityItemKeys.i}

    DEFINE DATASET dsSecurityItemKeys FOR ttSecurityItemKeys .

    /**
     * Purpose: Constructor for the SecurityServiceClient class
     * Notes:
     */
    CONSTRUCTOR PUBLIC SecurityServiceClient ():
        SUPER ().

        IF CONNECTED (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) THEN
            UNDO, THROW NEW AppError ("The Security Service Client is only supported on an AppServer client."{&TRAN}, 0) .

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the level on which a security item is restricted for a given
     *          group
     * Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
     *          RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
     *          NOTRESTRICTED-BY-REALM
     *          Used by the AuthorizationMaintenanceService
     * @param pcSecurityRealmGuid The security realm
     * @param pcSecurityItemGuid The security item
     * @param pcGroupGuid The group guid
     * @return The level on which a security item is restricted for a given group
     */
    METHOD PUBLIC CHARACTER GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                            pcSecurityItemGuid AS CHARACTER,
                                                            pcGroupGuid AS CHARACTER):

        RETURN THIS-OBJECT:GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid,
                                                           pcSecurityItemGuid,
                                                           pcGroupGuid,
                                                           "":U,
                                                           "":U) .
    END METHOD .

    /**
     * Purpose: Returns the level on which a security item is restricted for a given
     *          group
     * Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
     *          RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
     *          NOTRESTRICTED-BY-REALM
     *          Used by the AuthorizationMaintenanceService
     * @param pcSecurityRealmGuid The security realm
     * @param pcSecurityItemGuid The security item
     * @param pcGroupGuid The group guid
     * @param pcGroupGuids The comma delimited list of group guids that this group is member of
     * @param pcLoginCompanyKey And alternative Login Company Key, when requesting for a different company
     * @return The level on which a security item is restricted for a given group
     */
    METHOD PUBLIC CHARACTER GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                            pcSecurityItemGuid AS CHARACTER,
                                                            pcGroupGuid AS CHARACTER,
                                                            pcGroupGuids AS CHARACTER,
                                                            pcLoginCompanyKey AS CHARACTER):

        DEFINE VARIABLE oParameter AS GetGroupRestrictedDefaultLevelParameter NO-UNDO .

        FIND ttGetGroupRestrictedDefaultLevel WHERE ttGetGroupRestrictedDefaultLevel.SecurityRealmGuid = pcSecurityRealmGuid
                                                AND ttGetGroupRestrictedDefaultLevel.SecurityItemGuid  = pcSecurityItemGuid
                                                AND ttGetGroupRestrictedDefaultLevel.GroupGuid         = pcGroupGuid
            NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        IF AVAILABLE ttGetGroupRestrictedDefaultLevel THEN
            RETURN ttGetGroupRestrictedDefaultLevel.DefaultLevel .

        oParameter = NEW GetGroupRestrictedDefaultLevelParameter (pcSecurityRealmGuid,
                                                                  pcSecurityItemGuid,
                                                                  pcGroupGuid,
                                                                  pcLoginCompanyKey) .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityService":U,
                                                     "GetGroupRestrictedDefaultLevel":U,
                                                     oParameter,
                                                     SessionManager:ContextDataset) .

        CREATE ttGetGroupRestrictedDefaultLevel .
        ASSIGN ttGetGroupRestrictedDefaultLevel.SecurityRealmGuid = pcSecurityRealmGuid
               ttGetGroupRestrictedDefaultLevel.SecurityItemGuid  = pcSecurityItemGuid
               ttGetGroupRestrictedDefaultLevel.GroupGuid         = pcGroupGuid
               ttGetGroupRestrictedDefaultLevel.LoginCompanyKey   = pcLoginCompanyKey
               ttGetGroupRestrictedDefaultLevel.DefaultLevel      = oParameter:DefaultLevel .

        RETURN oParameter:DefaultLevel .

    END METHOD .

    /**
     * Purpose: Returns the level on which a security item is restricted for a given
     *          group
     * Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
     *          RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
     *          NOTRESTRICTED-BY-REALM
     *          Used by the AuthorizationMaintenanceService
     * @param pcSecurityRealmGuid The security realm
     * @param pcSecurityItemGuid The security item
     * @param pcGroupGuid The group guid
     * @param pcGroupGuids The comma delimited list of group guids that this group is member of
     * @return The level on which a security item is restricted for a given group
     */
    METHOD PUBLIC CHARACTER GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                            pcSecurityItemGuid AS CHARACTER,
                                                            pcGroupGuid AS CHARACTER,
                                                            pcGroupGuids AS CHARACTER):

        RETURN THIS-OBJECT:GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid,
                                                           pcSecurityItemGuid,
                                                           pcGroupGuid,
                                                           "":U,
                                                           "":U) .

    END METHOD .

    /**
     * Purpose: Returns the level on which a security item is restricted for a given
     *          user
     * Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
     *          RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
     *          NOTRESTRICTED-BY-REALM
     *          Used by the AuthorizationMaintenanceService
     * @param pcSecurityRealmGuid The security realm
     * @param pcSecurityItemGuid The security item
     * @param pcUserGuid The users security groups
     * @return The level on which a security item is restricted for a given user
     */
    METHOD PUBLIC CHARACTER GetUserRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                           pcSecurityItemGuid AS CHARACTER,
                                                           pcUserGuid AS CHARACTER):

        RETURN THIS-OBJECT:GetUserRestrictedDefaultLevel (pcSecurityRealmGuid,
                                                          pcSecurityItemGuid,
                                                          pcUserGuid,
                                                          "":U,
                                                          "":U) .
    END METHOD .

    /**
     * Purpose: Returns the level on which a security item is restricted for a given
     *          user
     * Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
     *          RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
     *          NOTRESTRICTED-BY-REALM
     *          Used by the AuthorizationMaintenanceService
     * @param pcSecurityRealmGuid The security realm
     * @param pcSecurityItemGuid The security item
     * @param pcUserGuid The user guid
     * @param pcLoginCompanyKey And alternative Login Company Key, when requesting for a different company
     * @param pcGroupGuids The comma delimited list of groups (and parent groups) the user belongs to
     * @return The level on which a security item is restricted for a given user
     */
    METHOD PUBLIC CHARACTER GetUserRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                           pcSecurityItemGuid AS CHARACTER,
                                                           pcUserGuid AS CHARACTER,
                                                           pcGroupGuids AS CHARACTER,
                                                           pcLoginCompanyKey AS CHARACTER):

        DEFINE VARIABLE oParameter AS GetUserRestrictedDefaultLevelParameter NO-UNDO .

        FIND ttGetUserRestrictedDefaultLevel WHERE ttGetUserRestrictedDefaultLevel.SecurityRealmGuid = pcSecurityRealmGuid
                                               AND ttGetUserRestrictedDefaultLevel.SecurityItemGuid  = pcSecurityItemGuid
                                               AND ttGetUserRestrictedDefaultLevel.UserGuid          = pcUserGuid
            NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        IF AVAILABLE ttGetUserRestrictedDefaultLevel THEN
            RETURN ttGetUserRestrictedDefaultLevel.DefaultLevel .

        oParameter = NEW GetUserRestrictedDefaultLevelParameter (pcSecurityRealmGuid,
                                                                 pcSecurityItemGuid,
                                                                 pcUserGuid,
                                                                 pcLoginCompanyKey) .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityService":U,
                                                     "GetUserRestrictedDefaultLevel":U,
                                                     oParameter,
                                                     SessionManager:ContextDataset) .

        CREATE ttGetUserRestrictedDefaultLevel .
        ASSIGN ttGetUserRestrictedDefaultLevel.SecurityRealmGuid = pcSecurityRealmGuid
               ttGetUserRestrictedDefaultLevel.SecurityItemGuid  = pcSecurityItemGuid
               ttGetUserRestrictedDefaultLevel.UserGuid          = pcUserGuid
               ttGetUserRestrictedDefaultLevel.LoginCompanyKey   = pcLoginCompanyKey
               ttGetUserRestrictedDefaultLevel.DefaultLevel      = oParameter:DefaultLevel .

        RETURN oParameter:DefaultLevel .

    END METHOD .

    /**
     * Purpose: Returns the level on which a security item is restricted for a given
     *          user
     * Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
     *          RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
     *          NOTRESTRICTED-BY-REALM
     *          Used by the AuthorizationMaintenanceService
     * @param pcSecurityRealmGuid The security realm
     * @param pcSecurityItemGuid The security item
     * @param pcUserGuid The user guid
     * @param pcGroupGuids The comma delimited list of groups (and parent groups) the user belongs to
     * @return The level on which a security item is restricted for a given user
     */
    METHOD PUBLIC CHARACTER GetUserRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                           pcSecurityItemGuid AS CHARACTER,
                                                           pcUserGuid AS CHARACTER,
                                                           pcGroupGuids AS CHARACTER):

        RETURN THIS-OBJECT:GetUserRestrictedDefaultLevel (pcSecurityRealmGuid,
                                                          pcSecurityItemGuid,
                                                          pcUserGuid) .

    END METHOD .


    /**
     * Purpose: Clears the cache of the SecurityServiceClient
     * Notes:
     */
    METHOD PUBLIC VOID ClearCache ():

        EMPTY TEMP-TABLE ttIsRestricted .
        EMPTY TEMP-TABLE ttGetUserRestrictedDefaultLevel .
        EMPTY TEMP-TABLE ttGetGroupRestrictedDefaultLevel .

    END METHOD .

    /**
     * Purpose: Performs a security authorization check for the specified function
     * Notes:
     * @param pcRealmCode The code of the security realm to verify
     * @param pcSecurityItemGuid The guid of the security item to verify
     * @return Logical value indicating if the specified function is restricted for the current user
     */
    METHOD PUBLIC LOGICAL IsRestricted (pcRealmCode AS CHARACTER,
                                        pcSecurityItemGuid AS CHARACTER):

        DEFINE VARIABLE oParameter AS IsRestrictedParameter NO-UNDO .

        FIND ttIsRestricted WHERE ttIsRestricted.RealmCode        = pcRealmCode
                              AND ttIsRestricted.SecurityItemGuid = pcSecurityItemGuid
            NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        IF AVAILABLE ttIsRestricted THEN
            RETURN ttIsRestricted.IsRestricted .

        oParameter = NEW IsRestrictedParameter (pcRealmCode, pcSecurityItemGuid) .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityService":U,
                                                     "IsRestricted":U,
                                                     oParameter,
                                                     SessionManager:ContextDataset) .

        CREATE ttIsRestricted .
        ASSIGN ttIsRestricted.RealmCode        = pcRealmCode
               ttIsRestricted.SecurityItemGuid = pcSecurityItemGuid
               ttIsRestricted.IsRestricted     = oParameter:IsRestricted .

        RETURN oParameter:IsRestricted .

    END METHOD .

    /**
     * Purpose: Performs a security authorization check for the specified function
     * Notes:   Directly accesses the SmartSecurityAssignment database table for
     *          Performance reasons and because a caching other than what the DB
     *          does anyway is not useful for the security settings
     * @param pcRealmCode The code of the security realm to verify
     * @param pcSecurityItemGuid The guid of the security item to verify
     * @param pcUserGuid The Guid of the user to check
     * @param pcCurrentLoginCompanyGuid The Guid of the login company to check
     * @param pcUserGroupGuids The comma delimited list of user group guids the user belongs to
     * @param plRestricted OUTPUT Returns if the user is not allowed to perform the action in question
     */
    METHOD PUBLIC VOID IsRestrictedCheck (pcRealmCode AS CHARACTER,
                                          pcSecurityItemGuid AS CHARACTER,
                                          pcUserGuid AS CHARACTER,
                                          pcCurrentLoginCompanyGuid AS CHARACTER,
                                          pcUserGroupGuids AS CHARACTER,
                                          OUTPUT plRestricted AS LOGICAL):

        DEFINE VARIABLE cReason AS CHARACTER NO-UNDO.

        THIS-OBJECT:IsRestrictedCheck (pcRealmCode,
                                       pcSecurityItemGuid,
                                       pcUserGuid,
                                       pcCurrentLoginCompanyGuid,
                                       pcUserGroupGuids,
                                       OUTPUT plRestricted,
                                       FALSE,
                                       OUTPUT cReason) .

    END METHOD .

    /**
     * Purpose: Performs a security authorization check for the specified function
     * Notes:   Directly accesses the SmartSecurityAssignment database table for
     *          Performance reasons and because a caching other than what the DB
     *          does anyway is not useful for the security settings
     * @param pcRealmCode The code of the security realm to verify
     * @param pcSecurityItemGuid The guid of the security item to verify
     * @param pcUserGuid The Guid of the user to check
     * @param pcCurrentLoginCompanyGuid The Guid of the login company to check
     * @param pcUserGroupGuids The comma delimited list of user group guids the user belongs to
     * @param plRestricted OUTPUT Returns if the user is not allowed to perform the action in question
     * @param pcReason OUTPUT Returns the reason based on which setting the user is receiving the restriction or not
     */
    METHOD PUBLIC VOID IsRestrictedCheck (pcRealmCode AS CHARACTER,
                                          pcSecurityItemGuid AS CHARACTER,
                                          pcUserGuid AS CHARACTER,
                                          pcCurrentLoginCompanyGuid AS CHARACTER,
                                          pcUserGroupGuids AS CHARACTER,
                                          OUTPUT plRestricted AS LOGICAL,
                                          OUTPUT pcReason AS CHARACTER):

        THIS-OBJECT:IsRestrictedCheck (pcRealmCode,
                                       pcSecurityItemGuid,
                                       pcUserGuid,
                                       pcCurrentLoginCompanyGuid,
                                       pcUserGroupGuids,
                                       OUTPUT plRestricted,
                                       TRUE,
                                       OUTPUT pcReason) .

    END METHOD .

    /**
     * Purpose: Performs a security authorization check for the specified function
     * Notes:   Directly accesses the SmartSecurityAssignment database table for
     *          Performance reasons and because a caching other than what the DB
     *          does anyway is not useful for the security settings
     * @param pcRealmCode The code of the security realm to verify
     * @param pcSecurityItemGuid The guid of the security item to verify
     * @param pcUserGuid The Guid of the user to check
     * @param pcCurrentLoginCompanyGuid The Guid of the login company to check
     * @param pcUserGroupGuids The comma delimited list of user group guids the user belongs to
     * @param plRestricted OUTPUT Returns if the user is not allowed to perform the action in question
     * @param plIncludeReason Logical flag indicating if the pcReason should be populated
     * @param pcReason OUTPUT Returns the reason based on which setting the user is receiving the restriction or not
     */
    METHOD PUBLIC VOID IsRestrictedCheck (pcRealmCode AS CHARACTER,
                                          pcSecurityItemGuid AS CHARACTER,
                                          pcUserGuid AS CHARACTER,
                                          pcCurrentLoginCompanyGuid AS CHARACTER,
                                          pcUserGroupGuids AS CHARACTER,
                                          OUTPUT plRestricted AS LOGICAL,
                                          plIncludeReason AS LOGICAL,
                                          OUTPUT pcReason AS CHARACTER):


        DEFINE VARIABLE oParameter AS IsRestrictedCheckParameter NO-UNDO .
        DEFINE VARIABLE hDataset   AS HANDLE                     NO-UNDO .

        oParameter = NEW IsRestrictedCheckParameter () .

        ASSIGN oParameter:RealmCode        = pcRealmCode
               oParameter:SecurityItemGuid = pcSecurityItemGuid
               oParameter:UserGuid         = pcUserGuid
               oParameter:LoginCompanyGuid = pcCurrentLoginCompanyGuid
               oParameter:ReturnGroups     = TRUE .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityService":U,
                                                     "IsRestrictedCheck":U,
                                                     INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                     oParameter) .

        ASSIGN plRestricted = oParameter:IsRestricted
               pcReason     = oParameter:Reason .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.


    END METHOD .

    /**
     * Purpose: Returns a temp-table with all restricted security item keys
     *          of the given security realm
     * Notes:   Is expected to leverage the lookup definition form the realm
     *          service to determine the list of available security items
     * @param pcRealmCode The code of the security realm to verify
     * @param ttSecurityItemKeys The temp-table with the resulting security item keys, from Consultingwerk/SmartFramework/Authorization/ttSecurityItemKeys.i
     */
    METHOD PUBLIC VOID RetrieveAllRestrictedSecurityItems (pcRealmCode AS CHARACTER,
                                                           OUTPUT TABLE ttSecurityItemKeys):

        DEFINE VARIABLE oParameter AS SecurityRealmParameter NO-UNDO .

        ASSIGN oParameter = NEW SecurityRealmParameter(pcRealmCode) .

        DATASET dsSecurityItemKeys:EMPTY-DATASET().

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityService":U,
                                                     "RetrieveAllRestrictedSecurityItems":U,
                                                     INPUT-OUTPUT DATASET dsSecurityItemKeys,
                                                     oParameter) .
    END METHOD .

    /**
     * Purpose: Returns a temp-table with all unrestricted security item keys
     *          of the given security realm
     * Notes:   Is expected to leverage the lookup definition form the realm
     *          service to determine the list of available security items
     * @param pcRealmCode The code of the security realm to verify
     * @param ttSecurityItemKeys The temp-table with the resulting security item keys, from Consultingwerk/SmartFramework/Authorization/ttSecurityItemKeys.i
     */
    METHOD PUBLIC VOID RetrieveAllUnrestrictedSecurityItems (pcRealmCode AS CHARACTER,
                                                             OUTPUT TABLE ttSecurityItemKeys):

        DEFINE VARIABLE oParameter AS SecurityRealmParameter NO-UNDO .

        ASSIGN oParameter = NEW SecurityRealmParameter(pcRealmCode) .

        DATASET dsSecurityItemKeys:EMPTY-DATASET().

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.SmartFramework.Authorization.SecurityService":U,
                                                     "RetrieveAllUnrestrictedSecurityItems":U,
                                                     INPUT-OUTPUT DATASET dsSecurityItemKeys,
                                                     oParameter) .
    END METHOD .

END CLASS.
