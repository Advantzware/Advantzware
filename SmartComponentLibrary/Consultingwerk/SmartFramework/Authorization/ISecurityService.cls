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
    File        : ISecurityService
    Purpose     : Service Interface for Security Enquiries
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 29 02:35:03 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.Authorization.ISecurityService:

    {Consultingwerk/SmartFramework/Authorization/ttSecurityItemKeys.i}

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
                                                            pcGroupGuid AS CHARACTER) .

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
                                                            pcGroupGuids AS CHARACTER) .

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
                                                            pcLoginCompanyKey AS CHARACTER) .

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
     * @return The level on which a security item is restricted for a given user
     */
    METHOD PUBLIC CHARACTER GetUserRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                           pcSecurityItemGuid AS CHARACTER,
                                                           pcUserGuid AS CHARACTER) .

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
                                                           pcGroupGuids AS CHARACTER) .

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
                                                           pcLoginCompanyKey AS CHARACTER) .

    /**
     * Purpose: Performs a security authorization check for the specified function
     * Notes:
     * @param pcRealmCode The code of the security realm to verify
     * @param pcSecurityItemGuid The guid of the security item to verify
     * @return Logical value indicating if the specified function is restricted for the current user
     */
    METHOD PUBLIC LOGICAL IsRestricted (pcRealmCode AS CHARACTER,
                                        pcSecurityItemGuid AS CHARACTER).

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
                                          OUTPUT plRestricted AS LOGICAL) .

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
                                          OUTPUT pcReason AS CHARACTER) .

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
                                          OUTPUT pcReason AS CHARACTER) .

    /**
     * Purpose: Returns a temp-table with all restricted security item keys
     *          of the given security realm
     * Notes:   Is expected to leverage the lookup definition form the realm
     *          service to determine the list of available security items
     * @param pcRealmCode The code of the security realm to verify
     * @param ttSecurityItemKeys The temp-table with the resulting security item keys, from Consultingwerk/SmartFramework/Authorization/ttSecurityItemKeys.i
     */
    METHOD PUBLIC VOID RetrieveAllRestrictedSecurityItems (pcRealmCode AS CHARACTER,
                                                           OUTPUT TABLE ttSecurityItemKeys).

    /**
     * Purpose: Returns a temp-table with all unrestricted security item keys
     *          of the given security realm
     * Notes:   Is expected to leverage the lookup definition form the realm
     *          service to determine the list of available security items
     * @param pcRealmCode The code of the security realm to verify
     * @param ttSecurityItemKeys The temp-table with the resulting security item keys, from Consultingwerk/SmartFramework/Authorization/ttSecurityItemKeys.i
     */
    METHOD PUBLIC VOID RetrieveAllUnrestrictedSecurityItems (pcRealmCode AS CHARACTER,
                                                             OUTPUT TABLE ttSecurityItemKeys).

END INTERFACE.
