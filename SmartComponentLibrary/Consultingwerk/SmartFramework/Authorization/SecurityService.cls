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
    File        : SecurityService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 29 02:38:10 CEST 2013
    Notes       : This service is intended to run on the appserver or 
                  a client-server session
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Assertion.*                     FROM PROPATH . 
USING Consultingwerk.Framework.*                     FROM PROPATH .
USING Consultingwerk.Framework.Session.*             FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH . 
USING Consultingwerk.SmartFramework.Authorization.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*         FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .   
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityService 
    IMPLEMENTS ISecurityService, IBusinessService: 

    DEFINE VARIABLE cUserGuid AS CHARACTER          NO-UNDO .
    DEFINE VARIABLE oEntity   AS UserBusinessEntity NO-UNDO . 

    {Consultingwerk/SmartFramework/Authentication/dsUser.i}
    {Consultingwerk/SmartFramework/Authorization/dsSecurityRealm.i}

    {Consultingwerk/SmartFramework/Authorization/ttUserGoups.i}

    DEFINE DATASET dsUserGroups FOR ttUserGroups . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityService class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SecurityService ():
		SUPER ().
				
		SessionManager:UserNameChanged:Subscribe (UserNameChangedHandler) .
        SessionManager:LoginCompanyKeyChanged:Subscribe (LoginCompanyKeyChangedHandler) .
				
        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.SecurityRealmBusinessEntity":U,
                                    NEW FetchDataRequest ("eSmartSecurityRealm":U,
                                                          "FOR EACH eSmartSecurityRealm":U),
                                    OUTPUT DATASET dsSecurityRealm) .

        /* Mike Fechner, Consultingwerk Ltd. 16.10.2013
           The Security Service keeps it's own instance of the UserBusinessEntity
           It makes sense to exceptionally bypass the ServiceInterface here as the 
           ServiceInterface might implement RequestAuthorization and a user might 
           not be allows to see users for maintaining them. But the user needs to
           be able to retrieve his own user record from the SecurityService */
        oEntity = NEW UserBusinessEntity () . 
				
	    ServiceManager:RegisterBusinessService ("Consultingwerk.SmartFramework.Authorization.SecurityService":U,
	                                            THIS-OBJECT) .
				
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Determines the Guid of the current user 
        Notes:   Based on the SessionManager:UserName and SessionManager:LoginCompanyKey   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID DetermineUserGuid ():
		
        DEFINE VARIABLE cQueryString AS CHARACTER          NO-UNDO .

        ASSIGN cUserGuid = "":U . 

        IF SessionManager:UserName > "":U THEN .
        ELSE 
            RETURN . 

        ASSIGN cQueryString = SUBSTITUTE ("FOR EACH eSmartUser WHERE (eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2)":U,
                                          QUOTER (SessionManager:UserName),
                                          QUOTER ("":U)) .

        IF SessionManager:LoginCompanyKey > "":U THEN 
            ASSIGN cQueryString = cQueryString + SUBSTITUTE (" OR (eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2":U,
                                                             QUOTER (SessionManager:UserName),
                                                             QUOTER (SessionManager:LoginCompanyKey)) .        
        
        oEntity:FetchData (NEW FetchDataRequest ("eSmartUser":U, cQueryString),
                           OUTPUT DATASET dsUser) .

        IF SessionManager:LoginCompanyKey > "":U THEN DO: 
            FIND FIRST eSmartUser WHERE eSmartUser.UserName         = SessionManager:UserName   
                                    AND eSmartUser.LoginCompanyGuid = SessionManager:LoginCompanyKey
                    NO-ERROR . 
                    
            IF AVAILABLE eSmartUser THEN DO: 
                ASSIGN cUserGuid = eSmartUser.UserGuid . 
                RETURN . 
            END.
        END.
        
        FIND FIRST eSmartUser NO-ERROR . 
        
        IF AVAILABLE eSmartUser THEN 
            ASSIGN cUserGuid = eSmartUser.UserGuid .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 group  
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the SecurityServiceClient
        @param poParameter The GetGroupRestrictedDefaultLevelParameter instance with the data for this call 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetGroupRestrictedDefaultLevel (poParameter AS GetGroupRestrictedDefaultLevelParameter):
        
        DEFINE VARIABLE cGroupIds           AS CHARACTER                NO-UNDO .
        DEFINE VARIABLE oGetGroupsParameter AS GetParentGroupsParameter NO-UNDO .
        DEFINE VARIABLE hDataset            AS HANDLE                   NO-UNDO .

        ObjectAssert:IsValid (poParameter, "poParameter":U) .

        oGetGroupsParameter = NEW GetParentGroupsParameter (poParameter:GroupGuid) .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U,
                                       "GetParentGroups":U,
                                       DATASET-HANDLE hDataset,
                                       oGetGroupsParameter) .

        ASSIGN cGroupIds = oGetGroupsParameter:ParentGroupGuids .
        
        ASSIGN poParameter:DefaultLevel = THIS-OBJECT:GetGroupRestrictedDefaultLevel (poParameter:SecurityRealmGuid,
                                                                                      poParameter:SecurityItemGuid,
                                                                                      poParameter:GroupGuid,
                                                                                      cGroupIds,
                                                                                      poParameter:LoginCompanyKey) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .        
        END FINALLY.
        
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 group
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the AuthorizationMaintenanceService
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcGroupGuid The groups guid
        @return The level on which a security item is restricted for a given group
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                            pcSecurityItemGuid AS CHARACTER,
                                                            pcGroupGuid AS CHARACTER):
        
        DEFINE VARIABLE cGroupIds           AS CHARACTER                NO-UNDO .
        DEFINE VARIABLE oGetGroupsParameter AS GetParentGroupsParameter NO-UNDO .
        DEFINE VARIABLE hDataset            AS HANDLE                   NO-UNDO .

        oGetGroupsParameter = NEW GetParentGroupsParameter (pcGroupGuid) .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U,
                                       "GetParentGroups":U,
                                       DATASET-HANDLE hDataset,
                                       oGetGroupsParameter) .

        ASSIGN cGroupIds = oGetGroupsParameter:ParentGroupGuids .

        RETURN THIS-OBJECT:GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid,
                                                            pcSecurityItemGuid,
                                                            pcGroupGuid,
                                                            cGroupIds,
                                                            "":U) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .        
        END FINALLY.

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 group 
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the AuthorizationMaintenanceService
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcGroupGuid The group guid
        @param pcGroupGuids The comma delimited list of group guids that this group is member of
        @return The level on which a security item is restricted for a given group
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                            pcSecurityItemGuid AS CHARACTER,
                                                            pcGroupGuid AS CHARACTER,
                                                            pcGroupGuids AS CHARACTER):

        RETURN GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid,
                                               pcSecurityItemGuid,
                                               pcGroupGuid,
                                               pcGroupGuids,
                                               "":U) .
    END METHOD . 
                              
    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 group 
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the AuthorizationMaintenanceService
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcGroupGuid The group guid
        @param pcGroupGuids The comma delimited list of group guids that this group is member of
        @param pcLoginCompanyKey And alternative Login Company Key, when requesting for a different company 
        @return The level on which a security item is restricted for a given group
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetGroupRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                            pcSecurityItemGuid AS CHARACTER,
                                                            pcGroupGuid AS CHARACTER,
                                                            pcGroupGuids AS CHARACTER,
                                                            pcLoginCompanyKey AS CHARACTER):
                              
        DEFINE VARIABLE cCompanyKey AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER NO-UNDO.                                                                
                                                                
        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmGuid = pcSecurityRealmGuid NO-ERROR . 

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .
        
        IF eSmartSecurityRealm.CompanyDependent = TRUE THEN DO:
            IF pcLoginCompanyKey > "":U THEN 
                ASSIGN cCompanyKey = pcLoginCompanyKey . 
            ELSE 
                ASSIGN cCompanyKey = SessionManager:LoginCompanyKey .
        END.                                                             
                                                             
        /* Restricted for parent groups */
        DO i = 1 TO NUM-ENTRIES (pcGroupGuids):
            FIND SmartSecurityAssignment WHERE SmartSecurityAssignment.SecurityRealmGuid = eSmartSecurityRealm.SecurityRealmGuid 
                                           AND SmartSecurityAssignment.SecurityItemGuid  = pcSecurityItemGuid 
                                           AND SmartSecurityAssignment.LoginCompanyGuid  = cCompanyKey
                                           AND SmartSecurityAssignment.GroupGuid         = ENTRY (i, pcGroupGuids)
                                           AND SmartSecurityAssignment.UserGuid          = "":U NO-LOCK NO-ERROR . 

            IF AVAILABLE SmartSecurityAssignment THEN DO:
                /* Ensure uncached data */
                FIND CURRENT SmartSecurityAssignment NO-LOCK NO-ERROR . 
    
                IF SmartSecurityAssignment.Restricted THEN 
                    RETURN RestrictedDefaultLevel:RESTRICTED-BY-GROUP . 
                ELSE 
                    RETURN RestrictedDefaultLevel:NOTRESTRICTED-BY-GROUP . 
            END. 
        END.        
                                                
        /* Restricted for everybody? */                                        
        FIND SmartSecurityAssignment WHERE SmartSecurityAssignment.SecurityRealmGuid = eSmartSecurityRealm.SecurityRealmGuid 
                                       AND SmartSecurityAssignment.SecurityItemGuid  = pcSecurityItemGuid
                                       AND SmartSecurityAssignment.LoginCompanyGuid  = cCompanyKey
                                       AND SmartSecurityAssignment.GroupGuid         = "":U
                                       AND SmartSecurityAssignment.UserGuid          = "":U NO-LOCK NO-ERROR . 

        IF AVAILABLE SmartSecurityAssignment THEN DO:
            /* Ensure uncached data */
            FIND CURRENT SmartSecurityAssignment NO-LOCK NO-ERROR . 
            
            IF SmartSecurityAssignment.Restricted THEN 
                RETURN RestrictedDefaultLevel:RESTRICTED-FOR-ALL . 
            ELSE 
                RETURN RestrictedDefaultLevel:NOTRESTRICTED-FOR-ALL . 
        END.                                        
                                        
        IF eSmartSecurityRealm.DefaultRestricted THEN 
            RETURN RestrictedDefaultLevel:RESTRICTED-BY-REALM . 
        ELSE 
            RETURN RestrictedDefaultLevel:NOTRESTRICTED-BY-REALM . 
                                        
        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR . /* reset error-status from FIND NO-ERROR */       
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 user  
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the SecurityServiceClient
        @param poParameter The GetUserRestrictedDefaultLevelParameter instance with the data for this call 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetUserRestrictedDefaultLevel (poParameter AS GetUserRestrictedDefaultLevelParameter):

        DEFINE VARIABLE cGroupIds             AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE oGetGroupsParameter   AS GetUserGroupAssignmentsParameter NO-UNDO . 
        DEFINE VARIABLE oUser                 AS UserDatasetModel                 NO-UNDO . 
        DEFINE VARIABLE hDataset              AS HANDLE                           NO-UNDO .
        
        ObjectAssert:IsValid (poParameter, "poParameter":U) .

        ASSIGN oUser = NEW UserDatasetModel (poParameter:UserGuid) .

        oGetGroupsParameter = NEW GetUserGroupAssignmentsParameter (oUser:SmartUser:UserName,
                                                                    Consultingwerk.Framework.Session.SessionManager:LoginCompanyKey) .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                       "GetUserGroupAssignments":U,
                                       DATASET-HANDLE hDataset,
                                       oGetGroupsParameter) .
                                                             
        ASSIGN cGroupIds = oGetGroupsParameter:GroupGuids .                                                                                             
        
        ASSIGN poParameter:DefaultLevel = THIS-OBJECT:GetUserRestrictedDefaultLevel (poParameter:SecurityRealmGuid,
                                                                                     poParameter:SecurityItemGuid,
                                                                                     poParameter:UserGuid,
                                                                                     cGroupIds,
                                                                                     poParameter:LoginCompanyKey) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .        
        END FINALLY.

    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 user  
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the AuthorizationMaintenanceService
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcUserGuid The users security groups
        @return The level on which a security item is restricted for a given user
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetUserRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
	                                                       pcSecurityItemGuid AS CHARACTER,
	                                                       pcUserGuid AS CHARACTER):
		
        DEFINE VARIABLE cGroupIds             AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE oGetGroupsParameter   AS GetUserGroupAssignmentsParameter NO-UNDO . 
        DEFINE VARIABLE oUser                 AS UserDatasetModel                 NO-UNDO . 
        DEFINE VARIABLE hDataset              AS HANDLE                           NO-UNDO .
        
        ASSIGN oUser = NEW UserDatasetModel (pcUserGuid) .

        oGetGroupsParameter = NEW GetUserGroupAssignmentsParameter (oUser:SmartUser:UserName,
                                                                    Consultingwerk.Framework.Session.SessionManager:LoginCompanyKey) .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                       "GetUserGroupAssignments":U,
                                       DATASET-HANDLE hDataset,
                                       oGetGroupsParameter) .
                                                             
        ASSIGN cGroupIds = oGetGroupsParameter:GroupGuids .                                                                                             

        RETURN THIS-OBJECT:GetUserRestrictedDefaultLevel (pcSecurityRealmGuid,
                                                          pcSecurityItemGuid,
                                                          pcUserGuid,
                                                          cGroupIds, 
                                                          "":U) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .        
        END FINALLY.

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 user  
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the AuthorizationMaintenanceService
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcUserGuid The user guid
        @param pcGroupGuids The comma delimited list of groups (and parent groups) the user belongs to
        @return The level on which a security item is restricted for a given user
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetUserRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                           pcSecurityItemGuid AS CHARACTER,
                                                           pcUserGuid AS CHARACTER,
                                                           pcGroupGuids AS CHARACTER):

        RETURN GetUserRestrictedDefaultLevel (pcSecurityRealmGuid,
                                              pcSecurityItemGuid,
                                              pcUserGuid,
                                              pcGroupGuids,
                                              "":U) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the level on which a security item is restricted for a given
                 user  
        Notes:   Possible values: RESTRICTED-BY-GROUP, NOTRESTRICTED-BY-GROUP,
                 RESTRICTED-FOR-ALL, NOTRESTRICTED-FOR-ALL, RESTRICTED-BY-REALM,
                 NOTRESTRICTED-BY-REALM
                 Used by the AuthorizationMaintenanceService
        @param pcSecurityRealmGuid The security realm
        @param pcSecurityItemGuid The security item
        @param pcUserGuid The user guid
        @param pcLoginCompanyKey And alternative Login Company Key, when requesting for a different company 
        @param pcGroupGuids The comma delimited list of groups (and parent groups) the user belongs to
        @return The level on which a security item is restricted for a given user
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetUserRestrictedDefaultLevel (pcSecurityRealmGuid AS CHARACTER,
                                                           pcSecurityItemGuid AS CHARACTER,
                                                           pcUserGuid AS CHARACTER,
                                                           pcGroupGuids AS CHARACTER,
                                                           pcLoginCompanyKey AS CHARACTER):
                                                                                                                                                                                                               
        DEFINE VARIABLE cCompanyKey AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .                              
                                                               
        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmGuid = pcSecurityRealmGuid NO-ERROR . 

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .
        
        IF eSmartSecurityRealm.CompanyDependent = TRUE THEN DO:
            IF pcLoginCompanyKey > "":U THEN 
                ASSIGN cCompanyKey = pcLoginCompanyKey . 
            ELSE 
                ASSIGN cCompanyKey = SessionManager:LoginCompanyKey .
        END.                                                             
                                                             
        /* Restricted for user's groups */
        DO i = 1 TO NUM-ENTRIES (pcGroupGuids):
            FIND SmartSecurityAssignment WHERE SmartSecurityAssignment.SecurityRealmGuid = eSmartSecurityRealm.SecurityRealmGuid 
                                           AND SmartSecurityAssignment.SecurityItemGuid  = pcSecurityItemGuid 
                                           AND SmartSecurityAssignment.LoginCompanyGuid  = cCompanyKey
                                           AND SmartSecurityAssignment.GroupGuid         = ENTRY (i, pcGroupGuids)
                                           AND SmartSecurityAssignment.UserGuid          = "":U NO-LOCK NO-ERROR . 

            IF AVAILABLE SmartSecurityAssignment THEN DO:
                /* Ensure uncached data */
                FIND CURRENT SmartSecurityAssignment NO-LOCK NO-ERROR . 

                IF SmartSecurityAssignment.Restricted THEN 
                    RETURN RestrictedDefaultLevel:RESTRICTED-BY-GROUP . 
                ELSE 
                    RETURN RestrictedDefaultLevel:NOTRESTRICTED-BY-GROUP . 
            END. 
        END.        
                                                
        /* Restricted for everybody? */                                        
        FIND SmartSecurityAssignment WHERE SmartSecurityAssignment.SecurityRealmGuid = eSmartSecurityRealm.SecurityRealmGuid 
                                       AND SmartSecurityAssignment.SecurityItemGuid  = pcSecurityItemGuid
                                       AND SmartSecurityAssignment.LoginCompanyGuid  = cCompanyKey
                                       AND SmartSecurityAssignment.GroupGuid         = "":U
                                       AND SmartSecurityAssignment.UserGuid          = "":U NO-LOCK NO-ERROR . 

        IF AVAILABLE SmartSecurityAssignment THEN DO:
            /* Ensure uncached data */
            FIND CURRENT SmartSecurityAssignment NO-LOCK NO-ERROR . 

            IF SmartSecurityAssignment.Restricted THEN 
                RETURN RestrictedDefaultLevel:RESTRICTED-FOR-ALL . 
            ELSE 
                RETURN RestrictedDefaultLevel:NOTRESTRICTED-FOR-ALL . 
        END.                                        
                                        
        IF eSmartSecurityRealm.DefaultRestricted THEN 
            RETURN RestrictedDefaultLevel:RESTRICTED-BY-REALM . 
        ELSE 
            RETURN RestrictedDefaultLevel:NOTRESTRICTED-BY-REALM . 
                                        
        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR . /* reset error-status from FIND NO-ERROR */       
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a security authorization check for the specified function
        Notes:   Client callable wrapper to the IsRestricted method of the ISecurityService
                 interface 
                 Used by the SecurityServiceClient
        @param poParameter The IsRestrictedParameter instance with the data for this call 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID IsRestricted (poParameter AS IsRestrictedParameter):
        
        ObjectAssert:IsValid (poParameter, "poParameter":U) .
        
        ASSIGN poParameter:IsRestricted = THIS-OBJECT:IsRestricted (poParameter:RealmCode,
                                                                    poParameter:SecurityItemGuid) .
        
    END METHOD . 
    
    /*------------------------------------------------------------------------------
        Purpose: Performs a security authorization check for the specified function
        Notes:   Directly accesses the SmartSecurityAssignment database table for 
                 Performance reasons and because a caching other than what the DB 
                 does anyway is not useful for the security settings 
        @param pcRealmCode The code of the security realm to verify
        @param pcSecurityItemGuid The guid of the security item to verify
        @return Logical value indicating if the specified function is restricted for the current user 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsRestricted (pcRealmCode AS CHARACTER,
                                        pcSecurityItemGuid AS CHARACTER):
		
		DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO .
		DEFINE VARIABLE lRestricted  AS LOGICAL   NO-UNDO .
		DEFINE VARIABLE cReason      AS CHARACTER NO-UNDO .
		
		IF cUserGuid = ? THEN 
		    THIS-OBJECT:DetermineUserGuid () .

        /* Mike Fechner, Consultingwerk Ltd. 30.07.2013
           When there is no known user, restricted is always true  */
        IF cUserGuid > "":U THEN . 
        ELSE RETURN TRUE . 

        THIS-OBJECT:IsRestrictedCheck (pcRealmCode,
                                       pcSecurityItemGuid,
                                       cUserGuid,
                                       SessionManager:LoginCompanyKey,
                                       SessionManager:UserGroupKeys, 
                                       OUTPUT lRestricted,
                                       FALSE, 
                                       OUTPUT cReason) .

        RETURN lRestricted . 

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Performs a security authorization check for the specified function
        Notes:   Directly accesses the SmartSecurityAssignment database table for 
                 Performance reasons and because a caching other than what the DB 
                 does anyway is not useful for the security settings 
        @param pcRealmCode The code of the security realm to verify
        @param pcSecurityItemGuid The guid of the security item to verify
        @param pcUserGuid The Guid of the user to check
        @param pcCurrentLoginCompanyGuid The Guid of the login company to check
        @param pcUserGroupGuids The comma delimited list of user group guids the user belongs to
        @param plRestricted OUTPUT Returns if the user is not allowed to perform the action in question
        @param pcReason OUTPUT Returns the reason based on which setting the user is receiving the restriction or not
    ------------------------------------------------------------------------------*/
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
    
    /*------------------------------------------------------------------------------
        Purpose: Performs a security authorization check for the specified function
        Notes:   Directly accesses the SmartSecurityAssignment database table for 
                 Performance reasons and because a caching other than what the DB 
                 does anyway is not useful for the security settings 
        @param pcRealmCode The code of the security realm to verify
        @param pcSecurityItemGuid The guid of the security item to verify
        @param pcUserGuid The Guid of the user to check
        @param pcCurrentLoginCompanyGuid The Guid of the login company to check
        @param pcUserGroupGuids The comma delimited list of user group guids the user belongs to
        @param plRestricted OUTPUT Returns if the user is not allowed to perform the action in question
        @param plIncludeReason Logical flag indicating if the pcReason should be populated
        @param pcReason OUTPUT Returns the reason based on which setting the user is receiving the restriction or not
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID IsRestrictedCheck (pcRealmCode AS CHARACTER,
                                             pcSecurityItemGuid AS CHARACTER,
                                             pcUserGuid AS CHARACTER,
                                             pcCurrentLoginCompanyGuid AS CHARACTER,
                                             pcUserGroupGuids AS CHARACTER,
                                             OUTPUT plRestricted AS LOGICAL,
                                             plIncludeReason AS LOGICAL,
                                             OUTPUT pcReason AS CHARACTER):
    
        DEFINE VARIABLE cCompanyKey  AS CHARACTER NO-UNDO .
		DEFINE VARIABLE i            AS INTEGER   NO-UNDO .
		DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO .
		
        FIND eSmartSecurityRealm WHERE eSmartSecurityRealm.SecurityRealmCode = pcRealmCode NO-ERROR . 

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER eSmartSecurityRealm:HANDLE"} .
		
		IF eSmartSecurityRealm.CompanyDependent = TRUE THEN 
		    ASSIGN cCompanyKey = pcCurrentLoginCompanyGuid .
		
        /* Restricted for user */
        FIND SmartSecurityAssignment WHERE SmartSecurityAssignment.SecurityRealmGuid = eSmartSecurityRealm.SecurityRealmGuid 
                                       AND SmartSecurityAssignment.SecurityItemGuid  = pcSecurityItemGuid
                                       AND SmartSecurityAssignment.LoginCompanyGuid  = cCompanyKey
                                       AND SmartSecurityAssignment.GroupGuid         = "":U
                                       AND SmartSecurityAssignment.UserGuid          = pcUserGuid NO-LOCK NO-ERROR . 

        IF AVAILABLE SmartSecurityAssignment THEN DO:
            /* Ensure uncached data */
            FIND CURRENT SmartSecurityAssignment NO-LOCK NO-ERROR . 

            ASSIGN plRestricted = SmartSecurityAssignment.Restricted . 
            
            IF plIncludeReason THEN 
                ASSIGN pcReason = "User":U .
            
            RETURN .   
        END.                                                     
                                                             
        /* Restricted for user's groups */
        ASSIGN iCount = NUM-ENTRIES (pcUserGroupGuids) .
         
        DO i = 1 TO iCount:
            FIND SmartSecurityAssignment WHERE SmartSecurityAssignment.SecurityRealmGuid = eSmartSecurityRealm.SecurityRealmGuid 
                                           AND SmartSecurityAssignment.SecurityItemGuid  = pcSecurityItemGuid 
                                           AND SmartSecurityAssignment.LoginCompanyGuid  = cCompanyKey
                                           AND SmartSecurityAssignment.GroupGuid         = ENTRY (i, pcUserGroupGuids)
                                           AND SmartSecurityAssignment.UserGuid          = "":U NO-LOCK NO-ERROR . 

            IF AVAILABLE SmartSecurityAssignment THEN DO:
                /* Ensure uncached data */
                FIND CURRENT SmartSecurityAssignment NO-LOCK NO-ERROR . 
                
                ASSIGN plRestricted = SmartSecurityAssignment.Restricted  . 

                IF plIncludeReason THEN 
                    ASSIGN pcReason = SUBSTITUTE ("Group:&1":U, SmartSecurityAssignment.GroupGuid) .
                
                RETURN .   
            END. 
        END.        
                                                
        /* Restricted for everybody? */                                        
        FIND SmartSecurityAssignment WHERE SmartSecurityAssignment.SecurityRealmGuid = eSmartSecurityRealm.SecurityRealmGuid 
                                       AND SmartSecurityAssignment.SecurityItemGuid  = pcSecurityItemGuid
                                       AND SmartSecurityAssignment.LoginCompanyGuid  = cCompanyKey
                                       AND SmartSecurityAssignment.GroupGuid         = "":U
                                       AND SmartSecurityAssignment.UserGuid          = "":U NO-LOCK NO-ERROR . 

        IF AVAILABLE SmartSecurityAssignment THEN DO:
            /* Ensure uncached data */
            FIND CURRENT SmartSecurityAssignment NO-LOCK NO-ERROR . 
            
            ASSIGN plRestricted = SmartSecurityAssignment.Restricted . 
            
            IF plIncludeReason THEN 
                ASSIGN pcReason = "Everybody":U .
            
            RETURN .   
        END.

        ASSIGN plRestricted = eSmartSecurityRealm.DefaultRestricted . 
        
        IF plIncludeReason THEN 
            ASSIGN pcReason = "Realm":U .
                                        
        FINALLY:
            ERROR-STATUS:ERROR = FALSE NO-ERROR . /* reset error-status from FIND NO-ERROR */		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokable method that performs an Authorization check for the given 
                 user and method
        Notes:   Optionally returns the Dataset with the user groups the user is member of
        @param dsUserGroups INPUT-OUTPUT Dataset to return the user group information to the client
        @param poParameter The IsRestrictedCheckParameter instance with the parameters for this call
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID IsRestrictedCheck (INPUT-OUTPUT DATASET dsUserGroups,
	                                      poParameter AS IsRestrictedCheckParameter):
		
		DATASET dsUserGroups:EMPTY-DATASET () .

        DEFINE VARIABLE oGetGroupsParameter AS GetUserGroupAssignmentsParameter NO-UNDO . 
        DEFINE VARIABLE oUser               AS UserDatasetModel                 NO-UNDO .
        DEFINE VARIABLE oGroup              AS GroupDatasetModel                NO-UNDO .  
        DEFINE VARIABLE hDataset            AS HANDLE                           NO-UNDO .
        
        DEFINE VARIABLE lRestricted         AS LOGICAL                          NO-UNDO .
        DEFINE VARIABLE cReason             AS CHARACTER                        NO-UNDO .
        DEFINE VARIABLE i                   AS INTEGER                          NO-UNDO .
        DEFINE VARIABLE iCount              AS INTEGER                          NO-UNDO .
        DEFINE VARIABLE dtStart             AS DATETIME-TZ                      NO-UNDO . 
        
        ASSIGN oUser = NEW UserDatasetModel (poParameter:UserGuid) .

        oGetGroupsParameter = NEW GetUserGroupAssignmentsParameter (oUser:SmartUser:UserName,
                                                                    poParameter:LoginCompanyGuid) .

        ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                       "GetUserGroupAssignments":U,
                                       DATASET-HANDLE hDataset,
                                       oGetGroupsParameter) .

        dtStart = NOW .

        THIS-OBJECT:IsRestrictedCheck (poParameter:RealmCode,
                                       poParameter:SecurityItemGuid,
                                       poParameter:UserGuid, 
                                       poParameter:LoginCompanyGuid,
                                       oGetGroupsParameter:GroupGuids,
                                       OUTPUT lRestricted,
                                       TRUE,
                                       OUTPUT cReason) .

        ASSIGN poParameter:Runtime      = INTERVAL (NOW, dtStart, "milliseconds":U)
               poParameter:IsRestricted = lRestricted . 
               poParameter:Reason       = cReason . 

        IF poParameter:ReturnGroups THEN DO:

            ASSIGN iCount = NUM-ENTRIES (oGetGroupsParameter:GroupGuids) . 

            DO i = 1 TO iCount:
                oGroup = NEW GroupDatasetModel (ENTRY (i, oGetGroupsParameter:GroupGuids)) .
                
                CREATE ttUserGroups.
                ASSIGN ttUserGroups.Order = i 
                       ttUserGroups.GroupGuid        = oGroup:SmartGroup:GroupGuid        
                       ttUserGroups.GroupName        = oGroup:SmartGroup:GroupName       
                       ttUserGroups.ParentGroupGuid  = oGroup:SmartGroup:ParentGroupGuid 
                       ttUserGroups.LoginCompanyGuid = oGroup:SmartGroup:LoginCompanyGuid
                       ttUserGroups.ParentGroupName  = oGroup:SmartGroup:ParentGroupName 
                       ttUserGroups.LoginCompanyName = oGroup:SmartGroup:LoginCompanyName .                   
            END.
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .        
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the LoginCompanyKeyChanged event of the SessionManager class  
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID LoginCompanyKeyChangedHandler ():
		
		ASSIGN cUserGuid = ? . 
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the UserNameChanged event of the SessionManager class  
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID UserNameChangedHandler ():

        ASSIGN cUserGuid = ? . 		
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SecurityService class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SecurityService ():

        GarbageCollectorHelper:DeleteObject (oEntity) .

    END DESTRUCTOR .

END CLASS.
