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
    File        : AuthorizationMaintenanceService
    Purpose     : Business Task for retrieving and updating authorization       
                  for multiple users/groups and security items 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 21 09:35:26 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.Exceptions.*                    FROM PROPATH .
USING Consultingwerk.Framework.Session.*             FROM PROPATH .
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH . 
USING Consultingwerk.SmartFramework.Authorization.*  FROM PROPATH .  
USING Consultingwerk.Util.*                          FROM PROPATH . 
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.AuthorizationMaintenanceService 
    IMPLEMENTS IBusinessService: 

    {Consultingwerk/SmartFramework/Authorization/dsAuthorizationMaintenance.i}
    {Consultingwerk/SmartFramework/Authorization/dsSecurityAssignment.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns the dsAuthorizationMaintenance Dataset with the current 
                 authorization assignment for the given realm and list of users/groups  
        Notes:   Currently a maximum of 20 entries in the OwnerGuids list is supported. 
        @param dsAuthorizationMaintenance The dataset with the resulting authorization assignments
        @param poParameter The AuthorizationMaintenanceServiceParameter with the parameters for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetAuthorization (INPUT-OUTPUT DATASET dsAuthorizationMaintenance,
                                         poParameter AS AuthorizationMaintenanceServiceParameter):
        
        DEFINE VARIABLE iEntry                AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE lIsUser               AS LOGICAL                      NO-UNDO .
        DEFINE VARIABLE cEntry                AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cCompanyKey           AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cQueryStringPattern   AS CHARACTER                    NO-UNDO 
            INIT "for each eSmartSecurityAssignment where eSmartSecurityAssignment.SecurityRealmGuid = &1~
                                                      and eSmartSecurityAssignment.LoginCompanyGuid = &2~
                                                      and eSmartSecurityAssignment.GroupGuid = &3~
                                                      and eSmartSecurityAssignment.UserGuid = &4":U .

        DEFINE VARIABLE cGroupGuids              AS CHARACTER                        NO-UNDO.
        DEFINE VARIABLE oGetUserGroupsParameter  AS GetUserGroupAssignmentsParameter NO-UNDO . 
        DEFINE VARIABLE oGetGroupGroupsParameter AS GetParentGroupsParameter         NO-UNDO .

        DEFINE VARIABLE oUser                 AS UserDatasetModel                 NO-UNDO . 
        DEFINE VARIABLE oGroup                AS GroupDatasetModel                NO-UNDO . 
        DEFINE VARIABLE oRealm                AS SecurityRealmDatasetModel        NO-UNDO . 
        DEFINE VARIABLE oRealmLookupParameter AS SecurityRealmLookupParameter     NO-UNDO . 
        DEFINE VARIABLE oRequest              AS FetchDataRequest                 NO-UNDO . 
        DEFINE VARIABLE oSecurityService      AS ISecurityService                 NO-UNDO . 
        
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO .
        DEFINE VARIABLE hBuffer  AS HANDLE NO-UNDO .
        
        ObjectAssert:IsValid (poParameter, "poParameter":U) .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i poParameter:SecurityRealmGuid "'poParameter:SecurityRealmGuid':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i poParameter:OwnerGuids "'poParameter:OwnerGuids':U"} .
        
        oRealm = NEW SecurityRealmDatasetModel (poParameter:SecurityRealmGuid) .
        
        IF NOT oRealm:SmartSecurityRealm:Available THEN 
            UNDO, THROW NEW InvalidValueException (poParameter:SecurityRealmGuid, "poParameter:SecurityRealmGuid":U) .          
        
        IF oRealm:SmartSecurityRealm:CompanyDependent = TRUE THEN DO:
            IF poParameter:AlternativeLoginCompanyKey > "":U THEN 
                ASSIGN cCompanyKey = poParameter:AlternativeLoginCompanyKey .
            ELSE  
                ASSIGN cCompanyKey = Consultingwerk.Framework.Session.SessionManager:LoginCompanyKey .
        END.
        ELSE 
            ASSIGN cCompanyKey = "":U .  
        
        oSecurityService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService
                                                         "NEW Consultingwerk.SmartFramework.Authorization.SecurityService ()"} .
        
        /* Retrieve realm items */
        oRealmLookupParameter = NEW SecurityRealmLookupParameter (poParameter:SecurityRealmGuid) .
        
        ServiceInterface:InvokeTask ("Consultingwerk.SmartFramework.Authorization.SecurityRealmInfoService":U,
                                     "GetSecurityRealmLookupParameter":U,
                                     oRealmLookupParameter) .
        
        /* Populate record list */
        DO ON ERROR UNDO, THROW:
            oRequest = NEW FetchDataRequest (oRealmLookupParameter:LookupDescriptor:LookupEntityTable,
                                             oRealmLookupParameter:LookupDescriptor:LookupDialogQueryString) .
            
            ServiceInterface:FetchData (oRealmLookupParameter:LookupDescriptor:LookupEntityName,
                                        oRequest,
                                        OUTPUT DATASET-HANDLE hDataset) .

            ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (oRealmLookupParameter:LookupDescriptor:LookupEntityTable)
                   hQuery  = QueryHelper:CreatePreparedQuery (hBuffer) .
            
            DO WHILE NOT hQuery:QUERY-OFF-END:
                CREATE ttAuthorizationMaintenance .
                ASSIGN ttAuthorizationMaintenance.SecurityItemGuid = hBuffer:BUFFER-FIELD (oRealmLookupParameter:LookupDescriptor:LookupKeyValueColumn):BUFFER-VALUE 
                       ttAuthorizationMaintenance.Restricted       = ? . 
                
                hQuery:GET-NEXT . 
            END.
            
            FINALLY:
                GarbageCollectorHelper:DeleteObject (hQuery) .
                GarbageCollectorHelper:DeleteObject (hDataset) .
                ASSIGN hDataset = ? .        
            END FINALLY.
        END.        
        
        DO iEntry = 1 TO NUM-ENTRIES (poParameter:OwnerGuids):

            ASSIGN cEntry = ENTRY (iEntry, poParameter:OwnerGuids) .

            ASSIGN oUser = NEW UserDatasetModel (cEntry) .
            
            IF oUser:SmartUser:Available THEN DO ON ERROR UNDO, THROW: 
                ASSIGN lIsUser = TRUE .
                
                IF poParameter:AlternativeLoginCompanyKey > "":U THEN 
                    oGetUserGroupsParameter = NEW GetUserGroupAssignmentsParameter (oUser:SmartUser:UserName,
                                                                                    poParameter:AlternativeLoginCompanyKey) .
                ELSE 
                    oGetUserGroupsParameter = NEW GetUserGroupAssignmentsParameter (oUser:SmartUser:UserName,
                                                                                    SessionManager:LoginCompanyKey) .
        
                ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                               "GetUserGroupAssignments":U,
                                               INPUT-OUTPUT DATASET-HANDLE hDataset,
                                               oGetUserGroupsParameter) .
                                                                     
                ASSIGN cGroupGuids = oGetUserGroupsParameter:GroupGuids .  
                
                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hDataset) .
                    ASSIGN hDataset = ? .        
                END FINALLY.                                                                                           
            END. 
            ELSE DO ON ERROR UNDO, THROW:
                ASSIGN oGroup = NEW GroupDatasetModel (cEntry) .
                
                IF oGroup:SmartGroup:Available THEN 
                    ASSIGN lIsUser = FALSE . 
                ELSE 
                    UNDO, THROW NEW InvalidValueException (cEntry, poParameter:OwnerGuids) .

                oGetGroupGroupsParameter = NEW GetParentGroupsParameter (cEntry) . 
        
                ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U,
                                               "GetParentGroups":U,
                                               DATASET-HANDLE hDataset,
                                               oGetGroupGroupsParameter) .
        
                ASSIGN cGroupGuids = oGetGroupGroupsParameter:ParentGroupGuids .
                    
                FINALLY:
                    GarbageCollectorHelper:DeleteObject (hDataset) .
                    ASSIGN hDataset = ? .        
                END FINALLY.                                                                                                               
            END. 
            
            /* Fetch existing direct assignments for owner/realm */
            IF lIsUser = TRUE THEN 
                oRequest = NEW FetchDataRequest ("eSmartSecurityAssignment":U,
                                                 SUBSTITUTE (cQueryStringPattern,
                                                             QUOTER (poParameter:SecurityRealmGuid),
                                                             QUOTER (cCompanyKey),
                                                             QUOTER ("":U),
                                                             QUOTER (cEntry))) .                                                             
            ELSE 
                oRequest = NEW FetchDataRequest ("eSmartSecurityAssignment":U,
                                                 SUBSTITUTE (cQueryStringPattern,
                                                             QUOTER (poParameter:SecurityRealmGuid),
                                                             QUOTER (cCompanyKey),
                                                             QUOTER (cEntry),
                                                             QUOTER ("":U))) . 
                
            ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.SecurityAssignmentBusinessEntity":U,
                                        oRequest,
                                        OUTPUT DATASET dsSecurityAssignment) .                                            
                
            FOR EACH ttAuthorizationMaintenance ON ERROR UNDO, THROW:
                FIND FIRST eSmartSecurityAssignment WHERE eSmartSecurityAssignment.SecurityItemGuid = ttAuthorizationMaintenance.SecurityItemGuid 
                    NO-ERROR . 
                    
                IF AVAILABLE eSmartSecurityAssignment THEN
                    ASSIGN ttAuthorizationMaintenance.Restricted[iEntry] = eSmartSecurityAssignment.Restricted . 
                ELSE 
                    ASSIGN ttAuthorizationMaintenance.Restricted[iEntry] = ? . 
                    
                IF lIsUser THEN 
                    ASSIGN ttAuthorizationMaintenance.DefaultLevel[iEntry] = oSecurityService:GetUserRestrictedDefaultLevel (poParameter:SecurityRealmGuid,
                                                                                                                             ttAuthorizationMaintenance.SecurityItemGuid,
                                                                                                                             oUser:SmartUser:UserGuid,
                                                                                                                             cGroupGuids,
                                                                                                                             cCompanyKey) .
                ELSE 
                    ASSIGN ttAuthorizationMaintenance.DefaultLevel[iEntry] = oSecurityService:GetGroupRestrictedDefaultLevel (poParameter:SecurityRealmGuid,
                                                                                                                              ttAuthorizationMaintenance.SecurityItemGuid,
                                                                                                                              oGroup:SmartGroup:GroupGuid,
                                                                                                                              cGroupGuids,
                                                                                                                              cCompanyKey) .  
            END.                 
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the security assignment based on the dsAuthorizationMaintenance 
                 Dataset with new authorization assignment for the given realm and list 
                 of users/groups  
        Notes:   Currently a maximum of 20 entries in the OwnerGuids list is supported. 
        @param dsAuthorizationMaintenance The dataset with the resulting authorization assignments
        @param poParameter The AuthorizationMaintenanceServiceParameter with the parameters for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreAuthorization (INPUT-OUTPUT DATASET dsAuthorizationMaintenance,
                                           poParameter AS AuthorizationMaintenanceServiceParameter):
        
        DEFINE VARIABLE iEntry                AS INTEGER                      NO-UNDO .
        DEFINE VARIABLE lIsUser               AS LOGICAL                      NO-UNDO .
        DEFINE VARIABLE cEntry                AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cCompanyKey           AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE cQueryStringPattern   AS CHARACTER                    NO-UNDO 
            INIT "for each eSmartSecurityAssignment where eSmartSecurityAssignment.SecurityRealmGuid = &1~
                                                      and eSmartSecurityAssignment.LoginCompanyGuid = &2~
                                                      and eSmartSecurityAssignment.GroupGuid = &3~
                                                      and eSmartSecurityAssignment.UserGuid = &4":U .

        DEFINE VARIABLE oUser                 AS UserDatasetModel                 NO-UNDO . 
        DEFINE VARIABLE oGroup                AS GroupDatasetModel                NO-UNDO . 
        DEFINE VARIABLE oRealm                AS SecurityRealmDatasetModel        NO-UNDO . 
        DEFINE VARIABLE oRequest              AS FetchDataRequest                 NO-UNDO . 
        DEFINE VARIABLE oSecurityService      AS ISecurityService                 NO-UNDO . 
        
        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO .
        DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO .
        DEFINE VARIABLE hBuffer  AS HANDLE NO-UNDO .
        
        ObjectAssert:IsValid (poParameter, "poParameter":U) .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i poParameter:SecurityRealmGuid "'poParameter:SecurityRealmGuid':U"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i poParameter:OwnerGuids "'poParameter:OwnerGuids':U"} .
        
        oRealm = NEW SecurityRealmDatasetModel (poParameter:SecurityRealmGuid) .
        
        IF NOT oRealm:SmartSecurityRealm:Available THEN 
            UNDO, THROW NEW InvalidValueException (poParameter:SecurityRealmGuid, "poParameter:SecurityRealmGuid":U) .        
        
        IF oRealm:SmartSecurityRealm:CompanyDependent = TRUE THEN DO:
            IF poParameter:AlternativeLoginCompanyKey > "":U THEN 
                ASSIGN cCompanyKey = poParameter:AlternativeLoginCompanyKey .
            ELSE  
                ASSIGN cCompanyKey = Consultingwerk.Framework.Session.SessionManager:LoginCompanyKey .
        END.
        ELSE 
            ASSIGN cCompanyKey = "":U .  
        
        oSecurityService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Authorization.ISecurityService} .
                 
        DO iEntry = 1 TO NUM-ENTRIES (poParameter:OwnerGuids) ON ERROR UNDO, THROW:

            ASSIGN cEntry = ENTRY (iEntry, poParameter:OwnerGuids) .

            ASSIGN oUser = NEW UserDatasetModel (cEntry) .
            
            IF oUser:SmartUser:Available THEN 
                ASSIGN lIsUser = TRUE . 
            ELSE DO:
                ASSIGN oGroup = NEW GroupDatasetModel (cEntry) .
                
                IF oGroup:SmartGroup:Available THEN 
                    ASSIGN lIsUser = FALSE . 
                ELSE 
                    UNDO, THROW NEW InvalidValueException (cEntry, poParameter:OwnerGuids) .
            END. 
            
            /* Fetch existing direct assignments for owner/realm */
            IF lIsUser = TRUE THEN 
                oRequest = NEW FetchDataRequest ("eSmartSecurityAssignment":U,
                                                 SUBSTITUTE (cQueryStringPattern,
                                                             QUOTER (poParameter:SecurityRealmGuid),
                                                             QUOTER (cCompanyKey),
                                                             QUOTER ("":U),
                                                             QUOTER (cEntry))) .                                                             
            ELSE 
                oRequest = NEW FetchDataRequest ("eSmartSecurityAssignment":U,
                                                 SUBSTITUTE (cQueryStringPattern,
                                                             QUOTER (poParameter:SecurityRealmGuid),
                                                             QUOTER (cCompanyKey),
                                                             QUOTER (cEntry),
                                                             QUOTER ("":U))) . 
                
            ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.SecurityAssignmentBusinessEntity":U,
                                        oRequest,
                                        OUTPUT DATASET dsSecurityAssignment) .                                            

            DatasetHelper:SetTrackingChanges (DATASET dsSecurityAssignment:HANDLE, TRUE) .                 
                
            loop: 
            FOR EACH ttAuthorizationMaintenance ON ERROR UNDO, THROW:
                FIND FIRST eSmartSecurityAssignment WHERE eSmartSecurityAssignment.SecurityItemGuid = ttAuthorizationMaintenance.SecurityItemGuid 
                    NO-ERROR . 
                    
                IF ttAuthorizationMaintenance.Restricted[iEntry] = ? AND AVAILABLE eSmartSecurityAssignment THEN DO: 
                    DELETE eSmartSecurityAssignment .                      
                    NEXT loop . 
                END.
                   
                IF ttAuthorizationMaintenance.Restricted[iEntry] <> ? THEN DO:  
                    IF NOT AVAILABLE eSmartSecurityAssignment THEN DO:
                        CREATE eSmartSecurityAssignment. 
                        ASSIGN eSmartSecurityAssignment.SecurityItemGuid       = ttAuthorizationMaintenance.SecurityItemGuid
                               eSmartSecurityAssignment.SecurityRealmGuid      = poParameter:SecurityRealmGuid
                               eSmartSecurityAssignment.LoginCompanyGuid       = cCompanyKey 
                               eSmartSecurityAssignment.SecurityAssignmentGuid = ?
                               .
                               
                        IF lIsUser THEN 
                            ASSIGN eSmartSecurityAssignment.UserGuid = cEntry . 
                        ELSE
                            ASSIGN eSmartSecurityAssignment.GroupGuid = cEntry .                             
                    END.                    
                        
                    ASSIGN eSmartSecurityAssignment.Restricted = ttAuthorizationMaintenance.Restricted[iEntry] . 
                END.
            END.                 
            
            ASSIGN hDataset = Consultingwerk.Util.DatasetHelper:GetChangesDataset (DATASET dsSecurityAssignment:HANDLE) .
            
            ServiceInterface:SaveChanges ("Consultingwerk.SmartFramework.Authorization.SecurityAssignmentBusinessEntity":U,
                                          INPUT-OUTPUT DATASET-HANDLE hDataset) .
                                          
            IF hDataset:ERROR THEN 
                UNDO, THROW NEW AppError (DatasetHelper:DatasetErrorStrings (hDataset), 0) .                                          
                                          
            FINALLY:
                GarbageCollectorHelper:DeleteObject (hDataset) .    
                ErrorHelper:ResetErrorStatus() .    
            END FINALLY.                                          
        END.        
    END METHOD . 
    
END CLASS.
    