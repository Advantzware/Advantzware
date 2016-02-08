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
    File        : UserBusinessEntity
    Purpose     : Business Entity for User
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 12.10.2012 09:37:46
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Framework.*                     FROM PROPATH . 
USING Consultingwerk.Framework.Collections.*         FROM PROPATH . 
USING Consultingwerk.OERA.*                          FROM PROPATH .
USING Consultingwerk.SmartFramework.*                FROM PROPATH . 
USING Consultingwerk.SmartFramework.Authentication.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Exceptions.*     FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .       
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authentication.UserBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/System/dsLoginCompany.i }
    { Consultingwerk/SmartFramework/Authorization/dsUserGroup.i }
    { Consultingwerk/SmartFramework/Authorization/dsGroup.i }
    { Consultingwerk/SmartFramework/Authentication/dsUser.i }

    DEFINE VARIABLE cAuthenticationDB AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UserBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserBusinessEntity ():

        DEFINE VARIABLE oConfiguration AS IConfigurationProvider NO-UNDO .

        SUPER (DATASET dsUser:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.Authentication.UserDataAccess":U .

        oConfiguration = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .
        
        IF VALID-OBJECT (oConfiguration) THEN 
            cAuthenticationDB = oConfiguration:GetValue ("authenticationDb":U, LDBNAME(1)) . 
        ELSE 
            cAuthenticationDB = LDBNAME (1) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds the user groups to the Groups Dictionary and populates a list 
                 with the parent groups 
        Notes:   
        @param poGroupsDictionary The Dictionary of groups to add the groups to
        @param poParentGroupsList The List of Parent groups to add the direct parent groups to
        @param pcUserGuid The guid of the user to query groups for 
        @param pcLoginCompanyGuid The guid of the current login company
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID AddGroupsToDictionary (poGroupsDictionary AS CharacterDictionary,
	                                             poParentGroupsList AS CharacterList,
	                                             pcUserGuid AS CHARACTER, 
	                                             pcLoginCompanyGuid AS CHARACTER):
		
        DEFINE VARIABLE oRequest AS IFetchDataRequest NO-UNDO . 
		
        oRequest = NEW FetchDataRequest ("eSmartUserGroup":U,
                                         SUBSTITUTE ("FOR EACH eSmartUserGroup WHERE eSmartUserGroup.UserGuid = &1 AND eSmartUserGroup.GroupLoginCompanyGuid = &2":U,
                                                     QUOTER (pcUserGuid),
                                                     QUOTER (pcLoginCompanyGuid))) . 

        Consultingwerk.OERA.ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.UserGroupBusinessEntity":U,
                                                        oRequest,
                                                        OUTPUT DATASET dsUserGroup) .

        FOR EACH eSmartUserGroup:
            poGroupsDictionary:Add (eSmartUserGroup.GroupGuid,
                                   eSmartUserGroup.GroupName) .               

            IF eSmartUserGroup.ParentGroupGuid > "":U THEN 
                poParentGroupsList:Add (eSmartUserGroup.ParentGroupGuid) .
        END.
		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a Password change for a user in the _User table 
        Notes:   
        @param dsUser INPUT-OUTPUT DATASET, not used
        @param poParameter The AuthenticationParameter with the parameter for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ChangeDatabasePassword (INPUT-OUTPUT DATASET dsUser,
                                               poParameter AS ChangePasswordParameter):
        DEFINE VARIABLE hQuery         AS HANDLE            NO-UNDO .
        DEFINE VARIABLE hBuffer        AS HANDLE            NO-UNDO .
        DEFINE VARIABLE oRequest       AS IFetchDataRequest NO-UNDO . 
        DEFINE VARIABLE cLoginUserName AS CHARACTER         NO-UNDO .
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poParameter, "ChangePasswordParameter":U) .

        CREATE QUERY hQuery.
        CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE ("&1._User":U,
                                                    cAuthenticationDB).

        hQuery:ADD-BUFFER (hBuffer).

        oRequest = NEW FetchDataRequest ("eSmartLoginCompany":U,
                                         SUBSTITUTE ("FOR EACH eSmartLoginCompany WHERE eSmartLoginCompany.LoginCompanyGuid = &1":U,
                                                     QUOTER (poParameter:CompanyKey))) .
    
        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity":U,
                                    oRequest,
                                    OUTPUT DATASET dsLoginCompany) .

        FIND FIRST eSmartLoginCompany NO-ERROR . 
        
        IF NOT AVAILABLE eSmartLoginCompany THEN 
            UNDO, THROW NEW SmartException ("SFR":U, 304). 
            
        IF CharacterType:IsNotNullOrEmpty (eSmartLoginCompany.LoginCompanyTenantDomain) THEN
            cLoginUserName = SUBSTITUTE ("&1@&2":U,
                                         poParameter:UserName,
                                         eSmartLoginCompany.LoginCompanyTenantDomain).
        ELSE
            cLoginUserName = poParameter:UserName.

        hQuery:QUERY-PREPARE (SUBSTITUTE ("FOR EACH _User WHERE _Userid = &1 and _Password = &2":U,
                                          QUOTER (cLoginUserName),
                                          QUOTER (ENCODE (poParameter:Password)))).

        hQuery:QUERY-OPEN ().
        hQuery:GET-FIRST (NO-LOCK).

        IF NOT hBuffer:AVAILABLE THEN
            UNDO, THROW NEW SmartException ("SFR":U, 303).

        DO TRANSACTION ON ERROR UNDO, THROW:
            hBuffer:FIND-CURRENT (EXCLUSIVE-LOCK).

            hBuffer::_Password = ENCODE (poParameter:NewPassword).
            hBuffer:BUFFER-RELEASE ().
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs a Password change for a user in the SmartUser table 
        Notes:   
        @param dsUser INPUT-OUTPUT DATASET, not used
        @param poParameter The AuthenticationParameter with the parameter for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ChangeSmartUserPassword (INPUT-OUTPUT DATASET dsUser,
                                                poParameter AS ChangePasswordParameter):
        
        DEFINE VARIABLE oUser                    AS UserDatasetModel        NO-UNDO . 

        Consultingwerk.Assertion.ObjectAssert:IsValid (poParameter, "ChangePasswordParameter":U) .

        ASSIGN oUser = NEW UserDatasetModel () .
        oUser:SmartUser:Filter:UserName:EQ (poParameter:UserName).
        oUser:SmartUser:Filter:LoginCompanyGuid:EQ (poParameter:CompanyKey).
        oUser:SmartUser:Filter:UserPassword:EQ (ENCODE (poParameter:Password)):Run ().
        
        IF NOT oUser:SmartUser:Available THEN 
            UNDO, THROW NEW SmartException ("SFR":U, 303).
        
        oUser:TrackingChanges = TRUE.
        oUser:SmartUser:UserPassword = poParameter:NewPassword.
        oUser:SmartUser:UserPasswordChangedDate = TODAY.
        oUser:SaveChanges ().

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oUser).
        END FINALLY.        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs Database Authentication 
        Notes:   Uses the Domain name for DB authentication when the SmartLoginCompany
                 has a Tenant Domain defined and the Configuration Manager property 
                 "authenticateWithDomainName" is empty or not false 
        @param dsUser INPUT-OUTPUT DATASET, not used
        @param poParameter The AuthenticationParameter with the parameter for this method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformDatabaseAuthentication (INPUT-OUTPUT DATASET dsUser,
                                                      poParameter AS AuthenticationParameter):
        
        DEFINE VARIABLE oRequest   AS IFetchDataRequest      NO-UNDO . 
        DEFINE VARIABLE cUserName  AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE lOk        AS LOGICAL                NO-UNDO .
        DEFINE VARIABLE hPrincipal AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE oConfig    AS IConfigurationProvider NO-UNDO .
        DEFINE VARIABLE lDomain    AS LOGICAL                NO-UNDO .

        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poParameter, "AuthenticationParameter":U) .
        
        oRequest = NEW FetchDataRequest ("eSmartLoginCompany":U,
                                         SUBSTITUTE ("FOR EACH eSmartLoginCompany WHERE eSmartLoginCompany.LoginCompanyGuid = &1":U,
                                                     QUOTER (poParameter:CompanyKey))) .
    
        ServiceInterface:FetchData ("Consultingwerk.SmartFramework.System.LoginCompanyBusinessEntity":U,
                                    oRequest,
                                    OUTPUT DATASET dsLoginCompany) .

        FIND FIRST eSmartLoginCompany NO-ERROR . 
        
        IF NOT AVAILABLE eSmartLoginCompany THEN 
            UNDO, THROW NEW AppError ("Invalid Login Company."{&TRAN}, 0) . 
            
        IF VALID-OBJECT (oConfig) THEN  
            lDomain = DataTypeHelper:ToLogical(oConfig:GetValue ("authenticateWithDomainName":U, "true":U)) .                     
        ELSE 
            ASSIGN lDomain = TRUE .        
                                         
        IF lDomain AND CharacterType:IsNotNullOrEmpty (eSmartLoginCompany.LoginCompanyTenantDomain) THEN
            lOk = SETUSERID (SUBSTITUTE ("&1@&2":U,
                                         poParameter:UserName,
                                         eSmartLoginCompany.LoginCompanyTenantDomain),
                             poParameter:Password,
                             cAuthenticationDB) .
        ELSE
            lOk = SETUSERID (poParameter:UserName,
                             poParameter:Password, 
                             cAuthenticationDB) .

        IF NOT lOk THEN RETURN . 

        /* Mike Fechner, Consultingwerk Ltd. 21.05.2014
           SCL-300: Verify user has access to all companies or the one he's logging in to */
        oRequest = NEW FetchDataRequest ("eSmartUser":U,
                                         SUBSTITUTE ("FOR EACH eSmartUser WHERE eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2":U,
                                                     QUOTER (poParameter:UserName),
                                                     QUOTER (eSmartLoginCompany.LoginCompanyGuid))) .
        
        THIS-OBJECT:FetchData (oRequest) .
    
        FIND FIRST eSmartUser NO-ERROR . 

        IF NOT AVAILABLE eSmartUser THEN DO:
            oRequest = NEW FetchDataRequest ("eSmartUser":U,
                                             SUBSTITUTE ("FOR EACH eSmartUser WHERE eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2":U,
                                                         QUOTER (poParameter:UserName),
                                                         QUOTER ("":U))) .
            
            THIS-OBJECT:FetchData (oRequest) .
        
            FIND FIRST eSmartUser NO-ERROR . 
        END.

        IF NOT AVAILABLE eSmartUser THEN 
            RETURN . /* Return without Client Principal */

&IF PROVERSION NE "10.2B":U &THEN    
        /* Mike Fechner, Consultingwerk Ltd. 21.05.2014
           Return Client-Principal to caller */
        hPrincipal = GET-DB-CLIENT (cAuthenticationDB).

&ELSE
        /* SCL-678 : OpenEdge 10.2B does not support the GET-DB-CLIENT method
           to retrieve the client-principal based on the _oeuser domain.
           Instead we rely on the "SmartFramework" authentication domain 
           with the "smart" key */         
        CREATE CLIENT-PRINCIPAL hPrincipal.
        
        hPrincipal:SESSION-ID = BASE64-ENCODE(GENERATE-UUID).
        hPrincipal:USER-ID = "demo":U. /* whatever user  that would have proper access to your tables */
        hPrincipal:DOMAIN-NAME = "SmartFramework":U . /* domain name from the database */
        hPrincipal:SEAL ("smart":U).  /* it has to be the same key from the database */
&ENDIF

        poParameter:ClientPrincipal = hPrincipal:EXPORT-PRINCIPAL () .

        /* Mike Fechner, Consultingwerk Ltd. 20.02.2013
           Update Last Login Date in SmartUser */
        DatasetHelper:SetTrackingChanges (DATASET dsUser:HANDLE, TRUE) .
        
        eSmartUser.UserLastLogin = NOW . 

        DatasetHelper:SetTrackingChanges (DATASET dsUser:HANDLE, FALSE) .
    
        THIS-OBJECT:SaveChanges () .

        CATCH err AS Progress.Lang.Error :
        	Consultingwerk.Util.LogManager:WriteError (err) .
        	
        	UNDO, THROW err .	
        END CATCH.

        FINALLY:
            IF VALID-HANDLE (hPrincipal) THEN 
                DELETE OBJECT hPrincipal .		
        END FINALLY.
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets the users groups assignments
        Notes:   Gets the direct groups a user is member of an the parent groups of those
                 groups
        @param dsUser INPUT-OUTPUT The dataset of the user business entity  
        @param poParameter The GetUserGroupAssignmentsParameter with the data for this request 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID GetUserGroupAssignments (INPUT-OUTPUT DATASET dsUser,
	                                            poParameter AS GetUserGroupAssignmentsParameter):
		
        DEFINE VARIABLE oRequest          AS IFetchDataRequest   NO-UNDO . 
        DEFINE VARIABLE oGroupsDictionary AS CharacterDictionary NO-UNDO .
        DEFINE VARIABLE oParentGroupsList AS CharacterList       NO-UNDO .    
        DEFINE VARIABLE cParentGroup      AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE i                 AS INTEGER             NO-UNDO .

        /* Fetch the groups a user is directly assigned to */
        Consultingwerk.Assertion.ObjectAssert:IsValid (poParameter, "GetUserGroupAssignmentsParameter":U) .

        /* Fetch the user */
        oRequest = NEW FetchDataRequest ("eSmartUser":U,
                                         SUBSTITUTE ("FOR EACH eSmartUser WHERE eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2":U,
                                                     QUOTER (poParameter:UserName),
                                                     QUOTER (poParameter:LoginCompanyKey))) . 
    
        THIS-OBJECT:FetchData (oRequest) .

        FIND FIRST eSmartUser NO-ERROR . 

        /* Mike Fechner, Consultingwerk Ltd. 13.05.2015
           SCL-808: When the user could not be found with the login company
                    provided, lookup a user with no specific login company
                    set */
        IF NOT AVAILABLE eSmartUser THEN DO:
            /* Fetch the user */
            oRequest = NEW FetchDataRequest ("eSmartUser":U,
                                             SUBSTITUTE ("FOR EACH eSmartUser WHERE eSmartUser.UserName = &1 AND eSmartUser.LoginCompanyGuid = &2":U,
                                                         QUOTER (poParameter:UserName),
                                                         QUOTER ("":U))) .

            THIS-OBJECT:FetchData (oRequest) .

            FIND FIRST eSmartUser NO-ERROR .
        END.

        IF NOT AVAILABLE eSmartUser THEN DO:
            ASSIGN poParameter:GroupGuids = "":U
                   poParameter:GroupNames = "":U .
            
            RETURN . 
        END.

        ASSIGN poParameter:GroupGuids = "":U
               poParameter:GroupNames = "":U 
               oGroupsDictionary       = NEW CharacterDictionary ()
               oParentGroupsList       = NEW CharacterList () . 

        IF poParameter:LoginCompanyKey > "":U THEN
            THIS-OBJECT:AddGroupsToDictionary (oGroupsDictionary,
                                               oParentGroupsList,
                                               eSmartUser.UserGuid,
                                               poParameter:LoginCompanyKey) .

        THIS-OBJECT:AddGroupsToDictionary (oGroupsDictionary,
                                           oParentGroupsList,
                                           eSmartUser.UserGuid,
                                           "":U) .

        /* And now loop though the parent groups of the groups the user is member of */
        ASSIGN i = 1 . 
        
        IF oParentGroupsList:Count > 0 THEN DO: 
            REPEAT ON ERROR UNDO, THROW:
                ASSIGN cParentGroup = oParentGroupsList:GetValue(i) .               
                
                oRequest = NEW FetchDataRequest ("eSmartGroup":U,
                                                 SUBSTITUTE ("FOR EACH eSmartGroup WHERE eSmartGroup.GroupGuid = &1":U,
                                                             QUOTER (cParentGroup))) . 
                
                Consultingwerk.OERA.ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U,
                                                                oRequest,
                                                                OUTPUT DATASET dsGroup) .
                
                FOR EACH eSmartGroup WHERE eSmartGroup.LoginCompanyGuid = poParameter:LoginCompanyKey
                                        OR eSmartGroup.LoginCompanyGuid = "":U:
                                            
                    IF NOT oGroupsDictionary:ContainsKey (eSmartGroup.GroupGuid) THEN DO:
                        
                        oGroupsDictionary:Add (eSmartGroup.GroupGuid,
                                               eSmartGroup.GroupName) .
                    END.
                    
                    IF eSmartGroup.ParentGroupGuid > "":U AND NOT oParentGroupsList:ContainsValue (eSmartGroup.ParentGroupGuid) THEN 
                        oParentGroupsList:Add (eSmartGroup.ParentGroupGuid) .
                END.
                
                i = i + 1 . 
                
                IF i > oParentGroupsList:Count THEN 
                     LEAVE .    
            END.
        END.

        ASSIGN poParameter:GroupGuids = oGroupsDictionary:Keys
               poParameter:GroupNames = oGroupsDictionary:Values .

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
        
        FOR EACH eSmartUser:
    
            FIND eSmartUserBefore WHERE ROWID (eSmartUserBefore) = BUFFER eSmartUser:BEFORE-ROWID NO-ERROR . 
            
            IF AVAILABLE eSmartUserBefore THEN DO:
                IF eSmartUserBefore.UserPassword <> eSmartUser.UserPassword THEN
                 
                    ASSIGN eSmartUser.UserPassword            = ENCODE (eSmartUser.UserPassword) 
                           eSmartUser.UserPasswordChangedDate = TODAY .  
            END.
            
            FIND eSmartUserBefore WHERE ROWID(eSmartUserBefore) = BUFFER eSmartUser:BEFORE-ROWID NO-ERROR.
                
            IF AVAILABLE eSmartUserBefore THEN DO:  
                
                IF NUM-ENTRIES (eSmartUser.UserName, ",":U) > 1 THEN
                    Consultingwerk.Util.DatasetHelper:AddErrorString 
                        (BUFFER eSmartUser:HANDLE,
                         "It is not allowed to use a comma in the username!"{&TRAN}, 
                         "UserName":U) .
                
            END.  
        END.

    END METHOD.

END CLASS.
