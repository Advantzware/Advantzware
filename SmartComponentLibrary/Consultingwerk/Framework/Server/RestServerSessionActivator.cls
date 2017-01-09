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
    Purpose     : 
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

    DEFINE VARIABLE cAuthenticationDB AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogEntryTypes    AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lDebugMode        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCustomLogEntries AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Raised when the ActivateServerSession was not able to reactivate
                 a users client-principal because the length of the CP was 0 (no 
                 CP available)
        Notes:   This event may be used to authenticate a technical user that is 
                 authorized to run requests on behalf of unauthenticated sessions
        @param sender The object that raised the PerformTechnicalLogin event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT PerformTechnicalLogin SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                              e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ServerSessionActivator class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RestServerSessionActivator ():

		DEFINE VARIABLE oConfiguration AS IConfigurationProvider NO-UNDO .
		
		SUPER ().

        ServiceInterface:Activated:Subscribe (ActivateServerSession) .        
        ServiceInterface:Deactivated:Subscribe (DeactivateServerSession) .        
		
		oConfiguration = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .
		
		IF VALID-OBJECT (oConfiguration) THEN 
		    cAuthenticationDB = oConfiguration:GetValue ("authenticationDb":U, LDBNAME(1)) . 
		ELSE 
		    cAuthenticationDB = LDBNAME(1) . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates a server session
        Notes:   
        @param e The Consultingwerk.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ActivateServerSession (e AS Consultingwerk.EventArgs):

        DEFINE VARIABLE hPrincipal          AS HANDLE                           NO-UNDO.
        DEFINE VARIABLE cName               AS CHARACTER                        NO-UNDO.
        DEFINE VARIABLE cDomain             AS CHARACTER                        NO-UNDO.
        
        DEFINE VARIABLE oLoginCompany       AS LoginCompanyDatasetModel         NO-UNDO . 
        DEFINE VARIABLE oUser               AS UserDatasetModel                 NO-UNDO . 
        DEFINE VARIABLE oGetGroupsParameter AS GetUserGroupAssignmentsParameter NO-UNDO . 
        DEFINE VARIABLE hDataset            AS HANDLE                           NO-UNDO .

/*        LogManager:WriteSeparator() .                                               */
/*        LogManager:WriteObjectProperties (SESSION:CURRENT-REQUEST-INFO, "REQUEST") .*/
 
        ASSIGN hPrincipal = SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal().

/*        LogManager:WriteSeparator() .                                      */
/*        LogManager:WriteWidgetProperties (hPrincipal, "CLIENT-PRINCIPAL") .*/
/*        LogManager:WriteSeparator() .                                      */

        IF VALID-HANDLE (hPrincipal) THEN DO:
            hPrincipal:VALIDATE-SEAL ({Consultingwerk/get-service.i IConfigurationProvider}:GetValue ("clientPrincipalSeal":U)) .
            
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
                   SessionManager:LanguageDisplayName = oUser:SmartUser:LanguageName                                           
                   SessionManager:LanguageIsoCode     = oUser:SmartUser:LanguageIsoCode
                   SessionManager:LanguageKey         = oUser:SmartUser:LanguageGuid .       
                
            oGetGroupsParameter = NEW GetUserGroupAssignmentsParameter (SessionManager:UserName,
                                                                        SessionManager:LoginCompanyKey) .
    
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
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
		
		SessionManager:Reset() .
		
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the PerformTechnicalLogin event
        Notes:   
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnPerformTechnicalLogin (e AS Consultingwerk.EventArgs):
            
        IF NOT VALID-OBJECT (e) THEN 
            e = Consultingwerk.EventArgs:Empty . 
    
        THIS-OBJECT:PerformTechnicalLogin:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    &ENDIF

END CLASS.
