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
    File        : ServiceManager
    Purpose     : Manages running Service (Business Services) instances
                  Typically Business Entities and Business Tasks
    Syntax      : 
    Description : Provides a facade to the current IServiceManager implementation,
                  by default: Consultingwerk.OERA.ServiceManagerImpl 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 30 22:07:02 CET 2010
    Notes       : It is possible to use multiple instances of the same 
                  Business Service class at once. In this case the business
                  service name should be concatenated from the class name
                  followed by CHR(1) and the instance identified  
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.OERA.*            FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.OERA.ServiceManager: 

    /*------------------------------------------------------------------------------
        Purpose: Deregisters an external Business Service from the ServiceManager class
        Notes:   External Business Services are launched externally, may even be 
                 regsitered in the ServiceContainer class. This is useful for allowing
                 access to Session Services (e.g. FrameworkSettings:ServiceContainer) to 
                 be exposed to clients using the Service Interface while staying directly
                 accessible on the back end though the ServiceContainer. External Business 
                 Services are considered not to be managed by the ServiceManager, that 
                 is the ServiceManager will not attempt to shut them down in the Stop... 
                 kind of methods. 
        @param pcServiceName The name of the Service to be deregistered (typically the class name)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DeregisterBusinessService (pcServiceName AS CHARACTER):
        
        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .

        oServiceManager:DeregisterBusinessService (pcServiceName) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to a Business Service Instance                                                                        
        Notes:   Starts the Business Service if required (using LaunchBusinessService)
        @param pcServiceName The Name of the Business Service, evtl. containing a # or |, see Notes at LaunchBusinessService   
        @return The reference to the IBusinessService                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IBusinessService GetBusinessService (pcServiceName AS CHARACTER):
        
        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .

        RETURN oServiceManager:GetBusinessService (pcServiceName) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given Business Service is already running
        Notes:   Default version of the method only checks for externally launched 
                 (=registered) Business Services
        @param pcServiceName The name under which the service is registered (typically the class name)
        @return Logical value indicating if the Business Service is running
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC LOGICAL IsRunning (pcServiceName AS CHARACTER):
		
        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .
        
        RETURN oServiceManager:IsRunning (pcServiceName) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given Business Service is already running
        Notes:   
        @param pcServiceName The name under which the service is registered (typically the class name)
        @param poServiceType The service type to check for 
        @return Logical value indicating if the Business Service is running
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsRunning (pcServiceName AS CHARACTER, 
                                     poServiceType AS BusinessServiceTypeEnum):

        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .
        
        RETURN oServiceManager:IsRunning (pcServiceName, poServiceType) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Registers an external Business Service with the ServiceManager class
        Notes:   External Business Services are launched externally, may even be 
                 regsitered in the ServiceContainer class. This is useful for allowing
                 access to Session Services (e.g. FrameworkSettings:ServiceContainer) to 
                 be exposed to clients using the Service Interface while staying directly
                 accessible on the back end though the ServiceContainer. External Business 
                 Services are considered not to be managed by the ServiceManager, that 
                 is the ServiceManager will not attempt to shut them down in the Stop... 
                 kind of methods. 
        @param pcServiceName The name under which the service will be registered (typically the class name)
        @param poInstance The instance of the business service
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RegisterBusinessService (pcServiceName AS CHARACTER,
                                                       poInstance AS IBusinessService):
        
        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .
        
        oServiceManager:RegisterBusinessService (pcServiceName,
                                                 poInstance) .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Stops all Business Services Instances                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StopAllBusinessServices ():

        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .
        
        oServiceManager:StopAllBusinessServices () .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stops all Business Services Instances of the given Class                                                                        
        Notes:   
        @param poClass The Progress.Lang.Class reference (Class or Interface)                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StopAllBusinessServices (poClass AS Progress.Lang.Class):
        
        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .

        oServiceManager:StopAllBusinessServices (poClass) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stops an Business Service Instance                                                                        
        Notes:   Defaults to StopBusinessService (pcServiceName, NotRunningServiceEnum:Throw) 
        @param pcServiceName The Name of the Business Service, evtl. containing a # or |, see Notes at LaunchBusinessService                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StopBusinessService (pcServiceName AS CHARACTER):
        
        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .

        oServiceManager:StopBusinessService (pcServiceName) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stops an Business Service Instance                                                                        
        Notes:    
        @param pcServiceName The Name of the Business Service, evtl. containing a # or |, see Notes at LaunchBusinessService                                                                       
        @param poNotRunningServiceEnum The NotRunningServiceEnum value indicating how to handle a Business Service that is not running                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StopBusinessService (pcServiceName AS CHARACTER,
                                                   poNotRunningServiceEnum AS NotRunningServiceEnum):
        
        DEFINE VARIABLE oServiceManager AS IServiceManager NO-UNDO .
        
        oServiceManager = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManager 
                                                        "NEW Consultingwerk.OERA.ServiceManagerImpl ()"} .

        oServiceManager:StopBusinessService (pcServiceName, poNotRunningServiceEnum) .
        
    END METHOD.

END CLASS.
