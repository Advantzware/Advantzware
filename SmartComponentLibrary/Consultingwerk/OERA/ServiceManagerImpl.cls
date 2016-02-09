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
    File        : ServiceManagerImpl
    Purpose     : Default IServiceManager implementation that manages 
                  Business Entity or Business Task instances (IBusinessService 
                  interface) for the Service Interface
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 17 22:21:46 CEST 2013
    Notes       : Consultingwerk.OERA.ServiceManager provides a facade to
                  the current IServiceManager instance
                  It is possible to use multiple instances of the same 
                  Business Service class at once. In this case the business
                  service name should be concatenated from the class name
                  followed by CHR(1) and the instance identified  
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.OERA.*            FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Consultingwerk.Util.* 		   FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceManagerImpl 
    IMPLEMENTS IServiceManager: 

    DEFINE STATIC TEMP-TABLE ttServices NO-UNDO 
        FIELD ServiceName AS CHARACTER 
        FIELD Instance   AS Progress.Lang.Object 
        FIELD Managed    AS LOGICAL INIT TRUE 
        INDEX ServiceName IS UNIQUE ServiceName . 

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
    METHOD PUBLIC VOID DeregisterBusinessService (pcServiceName AS CHARACTER):
        
        DEFINE BUFFER ttServices FOR ttServices . 
        
        FIND ttServices WHERE ttServices.ServiceName = pcServiceName 
                          AND ttServices.Managed     = FALSE NO-ERROR . 
        
        ERROR-STATUS:ERROR = FALSE NO-ERROR . 
        
        IF NOT AVAILABLE ttServices THEN                           
            UNDO, THROW NEW ExternalBusinessServiceNotRegistered (pcServiceName) .
        
        DELETE ttServices .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to a Business Service Instance                                                                        
        Notes:   Starts the Business Service if required (using LaunchBusinessService)
                 It is possible to use multiple instances of the same Business Service 
                 class at once. In this case the business service name should be 
                 concatenated from the class name followed by CHR(1) and the instance 
                 identified
        @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService   
        @return The reference to the IBusinessService                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IBusinessService GetBusinessService (pcServiceName AS CHARACTER):

        DEFINE BUFFER ttServices FOR ttServices . 
        
        FIND ttServices WHERE ttServices.ServiceName = pcServiceName NO-LOCK NO-ERROR . 
        
        ERROR-STATUS:ERROR = FALSE NO-ERROR . 

        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN 
            RETURN CAST (ttServices.Instance, IBusinessService) .
        
        RETURN LaunchBusinessService (pcServiceName) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given Business Service is already running
        Notes:   Default version of the method only checks for externally launched 
                 (=registered) Business Services
        @param pcServiceName The name under which the service is registered (typically the class name)
        @return Logical value indicating if the Business Service is running
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsRunning (pcServiceName AS CHARACTER):
        
        DEFINE BUFFER ttServices FOR ttServices . 
        
        FIND ttServices WHERE ttServices.ServiceName = pcServiceName 
                          AND ttServices.Managed     = FALSE NO-ERROR . 
        
        ERROR-STATUS:ERROR = FALSE NO-ERROR . 
        
        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN 
            RETURN TRUE . 
            
        RETURN FALSE .                            
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given Business Service is already running
        Notes:   
        @param pcServiceName The name under which the service is registered (typically the class name)
        @param poServiceType The service type to check for, defaults to Any when ? is passed in
        @return Logical value indicating if the Business Service is running
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsRunning (pcServiceName AS CHARACTER, 
                                     poServiceType AS BusinessServiceTypeEnum):
                                         
        DEFINE BUFFER ttServices FOR ttServices . 
        
        IF NOT VALID-OBJECT (poServiceType) THEN 
            poServiceType = BusinessServiceTypeEnum:Any .                  
        
        CASE poServiceType:
            WHEN BusinessServiceTypeEnum:ExternallyRegistered THEN 
                FIND ttServices WHERE ttServices.ServiceName = pcServiceName 
                                  AND ttServices.Managed     = FALSE NO-ERROR . 
            WHEN BusinessServiceTypeEnum:Managed THEN 
                FIND ttServices WHERE ttServices.ServiceName = pcServiceName 
                                  AND ttServices.Managed     = TRUE NO-ERROR . 
            OTHERWISE 
                FIND ttServices WHERE ttServices.ServiceName = pcServiceName NO-ERROR . 
        END CASE . 
        
        ERROR-STATUS:ERROR = FALSE NO-ERROR . 
        
        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN 
            RETURN TRUE . 
            
        RETURN FALSE .                            
            
    END METHOD .                                       
       
    /*------------------------------------------------------------------------------
        Purpose: Launches a Business Services                                                                        
        Notes:   When a # or | is used in the service name parameter, use the second entry  
                 as the character parameter to the Constructor. The first entry needs 
                 to contain the service class name. The Business Service is is referenced
                 in a temp-table for latter access to the running instance. 
        @param pcServiceName The Name of the Business Service, evtl. containing a # or |, see Notes                                                                      
        @return The reference to the IBusinessService                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED IBusinessService LaunchBusinessService (pcServiceName AS CHARACTER):
        
        DEFINE VARIABLE oBusinessService  AS IBusinessService NO-UNDO .
        DEFINE VARIABLE cServiceClassName AS CHARACTER        NO-UNDO.

        DEFINE BUFFER ttServices FOR ttServices . 

        FIND ttServices WHERE ttServices.ServiceName = pcServiceName EXCLUSIVE-LOCK NO-ERROR . 
        
        ERROR-STATUS:ERROR = FALSE NO-ERROR .
        
        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN 
            UNDO, THROW NEW BusinessServiceAlreadyRunning (pcServiceName) . 
        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           When there is an Context ID / instance ID, truncate this from the class name */
        ASSIGN cServiceClassName = ENTRY (1, pcServiceName, CHR (1)) .
        
        /* Mike Fechner, Consultingwerk Ltd. 05.08.2011
           When a # is used in the service name, use entry (2,....) 
           as the parameters to the Constructor */
        IF NUM-ENTRIES (cServiceClassName, "#":U) > 1 THEN 
            oBusinessService = DYNAMIC-NEW (ENTRY(1, cServiceClassName, "#":U)) 
                                           (ENTRY(2, cServiceClassName, "#":U)) .
        /* Mike Fechner, Consultingwerk Ltd. 22.12.2015
           SCL-1161: Alternative support for "|" as class name/parameter delimiter */
        ELSE IF NUM-ENTRIES (cServiceClassName, "|":U) > 1 THEN 
            oBusinessService = DYNAMIC-NEW (ENTRY(1, cServiceClassName, "|":U)) 
                                           (ENTRY(2, cServiceClassName, "|":U)) .
        ELSE 
            oBusinessService = DYNAMIC-NEW (cServiceClassName) () .
                
        /* Mike Fechner, Consultingwerk Ltd. 08.12.2015
           SCL-1137: FIND service again, as the service might have been registerd itself as well 
           (from it's own constructor as an unmanaged business service), e.g.
           Consultingwerk.SmartFramework.Authorization.SecurityService */
        FIND ttServices WHERE ttServices.ServiceName = pcServiceName EXCLUSIVE-LOCK NO-ERROR . 

        ERROR-STATUS:ERROR = FALSE NO-ERROR .
                
        IF NOT AVAILABLE ttServices THEN DO:
            CREATE ttServices .
            ASSIGN ttServices.ServiceName = pcServiceName 
                   ttServices.Managed     = TRUE .
        END. 
                     
        ASSIGN ttServices.Instance = oBusinessService .

        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           Let BusinessService Instance know the ContextID */
        IF NUM-ENTRIES (pcServiceName, CHR(1)) > 1 AND TYPE-OF (ttServices.Instance, Consultingwerk.OERA.IHasContextID) THEN 
            CAST (ttServices.Instance, 
                  IHasContextID):ContextID = ENTRY (2, pcServiceName, CHR(1)) . 
        
        RETURN oBusinessService .

    END METHOD.

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
    METHOD PUBLIC VOID RegisterBusinessService (pcServiceName AS CHARACTER,
                                                       poInstance AS IBusinessService):
        
        DEFINE BUFFER ttServices FOR ttServices . 
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poInstance """Instance to be registered""{&TRAN}"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcServiceName "'Service Name to be registered'{&TRAN}"} .
        
        IF CAN-FIND (ttServices WHERE ttServices.ServiceName = pcServiceName) THEN 
            UNDO, THROW NEW BusinessServiceAlreadyRegistered (pcServiceName) .
        
        CREATE ttServices.
        ASSIGN ttServices.ServiceName = pcServiceName
               ttServices.Managed     = FALSE 
               ttServices.Instance    = poInstance . 
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Stops all Business Services Instances                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StopAllBusinessServices ():
        
        DEFINE BUFFER ttServices FOR ttServices . 
        
        /* Only shutdown services that were started by the ServiceManager itself */
        FOR EACH ttServices WHERE ttServices.Managed = TRUE:
            IF VALID-OBJECT (ttServices.Instance) THEN         
                DELETE OBJECT ttServices.Instance .
    
            DELETE ttServices .
        END. 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stops all Business Services Instances of the given Class                                                                        
        Notes:   
        @param poClass The Progress.Lang.Class reference (Class or Interface)                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StopAllBusinessServices (poClass AS Progress.Lang.Class):
        
        DEFINE BUFFER ttServices FOR ttServices . 
        
        FOR EACH ttServices WHERE ttServices.Managed = TRUE:
            IF VALID-OBJECT (ttServices.Instance) THEN DO:
                IF ttServices.Instance:GetClass():IsA (poClass) THEN DO: 
                    DELETE OBJECT ttServices.Instance .
                    
                    DELETE ttServices .
                END.    
            END.
            ELSE 
                DELETE ttServices .
        END. 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stops an Business Service Instance                                                                        
        Notes:   Defaults to StopBusinessService (pcServiceName, NotRunningServiceEnum:Throw) 
        Throws:  Consultingwerk.OERA.Exceptions.BusinessServiceNotRunning    
        @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StopBusinessService (pcServiceName AS CHARACTER):
        
        THIS-OBJECT:StopBusinessService (pcServiceName, NotRunningServiceEnum:Throw) .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stops an Business Service Instance                                                                        
        Notes:   
        Throws:  Consultingwerk.OERA.Exceptions.BusinessServiceNotRunning    
        @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
        @param poNotRunningServiceEnum The NotRunningServiceEnum value indicating how to handle a Business Service that is not running                                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StopBusinessService (pcServiceName AS CHARACTER,
                                            poNotRunningServiceEnum AS NotRunningServiceEnum):
        
        DEFINE BUFFER ttServices FOR ttServices . 
        
        FIND ttServices WHERE ttServices.ServiceName = pcServiceName 
                          AND ttServices.Managed = TRUE NO-LOCK NO-ERROR . 

        ERROR-STATUS:ERROR = FALSE NO-ERROR .        
        
        IF NOT AVAILABLE ttServices THEN DO:
            
            IF poNotRunningServiceEnum = NotRunningServiceEnum:Throw THEN   
                UNDO, THROW NEW BusinessServiceNotRunning (pcServiceName) .
            ELSE 
                RETURN .
        END.
        
        IF VALID-OBJECT (ttServices.Instance) THEN         
            DELETE OBJECT ttServices.Instance .

        DELETE ttServices .

    END METHOD.

END CLASS.
