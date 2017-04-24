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
                  Uses the "ServiceManager" custom log entry type
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                 FROM PROPATH .
USING Consultingwerk.Framework.*       FROM PROPATH .
USING Consultingwerk.OERA.*            FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Consultingwerk.OERA.Monitoring.* FROM PROPATH .
USING Consultingwerk.Util.*            FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.ServiceManagerImpl
    IMPLEMENTS IServiceManager, IServiceManagerMonitoring:

    DEFINE STATIC TEMP-TABLE ttServices NO-UNDO
        FIELD ServiceName AS CHARACTER            /* The service name as requested by consumers, potentially including CHR(1) and a unique ID */
        FIELD ClassName   AS CHARACTER            /* The actual class name for mapping to the life cycle configuration */
        FIELD Instance    AS Progress.Lang.Object
        FIELD Managed     AS LOGICAL INITIAL TRUE /* Managed = FALSE means started externally and registered here */
        FIELD Started     AS DATETIME-TZ          /* Time the service was started */
        FIELD LastRequest AS DATETIME-TZ          /* Time of last request */
        INDEX ServiceName IS UNIQUE ServiceName
        INDEX ClassNameStarted ClassName Started
        INDEX ClassNameLastRequest ClassName LastRequest
        INDEX ManagedClassName Managed ClassName  .

    {Consultingwerk/OERA/ttServiceLifeCycleDefinition.i}
    {Consultingwerk/OERA/Monitoring/eBusinessServices.i &NO-BEFORE=YES}

    DEFINE PRIVATE VARIABLE oService        AS IServiceNameMappingService NO-UNDO .
    DEFINE PRIVATE VARIABLE lServiceQueried AS LOGICAL                    NO-UNDO INITIAL FALSE .

    /**
     * Purpose: Constructor for the ServiceManagerImpl class
     * Notes:   Parses the life services cycle configuration file
     */
    CONSTRUCTOR PUBLIC ServiceManagerImpl ():

        DEFINE VARIABLE oLifeCycleProvider AS IServiceManagerLifeCycleProvider NO-UNDO .
        DEFINE VARIABLE iNumRecords        AS INTEGER                          NO-UNDO .

        ASSIGN oLifeCycleProvider = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceManagerLifeCycleProvider}.

        IF VALID-OBJECT (oLifeCycleProvider) THEN DO:
            oLifeCycleProvider:GetDefinitions (OUTPUT TABLE ttServiceLifeCycleDefinition) .

            ASSIGN iNumRecords = BufferHelper:NumRecords(BUFFER ttServiceLifeCycleDefinition:HANDLE) .

            LogManager:WriteMessage (SUBSTITUTE ("Imported &1 Service Life Cycle Definition records."{&TRAN},
                                                 iNumRecords),
                                     "ServiceManager":U) .

            THIS-OBJECT:ValidateServiceLifeCycleDefinition () .

            IF iNumRecords > 0 THEN
                ServiceInterface:Deactivated:Subscribe (DeactivateHandler) .
        END.

    END CONSTRUCTOR.

    /**
     * Purpose: Event handler for the Deactivated event of the Service Manager
     * Notes:
     * @param e The Consultingwerk.EventArgs with the data for this event
     */
    METHOD PROTECTED VOID DeactivateHandler (e AS Consultingwerk.EventArgs):

        THIS-OBJECT:ProcessServiceLifeCycle() .

    END METHOD.

    /**
     * Purpose: Deregisters an external Business Service from the ServiceManager class
     * Notes:   External Business Services are launched externally, may even be
     *          registered in the ServiceContainer class. This is useful for allowing
     *          access to Session Services (e.g. FrameworkSettings:ServiceContainer) to
     *          be exposed to clients using the Service Interface while staying directly
     *          accessible on the back end though the ServiceContainer. External Business
     *          Services are considered not to be managed by the ServiceManager, that
     *          is the ServiceManager will not attempt to shut them down in the Stop...
     *          kind of methods.
     * @param pcServiceName The name of the Service to be deregistered (typically the class name)
     */
    METHOD PUBLIC VOID DeregisterBusinessService (pcServiceName AS CHARACTER):

        DEFINE BUFFER ttServices FOR ttServices .

        FIND ttServices WHERE ttServices.ServiceName = pcServiceName
                          AND ttServices.Managed     = FALSE NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        LogManager:WriteMessage (SUBSTITUTE ("Deregister Business Service: &1 Available: &2."{&TRAN},
                                             pcServiceName,
                                             AVAILABLE ttServices),
                                 "ServiceManager":U) .

        IF NOT AVAILABLE ttServices THEN
            UNDO, THROW NEW ExternalBusinessServiceNotRegistered (pcServiceName) .

        DELETE ttServices .

    END METHOD .

    /**
     * Purpose: Returns a reference to a Business Service Instance
     * Notes:   Starts the Business Service if required (using LaunchBusinessService)
     *          It is possible to use multiple instances of the same Business Service
     *          class at once. In this case the business service name should be
     *          concatenated from the class name followed by CHR(1) and the instance
     *          identified
     * @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
     * @return The reference to the IBusinessService
     */
    METHOD PUBLIC IBusinessService GetBusinessService (pcServiceName AS CHARACTER):

        DEFINE BUFFER ttServices FOR ttServices .

        &IF DEFINED (UseServiceNameMappingService) NE 0 &THEN
        ASSIGN pcServiceName = THIS-OBJECT:GetMappedServiceName (pcServiceName) .
        &ENDIF

        FIND ttServices WHERE ttServices.ServiceName = pcServiceName NO-LOCK NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN DO:
            ASSIGN ttServices.LastRequest = NOW .

            RETURN CAST (ttServices.Instance, IBusinessService) .
        END.

        RETURN LaunchBusinessService (pcServiceName) .

    END METHOD.

    /**
     * Purpose: Returns the mapped business service name from the current registerd
     *          implementation of the IServiceNameMappingService interface (can but
     *          does not need to be an instance of this class here)
     * Notes:   Returns the input parameter when no IServiceNameMappingService exists
     *          or that implementation returns "" or ?
     * @param pcServiceName The service name as provided by the consumer
     * @return The name of the actual business service implementing class
     */
    METHOD PUBLIC CHARACTER GetMappedServiceName (pcServiceName AS CHARACTER):

        DEFINE VARIABLE cMappedServiceName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iPos               AS INTEGER   NO-UNDO .

        IF lServiceQueried = FALSE THEN DO:
            ASSIGN oService        = {Consultingwerk/get-service.i Consultingwerk.OERA.IServiceNameMappingService
                                                                   "NEW ServiceNameMappingService()"}
                   lServiceQueried = TRUE .
        END.

        IF NOT VALID-OBJECT (oService) THEN
            RETURN pcServiceName .

        ASSIGN iPos = StringHelper:FirstOccurenceOf (pcServiceName, "|,#,":U + CHR (1)) .

        IF iPos > 1 THEN
            ASSIGN cMappedServiceName = oService:GetBusinessServiceName (SUBSTRING (pcServiceName, 1, iPos - 1, "CHARACTER":U)) .
        ELSE
            ASSIGN cMappedServiceName = oService:GetBusinessServiceName (pcServiceName) .

        IF cMappedServiceName > "":U THEN DO:
            IF iPos > 1 THEN
                RETURN cMappedServiceName + SUBSTRING (pcServiceName, iPos, -1, "CHARACTER":U).
            ELSE
                RETURN cMappedServiceName .
        END.
        ELSE
            RETURN pcServiceName .

    END METHOD .

    /**
     * Purpose: Returns the eBusinessServices Temp-Table with the monitoring information
     *          about the service manager
     * Notes:
     * @param eBusinessServices Temp-Table from Consultingwerk/OERA/Monitoring/eBusinessServices.i
     */
    METHOD PUBLIC VOID GetServiceManagerMonitoring (OUTPUT TABLE eBusinessServices):

        EMPTY TEMP-TABLE eBusinessServices .

        FOR EACH ttServices ON ERROR UNDO, THROW:
            CREATE eBusinessServices .
            BUFFER-COPY ttServices TO eBusinessServices .
        END.

    END METHOD .

    /**
     * Purpose: Returns if the given Business Service is already running
     * Notes:   Default version of the method only checks for externally launched
     *          (=registered) Business Services
     * @param pcServiceName The name under which the service is registered (typically the class name)
     * @return Logical value indicating if the Business Service is running
     */
    METHOD PUBLIC LOGICAL IsRunning (pcServiceName AS CHARACTER):

        DEFINE BUFFER ttServices FOR ttServices .

        &IF DEFINED (UseServiceNameMappingService) NE 0 &THEN
        ASSIGN pcServiceName = THIS-OBJECT:GetMappedServiceName (pcServiceName) .
        &ENDIF

        FIND ttServices WHERE ttServices.ServiceName = pcServiceName
                          AND ttServices.Managed     = FALSE NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN DO:
            ASSIGN ttServices.LastRequest = NOW .

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Returns if the specified Business Service has a matching life cycle
     *          definition
     * Notes:
     * @param pcServiceName The name of the Business Service
     * @return Logical value indicating if the specified service has a matching life cycle definition
     */
    METHOD PUBLIC LOGICAL HasLifeCycleDefinition (pcServiceName AS CHARACTER):

        DEFINE VARIABLE iPos AS INTEGER NO-UNDO.

        FOR EACH ttServiceLifeCycleDefinition ON ERROR UNDO, THROW:

            ASSIGN iPos = INDEX (ttServiceLifeCycleDefinition.ServiceNamePattern, "*":U) .

            CASE iPos:
                WHEN 0 THEN
                    IF ttServiceLifeCycleDefinition.ServiceNamePattern = pcServiceName THEN
                        RETURN TRUE .
                WHEN LENGTH (ttServiceLifeCycleDefinition.ServiceNamePattern, "CHARACTER":U) THEN
                    IF pcServiceName BEGINS SUBSTRING (ttServiceLifeCycleDefinition.ServiceNamePattern,
                                                       1,
                                                       iPos - 1,
                                                       "CHARACTER":U) THEN
                        RETURN TRUE .
                OTHERWISE
                    IF pcServiceName MATCHES ttServiceLifeCycleDefinition.ServiceNamePattern THEN
                        RETURN TRUE .
            END CASE.

        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Returns if the given Business Service is already running
     * Notes:
     * @param pcServiceName The name under which the service is registered (typically the class name)
     * @param poServiceType The service type to check for, defaults to Any when ? is passed in
     * @return Logical value indicating if the Business Service is running
     */
    METHOD PUBLIC LOGICAL IsRunning (pcServiceName AS CHARACTER,
                                     poServiceType AS BusinessServiceTypeEnum):

        DEFINE BUFFER ttServices FOR ttServices .

        &IF DEFINED (UseServiceNameMappingService) NE 0 &THEN
        ASSIGN pcServiceName = THIS-OBJECT:GetMappedServiceName (pcServiceName) .
        &ENDIF

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

        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN DO:
            ASSIGN ttServices.LastRequest = NOW .

            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Launches a Business Services
     * Notes:   When a # or | is used in the service name parameter, use the second entry
     *          as the character parameter to the Constructor. The first entry needs
     *          to contain the service class name. The Business Service is is referenced
     *          in a temp-table for latter access to the running instance.
     * @param pcServiceName The Name of the Business Service, evtl. containing a # or |, see Notes
     * @return The reference to the IBusinessService
     */
    METHOD PROTECTED IBusinessService LaunchBusinessService (pcServiceName AS CHARACTER):

        DEFINE VARIABLE oBusinessService  AS IBusinessService NO-UNDO .
        DEFINE VARIABLE cServiceClassName AS CHARACTER        NO-UNDO.

        DEFINE BUFFER ttServices FOR ttServices .

        FIND ttServices WHERE ttServices.ServiceName = pcServiceName NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        IF AVAILABLE ttServices AND VALID-OBJECT (ttServices.Instance) THEN DO:
            LogManager:WriteMessage (SUBSTITUTE ("Business Service is already running: &1."{&TRAN},
                                                 pcServiceName),
                                     "ServiceManager":U) .

            UNDO, THROW NEW BusinessServiceAlreadyRunning (pcServiceName) .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           When there is an Context ID / instance ID, truncate this from the class name */
        ASSIGN cServiceClassName = ENTRY (1, pcServiceName, CHR (1)) .

        /* Mike Fechner, Consultingwerk Ltd. 30.05.2016
           SCL-1337 - handle DYNAMIC-NEW could not find class name errors */
        DO ON ERROR UNDO, THROW:
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

            CATCH err AS Progress.Lang.SysError:
                IF err:NumMessages > 0 AND err:GetMessageNum (1) = 14284 THEN
                    UNDO, THROW NEW UnableToLaunchBusinessServiceException (err,
                                                                            cServiceClassName) .
                ELSE
                    UNDO, THROW err .
            END CATCH.
        END.

        LogManager:WriteMessage (SUBSTITUTE ("Launching Business Service: &1 Class Name: &2."{&TRAN},
                                             pcServiceName,
                                             cServiceClassName),
                                 "ServiceManager":U) .

        /* Mike Fechner, Consultingwerk Ltd. 08.12.2015
           SCL-1137: FIND service again, as the service might have been registerd itself as well
           (from it's own constructor as an unmanaged business service), e.g.
           Consultingwerk.SmartFramework.Authorization.SecurityService */
        FIND ttServices WHERE ttServices.ServiceName = pcServiceName NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        IF NOT AVAILABLE ttServices THEN DO:
            CREATE ttServices .
            ASSIGN ttServices.ServiceName = pcServiceName
                   ttServices.ClassName   = cServiceClassName
                   ttServices.Started     = NOW
                   ttServices.LastRequest = NOW
                   ttServices.Managed     = TRUE .
        END.
        ELSE ASSIGN ttServices.ClassName   = cServiceClassName
                    ttServices.Started     = NOW
                    ttServices.LastRequest = NOW .

        ASSIGN ttServices.Instance = oBusinessService .

        /* Mike Fechner, Consultingwerk Ltd. 01.09.2010
           Let BusinessService Instance know the ContextID */
        IF NUM-ENTRIES (pcServiceName, CHR(1)) > 1 AND TYPE-OF (ttServices.Instance, Consultingwerk.OERA.IHasContextID) THEN
            CAST (ttServices.Instance,
                  IHasContextID):ContextID = ENTRY (2, pcServiceName, CHR(1)) .

        RETURN oBusinessService .

    END METHOD.

    /**
     * Purpose: Processes the Service Life Cycle
     * Notes:   Shuts down Business Services based on the definition of the life cycle
     */
    METHOD PUBLIC VOID ProcessServiceLifeCycle():

        DEFINE VARIABLE iPos AS INTEGER NO-UNDO.

        DEFINE QUERY qQuery FOR ttServices .

        FOR EACH ttServiceLifeCycleDefinition ON ERROR UNDO, THROW:

            ASSIGN iPos = INDEX (ttServiceLifeCycleDefinition.ServiceNamePattern, "*":U) .

            CASE iPos:
                WHEN 0 THEN
                    OPEN QUERY qQuery FOR EACH ttServices WHERE ttServices.Managed
                                                            AND ttServices.ClassName EQ ttServiceLifeCycleDefinition.ServiceNamePattern .
                WHEN LENGTH (ttServiceLifeCycleDefinition.ServiceNamePattern, "CHARACTER":U) THEN
                    OPEN QUERY qQuery FOR EACH ttServices WHERE ttServices.Managed
                                                            AND ttServices.ClassName BEGINS SUBSTRING (ttServiceLifeCycleDefinition.ServiceNamePattern,
                                                                                                       1,
                                                                                                       iPos - 1,
                                                                                                       "CHARACTER":U) .
                OTHERWISE
                    OPEN QUERY qQuery FOR EACH ttServices WHERE ttServices.Managed
                                                            AND ttServices.ClassName MATCHES ttServiceLifeCycleDefinition.ServiceNamePattern .
            END CASE.

            GET FIRST qQuery .

            DO WHILE NOT QUERY qQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

                IF ttServiceLifeCycleDefinition.StopAfterEachRequest THEN
                    THIS-OBJECT:StopBusinessService (ttServices.ServiceName, NotRunningServiceEnum:Ignore) .
                ELSE IF ttServiceLifeCycleDefinition.TimeoutAfterStart > 0 AND
                        INTERVAL (NOW, ttServices.Started, "seconds":U) > ttServiceLifeCycleDefinition.TimeoutAfterStart THEN
                    THIS-OBJECT:StopBusinessService (ttServices.ServiceName, NotRunningServiceEnum:Ignore) .
                ELSE IF ttServiceLifeCycleDefinition.TimeoutAfterLastRequest > 0 AND
                        INTERVAL (NOW, ttServices.LastRequest, "seconds":U) > ttServiceLifeCycleDefinition.TimeoutAfterStart THEN
                    THIS-OBJECT:StopBusinessService (ttServices.ServiceName, NotRunningServiceEnum:Ignore) .

                GET NEXT qQuery .
            END.

            CLOSE QUERY qQuery .
        END.

    END METHOD.

    /**
     * Purpose: Registers an external Business Service with the ServiceManager class
     * Notes:   External Business Services are launched externally, may even be
     *          regsitered in the ServiceContainer class. This is useful for allowing
     *          access to Session Services (e.g. FrameworkSettings:ServiceContainer) to
     *          be exposed to clients using the Service Interface while staying directly
     *          accessible on the back end though the ServiceContainer. External Business
     *          Services are considered not to be managed by the ServiceManager, that
     *          is the ServiceManager will not attempt to shut them down in the Stop...
     *          kind of methods.
     * @param pcServiceName The name under which the service will be registered (typically the class name)
     * @param poInstance The instance of the business service
     */
    METHOD PUBLIC VOID RegisterBusinessService (pcServiceName AS CHARACTER,
                                                poInstance AS IBusinessService):

        DEFINE BUFFER ttServices FOR ttServices .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poInstance """Instance to be registered""{&TRAN}"} .
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcServiceName "'Service Name to be registered'{&TRAN}"} .

        IF CAN-FIND (ttServices WHERE ttServices.ServiceName = pcServiceName) THEN DO:
            LogManager:WriteMessage (SUBSTITUTE ("Business Service is already running: &1."{&TRAN},
                                                 pcServiceName),
                                     "ServiceManager":U) .

            UNDO, THROW NEW BusinessServiceAlreadyRegistered (pcServiceName) .
        END.

        LogManager:WriteMessage (SUBSTITUTE ("Registering Business Service: &1."{&TRAN},
                                             pcServiceName),
                                 "ServiceManager":U) .

        CREATE ttServices.
        ASSIGN ttServices.ServiceName = pcServiceName
               ttServices.Managed     = FALSE
               ttServices.Instance    = poInstance .

    END METHOD .

    /**
     * Purpose: Stops all Business Services Instances
     * Notes:
     */
    METHOD PUBLIC VOID StopAllBusinessServices ():

        DEFINE BUFFER ttServices FOR ttServices .

        LogManager:WriteMessage ("Stopping all Business Services."{&TRAN},
                                 "ServiceManager":U) .

        /* Only shutdown services that were started by the ServiceManager itself */
        FOR EACH ttServices WHERE ttServices.Managed = TRUE:
            IF VALID-OBJECT (ttServices.Instance) THEN
                DELETE OBJECT ttServices.Instance .

            DELETE ttServices .
        END.

    END METHOD.

    /**
     * Purpose: Stops all Business Services Instances of the given Class
     * Notes:
     * @param poClass The Progress.Lang.Class reference (Class or Interface)
     */
    METHOD PUBLIC VOID StopAllBusinessServices (poClass AS Progress.Lang.Class):

        DEFINE BUFFER ttServices FOR ttServices .

        LogManager:WriteMessage (SUBSTITUTE ("Stopping all Business Servicees of type: &1."{&TRAN},
                                             poClass:GetClass():TypeName),
                                 "ServiceManager":U) .

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

    /**
     * Purpose: Stops an Business Service Instance
     * Notes:   Defaults to StopBusinessService (pcServiceName, NotRunningServiceEnum:Throw)
     * Throws:  Consultingwerk.OERA.Exceptions.BusinessServiceNotRunning
     * @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
     */
    METHOD PUBLIC VOID StopBusinessService (pcServiceName AS CHARACTER):

        THIS-OBJECT:StopBusinessService (pcServiceName, NotRunningServiceEnum:Throw) .

    END METHOD.

    /**
     * Purpose: Stops an Business Service Instance
     * Notes:
     * Throws:  Consultingwerk.OERA.Exceptions.BusinessServiceNotRunning
     * @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
     * @param poNotRunningServiceEnum The NotRunningServiceEnum value indicating how to handle a Business Service that is not running
     */
    METHOD PUBLIC VOID StopBusinessService (pcServiceName AS CHARACTER,
                                            poNotRunningServiceEnum AS NotRunningServiceEnum):

        DEFINE BUFFER ttServices FOR ttServices .

        LogManager:WriteMessage (SUBSTITUTE ("Stopping Business Service: &1."{&TRAN},
                                             pcServiceName),
                                 "ServiceManager":U) .

        FIND ttServices WHERE ttServices.ServiceName = pcServiceName
                          AND ttServices.Managed = TRUE NO-LOCK NO-ERROR .

        &IF DEFINED (UseServiceNameMappingService) NE 0 &THEN
        ASSIGN pcServiceName = THIS-OBJECT:GetMappedServiceName (pcServiceName) .
        &ENDIF

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

    /**
     * Purpose: Validates the imported service life cycle definitions for consistency
     * Notes:   Either StopAfterEachRequest, TimeoutAfterLastRequest or
     *          TimeoutAfterStart should be set per entry - but one setting must be
     *          set
     * Throws:  Consultingwerk.OERA.Exceptions.BusinessServiceNotRunning
     */
    METHOD PUBLIC VOID ValidateServiceLifeCycleDefinition ():

        FOR EACH ttServiceLifeCycleDefinition ON ERROR UNDO, THROW:

            IF ttServiceLifeCycleDefinition.ServiceNamePattern = "":U OR
               ttServiceLifeCycleDefinition.ServiceNamePattern = ? THEN

                UNDO, THROW NEW InconsistentServiceLifeCycleDefinitionsException ("Invalid service name pattern ~"~" or ?."{&TRAN},
                                                                                  0,
                                                                                  ttServiceLifeCycleDefinition.ServiceNamePattern) .

            IF ttServiceLifeCycleDefinition.StopAfterEachRequest    = ? AND
               ttServiceLifeCycleDefinition.TimeoutAfterLastRequest = ? AND
               ttServiceLifeCycleDefinition.TimeoutAfterStart       = ? THEN

                UNDO, THROW NEW InconsistentServiceLifeCycleDefinitionsException ("Exactly one setting of StopAfterEachRequest, TimeoutAfterLastRequest, TimeoutAfterStart must be set."{&TRAN},
                                                                                  0,
                                                                                  ttServiceLifeCycleDefinition.ServiceNamePattern) .

            IF ttServiceLifeCycleDefinition.StopAfterEachRequest    <> ? AND
               (ttServiceLifeCycleDefinition.TimeoutAfterLastRequest <> ? OR
               ttServiceLifeCycleDefinition.TimeoutAfterStart       <> ?) THEN

                UNDO, THROW NEW InconsistentServiceLifeCycleDefinitionsException ("Exactly one setting of StopAfterEachRequest, TimeoutAfterLastRequest, TimeoutAfterStart must be set."{&TRAN},
                                                                                  0,
                                                                                  ttServiceLifeCycleDefinition.ServiceNamePattern) .

            IF ttServiceLifeCycleDefinition.TimeoutAfterLastRequest <> ? AND
               (ttServiceLifeCycleDefinition.StopAfterEachRequest   <> ? OR
               ttServiceLifeCycleDefinition.TimeoutAfterStart       <> ?) THEN

                UNDO, THROW NEW InconsistentServiceLifeCycleDefinitionsException ("Exactly one setting of StopAfterEachRequest, TimeoutAfterLastRequest, TimeoutAfterStart must be set."{&TRAN},
                                                                                  0,
                                                                                  ttServiceLifeCycleDefinition.ServiceNamePattern) .
        END.

    END METHOD.

END CLASS.
