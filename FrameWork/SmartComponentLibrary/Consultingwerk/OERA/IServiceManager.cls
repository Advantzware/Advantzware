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
    File        : IServiceManager
    Purpose     : Service Interface for classes that manages Business Entity
                  or Business Task instances (IBusinessService interface)
                  for the Service Interface
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 17 22:11:43 CEST 2013
    Notes       : Consultingwerk.OERA.ServiceInterface provides a static
                  facade to the IServiceManager implementation
                  It is possible to use multiple instances of the same
                  Business Service class at once. In this case the business
                  service name should be concatenated from the class name
                  followed by CHR(1) and the instance identified
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH.

INTERFACE Consultingwerk.OERA.IServiceManager:

    /**
     * Purpose: Deregisters an external Business Service from the ServiceManager class
     * Notes:   External Business Services are launched externally, may even be
     *          regsitered in the ServiceContainer class. This is useful for allowing
     *          access to Session Services (e.g. FrameworkSettings:ServiceContainer) to
     *          be exposed to clients using the Service Interface while staying directly
     *          accessible on the back end though the ServiceContainer. External Business
     *          Services are considered not to be managed by the ServiceManager, that
     *          is the ServiceManager will not attempt to shut them down in the Stop...
     *          kind of methods.
     * @param pcServiceName The name of the Service to be deregistered (typically the class name)
     */
    METHOD PUBLIC VOID DeregisterBusinessService (pcServiceName AS CHARACTER) .

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
    METHOD PUBLIC IBusinessService GetBusinessService (pcServiceName AS CHARACTER) .

    /**
     * Purpose: Returns if the specified Business Service has a matching life cycle
     *          definition
     * Notes:
     * @param pcServiceName The name of the Business Service
     * @return Logical value indicating if the specified service has a matching life cycle definition
     */
    METHOD PUBLIC LOGICAL HasLifeCycleDefinition (pcServiceName AS CHARACTER).

    /**
     * Purpose: Returns if the given Business Service is already running
     * Notes:   Default version of the method only checks for externally launched
     *          (=registered) Business Services
     * @param pcServiceName The name under which the service is registered (typically the class name)
     * @return Logical value indicating if the Business Service is running
     */
    METHOD PUBLIC LOGICAL IsRunning (pcServiceName AS CHARACTER).

    /**
     * Purpose: Returns if the given Business Service is already running
     * Notes:
     * @param pcServiceName The name under which the service is registered (typically the class name)
     * @param poServiceType The service type to check for
     * @return Logical value indicating if the Business Service is running
     */
    METHOD PUBLIC LOGICAL IsRunning (pcServiceName AS CHARACTER,
                                     poServiceType AS BusinessServiceTypeEnum).

    /**
     * Purpose: Processes the Service Life Cycle
     * Notes:   Shuts down Business Services based on the definition of the life cycle
     */
    METHOD PUBLIC VOID ProcessServiceLifeCycle() .

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
                                                poInstance AS IBusinessService) .

    /**
     * Purpose: Stops all Business Services Instances
     * Notes:
     */
    METHOD PUBLIC VOID StopAllBusinessServices () .

    /**
     * Purpose: Stops all Business Services Instances of the given Class
     * Notes:
     * @param poClass The Progress.Lang.Class reference (Class or Interface)
     */
    METHOD PUBLIC VOID StopAllBusinessServices (poClass AS Progress.Lang.Class) .

    /**
     * Purpose: Stops an Business Service Instance
     * Notes:   Should default to StopBusinessService (pcServiceName, NotRunningServiceEnum:Throw)
     * @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
     */
    METHOD PUBLIC VOID StopBusinessService (pcServiceName AS CHARACTER) .

    /**
     * Purpose: Stops an Business Service Instance
     * Notes:   Defaults to StopBusinessService (pcServiceName, NotRunningServiceEnum:Throw)
     * @param pcServiceName The Name of the Business Service, evtl. containing a #, see Notes at LaunchBusinessService
     * @param poNotRunningServiceEnum The NotRunningServiceEnum value indicating how to handle a Business Service that is not running
     */
    METHOD PUBLIC VOID StopBusinessService (pcServiceName AS CHARACTER,
                                            poNotRunningServiceEnum AS NotRunningServiceEnum) .

END INTERFACE.
