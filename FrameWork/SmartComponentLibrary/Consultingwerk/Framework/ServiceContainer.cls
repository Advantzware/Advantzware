/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ServiceContainer
    Purpose     : Provides a container for services, default implementation
                  of the IServiceContainer interface
    Syntax      :
    Description : General purpose container to manage services by type / class
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 23 13:11:30 CET 2010
    Notes       : An instance is by default accessible from the
                  Consultingwerk.Framework.FrameworkSettings class.
                  Supports also lazy loading of services similar to
                  http://msdn.microsoft.com/de-de/library/system.componentmodel.design.servicecreatorcallback(v=vs.110).aspx
                  (SCL-573). To lazy load services, register instances of
                  the IServiceCreator in the ServiceContainer. Those factory
                  instances will be used to create the actual service. The
                  requested service type will be passed as an input parameter
                  to the IServiceCreator:CreateInstance method, so that the
                  same IServiceCreator instance might be used as a factory
                  for multiple service instances
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*            FROM PROPATH .
USING Consultingwerk.Framework.Exceptions.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Framework.ServiceContainer
    IMPLEMENTS IServiceContainer, IServiceContainerDebugging, IOverloadedServiceContainer:

    DEFINE PRIVATE TEMP-TABLE ttServices NO-UNDO
        FIELD ServiceType   AS CHARACTER
        FIELD ServiceObject AS Progress.Lang.Object
        INDEX ServiceType IS UNIQUE ServiceType
        .

    { Consultingwerk/Framework/ttServiceTable.i }

    /**
     * Purpose: Gets and sets the reference to the overloading service container
     * Notes:   The overloading service container is queried by the current
     *          instance before querying the servives of that service container
     *          itself
     */
    DEFINE PUBLIC PROPERTY OverloadingServiceContainer AS IServiceContainer NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Adds the specified service to the service container.
     * Notes:
     * @param poClass The reference to the class or interface of the service to add
     * @param poObject An instance of the service type to add. This object must implement or inherit from the type indicated by the serviceType parameter
     * @return The reference to the service that was added (poObject). This allows fluid style usage of this routine
     */
    METHOD PUBLIC Progress.Lang.Object AddService (poClass AS Progress.Lang.Class,
                                                   poObject AS Progress.Lang.Object):

        DEFINE BUFFER ttServices FOR ttServices .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass """poClass"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poObject """poObject"":U"} .

        IF FindService (poClass) THEN
            UNDO, THROW NEW ServiceAlreadyRegisteredException (poClass) .

        CREATE ttServices .
        ASSIGN ttServices.ServiceType   = poClass:TypeName
               ttServices.ServiceObject = poObject .

        RETURN poObject .

    END METHOD .

    /**
     * Purpose: Creates a service at first request
     * Notes:   Replaces the reference to the IServiceCreator with the reference to
     *          the actual service in the service table
     * @param poCreator The IServiceCreator instance
     * @param poClass The service type to create
     * @return The refrence to the created service instance
     */
    METHOD PROTECTED Progress.Lang.Object CreateService (poCreator AS IServiceCreator,
                                                         poClass AS Progress.Lang.Class):

        DEFINE VARIABLE oService AS Progress.Lang.Object NO-UNDO .

        oService = poCreator:CreateInstance (poClass) .

        THIS-OBJECT:FindService (poClass) .

        /* Register the service instead of the creator in the service table */
        ASSIGN ttServices.ServiceObject = oService .

        RETURN oService .

    END METHOD .

    /**
     * Purpose: Searches the passed in Service Class in the ttService temp-table
     * Notes:
     * @param poClass The reference to the class or interface of the service that should be located
     * @return Logical value indicating if the service is available
     */
    METHOD PROTECTED LOGICAL FindService (poClass AS Progress.Lang.Class):

        FIND ttServices WHERE ttServices.ServiceType = poClass:TypeName
            NO-ERROR .

        ERROR-STATUS:ERROR = FALSE NO-ERROR .

        RETURN AVAILABLE ttServices .

    END METHOD.

    /**
     * Purpose: Gets the service object of the specified type.
     * Notes:   Throws an ServiceNotRegisteredException when no service of that type
     *          registered
     * @param poClass The reference to the class or interface of the service to return
     * @return The reference to the instance of the service of ? when the service is not registered with the service container
     */
    METHOD PUBLIC Progress.Lang.Object GetMandatoryService (poClass AS Progress.Lang.Class).

        DEFINE VARIABLE oInstance AS Progress.Lang.Object NO-UNDO .

        oInstance = THIS-OBJECT:GetService (poClass) .

        IF NOT VALID-OBJECT (oInstance) THEN
            UNDO, THROW NEW ServiceNotRegisteredException (poClass) .

        RETURN oInstance .

    END METHOD .

    /**
     * Purpose: Gets the service object of the specified type
     * Notes:   Returns ? when no service of that type is registered
     * @param poClass The reference to the class or interface of the service to return
     * @return The reference to the instance of the service of ? when the service is not registered with the service container
     */
    METHOD PUBLIC Progress.Lang.Object GetService (poClass AS Progress.Lang.Class):

        DEFINE VARIABLE oInstance AS Progress.Lang.Object NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass """poClass"":U"} .

        /* Mike Fechner, Consultingwerk Ltd. 15.09.2016
           SCL-1453 - Support for Overloaded Service Container */
        IF VALID-OBJECT (THIS-OBJECT:OverloadingServiceContainer) THEN DO:
            ASSIGN oInstance = THIS-OBJECT:OverloadingServiceContainer:GetService (poClass) .

            IF VALID-OBJECT (oInstance) THEN
                RETURN oInstance .
        END.

        IF NOT FindService (poClass) THEN
            RETURN ? .

        IF VALID-OBJECT (ttServices.ServiceObject) AND
           TYPE-OF (ttServices.ServiceObject, IServiceCreator) THEN
            RETURN THIS-OBJECT:CreateService (CAST (ttServices.ServiceObject, IServiceCreator), poClass) .

        RETURN ttServices.ServiceObject .

    END METHOD .

    /**
     * Purpose: Returns the service instances as a ttServiceTable output
     * Notes:   See Consultingwerk/Framework/ttServiceTable.i
     * @param ttServiceTable OUTPUT TABLE The temp-table with the service definitions
     */
    METHOD PUBLIC VOID GetServiceTable (OUTPUT TABLE FOR ttServiceTable):

        EMPTY TEMP-TABLE ttServiceTable .

        FOR EACH ttServices ON ERROR UNDO, THROW:
            CREATE ttServiceTable .
            ASSIGN ttServiceTable.ServiceType     = ttServices.ServiceType
                   ttServiceTable.ServiceInstance = ttServices.ServiceObject:ToString() .
        END.

    END METHOD .

    /**
     * Purpose: Removes the specified service type from the service container.
     * Notes:
     * @param poClass The reference to the class or interface of the service to remove from the service container.
     */
    METHOD PUBLIC VOID RemoveService (poClass AS Progress.Lang.Class):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass """poClass"":U"} .

        IF NOT FindService (poClass) THEN
            RETURN .

        DELETE ttServices .

    END METHOD .

END CLASS.
