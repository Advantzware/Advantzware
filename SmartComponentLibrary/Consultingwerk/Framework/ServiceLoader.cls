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
    File        : ServiceLoader
    Purpose     : Automates loading of services into the ServiceContainer
    Syntax      :
    Description : Temp-Table Structure in Consultingwerk/Framework/ttServiceLoader.i
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 07 23:14:29 CET 2011
    Notes       : The ServiceTypeName field in the temp-table / xml document
                  may contain a comma-delimited list of interface/class names.
                  In that case a single service instance will be registered
                  using multiple interfaces
                  Sample XML structure:
                  <ttServiceLoader xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                    <ttServiceLoaderRow>
                      <Order>1</Order>
                      <ServiceTypeName>Consultingwerk.Framework.IAuthenticationService</ServiceTypeName>
                      <ServiceClassName>Consultingwerk.SmartFramework.Authentication.AuthenticationService</ServiceClassName>
                      <Disabled>false</Disabled>
                    </ttServiceLoaderRow>
                  </ttServiceLoader>
                  Supports custom log file messages using the "ServiceLoader" custom message sub system
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Framework.*  FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Framework.ServiceLoader:

    {Consultingwerk/Framework/ttServiceLoader.i}

    /**
     * Purpose: Returns the reference to the ServiceContainer used by this ServiceLoader
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ServiceContainer AS IServiceContainer NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Constructor for the ServiceLoader class
     * Notes:   Uses FrameworkSettings:ServiceContainer as the default ServiceContainer
     */
    CONSTRUCTOR PUBLIC ServiceLoader ():

        THIS-OBJECT (FrameworkSettings:ServiceContainer) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the ServiceLoader class
     * Notes:
     * @param poServiceContainer The ServiceContainer to work with
     */
    CONSTRUCTOR PUBLIC ServiceLoader (poServiceContainer AS IServiceContainer):
        SUPER ().

        THIS-OBJECT:ServiceContainer = poServiceContainer .

    END CONSTRUCTOR.

    /**
     * Purpose: Loads and registers services from a XML file
     * Notes:   Does not ignore duplicates (will error on duplicates)
     * @param pcFileName An XML Document containing the service definitions
     */
    METHOD PUBLIC VOID Load (pcFileName AS CHARACTER):

        THIS-OBJECT:Load (pcFileName, FALSE) .

    END METHOD .

    /**
     * Purpose: Loads and registers services from a XML file
     * Notes:
     * @param pcFileName An XML Document containing the service definitions
     * @param plIgnoreDuplicates Logical flag indicating if duplicates (by service interface) will be ignored
     */
    METHOD PUBLIC VOID Load (pcFileName AS CHARACTER,
                             plIgnoreDuplicates AS LOGICAL):

        DEFINE VARIABLE oDefinitions AS IServiceDefinitions NO-UNDO .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        LogManager:WriteMessage(SUBSTITUTE ("########################################################################~n":U +
                                            "### Loading service definitions~n":U +
                                            "###    File Name:          &1~n":U +
                                            "###    Absolute File Name: &2~n":U +
                                            "########################################################################~n":U,
                                            pcFileName,
                                            FILE-INFORMATION:FULL-PATHNAME), "ServiceLoader":U) .

        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcFileName":U,
                                                            pcFileName,
                                                            "Consultingwerk.Framework.ServiceLoader":U) .

        oDefinitions = {Consultingwerk/get-service.i Consultingwerk.Framework.IServiceDefinitions
                                                     "NEW Consultingwerk.Framework.ServiceDefinitions ()"} .

        oDefinitions:Add (pcFileName) .

        TEMP-TABLE ttServiceLoader:READ-XML ("FILE":U,
                                             FILE-INFORMATION:FULL-PATHNAME,
                                             "EMPTY":U,
                                             ?,
                                             ?) .

        THIS-OBJECT:Load (TEMP-TABLE ttServiceLoader:HANDLE, plIgnoreDuplicates) .

        FINALLY:
            EMPTY TEMP-TABLE ttServiceLoader .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Loads and registers services from a temp-table
     * Notes:   Does not ignore duplicates (will error on duplicates)
     * @param phTableHandle The handle to a temp-table containing the service definitions
     */
    METHOD PUBLIC VOID Load (phTableHandle AS HANDLE):

        THIS-OBJECT:Load (phTableHandle, FALSE) .

    END METHOD .

    /**
     * Purpose: Loads and registers services from a temp-table
     * Notes:
     * @param phTableHandle The handle to a temp-table containing the service definitions
     * @param plIgnoreDuplicates Logical flag indicating if duplicates (by service interface) will be ignored
     */
    METHOD PUBLIC VOID Load (phTableHandle AS HANDLE,
                             plIgnoreDuplicates AS LOGICAL):

        DEFINE VARIABLE hBuffer    AS HANDLE               NO-UNDO .
        DEFINE VARIABLE hQuery     AS HANDLE               NO-UNDO .
        DEFINE VARIABLE cInterface AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE cClassName AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oClass     AS Progress.Lang.Class  NO-UNDO .
        DEFINE VARIABLE oService   AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER              NO-UNDO .
        DEFINE VARIABLE oException AS ExceptionWithTitle   NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phTableHandle Consultingwerk.WidgetTypeEnum:TempTable} .

        ASSIGN hBuffer = phTableHandle:DEFAULT-BUFFER-HANDLE
               hQuery  = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hBuffer).

        serviceLoop:
        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            /* Mike Fechner, Consultingwerk Ltd. 07.01.2013
               Ability to temporarily disable loading of services by using the
               <disabled>yes</disabled> Tag */
            IF hBuffer::Disabled = TRUE THEN
                NEXT serviceLoop .

            IF hBuffer::RequiredDatabases > "":U THEN DO ON ERROR UNDO, THROW:
                DO i = 1 TO NUM-ENTRIES (hBuffer::RequiredDatabases) ON ERROR UNDO, THROW:
                    IF NOT CONNECTED (ENTRY (i, hBuffer::RequiredDatabases)) THEN
                        NEXT serviceLoop .
                END.
            END.

            ASSIGN cClassName = hBuffer::ServiceClassName .

            DO ON ERROR UNDO, THROW:

                oService = DYNAMIC-NEW (cClassName) () .

                CATCH err AS Progress.Lang.Error:
                    oException = NEW ExceptionWithTitle (err,
                                                         SUBSTITUTE ("Error while loading service: &1"{&TRAN},
                                                                     cClassName),
                                                         0,
                                                         "Error loading services"{&TRAN})  .

                    IF err:NumMessages > 0 THEN
                        oException:AddMessage (err:GetMessage(1),
                                               err:GetMessageNum(1)) .

                    UNDO, THROW oException .
                END CATCH.
            END.

            /* Mike Fechner, Consultingwerk Ltd. 11.01.2012
               Support for registering a single service instance for
               multiple interfaces using a comma-delimited list */
            interfaceloop:
            DO i = 1 TO NUM-ENTRIES (hBuffer::ServiceTypeName) ON ERROR UNDO, THROW:

                ASSIGN cInterface = ENTRY (i, hBuffer::ServiceTypeName)
                       oClass     = Progress.Lang.Class:GetClass (cInterface) .

                IF plIgnoreDuplicates THEN DO:
                    IF VALID-OBJECT (THIS-OBJECT:ServiceContainer:GetService (oClass)) THEN
                        NEXT interfaceloop .
                END.

                THIS-OBJECT:ServiceContainer:AddService (oClass, oService) .
            END.

            FINALLY:
                hQuery:GET-NEXT ().
            END FINALLY.
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Loads and registers services from an XML file
     * Notes:   Does not ignore duplicates (will error on duplicates)
     * @param pcFileName An XML Document containing the service definitions
     */
    METHOD PUBLIC STATIC VOID LoadFromFile (pcFileName AS CHARACTER):

        DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .

        oLoader = NEW ServiceLoader ().
        oLoader:Load (pcFileName, FALSE) .

        FINALLY:
            DELETE OBJECT oLoader .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Loads and registers services from an XML file
     * Notes:
     * @param pcFileName An XML Document containing the service definitions
     * @param plIgnoreDuplicates Logical flag indicating if duplicates (by service interface) will be ignored
     */
    METHOD PUBLIC STATIC VOID LoadFromFile (pcFileName AS CHARACTER,
                                            plIgnoreDuplicates AS LOGICAL):

        DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .

        oLoader = NEW ServiceLoader ().
        oLoader:Load (pcFileName, plIgnoreDuplicates) .

        FINALLY:
            DELETE OBJECT oLoader .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Unloads (removes) services based on an XML file
     * Notes:   Does not force the deletion of the service instance
     * @param pcFileName An XML Document containing the service definitions
     */
    METHOD PUBLIC VOID Unload (pcFileName AS CHARACTER):

        THIS-OBJECT:Unload (pcFileName, FALSE) .

    END METHOD .

    /**
     * Purpose: Unloads (removes) services based on an XML file
     * Notes:
     * @param pcFileName An XML Document containing the service definitions
     * @param plForceDelete Logical flag indicating if we should force the deletion of the instance
     */
    METHOD PUBLIC VOID Unload (pcFileName AS CHARACTER,
                               plForceDelete AS LOGICAL):

        DEFINE VARIABLE oDefinitions AS IServiceDefinitions NO-UNDO .

        FILE-INFORMATION:FILE-NAME = pcFileName .

        LogManager:WriteMessage(SUBSTITUTE ("########################################################################~n":U +
                                            "### Unloading services~n":U +
                                            "###    File Name:          &1~n":U +
                                            "###    Absolute File Name: &2~n":U +
                                            "########################################################################~n":U,
                                            pcFileName,
                                            FILE-INFORMATION:FULL-PATHNAME), "ServiceLoader":U) .

        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
            UNDO, THROW NEW InvalidParameterValueException ("pcFileName":U,
                                                            pcFileName,
                                                            "Consultingwerk.Framework.ServiceLoader":U) .

        oDefinitions = {Consultingwerk/get-service.i Consultingwerk.Framework.IServiceDefinitions
                                                     "NEW Consultingwerk.Framework.ServiceDefinitions ()"} .

        oDefinitions:Remove (pcFileName) .

        TEMP-TABLE ttServiceLoader:READ-XML ("FILE":U,
                                             FILE-INFORMATION:FULL-PATHNAME,
                                             "EMPTY":U,
                                             ?,
                                             ?) .

        THIS-OBJECT:Unload (TEMP-TABLE ttServiceLoader:HANDLE, plForceDelete) .

        FINALLY:
            EMPTY TEMP-TABLE ttServiceLoader .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Unloads (removes) services based on a temp-table
     * Notes:   Does not force the deletion of the service instance
     * @param phTableHandle The handle to a temp-table containing the service definitions
     */
    METHOD PUBLIC VOID Unload (phTableHandle AS HANDLE):

        THIS-OBJECT:Load (phTableHandle, FALSE) .

    END METHOD .

    /**
     * Purpose: Unloads (removes) services based on a temp-table
     * Notes:
     * @param phTableHandle The handle to a temp-table containing the service definitions
     * @param plForceDelete Logical flag indicating if we should force the deletion of the instance
     */
    METHOD PUBLIC VOID Unload (phTableHandle AS HANDLE,
                               plForceDelete AS LOGICAL):

        DEFINE VARIABLE hBuffer    AS HANDLE               NO-UNDO .
        DEFINE VARIABLE hQuery     AS HANDLE               NO-UNDO .
        DEFINE VARIABLE cInterface AS CHARACTER            NO-UNDO .
        DEFINE VARIABLE oClass     AS Progress.Lang.Class  NO-UNDO .
        DEFINE VARIABLE oService   AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER              NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phTableHandle Consultingwerk.WidgetTypeEnum:TempTable} .

        ASSIGN hBuffer = phTableHandle:DEFAULT-BUFFER-HANDLE
               hQuery  = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hBuffer,
                                                                              "for each ttServiceLoader BY Order DESCENDING":U).

        serviceLoop:
        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            /* Mike Fechner, Consultingwerk Ltd. 07.01.2013
               Ability to temporarily disable loading of services by using the
               <disabled>yes</disabled> Tag */
            IF hBuffer::Disabled = TRUE THEN
                NEXT serviceLoop .

            /* Mike Fechner, Consultingwerk Ltd. 11.01.2012
               Support for registering a single service instance for
               multiple interfaces using a comma-delimited list */
            interfaceloop:
            DO i = 1 TO NUM-ENTRIES (hBuffer::ServiceTypeName) ON ERROR UNDO, THROW:

                ASSIGN cInterface = ENTRY (i, hBuffer::ServiceTypeName)
                       oClass     = Progress.Lang.Class:GetClass (cInterface) .

                IF plForceDelete THEN
                    oService = THIS-OBJECT:ServiceContainer:GetService (oClass) .

                THIS-OBJECT:ServiceContainer:RemoveService (oClass) .

                IF plForceDelete AND VALID-OBJECT (oService)THEN
                    DELETE OBJECT oService .
            END.

            FINALLY:
                hQuery:GET-NEXT ().
            END FINALLY.
        END.

    END METHOD .

    /**
     * Purpose: Unloads (removes) services based on an XML file
     * Notes:   Does not force the deletion of the service instance
     * @param pcFileName An XML Document containing the service definitions
     */
    METHOD PUBLIC STATIC VOID UnloadFromFile (pcFileName AS CHARACTER):

        DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .

        oLoader = NEW ServiceLoader ().
        oLoader:Unload (pcFileName, FALSE) .

        FINALLY:
            DELETE OBJECT oLoader .
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Unloads (removes) services based on an XML file
     * Notes:
     * @param pcFileName An XML Document containing the service definitions
     * @param plForceDelete Logical flag indicating if we should force the deletion of the instance
     */
    METHOD PUBLIC STATIC VOID UnloadFromFile (pcFileName AS CHARACTER,
                                              plForceDelete AS LOGICAL):

        DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO .

        oLoader = NEW ServiceLoader ().
        oLoader:Unload (pcFileName, plForceDelete) .

        FINALLY:
            DELETE OBJECT oLoader .
        END FINALLY.

    END METHOD .

END CLASS.
