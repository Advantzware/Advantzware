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
    File        : RepositoryServiceClient
    Purpose     : Simple client class for the Repository Service
    Syntax      :
    Description : Accesses the serverside Repository service though a proxy
                  task class
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 27 22:11:29 CET 2012
    Notes       : Accesses the server side IRepositoryService implementation
                  through the RepositoryServiceTask class.
                  The AppServer needs to start an IRepositoryService implementation
                  during the start of the AppServer process.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.Framework.*                    FROM PROPATH .
USING Consultingwerk.Framework.RepositoryServices.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.Framework.RepositoryServices.RepositoryServiceClient
    IMPLEMENTS IRepositoryService:

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of class names that extend the given base class
                 (including the given class)
        Notes:
        @param pcObjectType The base class name
        @return Comma-delimited list of class names inheriting from the base class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ClassChildren (pcObjectType AS CHARACTER):

        DEFINE VARIABLE oParameter AS RepositoryTaskParameter NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter,
                                                      "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U) .

        oParameter = NEW RepositoryTaskParameter () .
        oParameter:ObjectType = pcObjectType .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.Framework.RepositoryServices.RepositoryServiceTask":U,
                                                     "ClassChildren":U,
                                                     oParameter,
                                                     ?) .

        IF VALID-OBJECT (oParameter) THEN
            RETURN oParameter:ClassChildren .
        ELSE
            RETURN "":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository structure for objects of the given type
        Notes:
        @param pcObjectType The object type to return the object structure for
        @param phObjectDataset OUTPUT DATASET-HANDLE containing the an empty temp-table with a single field per object attribute
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetEmptyObjectStructure (pcObjectType AS CHARACTER,
                                                OUTPUT DATASET-HANDLE phObjectDataset):

        DEFINE VARIABLE oParameter AS RepositoryTaskParameter NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter,
                                                      "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U) .

        oParameter = NEW RepositoryTaskParameter () .
        oParameter:ObjectType = pcObjectType .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.Framework.RepositoryServices.RepositoryServiceTask":U,
                                                     "GetEmptyObjectStructure":U,
                                                     INPUT-OUTPUT DATASET-HANDLE phObjectDataset,
                                                     oParameter,
                                                     ?) .
        DO ON ERROR UNDO, THROW:
            DELETE OBJECT phObjectDataset .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error:
                /* noop */
            END CATCH.
        END .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository data for the named object
        Notes:
        @param pcObjectName The object to return the repository definition for
        @param phObjectDataset OUTPUT DATASET-HANDLE containing the repository definition of the given object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectByName (pcObjectName AS CHARACTER,
                                        OUTPUT DATASET-HANDLE phObjectDataset):

        DEFINE VARIABLE oParameter AS RepositoryTaskParameter NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter,
                                                      "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U) .

        oParameter = NEW RepositoryTaskParameter () .
        oParameter:ObjectName = pcObjectName .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.Framework.RepositoryServices.RepositoryServiceTask":U,
                                                     "GetObjectByName":U,
                                                     INPUT-OUTPUT DATASET-HANDLE phObjectDataset,
                                                     oParameter,
                                                     ?) .
        DO ON ERROR UNDO, THROW:
            DELETE OBJECT phObjectDataset .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error:
                /* noop */
            END CATCH.
        END .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository data for the named object into the given
                 ProDataset
        Notes:
        @param pcObjectName The object to return the repository definition for
        @param phObjectDataset OUTPUT DATASET-HANDLE containing the repository definition of the given object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectByName (pcObjectName AS CHARACTER,
                                        INPUT-OUTPUT DATASET-HANDLE phObjectDataset).

        DEFINE VARIABLE oParameter AS RepositoryTaskParameter NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter,
                                                      "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U) .

        oParameter = NEW RepositoryTaskParameter () .
        oParameter:ObjectName = pcObjectName .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.Framework.RepositoryServices.RepositoryServiceTask":U,
                                                     "GetObjectByName":U,
                                                     INPUT-OUTPUT DATASET-HANDLE phObjectDataset,
                                                     oParameter,
                                                     ?) .
        DO ON ERROR UNDO, THROW:
            DELETE OBJECT phObjectDataset .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error:
                /* noop */
            END CATCH.
        END .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository data for the named object instance into the
                 given ProDataset
        Notes:
        @param pcContainerObjectName The object to return the repository definition for
        @param pcInstanceName The name of the object instance
        @param phObjectDataset OUTPUT DATASET-HANDLE containing the repository definition of the given object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectInstanceByName (pcContainerObjectName AS CHARACTER,
                                                pcInstanceName AS CHARACTER,
                                                INPUT-OUTPUT DATASET-HANDLE phObjectDataset).

        DEFINE VARIABLE oParameter AS RepositoryTaskParameter NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter,
                                                      "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U) .

        oParameter = NEW RepositoryTaskParameter () .
        oParameter:ObjectName = pcContainerObjectName .
        oParameter:ObjectInstanceName = pcInstanceName .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.Framework.RepositoryServices.RepositoryServiceTask":U,
                                                     "GetObjectInstanceByName":U,
                                                     INPUT-OUTPUT DATASET-HANDLE phObjectDataset,
                                                     oParameter,
                                                     ?) .
        DO ON ERROR UNDO, THROW:
            DELETE OBJECT phObjectDataset .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error:
                /* noop */
            END CATCH.
        END .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores the repository data for the named object
        Notes:
        @param plNew True for new objects, false for existing objects
        @param phObjectDataset DATASET-HANDLE containing the repository definition of the given object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreObject (plNew AS LOGICAL,
                                    DATASET-HANDLE phObjectDataset):

        DEFINE VARIABLE oParameter AS RepositoryTaskParameter NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter,
                                                      "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U) .

        oParameter = NEW RepositoryTaskParameter () .
        oParameter:New = plNew .

        FrameworkSettings:ServiceAdapter:InvokeTask ("":U,
                                                     "Consultingwerk.Framework.RepositoryServices.RepositoryServiceTask":U,
                                                     "StoreObject":U,
                                                     INPUT-OUTPUT DATASET-HANDLE phObjectDataset,
                                                     oParameter,
                                                     ?) .

        DO ON ERROR UNDO, THROW:
            DELETE OBJECT phObjectDataset .

            @SuppressUnusedWarnings.
            CATCH err AS Progress.Lang.Error:
                /* noop */
            END CATCH.
        END .

    END METHOD.

END CLASS.
