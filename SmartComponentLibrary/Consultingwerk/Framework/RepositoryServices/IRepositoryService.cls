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
    File        : IRepositoryService
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 25 08:53:01 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.RepositoryServices.IRepositoryService:

    /*------------------------------------------------------------------------------
        Purpose: Returns a list of class names that extend the given base class
                 (including the given class)
        Notes:
        @param pcObjectType The base class name
        @return Comma-delimited list of class names inheriting from the base class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ClassChildren (pcObjectType AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository structure for objects of the given types
        Notes:   Supports pcObjectType as a comma delimited list of object types
        @param pcObjectType The object types to return the object structure for
        @param phObjectDataset OUTPUT DATASET-HANDLE containing the an empty temp-table with a single field per object attribute
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetEmptyObjectStructure (pcObjectType AS CHARACTER,
                                                OUTPUT DATASET-HANDLE phObjectDataset).

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository data for the named object
        Notes:
        @param pcObjectName The object to return the repository definition for
        @param phObjectDataset OUTPUT DATASET-HANDLE containing the repository definition of the given object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectByName (pcObjectName AS CHARACTER,
                                        OUTPUT DATASET-HANDLE phObjectDataset).

    /*------------------------------------------------------------------------------
        Purpose: Returns the repository data for the named object into the given
                 ProDataset
        Notes:
        @param pcObjectName The object to return the repository definition for
        @param phObjectDataset OUTPUT DATASET-HANDLE containing the repository definition of the given object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectByName (pcObjectName AS CHARACTER,
                                        INPUT-OUTPUT DATASET-HANDLE phObjectDataset).

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

    /*------------------------------------------------------------------------------
        Purpose: Stores the repository data for the named object
        Notes:
        @param plNew True for new objects, false for existing objects
        @param phObjectDataset DATASET-HANDLE containing the repository definition of the given object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreObject (plNew AS LOGICAL,
                                    INPUT DATASET-HANDLE phObjectDataset).

END INTERFACE.
