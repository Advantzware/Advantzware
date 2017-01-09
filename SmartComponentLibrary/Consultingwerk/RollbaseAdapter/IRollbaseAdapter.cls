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
    File        : IRollbaseAdapter
    Purpose     : Provides methods to communicate with Progress Rollbase
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 03 09:51:55 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.RollbaseAdapter.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.RollbaseAdapter.IRollbaseAdapter:  
  
    { Consultingwerk/RollbaseAdapter/MetaData/dsObjectDef.i }
  
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Credentials to be used for Rollbase
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Credentials AS IRollbaseCredentials NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Performs an create2 request to create a record with given field 
                 values for a new Rollbase object instance 
        Notes:   
        @param pcObjectName The Integration Name of the obejct to create
        @param poValues The List of Name Value Pairs for of field names and values
        @return The Character reponse from Rollbase
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Create2 (pcObjectName AS CHARACTER, 
                                     poValues AS Consultingwerk.ListNameValuePair). 
                                         
    /*------------------------------------------------------------------------------
        Purpose: Returns a DATE from a Rollbase Returned Date String 
        Notes:   
        @param pcDateString The String containing a DATE
        @return The DATE value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATE DateValue (pcDateString AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns a DATETIME from a Rollbase Returned DateTime String 
        Notes:   
        @param pcDateTimeString The String containing a DATE
        @return The DATETIME value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATETIME DateTimeValue (pcDateTimeString AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Deletes a record 
        Notes:   
        @param piId The Id of the record to delete
        @return Logical value indicating if the record was deleted
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Delete (piId AS INT64).

    /*------------------------------------------------------------------------------
        Purpose: Returns an Character Array of available object definitions
        Notes:   
        @param poPermission The optional PermissionEnum value controlling if the output should be limited to certain objects 
        @return The Character Array of available object definitions 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER EXTENT GetObjectDefNames (poPermission AS PermissionEnum).

    /*------------------------------------------------------------------------------
        Purpose: Retrieves Rollbase Object Definition at a dsObjectDef Dataset 
        Notes:   
        @param pcObjectName The name of the object (class) to retrieve the definition for
        @param dsObjectDef OUTPUT Dataset The dataset with the object defintion 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectDef (pcObjectName AS CHARACTER,
                                     OUTPUT DATASET dsObjectDef). 
                                     
    /*------------------------------------------------------------------------------
        Purpose: Authenticates with Rollbase 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Login ().

    /*------------------------------------------------------------------------------
        Purpose: Closes a Rollbase session 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Logoff ().

    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL Data Type Name for a Rollbase Field Type 
        Notes:   Returns ? for unknown/unmappable field types  
        @param pcRollbaseType The Rollbase DataType
        @return The ABL Data Type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RollbaseDataTypeToAbl (pcRollbaseType AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Performs an selectQuery Request and populates the results to an 
                 ABL temp-table 
        Notes:   
        @param pcObjectName The name of the object to retrieve data from
        @param pcQuery The rollbase QueryString to use
        @param piStartRow The number of the row to start output with (0 by default)
        @param piNumRows The number of rows to return
        @param phTable INPUT-OUTPUT TABLE-HANLDLE The name of the temp-table to select into
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectToTempTable (pcObjectName AS CHARACTER, 
                                          pcQuery AS CHARACTER,
                                          piStartRow AS INTEGER, 
                                          piNumRows AS INTEGER, 
                                          INPUT-OUTPUT TABLE-HANDLE phTable).

    /*------------------------------------------------------------------------------
        Purpose: Performs an update2 request to update field values of a Rollbase 
                 object instance 
        Notes:   
        @param piObjectId The Id of the object to update
        @param poValues The List of Name Value Pairs for of field names and values
        @return The Character reponse from Rollbase
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Update2 (piObjectId AS INT64, 
                                     poValues AS Consultingwerk.ListNameValuePair) .
END INTERFACE.
