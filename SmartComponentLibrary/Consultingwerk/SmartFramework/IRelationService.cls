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
    File        : IRelationService
    Purpose     : Describes methods of services supporting database relations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 04 21:00:36 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.IRelationService:  

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of a query (prepared, opened and located at the 
                 first record) of all child records of the given child record
        Notes:   
        @param phParentBuffer The handle of the database buffer with the parent record
        @param phChildBuffer The handle of the CHild buffer which will be used in the returned query  
        @return The handle of a database query of the child records
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetChildRecordQueryOf (phParentBuffer AS HANDLE,
                                                phChildBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the first child record of the given child record
        Notes:   
        @param phParentBuffer The handle of the database buffer with the parent record
        @param pcChildTableName The name of the database table with the child record 
        @return The handle of a database buffer with the child record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetFirstChildRecordOf (phParentBuffer AS HANDLE,
                                                pcChildTableName AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the first parent record referenced by the given child field
        Notes:   The parent relation is identified by the child buffer name and the  
                 relation based on the given child field name
        @param phChildBuffer The handle of the database buffer with the child record
        @param pcChildFieldName The unqualified field name of the child buffer that references the parent table
        @return The handle of a database buffer with the parent record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordForField (phChildBuffer AS HANDLE,
                                                  pcChildFieldName AS CHARACTER).
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the first parent record of the given child record
        Notes:   
        @param phChildBuffer The handle of the database buffer with the child record
        @return The handle of a database buffer with the parent record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordOf (phChildBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the description value for the parent record referenced by 
                 the given field.
        Notes:   The parent relation is identified by the child buffer name and the  
                 relation based on the given child field name
        @param phChildBuffer The handle of the database buffer with the child record
        @param pcChildFieldName The unqualified field name of the child buffer that references the parent table
        @return The description value of the parent record as returned by the ITableInfoProvider.RecordDescription() method 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetParentDescriptionForField (phChildBuffer AS HANDLE, 
	                                                      pcChildFieldName AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Returns the description value for the default parent record of the 
                 given child buffer
        Notes:   
        @param phChildBuffer The handle of the database buffer with the child record
        @return The description value of the parent record as returned by the ITableInfoProvider.RecordDescription() method 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetParentDescriptionOf (phChildBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of a query (prepared, opened and located at the 
                 first record) of all parent records of the given child record
        Notes:   
        @param phChildBuffer The handle of the database buffer with the child record
        @param pcChildFieldName The unqualified field name of the child buffer that references the parent table
        @param phParentBuffer The handle of the database buffer for the parent record, this buffer will be navigated by the returned query
        @return The handle of a database query of the parent records
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordQueryForField (phChildBuffer AS HANDLE,
                                                       pcChildFieldName AS CHARACTER,
                                                       phParentBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of a query (prepared, opened and located at the 
                 first record) of all parent records of the given child record
        Notes:   
        @param phChildBuffer The handle of the database buffer with the child record
        @param phParentBuffer The handle of the database buffer for the parent record, this buffer will be navigated by the returned query
        @return The handle of a database query of the parent records
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetParentRecordQueryOf (phChildBuffer AS HANDLE,
                                                 phParentBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Processes the delete of a parent buffer 
        Notes:   On delete restrict, on delete cascase
                 Optionally deletes the parent buffer as well
        @param phBuffer The handle of the database buffer (parent) to process delete for 
        @param plDeleteParent Logical parameter, indicating of the parent buffer should actually be deleted
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ProcessDelete (phBuffer AS HANDLE,
	                                  plDeleteParent AS LOGICAL).

    /*------------------------------------------------------------------------------
        Purpose: Processes the write of a child buffer 
        Notes:   
        @param phBuffer The handle of the database buffer (parent) to process the write for 
        @param plNewRecord Logical parameter, indicating if the record has been created or updated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ProcessWrite (phBuffer AS HANDLE,
                                     plNewRecord AS LOGICAL).

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Cache used by the service  
        Notes:   When no cache is used by the service, this method should not throw an error
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RefreshCache ().

END INTERFACE.