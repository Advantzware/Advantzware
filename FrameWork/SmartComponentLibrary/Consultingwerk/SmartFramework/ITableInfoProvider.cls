/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ITableInfoProvider
    Purpose     : Provides Information about database tables
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 15 21:17:30 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartFramework.ITableInfoProvider:  
  
    /*------------------------------------------------------------------------------
        Purpose: Finds a buffer based on the unique key values
        Notes:
        @param phBuffer The buffer with the database record to return the description for
        @param pcUniqueKeyValues The unqiue key field values
        @param plExclusive Logical value indicating if the record should be accessed with exclusive-lock
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FindRecordByKeyValues (phBuffer AS HANDLE, 
                                              pcUniqueKeyValues AS CHARACTER,
                                              plExclusive  AS LOGICAL).

    /*------------------------------------------------------------------------------
        Purpose: Returns the standard description value for a database record                                                                        
        Notes:                                                              
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordDescription (phBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the standard description value for a database record                                                                     
        Notes:                                                         
        @param pcDatabaseName The name of the database 
        @param pcTableName The name of the database table      
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordDescription (pcDatabaseName AS CHARACTER,
                                               pcTableName AS CHARACTER,
                                               phBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the key field values for a database record                                                                     
        Notes:                                                              
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordKeyFields (phBuffer AS HANDLE).

    /*------------------------------------------------------------------------------
        Purpose: Returns the key field values for a database record                                                                     
        Notes:                                                         
        @param pcDatabaseName The name of the database 
        @param pcTableName The name of the database table      
        @param phBuffer The buffer with the database record to return the description for
        @return The standard description value for the record
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RecordKeyFields (pcDatabaseName AS CHARACTER,
                                             pcTableName AS CHARACTER,
                                             phBuffer AS HANDLE).

END INTERFACE.
