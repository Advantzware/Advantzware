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
    File        : ITableNameGeneratorService
    Purpose     : Interface for classes that support the TableNameGeneratorService
                  setting in the Business Entity Designer options. Classes 
                  implementing this Interface can be used to translate 
                  database table names into temp-table names (e.g. 
                  Replacing - or _ with a camel-case syntax.
                  The Temp-Table name prefix and suffix settings will still 
                  be applied to the temp-table names.
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 25 09:18:56 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .  
USING Progress.Lang.*                                  FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Services.ITableNameGeneratorService:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }  
  
    /*------------------------------------------------------------------------------
        Purpose: Generates a name for a temp-table
        Notes:   
        @param pcDatabaseName The name of the database table
        @param pcTableName The name of the database table
        @param dsBusinessEntity The Business Entity Designer Dataset
        @return The name for the new temp-table
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GenerateTempTableName (pcDatabaseName AS CHARACTER,
	                                               pcTableName AS CHARACTER, 
	                                               DATASET dsBusinessEntity).

END INTERFACE.
