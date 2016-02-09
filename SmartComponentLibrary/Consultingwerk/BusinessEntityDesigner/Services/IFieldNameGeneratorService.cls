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
    File        : IFieldNameGeneratorService
    Purpose     : Interface describing method(s) required by classes that
                  can be used to generate temp-table field names for the
                  business entity generator 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 17 10:54:55 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.BusinessEntityDesigner.Services.IFieldNameGeneratorService:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/eField.i &REFERENCE-ONLY="REFERENCE-ONLY" }  
  
    /*------------------------------------------------------------------------------
        Purpose:    Generates a temp-table field name                                                                        
        Notes:      Used by the Business Entity Generator when a table is dragged to the
                    design canvas or an existing temp-table (add new source table) or 
                    when refreshing the table 
        @param pcDatabaseTableName The name of the database talbe (source table)
        @param pcDatabaseFieldName The name of the database field (source field)
        @param pcTempTableName The name of the temp-table
        @param eField The temp-table with the fields currently present in the business entity model
        @return The name for the new temp-table field 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateFieldName (pcDatabaseTableName AS CHARACTER,
                                               pcDatabaseFieldName AS CHARACTER,
                                               pcTempTableName     AS CHARACTER,
                                               TABLE eField) . 

END INTERFACE.