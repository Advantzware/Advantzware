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
    File        : IDataRelationFieldsService
    Purpose     : Interface describing method(s) required by classes that
                  can be used to return the field names used by new 
                  DATA-RELATION objects in the business entity generator 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 20 22:52:47 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.BusinessEntityDesigner.Services.IDataRelationFieldsService:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }  
  
    /*------------------------------------------------------------------------------
        Purpose: Generates a set of data-relation field names                                                                     
        Notes:   Used by the Business Entity Generator when a new data-relation is 
                 dragged to the design canvas 
        @param pcParentTempTableName The name of the database talbe (source table)
        @param pcChildTempTableName The name of the database field (source field)
        @param dsBusinessEntity Dataset with the current Business Entity model  
        @return The field pairs for the specified data-relation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateDataRelationFields (pcParentTempTableName AS CHARACTER,
                                                        pcChildTempTableName  AS CHARACTER,
                                                        DATASET dsBusinessEntity) . 
  
END INTERFACE.