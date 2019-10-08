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
    File        : ttTablesAndFields.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 28 21:09:26 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTables NO-UNDO 
    FIELD TableName AS CHARACTER 
    INDEX TableName IS UNIQUE TableName .
    
DEFINE TEMP-TABLE ttFields NO-UNDO 
    FIELD TableName   AS CHARACTER
    FIELD ColumnName  AS CHARACTER 
    FIELD ColumnOrder AS INTEGER 
    FIELD DataType    AS CHARACTER 
    FIELD ColumnLabel AS CHARACTER   
    INDEX TableName IS UNIQUE TableName ColumnOrder.

DEFINE DATASET dsSchema 
    FOR ttTables, ttFields
    DATA-RELATION FieldRelation FOR ttTables, ttFields
        RELATION-FIELDS (TableName, TableName) .     
