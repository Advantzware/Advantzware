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
    File        : ITempTableFromTypeFactory
    Purpose     : Interface for classes supporting creating ABL temp-tables
                  from .NET types
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 19 08:50:31 CET 2014
    Notes       : SCL-591
                  http://jira.consultingwerkcloud.com:8090/display/SCL/.NET+Value+object+to+ABL+Temp-Table+mapping
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

{Consultingwerk/products.i}

INTERFACE Consultingwerk.Windows.TempTableObjectMapping.ITempTableFromTypeFactory:  
  
&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
       Purpose: Builds an ABL temp-table from a .NET System.Type
       Notes:
       @param poType The reference to the System.Type to build the temp-table from
       @param pcProperties The comma delimited list of property specs, supported entries are PropertyName or PropertyName|FieldName or PropertyName|FieldName(ABL DataType)
       @param pcUniqueKeyProperties The comma delimited list of property names that build the PUK, must be form the list in pcProperties
       @param pcTempTableName The name for the temp-table to create
       @return The handle of the created temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE CreateTempTable (poType AS System.Type,
                                          pcProperties AS CHARACTER, 
                                          pcUniqueKeyProperties AS CHARACTER,
                                          pcTempTableName AS CHARACTER) .

    /*------------------------------------------------------------------------------
       Purpose: Builds an ABL temp-table from a .NET System.Type
       Notes:   Defaults to using the short type name as the temp-table name
       @param poType The reference to the System.Type to build the temp-table from
       @param pcProperties The comma delimited list of property specs, supported entries are PropertyName or PropertyName|FieldName or PropertyName|FieldName(ABL DataType)
       @param pcUniqueKeyProperties The comma delimited list of property names that build the PUK, must be form the list in pcProperties
       @return The handle of the created temp-table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE CreateTempTable (poType AS System.Type,
                                          pcProperties AS CHARACTER, 
                                          pcUniqueKeyProperties AS CHARACTER) .
&ENDIF
  
END INTERFACE.