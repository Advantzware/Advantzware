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
    File        : dsBusinessEntity.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 09:32:17 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

{ Consultingwerk/BusinessEntityDesigner/Services/eDatabase.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eTable.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eField.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eIndex.i }

DEFINE {&ACCESS} DATASET dsDatabaseSchema {&REFERENCE-ONLY} FOR eDatabase, eTable, eField, eIndex 
    DATA-RELATION TableRelation FOR eDatabase, eTable
        RELATION-FIELDS (DatabaseName, BusinessEntityName) 
    DATA-RELATION FieldRelation FOR eTable, eField
        RELATION-FIELDS (BusinessEntityName, BusinessEntityName, TempTableName, TempTableName)  
    DATA-RELATION IndexRelation FOR eTable, eIndex
        RELATION-FIELDS (BusinessEntityName, BusinessEntityName, TempTableName, TempTableName) . 
    
