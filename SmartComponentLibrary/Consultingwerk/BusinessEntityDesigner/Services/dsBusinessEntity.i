/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    Purpose     : Business Entity for BusinessEntity

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 17.04.2015 15:13:00
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsBusinessEntity

{ Consultingwerk/BusinessEntityDesigner/Services/eBusinessEntity.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eDataRelation.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eDataRelationProperties.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eTable.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eField.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eFieldProperties.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eIndex.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eIndexProperties.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eTableProperties.i }
{ Consultingwerk/BusinessEntityDesigner/Services/eBusinessEntityProperties.i }


@BusinessEntityGenerator (entityname="Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsBusinessEntity{&SUFFIX} {&REFERENCE-ONLY} FOR eBusinessEntity{&SUFFIX}, eDataRelation{&SUFFIX}, eDataRelationProperties{&SUFFIX}, eTable{&SUFFIX}, eField{&SUFFIX}, eFieldProperties{&SUFFIX}, eIndex{&SUFFIX}, eIndexProperties{&SUFFIX}, eTableProperties{&SUFFIX}, eBusinessEntityProperties{&SUFFIX} 
    DATA-RELATION DataRelationPropertiesRelation FOR eDataRelation{&SUFFIX}, eDataRelationProperties{&SUFFIX} 
        RELATION-FIELDS (DataRelationName,DataRelationName)
    DATA-RELATION DataRelationRelation FOR eBusinessEntity{&SUFFIX}, eDataRelation{&SUFFIX} 
        RELATION-FIELDS (BusinessEntityName,BusinessEntityName)
    DATA-RELATION eFieldeFieldPropertiesRelation FOR eField{&SUFFIX}, eFieldProperties{&SUFFIX} 
        RELATION-FIELDS (TempTableName,TempTableName,FieldName,FieldName)
    DATA-RELATION eIndexeIndexPropertiesRelation FOR eIndex{&SUFFIX}, eIndexProperties{&SUFFIX} 
        RELATION-FIELDS (TempTableName,TempTableName,IndexName,IndexName)
    DATA-RELATION FieldRelation FOR eTable{&SUFFIX}, eField{&SUFFIX} 
        RELATION-FIELDS (TempTableName,TempTableName)
    DATA-RELATION IndexRelation FOR eTable{&SUFFIX}, eIndex{&SUFFIX} 
        RELATION-FIELDS (TempTableName,TempTableName)
    DATA-RELATION TablePropertiesRelation FOR eTable{&SUFFIX}, eTableProperties{&SUFFIX} 
        RELATION-FIELDS (TempTableName,TempTableName)
    DATA-RELATION TableRelation FOR eBusinessEntity{&SUFFIX}, eTable{&SUFFIX} 
        RELATION-FIELDS (BusinessEntityName,BusinessEntityName)

    .    
