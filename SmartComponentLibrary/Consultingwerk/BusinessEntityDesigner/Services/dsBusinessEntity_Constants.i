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
    File        : dsBusinessEntity_Constants.i
    Purpose     : Dataset Constants for dataset dsBusinessEntity

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 23.12.2014 21:48:11
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE eBusinessEntity "eBusinessEntity"
&GLOBAL-DEFINE eBusinessEntity.BusinessEntityName "eBusinessEntity.BusinessEntityName"
&GLOBAL-DEFINE eBusinessEntity.BusinessEntityPurpose "eBusinessEntity.BusinessEntityPurpose"
&GLOBAL-DEFINE eBusinessEntity.BusinessEntityDescription "eBusinessEntity.BusinessEntityDescription"
&GLOBAL-DEFINE eBusinessEntity.BusinessEntityPackage "eBusinessEntity.BusinessEntityPackage"
&GLOBAL-DEFINE eBusinessEntity.DataAccessName "eBusinessEntity.DataAccessName"
&GLOBAL-DEFINE eBusinessEntity.DataAccessPackage "eBusinessEntity.DataAccessPackage"
&GLOBAL-DEFINE eBusinessEntity.DatasetControllerName "eBusinessEntity.DatasetControllerName"
&GLOBAL-DEFINE eBusinessEntity.DatasetControllerPackage "eBusinessEntity.DatasetControllerPackage"
&GLOBAL-DEFINE eBusinessEntity.DatasetName "eBusinessEntity.DatasetName"
&GLOBAL-DEFINE eBusinessEntity.DatasetPath "eBusinessEntity.DatasetPath"
&GLOBAL-DEFINE eBusinessEntity.DefaultTablePath "eBusinessEntity.DefaultTablePath"
&GLOBAL-DEFINE eBusinessEntity.DefaultTablePrefix "eBusinessEntity.DefaultTablePrefix"
&GLOBAL-DEFINE eBusinessEntity.DefaultTableSuffix "eBusinessEntity.DefaultTableSuffix"
&GLOBAL-DEFINE eBusinessEntity.DefaultBeforeTablePrefix "eBusinessEntity.DefaultBeforeTablePrefix"
&GLOBAL-DEFINE eBusinessEntity.DefaultBeforeTableSuffix "eBusinessEntity.DefaultBeforeTableSuffix"
&GLOBAL-DEFINE eBusinessEntity.GenerateBusinessEntity "eBusinessEntity.GenerateBusinessEntity"
&GLOBAL-DEFINE eBusinessEntity.GenerateDataAccess "eBusinessEntity.GenerateDataAccess"
&GLOBAL-DEFINE eBusinessEntity.GenerateDatasetController "eBusinessEntity.GenerateDatasetController"
&GLOBAL-DEFINE eBusinessEntity.DistinctTempTableIncludeFiles "eBusinessEntity.DistinctTempTableIncludeFiles"

&GLOBAL-DEFINE eBusinessEntityProperties "eBusinessEntityProperties"
&GLOBAL-DEFINE eBusinessEntityProperties.PropertyName "eBusinessEntityProperties.PropertyName"
&GLOBAL-DEFINE eBusinessEntityProperties.PropertyValue "eBusinessEntityProperties.PropertyValue"

&GLOBAL-DEFINE eDataRelation "eDataRelation"
&GLOBAL-DEFINE eDataRelation.BusinessEntityName "eDataRelation.BusinessEntityName"
&GLOBAL-DEFINE eDataRelation.DataRelationName "eDataRelation.DataRelationName"
&GLOBAL-DEFINE eDataRelation.ParentTempTableName "eDataRelation.ParentTempTableName"
&GLOBAL-DEFINE eDataRelation.ChildTempTableName "eDataRelation.ChildTempTableName"
&GLOBAL-DEFINE eDataRelation.RelationFields "eDataRelation.RelationFields"
&GLOBAL-DEFINE eDataRelation.DataRelationDescription "eDataRelation.DataRelationDescription"
&GLOBAL-DEFINE eDataRelation.DataRelationReposition "eDataRelation.DataRelationReposition"
&GLOBAL-DEFINE eDataRelation.DataRelationNested "eDataRelation.DataRelationNested"
&GLOBAL-DEFINE eDataRelation.DataRelationForeignKeyHidden "eDataRelation.DataRelationForeignKeyHidden"
&GLOBAL-DEFINE eDataRelation.DataRelationNotActive "eDataRelation.DataRelationNotActive"
&GLOBAL-DEFINE eDataRelation.DataRelationRecursive "eDataRelation.DataRelationRecursive"
&GLOBAL-DEFINE eDataRelation.ChildCardinality "eDataRelation.ChildCardinality"
&GLOBAL-DEFINE eDataRelation.ChildPropertyName "eDataRelation.ChildPropertyName"

&GLOBAL-DEFINE eDataRelationProperties "eDataRelationProperties"
&GLOBAL-DEFINE eDataRelationProperties.DataRelationName "eDataRelationProperties.DataRelationName"
&GLOBAL-DEFINE eDataRelationProperties.PropertyName "eDataRelationProperties.PropertyName"
&GLOBAL-DEFINE eDataRelationProperties.PropertyValue "eDataRelationProperties.PropertyValue"

&GLOBAL-DEFINE eField "eField"
&GLOBAL-DEFINE eField.BusinessEntityName "eField.BusinessEntityName"
&GLOBAL-DEFINE eField.TempTableName "eField.TempTableName"
&GLOBAL-DEFINE eField.FieldName "eField.FieldName"
&GLOBAL-DEFINE eField.FieldOrder "eField.FieldOrder"
&GLOBAL-DEFINE eField.FieldDataType "eField.FieldDataType"
&GLOBAL-DEFINE eField.FieldDescription "eField.FieldDescription"
&GLOBAL-DEFINE eField.FieldInitial "eField.FieldInitial"
&GLOBAL-DEFINE eField.FieldInitialUnknownValue "eField.FieldInitialUnknownValue"
&GLOBAL-DEFINE eField.FieldLabel "eField.FieldLabel"
&GLOBAL-DEFINE eField.FieldFormat "eField.FieldFormat"
&GLOBAL-DEFINE eField.FieldSerializeHidden "eField.FieldSerializeHidden"
&GLOBAL-DEFINE eField.FieldSerializeName "eField.FieldSerializeName"
&GLOBAL-DEFINE eField.FieldXmlDataType "eField.FieldXmlDataType"
&GLOBAL-DEFINE eField.FieldXmlNodeType "eField.FieldXmlNodeType"
&GLOBAL-DEFINE eField.FieldXmlNodeName "eField.FieldXmlNodeName"
&GLOBAL-DEFINE eField.FieldSource "eField.FieldSource"
&GLOBAL-DEFINE eField.FieldExtent "eField.FieldExtent"
&GLOBAL-DEFINE eField.FieldGetAccess "eField.FieldGetAccess"
&GLOBAL-DEFINE eField.FieldSetAccess "eField.FieldSetAccess"
&GLOBAL-DEFINE eField.FieldCalcExpression "eField.FieldCalcExpression"
&GLOBAL-DEFINE eField.FieldCaseSensitive "eField.FieldCaseSensitive"
&GLOBAL-DEFINE eField.FieldCustomControlName "eField.FieldCustomControlName"

&GLOBAL-DEFINE eFieldProperties "eFieldProperties"
&GLOBAL-DEFINE eFieldProperties.TempTableName "eFieldProperties.TempTableName"
&GLOBAL-DEFINE eFieldProperties.FieldName "eFieldProperties.FieldName"
&GLOBAL-DEFINE eFieldProperties.PropertyName "eFieldProperties.PropertyName"
&GLOBAL-DEFINE eFieldProperties.PropertyValue "eFieldProperties.PropertyValue"

&GLOBAL-DEFINE eIndex "eIndex"
&GLOBAL-DEFINE eIndex.BusinessEntityName "eIndex.BusinessEntityName"
&GLOBAL-DEFINE eIndex.TempTableName "eIndex.TempTableName"
&GLOBAL-DEFINE eIndex.IndexOrder "eIndex.IndexOrder"
&GLOBAL-DEFINE eIndex.IndexName "eIndex.IndexName"
&GLOBAL-DEFINE eIndex.IndexUnique "eIndex.IndexUnique"
&GLOBAL-DEFINE eIndex.IndexPrimary "eIndex.IndexPrimary"
&GLOBAL-DEFINE eIndex.IndexWordIndex "eIndex.IndexWordIndex"
&GLOBAL-DEFINE eIndex.FieldNames "eIndex.FieldNames"
&GLOBAL-DEFINE eIndex.FieldSort "eIndex.FieldSort"
&GLOBAL-DEFINE eIndex.DefaultSearchCodeGeneration "eIndex.DefaultSearchCodeGeneration"

&GLOBAL-DEFINE eIndexProperties "eIndexProperties"
&GLOBAL-DEFINE eIndexProperties.TempTableName "eIndexProperties.TempTableName"
&GLOBAL-DEFINE eIndexProperties.IndexName "eIndexProperties.IndexName"
&GLOBAL-DEFINE eIndexProperties.PropertyName "eIndexProperties.PropertyName"
&GLOBAL-DEFINE eIndexProperties.PropertyValue "eIndexProperties.PropertyValue"

&GLOBAL-DEFINE eTable "eTable"
&GLOBAL-DEFINE eTable.BusinessEntityName "eTable.BusinessEntityName"
&GLOBAL-DEFINE eTable.TempTableName "eTable.TempTableName"
&GLOBAL-DEFINE eTable.SourceTableNames "eTable.SourceTableNames"
&GLOBAL-DEFINE eTable.SourceBufferNames "eTable.SourceBufferNames"
&GLOBAL-DEFINE eTable.SourceDefaultQuery "eTable.SourceDefaultQuery"
&GLOBAL-DEFINE eTable.TempTablePurpose "eTable.TempTablePurpose"
&GLOBAL-DEFINE eTable.TempTableDescription "eTable.TempTableDescription"
&GLOBAL-DEFINE eTable.TempTablePath "eTable.TempTablePath"
&GLOBAL-DEFINE eTable.TempTableBeforeName "eTable.TempTableBeforeName"
&GLOBAL-DEFINE eTable.LayoutColumn "eTable.LayoutColumn"
&GLOBAL-DEFINE eTable.LayoutRow "eTable.LayoutRow"
&GLOBAL-DEFINE eTable.LayoutWidth "eTable.LayoutWidth"
&GLOBAL-DEFINE eTable.LayoutHeight "eTable.LayoutHeight"
&GLOBAL-DEFINE eTable.NamespaceUri "eTable.NamespaceUri"
&GLOBAL-DEFINE eTable.NamespacePrefix "eTable.NamespacePrefix"
&GLOBAL-DEFINE eTable.XmlNodeName "eTable.XmlNodeName"
&GLOBAL-DEFINE eTable.SerializeName "eTable.SerializeName"
&GLOBAL-DEFINE eTable.NoBeforeTable "eTable.NoBeforeTable"
&GLOBAL-DEFINE eTable.EntityPackageName "eTable.EntityPackageName"
&GLOBAL-DEFINE eTable.EntityClassName "eTable.EntityClassName"
&GLOBAL-DEFINE eTable.DeletedFields "eTable.DeletedFields"

&GLOBAL-DEFINE eTableProperties "eTableProperties"
&GLOBAL-DEFINE eTableProperties.TempTableName "eTableProperties.TempTableName"
&GLOBAL-DEFINE eTableProperties.PropertyName "eTableProperties.PropertyName"
&GLOBAL-DEFINE eTableProperties.PropertyValue "eTableProperties.PropertyValue"

