/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dsObjectMaster_Constants.i
    Purpose     : Dataset Constants for dataset dsObjectMaster

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 31.12.2016 15:55:45
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE eSmartAttributeValue "eSmartAttributeValue"
&GLOBAL-DEFINE eSmartAttributeValue.AttributeValueGuid "eSmartAttributeValue.AttributeValueGuid"
&GLOBAL-DEFINE eSmartAttributeValue.ObjectTypeGuid "eSmartAttributeValue.ObjectTypeGuid"
&GLOBAL-DEFINE eSmartAttributeValue.ObjectMasterGuid "eSmartAttributeValue.ObjectMasterGuid"
&GLOBAL-DEFINE eSmartAttributeValue.ContainerObjectMasterGuid "eSmartAttributeValue.ContainerObjectMasterGuid"
&GLOBAL-DEFINE eSmartAttributeValue.ObjectInstanceGuid "eSmartAttributeValue.ObjectInstanceGuid"
&GLOBAL-DEFINE eSmartAttributeValue.ConstantValue "eSmartAttributeValue.ConstantValue"
&GLOBAL-DEFINE eSmartAttributeValue.AttributeLabel "eSmartAttributeValue.AttributeLabel"
&GLOBAL-DEFINE eSmartAttributeValue.CharacterValue "eSmartAttributeValue.CharacterValue"
&GLOBAL-DEFINE eSmartAttributeValue.IntegerValue "eSmartAttributeValue.IntegerValue"
&GLOBAL-DEFINE eSmartAttributeValue.Int64Value "eSmartAttributeValue.Int64Value"
&GLOBAL-DEFINE eSmartAttributeValue.DateValue "eSmartAttributeValue.DateValue"
&GLOBAL-DEFINE eSmartAttributeValue.DateTimeValue "eSmartAttributeValue.DateTimeValue"
&GLOBAL-DEFINE eSmartAttributeValue.DateTimeTzValue "eSmartAttributeValue.DateTimeTzValue"
&GLOBAL-DEFINE eSmartAttributeValue.DecimalValue "eSmartAttributeValue.DecimalValue"
&GLOBAL-DEFINE eSmartAttributeValue.LogicalValue "eSmartAttributeValue.LogicalValue"
&GLOBAL-DEFINE eSmartAttributeValue.RawValue "eSmartAttributeValue.RawValue"
&GLOBAL-DEFINE eSmartAttributeValue.AppliesAtRuntime "eSmartAttributeValue.AppliesAtRuntime"

&GLOBAL-DEFINE eSmartLink "eSmartLink"
&GLOBAL-DEFINE eSmartLink.LinkGuid "eSmartLink.LinkGuid"
&GLOBAL-DEFINE eSmartLink.ContainerObjectMasterGuid "eSmartLink.ContainerObjectMasterGuid"
&GLOBAL-DEFINE eSmartLink.LinkTypeGuid "eSmartLink.LinkTypeGuid"
&GLOBAL-DEFINE eSmartLink.LinkName "eSmartLink.LinkName"
&GLOBAL-DEFINE eSmartLink.SourceObjectInstanceGuid "eSmartLink.SourceObjectInstanceGuid"
&GLOBAL-DEFINE eSmartLink.TargetObjectInstanceGuid "eSmartLink.TargetObjectInstanceGuid"

&GLOBAL-DEFINE eSmartModifiedState "eSmartModifiedState"
&GLOBAL-DEFINE eSmartModifiedState.ModifiedStateGuid "eSmartModifiedState.ModifiedStateGuid"
&GLOBAL-DEFINE eSmartModifiedState.ModifiedStateTable "eSmartModifiedState.ModifiedStateTable"
&GLOBAL-DEFINE eSmartModifiedState.ModifiedStateKeyFieldValues "eSmartModifiedState.ModifiedStateKeyFieldValues"
&GLOBAL-DEFINE eSmartModifiedState.ModifiedStateTimeStamp "eSmartModifiedState.ModifiedStateTimeStamp"
&GLOBAL-DEFINE eSmartModifiedState.ModifiedStateUser "eSmartModifiedState.ModifiedStateUser"

&GLOBAL-DEFINE eSmartObjectInstance "eSmartObjectInstance"
&GLOBAL-DEFINE eSmartObjectInstance.ObjectInstanceGuid "eSmartObjectInstance.ObjectInstanceGuid"
&GLOBAL-DEFINE eSmartObjectInstance.ContainerObjectMasterGuid "eSmartObjectInstance.ContainerObjectMasterGuid"
&GLOBAL-DEFINE eSmartObjectInstance.ObjectMasterGuid "eSmartObjectInstance.ObjectMasterGuid"
&GLOBAL-DEFINE eSmartObjectInstance.ParentInstanceGuid "eSmartObjectInstance.ParentInstanceGuid"
&GLOBAL-DEFINE eSmartObjectInstance.LayoutPosition "eSmartObjectInstance.LayoutPosition"
&GLOBAL-DEFINE eSmartObjectInstance.InstanceName "eSmartObjectInstance.InstanceName"
&GLOBAL-DEFINE eSmartObjectInstance.InstanceDescription "eSmartObjectInstance.InstanceDescription"
&GLOBAL-DEFINE eSmartObjectInstance.PageGuid "eSmartObjectInstance.PageGuid"
&GLOBAL-DEFINE eSmartObjectInstance.ObjectSequence "eSmartObjectInstance.ObjectSequence"

&GLOBAL-DEFINE eSmartObjectMaster "eSmartObjectMaster"
&GLOBAL-DEFINE eSmartObjectMaster.ObjectMasterGuid "eSmartObjectMaster.ObjectMasterGuid"
&GLOBAL-DEFINE eSmartObjectMaster.ObjectName "eSmartObjectMaster.ObjectName"
&GLOBAL-DEFINE eSmartObjectMaster.CustomizationResultGuid "eSmartObjectMaster.CustomizationResultGuid"
&GLOBAL-DEFINE eSmartObjectMaster.ObjectTypeGuid "eSmartObjectMaster.ObjectTypeGuid"
&GLOBAL-DEFINE eSmartObjectMaster.ModuleGuid "eSmartObjectMaster.ModuleGuid"
&GLOBAL-DEFINE eSmartObjectMaster.ObjectDescription "eSmartObjectMaster.ObjectDescription"
&GLOBAL-DEFINE eSmartObjectMaster.ObjectPackage "eSmartObjectMaster.ObjectPackage"
&GLOBAL-DEFINE eSmartObjectMaster.ObjectExtension "eSmartObjectMaster.ObjectExtension"
&GLOBAL-DEFINE eSmartObjectMaster.StaticObject "eSmartObjectMaster.StaticObject"
&GLOBAL-DEFINE eSmartObjectMaster.TemplateObject "eSmartObjectMaster.TemplateObject"
&GLOBAL-DEFINE eSmartObjectMaster.DeploymentType "eSmartObjectMaster.DeploymentType"
&GLOBAL-DEFINE eSmartObjectMaster.RunnableFromMenu "eSmartObjectMaster.RunnableFromMenu"
&GLOBAL-DEFINE eSmartObjectMaster.Disabled "eSmartObjectMaster.Disabled"
&GLOBAL-DEFINE eSmartObjectMaster.DesignTimeDataSourceGuid "eSmartObjectMaster.DesignTimeDataSourceGuid"

&GLOBAL-DEFINE eSmartPage "eSmartPage"
&GLOBAL-DEFINE eSmartPage.PageGuid "eSmartPage.PageGuid"
&GLOBAL-DEFINE eSmartPage.ContainerObjectMasterGuid "eSmartPage.ContainerObjectMasterGuid"
&GLOBAL-DEFINE eSmartPage.PageSequence "eSmartPage.PageSequence"
&GLOBAL-DEFINE eSmartPage.PageLabel "eSmartPage.PageLabel"
&GLOBAL-DEFINE eSmartPage.SecurityToken "eSmartPage.SecurityToken"
&GLOBAL-DEFINE eSmartPage.EnableOnCreate "eSmartPage.EnableOnCreate"
&GLOBAL-DEFINE eSmartPage.EnableOnModify "eSmartPage.EnableOnModify"
&GLOBAL-DEFINE eSmartPage.EnableOnView "eSmartPage.EnableOnView"

