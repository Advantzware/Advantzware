/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : eSmartObjectMaster.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 30.12.2016 01:26:36
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="ObjectMasterGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartObjectMaster{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartObjectMasterBefore{&SUFFIX} &ENDIF
    FIELD ObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T
    FIELD ObjectName AS CHARACTER FORMAT "x(80)":U LABEL "Object Name":T
    FIELD CustomizationResultGuid AS CHARACTER FORMAT "x(32)":U LABEL "Customization Result":T
    FIELD CustomizationResultCode AS CHARACTER FORMAT "X(20)":U LABEL "Customization Result Code":T
    FIELD ObjectTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T
    FIELD ObjectTypeName AS CHARACTER FORMAT "X(20)":U LABEL "Object Type Name":T
    FIELD ModuleGuid AS CHARACTER FORMAT "x(36)":U LABEL "ModuleGuid":T
    FIELD ModuleName AS CHARACTER FORMAT "X(20)":U LABEL "Module Name":T
    FIELD ObjectDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD ObjectPackage AS CHARACTER FORMAT "x(80)":U LABEL "Object Package":T
    FIELD ObjectExtension AS CHARACTER FORMAT "x(8)":U LABEL "Extension":T
    FIELD StaticObject AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Static Object":T
    FIELD TemplateObject AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Template Object":T
    FIELD DeploymentType AS CHARACTER FORMAT "x(8)":U LABEL "Deployment Type":T
    FIELD RunnableFromMenu AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Runnable from Menu":T
    FIELD Disabled AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Disabled":T
    FIELD DesignTimeDataSourceGuid AS CHARACTER FORMAT "x(36)":U LABEL "Design Time Data Source":T
    FIELD DesignTimeDataSourceName AS CHARACTER FORMAT "X(20)":U LABEL "Design Time Data Source Name":T
    FIELD ContainerType AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Container":T

    INDEX CustomizationResultGuid CustomizationResultGuid ASCENDING
    INDEX ObjectDescription ObjectDescription ASCENDING
    INDEX ObjectMasterGuid AS UNIQUE PRIMARY ObjectMasterGuid ASCENDING
    INDEX ObjectNameCustomization ObjectName ASCENDING CustomizationResultGuid ASCENDING
    INDEX ObjectTypeGuid ObjectTypeGuid ASCENDING
    INDEX PackageObjectFileNameCustomizati AS UNIQUE ObjectPackage ASCENDING ObjectName ASCENDING CustomizationResultGuid ASCENDING
    INDEX RunnableFromMenu RunnableFromMenu ASCENDING
    INDEX TemplateObject TemplateObject ASCENDING

    .

    