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
    Created     : 02.01.2017 11:08:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="ObjectMasterGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartObjectMaster{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartObjectMasterBefore{&SUFFIX} &ENDIF
    FIELD ObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T XML-NODE-TYPE "attribute":U
    FIELD ObjectName AS CHARACTER FORMAT "x(80)":U LABEL "Object Name":T XML-NODE-TYPE "attribute":U
    FIELD CustomizationResultGuid AS CHARACTER FORMAT "x(32)":U LABEL "Customization Result":T XML-NODE-TYPE "attribute":U
    FIELD ObjectTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T XML-NODE-TYPE "attribute":U
    FIELD ModuleGuid AS CHARACTER FORMAT "x(36)":U LABEL "ModuleGuid":T XML-NODE-TYPE "attribute":U
    FIELD ObjectDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T XML-NODE-TYPE "attribute":U
    FIELD ObjectPackage AS CHARACTER FORMAT "x(80)":U LABEL "Object Package":T XML-NODE-TYPE "attribute":U
    FIELD ObjectExtension AS CHARACTER FORMAT "x(8)":U LABEL "Extension":T XML-NODE-TYPE "attribute":U
    FIELD StaticObject AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Static Object":T XML-NODE-TYPE "attribute":U
    FIELD TemplateObject AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Template Object":T XML-NODE-TYPE "attribute":U
    FIELD DeploymentType AS CHARACTER FORMAT "x(8)":U LABEL "Deployment Type":T XML-NODE-TYPE "attribute":U
    FIELD RunnableFromMenu AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Runnable from Menu":T XML-NODE-TYPE "attribute":U
    FIELD Disabled AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "Disabled":T XML-NODE-TYPE "attribute":U
    FIELD DesignTimeDataSourceGuid AS CHARACTER FORMAT "x(36)":U LABEL "Design Time Data Source":T XML-NODE-TYPE "attribute":U

    INDEX ObjectMasterGuid AS UNIQUE PRIMARY ObjectMasterGuid ASCENDING

    .

