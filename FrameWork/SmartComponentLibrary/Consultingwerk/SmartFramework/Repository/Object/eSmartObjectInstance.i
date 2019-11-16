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
    File        : eSmartObjectInstance.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 27.12.2016 14:40:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="ObjectInstanceGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartObjectInstance{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartObjectInstanceBefore{&SUFFIX} &ENDIF
    FIELD ObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectInstanceGuid":T
    FIELD ContainerObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T
    FIELD ObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T
    FIELD ObjectMasterName AS CHARACTER FORMAT "x(80)":U LABEL "Object Name":T
    FIELD ParentInstanceGuid AS CHARACTER FORMAT "x(36)":U INITIAL ? LABEL "Parent Instance":T
    FIELD LayoutPosition AS CHARACTER FORMAT "x(8)":U LABEL "Layout Position":T
    FIELD InstanceName AS CHARACTER FORMAT "x(80)":U LABEL "Instance Name":T
    FIELD InstanceDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD PageGuid AS CHARACTER FORMAT "x(36)":U LABEL "Page":T
    FIELD ObjectSequence AS INTEGER FORMAT "->,>>9":U INITIAL "0":U LABEL "Object Sequence":T

    INDEX ContainerInstanceName AS UNIQUE ContainerObjectMasterGuid ASCENDING InstanceName ASCENDING
    INDEX ContainerPageSequence ContainerObjectMasterGuid ASCENDING PageGuid ASCENDING ObjectSequence ASCENDING
    INDEX ContainerParentSequence ContainerObjectMasterGuid ASCENDING ParentInstanceGuid ASCENDING ObjectSequence ASCENDING
    INDEX ObjectInstanceGuid AS UNIQUE PRIMARY ObjectInstanceGuid ASCENDING
    INDEX ObjectMasterGuid ObjectMasterGuid ASCENDING
    INDEX PageGuid PageGuid ASCENDING ObjectSequence ASCENDING

    .

    