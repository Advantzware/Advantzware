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
    Created     : 02.01.2017 11:08:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="ObjectInstanceGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartObjectInstance{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartObjectInstanceBefore{&SUFFIX} &ENDIF
    FIELD ObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectInstanceGuid":T XML-NODE-TYPE "attribute":U
    FIELD ContainerObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T XML-NODE-TYPE "attribute":U
    FIELD ObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectMasterGuid":T XML-NODE-TYPE "attribute":U
    FIELD ParentInstanceGuid AS CHARACTER FORMAT "x(36)":U INITIAL ? LABEL "Parent Instance":T XML-NODE-TYPE "attribute":U
    FIELD LayoutPosition AS CHARACTER FORMAT "x(8)":U LABEL "Layout Position":T XML-NODE-TYPE "attribute":U
    FIELD InstanceName AS CHARACTER FORMAT "x(80)":U LABEL "Instance Name":T XML-NODE-TYPE "attribute":U
    FIELD InstanceDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T XML-NODE-TYPE "attribute":U
    FIELD PageGuid AS CHARACTER FORMAT "x(36)":U LABEL "Page":T XML-NODE-TYPE "attribute":U
    FIELD ObjectSequence AS INTEGER FORMAT "->,>>9":U INITIAL "0":U LABEL "ObjectSequence":T XML-NODE-TYPE "attribute":U

    INDEX ObjectInstanceGuid AS UNIQUE PRIMARY ObjectInstanceGuid ASCENDING

    .

