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
    File        : eSmartLink.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 02.01.2017 11:08:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="LinkGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartLink{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartLinkBefore{&SUFFIX} &ENDIF
    FIELD LinkGuid AS CHARACTER FORMAT "x(36)":U LABEL "LinkGuid":T XML-NODE-TYPE "attribute":U
    FIELD ContainerObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T XML-NODE-TYPE "attribute":U
    FIELD LinkTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "LinkTypeGuid":T XML-NODE-TYPE "attribute":U
    FIELD LinkName AS CHARACTER FORMAT "x(80)":U LABEL "Link Name":T XML-NODE-TYPE "attribute":U
    FIELD SourceObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "SourceObjectInstanceGuid":T XML-NODE-TYPE "attribute":U
    FIELD TargetObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "TargetObjectInstanceGuid":T XML-NODE-TYPE "attribute":U

    INDEX LinkGuid AS UNIQUE PRIMARY LinkGuid ASCENDING

    .

