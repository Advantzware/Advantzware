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
    Created     : 27.12.2016 14:40:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="LinkGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartLink{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartLinkBefore{&SUFFIX} &ENDIF
    FIELD LinkGuid AS CHARACTER FORMAT "x(36)":U LABEL "LinkGuid":T
    FIELD ContainerObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T
    FIELD LinkTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "LinkTypeGuid":T
    FIELD LinkName AS CHARACTER FORMAT "x(80)":U LABEL "Link Name":T
    FIELD SourceObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "SourceObjectInstanceGuid":T
    FIELD SourceObjectName AS CHARACTER FORMAT "x(80)":U LABEL "Source Object":T
    FIELD SourcePageNumber AS INTEGER FORMAT ">>9":U LABEL "Source Page":T
    FIELD TargetObjectInstanceGuid AS CHARACTER FORMAT "x(36)":U LABEL "TargetObjectInstanceGuid":T
    FIELD TargetObjectName AS CHARACTER FORMAT "x(80)":U LABEL "Target Object":T
    FIELD TargetPageNumber AS INTEGER FORMAT ">>9":U LABEL "Target Page":T

    INDEX ContainerSourceLinkTarget AS UNIQUE ContainerObjectMasterGuid ASCENDING SourceObjectInstanceGuid ASCENDING LinkName ASCENDING TargetObjectInstanceGuid ASCENDING
    INDEX LinkContainer LinkName ASCENDING ContainerObjectMasterGuid ASCENDING
    INDEX LinkGuid AS UNIQUE PRIMARY LinkGuid ASCENDING
    INDEX LinkTypeContainer LinkTypeGuid ASCENDING ContainerObjectMasterGuid ASCENDING
    INDEX SourceContainer SourceObjectInstanceGuid ASCENDING ContainerObjectMasterGuid ASCENDING
    INDEX TargetContainer TargetObjectInstanceGuid ASCENDING ContainerObjectMasterGuid ASCENDING

    .

