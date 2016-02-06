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
    File        : eSmartObjectType.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 16.10.2015 20:01:03
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ObjectTypeBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartObjectType{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartObjectTypeBefore{&SUFFIX} &ENDIF
    FIELD ObjectTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "ObjectTypeGuid":T
    FIELD ObjectTypeName AS CHARACTER FORMAT "x(80)":U LABEL "Object Type":T
    FIELD ObjectTypeDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD DeploymentType AS CHARACTER FORMAT "x(8)":U LABEL "Deployment Type":T
    FIELD StaticObject AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Static Object":T
    FIELD ExtendsObjectTypeGuid AS CHARACTER FORMAT "x(32)":U LABEL "Extends Object Type":T
    FIELD ExtendsObjectTypeName AS CHARACTER FORMAT "X(40)":U LABEL "Extends Object Type Name":T
    FIELD CustomObjectTypeGuid AS CHARACTER FORMAT "x(32)":U LABEL "Customized by Object Type":T
    FIELD CustomObjectTypeName AS CHARACTER FORMAT "X(40)":U LABEL "Custom Object Type Name":T
    FIELD TechnicalClassName AS CHARACTER FORMAT "x(80)":U LABEL "Technical Class Name":T
    FIELD ClassTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "Class Type":T
    FIELD ClassTypeName AS CHARACTER FORMAT "X(40)":U LABEL "Class Type Name":T
    FIELD ContainerType AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Container":T
    FIELD StoreInstances AS CHARACTER FORMAT "x(8)":U LABEL "Store instances for":T

    INDEX CustomizedByObjectType CustomObjectTypeGuid ASCENDING
    INDEX ExtendsObjectType ExtendsObjectTypeGuid ASCENDING
    INDEX ObjectTypeGuid AS UNIQUE PRIMARY ObjectTypeGuid ASCENDING
    INDEX ObjectTypeName AS UNIQUE ObjectTypeName ASCENDING

    .

    