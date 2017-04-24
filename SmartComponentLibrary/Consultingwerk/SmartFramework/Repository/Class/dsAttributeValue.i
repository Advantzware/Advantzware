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
    File        : dsAttributeValue.i
    Purpose     : Business Entity for SmartAttributeValue

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 29.04.2016 07:13:35
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsAttributeValue

{ Consultingwerk/SmartFramework/Repository/Class/eSmartAttributeValue.i }
{ Consultingwerk/SmartFramework/Repository/Class/eSmartAttribute.i &NO-BEFORE=YES }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsAttributeValue{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartAttributeValue{&SUFFIX}, eSmartAttribute{&SUFFIX}
    DATA-RELATION eSmartAttributeValueeSmartAttributeRelation FOR eSmartAttributeValue{&SUFFIX}, eSmartAttribute{&SUFFIX}
        RELATION-FIELDS (AttributeLabel,AttributeLabel)

    .
