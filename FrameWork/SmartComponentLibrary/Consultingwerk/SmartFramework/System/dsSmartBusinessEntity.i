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
    File        : dsSmartBusinessEntity.i
    Purpose     : Business Entity for SmartBusinessEntity

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.09.2015 14:10:59
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsSmartBusinessEntity

{ Consultingwerk/SmartFramework/System/eSmartBusinessEntity.i }
{ Consultingwerk/SmartFramework/System/eSmartBusinessEntityTable.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsSmartBusinessEntity{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartBusinessEntity{&SUFFIX}, eSmartBusinessEntityTable{&SUFFIX} 
    DATA-RELATION eSmartBusinessEntityeSmartBusine FOR eSmartBusinessEntity{&SUFFIX}, eSmartBusinessEntityTable{&SUFFIX} 
        RELATION-FIELDS (BusinessEntityGuid,BusinessEntityGuid)

    .    
