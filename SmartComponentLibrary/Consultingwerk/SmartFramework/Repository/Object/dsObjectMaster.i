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
    File        : dsObjectMaster.i
    Purpose     : Business Entity for SmartObjectMaster

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 26.05.2016 09:09:12
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsObjectMaster

{ Consultingwerk/SmartFramework/Repository/Object/eSmartObjectMaster.i }
{ Consultingwerk/SmartFramework/Repository/Object/eSmartLink.i }
{ Consultingwerk/SmartFramework/Repository/Object/eSmartObjectInstance.i }
{ Consultingwerk/SmartFramework/Repository/Object/eSmartPage.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.ObjectMasterBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsObjectMaster{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartObjectMaster{&SUFFIX}, eSmartLink{&SUFFIX}, eSmartObjectInstance{&SUFFIX}, eSmartPage{&SUFFIX} 
    DATA-RELATION eSmartObjectMastereSmartLinkRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartLink{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ContainerObjectMasterGuid)
    DATA-RELATION eSmartObjectMastereSmartObjectInstanceRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartObjectInstance{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ContainerObjectMasterGuid)
    DATA-RELATION eSmartObjectMastereSmartPageRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartPage{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ContainerObjectMasterGuid)

    .    
