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
    Purpose     : Business Entity for ObjectMaster

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.12.2016 15:55:44
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsObjectMaster

{ Consultingwerk/SmartFramework/Repository/Object/Export/eSmartObjectMaster.i }
{ Consultingwerk/SmartFramework/Repository/Object/Export/eSmartAttributeValue.i }
{ Consultingwerk/SmartFramework/Repository/Object/Export/eSmartLink.i }
{ Consultingwerk/SmartFramework/Repository/Object/Export/eSmartModifiedState.i &NO-BEFORE=YES }
{ Consultingwerk/SmartFramework/Repository/Object/Export/eSmartObjectInstance.i }
{ Consultingwerk/SmartFramework/Repository/Object/Export/eSmartPage.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsObjectMaster{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartObjectMaster{&SUFFIX}, eSmartAttributeValue{&SUFFIX}, eSmartLink{&SUFFIX}, eSmartModifiedState{&SUFFIX}, eSmartObjectInstance{&SUFFIX}, eSmartPage{&SUFFIX} 
    DATA-RELATION eSmartObjectMastereSmartAttributeValueRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartAttributeValue{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ContainerObjectMasterGuid)
    DATA-RELATION eSmartObjectMastereSmartLinkRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartLink{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ContainerObjectMasterGuid)
    DATA-RELATION eSmartObjectMastereSmartModifiedStateRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartModifiedState{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ModifiedStateKeyFieldValues)
    DATA-RELATION eSmartObjectMastereSmartObjectInstanceRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartObjectInstance{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ContainerObjectMasterGuid)
    DATA-RELATION eSmartObjectMastereSmartPageRelation FOR eSmartObjectMaster{&SUFFIX}, eSmartPage{&SUFFIX} 
        RELATION-FIELDS (ObjectMasterGuid,ContainerObjectMasterGuid)

    .    
