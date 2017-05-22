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
    File        : eSmartPage.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 02.01.2017 11:08:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="PageGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartPage{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartPageBefore{&SUFFIX} &ENDIF
    FIELD PageGuid AS CHARACTER FORMAT "x(36)":U LABEL "Page":T XML-NODE-TYPE "attribute":U
    FIELD ContainerObjectMasterGuid AS CHARACTER FORMAT "x(36)":U LABEL "ContainerObjectMasterGuid":T XML-NODE-TYPE "attribute":U
    FIELD PageSequence AS INTEGER FORMAT ">>9":U INITIAL "0":U LABEL "Page Number":T XML-NODE-TYPE "attribute":U
    FIELD PageLabel AS CHARACTER FORMAT "x(80)":U LABEL "Page Label":T XML-NODE-TYPE "attribute":U
    FIELD SecurityToken AS CHARACTER FORMAT "x(80)":U LABEL "Security Token":T XML-NODE-TYPE "attribute":U
    FIELD EnableOnCreate AS LOGICAL FORMAT "yes/no":U INITIAL "yes":U LABEL "Enable on create":T XML-NODE-TYPE "attribute":U
    FIELD EnableOnModify AS LOGICAL FORMAT "yes/no":U INITIAL "yes":U LABEL "Enable on modify":T XML-NODE-TYPE "attribute":U
    FIELD EnableOnView AS LOGICAL FORMAT "yes/no":U INITIAL "yes":U LABEL "Enable on view":T XML-NODE-TYPE "attribute":U

    INDEX PageGuid AS UNIQUE PRIMARY PageGuid ASCENDING

    .

