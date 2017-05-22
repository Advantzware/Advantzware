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
    File        : eSmartCustomization.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 09.03.2016 10:56:18
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartCustomization{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartCustomizationBefore{&SUFFIX} &ENDIF
    FIELD CustomizationGuid AS CHARACTER FORMAT "x(36)":U LABEL "CustomizationGuid":T
    FIELD CustomizationTypeGuid AS CHARACTER FORMAT "x(36)":U
    FIELD CustomizationResultGuid AS CHARACTER FORMAT "x(36)":U LABEL "Customization Result":T
    FIELD ReferenceValue AS CHARACTER FORMAT "x(80)":U LABEL "Reference Value":T
    FIELD CustomizationTypeCode AS CHARACTER FORMAT "x(80)":U LABEL "Customization Type":T
    FIELD CustomizationResultCode AS CHARACTER FORMAT "x(80)":U LABEL "Result Code":T

    INDEX CustomizationGuid AS UNIQUE PRIMARY CustomizationGuid ASCENDING
    INDEX CustomizationResultGuid CustomizationResultGuid ASCENDING
    INDEX CustomizationTypeGuid CustomizationTypeGuid ASCENDING
    INDEX ReferenceValue ReferenceValue ASCENDING

    .

    