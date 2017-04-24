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
    File        : eSmartCustomizationResult.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 09.03.2016 10:50:49
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartCustomizationResult{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartCustomizationResultBefore{&SUFFIX} &ENDIF
    FIELD CustomizationResultGuid AS CHARACTER FORMAT "x(36)":U LABEL "Customization Result":T
    FIELD CustomizationResultCode AS CHARACTER FORMAT "x(80)":U LABEL "Result Code":T
    FIELD CustomizationResultDescription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD CustomizationTypeGuid AS CHARACTER FORMAT "x(36)":U
    FIELD CustomizationTypeCode AS CHARACTER FORMAT "x(80)":U LABEL "Customization Type":T

    INDEX CustomizationResultCode AS UNIQUE CustomizationResultCode ASCENDING
    INDEX CustomizationResultDescription CustomizationResultDescription ASCENDING
    INDEX CustomizationResultGuid AS UNIQUE PRIMARY CustomizationResultGuid ASCENDING
    INDEX CustomizationTypeGuid CustomizationTypeGuid ASCENDING

    .

    