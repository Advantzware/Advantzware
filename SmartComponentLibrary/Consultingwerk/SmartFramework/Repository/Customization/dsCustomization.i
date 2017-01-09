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
    File        : dsCustomization.i
    Purpose     : Business Entity for Customization

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 09.03.2016 08:06:24
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsCustomization

{ Consultingwerk/SmartFramework/Repository/Customization/eSmartCustomizationResult.i }
{ Consultingwerk/SmartFramework/Repository/Customization/eSmartCustomization.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Customization.CustomizationBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsCustomization{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartCustomizationResult{&SUFFIX}, eSmartCustomization{&SUFFIX} 
    DATA-RELATION eSmartCustomizationResulteSmartCustomizationRelation FOR eSmartCustomizationResult{&SUFFIX}, eSmartCustomization{&SUFFIX} 
        RELATION-FIELDS (CustomizationResultGuid,CustomizationResultGuid)

    .    
