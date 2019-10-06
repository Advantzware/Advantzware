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
    File        : eSmartMenu.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15.12.2015 19:13:14
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.MenuBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartMenu{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartMenuBefore{&SUFFIX} &ENDIF
    FIELD MenuGuid AS CHARACTER FORMAT "x(36)":U LABEL "MenuGuid":T
    FIELD ParentMenuGuid AS CHARACTER FORMAT "x(36)":U LABEL "ParentMenuGuid":T
    FIELD MenuName AS CHARACTER FORMAT "x(20)":U LABEL "MenuName":T
    FIELD MenuStructureType AS CHARACTER FORMAT "x(12)":U LABEL "MenuStructureType":T
    FIELD MenuBeginsAGroup AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "MenuBeginsAGroup":T
    FIELD MenuSmallImage AS CHARACTER FORMAT "x(40)":U LABEL "MenuSmallImage":T
    FIELD MenuLargeImage AS CHARACTER FORMAT "x(40)":U LABEL "MenuLargeImage":T
    FIELD MenuSequence AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "MenuSequence":T
    FIELD FunctionGuid AS CHARACTER FORMAT "x(36)":U LABEL "FunctionGuid":T
    FIELD HasChild AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U LABEL "HasChild":T
    FIELD FunctionName AS CHARACTER FORMAT "x(20)":U LABEL "FunctionName":T
    FIELD MenuStyleGuid AS CHARACTER FORMAT "x(36)":U LABEL "MenuStyleGuid":T
    FIELD MenuStyleCode AS CHARACTER FORMAT "x(8)":U LABEL "Menu Style Code":T

    INDEX MenuGuid AS UNIQUE PRIMARY MenuGuid ASCENDING
    INDEX ParentMenuGuidSequence ParentMenuGuid ASCENDING MenuSequence ASCENDING

    .

    