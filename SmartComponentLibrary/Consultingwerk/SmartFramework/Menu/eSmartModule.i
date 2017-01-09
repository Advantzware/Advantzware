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
    File        : eSmartModule.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.12.2016 14:02:54
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="ModuleGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartModule{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartModuleBefore{&SUFFIX} &ENDIF
    FIELD ModuleGuid AS CHARACTER FORMAT "x(36)":U LABEL "ModuleGuid":T
    FIELD ModuleName AS CHARACTER FORMAT "x(30)":U LABEL "ModuleName":T
    FIELD ModuleDescription AS CHARACTER FORMAT "x(100)":U LABEL "ModuleDescription":T
    FIELD ProductGuid AS CHARACTER FORMAT "x(36)":U LABEL "ProductGuid":T
    FIELD ProductCode AS CHARACTER FORMAT "x(20)":U LABEL "Code":T
    FIELD ModuleDataFolder AS CHARACTER FORMAT "x(80)":U LABEL "Data Folder":T
    FIELD ModulePackage AS CHARACTER FORMAT "x(80)":U LABEL "Package Name":T

    INDEX ModuleGuid AS UNIQUE PRIMARY ModuleGuid ASCENDING

    .

    