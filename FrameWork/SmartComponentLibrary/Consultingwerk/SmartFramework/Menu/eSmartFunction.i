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
    File        : eSmartFunction.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15.12.2015 19:13:14
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.MenuBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartFunction{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartFunctionBefore{&SUFFIX} &ENDIF
    FIELD FunctionGuid AS CHARACTER FORMAT "x(36)":U LABEL "FunctionGuid":T
    FIELD FunctionName AS CHARACTER FORMAT "x(20)":U LABEL "FunctionName":T
    FIELD FunctionDescription AS CHARACTER FORMAT "x(100)":U LABEL "FunctionDescription":T
    FIELD FunctionSmallImage AS CHARACTER FORMAT "x(50)":U LABEL "FunctionSmallImage":T
    FIELD FunctionLargeImage AS CHARACTER FORMAT "x(50)":U LABEL "FunctionLargeImage":T
    FIELD FunctionCallParameter AS CLOB FORMAT "x(8)":U LABEL "FunctionCallParameter":T
    FIELD FunctionModuleGuid AS CHARACTER FORMAT "x(36)":U LABEL "FunctionModuleGuid":T
    FIELD ModuleName AS CHARACTER FORMAT "x(30)":U LABEL "ModuleName":T
    FIELD ExternalId AS CHARACTER FORMAT "x(60)":U LABEL "External ID":T

    INDEX FunctionGuid AS UNIQUE PRIMARY FunctionGuid ASCENDING
    INDEX FunctionName FunctionName ASCENDING
    INDEX ExternalId ExternalId ASCENDING

    .

    