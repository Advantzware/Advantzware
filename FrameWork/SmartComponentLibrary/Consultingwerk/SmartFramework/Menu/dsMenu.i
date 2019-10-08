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
    File        : dsMenu.i
    Purpose     : Business Entity for Menu

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15.12.2015 19:13:14
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsMenu

{ Consultingwerk/SmartFramework/Menu/eSmartMenu.i }
{ Consultingwerk/SmartFramework/Menu/eSmartFunction.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Menu.MenuBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsMenu{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartMenu{&SUFFIX}, eSmartFunction{&SUFFIX} 
    DATA-RELATION eSmartMenueSmartFunctionRelation FOR eSmartMenu{&SUFFIX}, eSmartFunction{&SUFFIX} 
        RELATION-FIELDS (FunctionGuid,FunctionGuid)

    .    
