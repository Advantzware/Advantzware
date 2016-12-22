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
    File        : dsClassType.i
    Purpose     : Business Entity for SmartClassType

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 04.09.2015 23:45:00
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsClassType

{ Consultingwerk/SmartFramework/Repository/Class/eSmartClassType.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsClassType{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartClassType{&SUFFIX} 

    .    
