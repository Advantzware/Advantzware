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
    File        : dsUser.i
    Purpose     : Business Entity for User

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 12.06.2015 01:48:48
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsUser

{ Consultingwerk/SmartFramework/Authentication/eSmartUser.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Authentication.UserBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsUser{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartUser{&SUFFIX} 

    .    
