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
    File        : dsLinkType.i
    Purpose     : Business Entity for LinkType

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.05.2016 23:33:50
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsLinkType

{ Consultingwerk/SmartFramework/Repository/Class/eSmartLinkType.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.LinkTypeBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsLinkType{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartLinkType{&SUFFIX} 

    .    
