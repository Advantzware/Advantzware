/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 10:27:57
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsLinkType

{ Consultingwerk/SmartFramework/Repository/Class/eSmartLinkType.i }


DEFINE {&ACCESS} DATASET dsLinkType {&REFERENCE-ONLY} FOR eSmartLinkType 

    .    
