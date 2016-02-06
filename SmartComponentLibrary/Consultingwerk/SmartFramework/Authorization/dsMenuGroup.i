/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dsMenuGroup.i
    Purpose     : Business Entity for MenuGroup

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories
    Created     : 02.11.2012 13:24:59
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsMenuGroup

{ Consultingwerk/SmartFramework/Authorization/ePossibleGroup.i }
{ Consultingwerk/SmartFramework/Authorization/eSmartMenuGroup.i }


DEFINE {&ACCESS} DATASET dsMenuGroup {&REFERENCE-ONLY} FOR ePossibleGroup, eSmartMenuGroup 

    .    
