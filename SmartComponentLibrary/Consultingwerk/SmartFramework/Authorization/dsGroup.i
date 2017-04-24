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
    File        : dsGroup.i
    Purpose     : Business Entity for Group

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories
    Created     : 12.10.2012 18:06:53
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsGroup

{ Consultingwerk/SmartFramework/Authorization/eSmartGroup.i }


DEFINE {&ACCESS} DATASET dsGroup {&REFERENCE-ONLY} FOR eSmartGroup 

    .    
