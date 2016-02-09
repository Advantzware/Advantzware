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
    File        : dsLanguage.i
    Purpose     : Business Entity for Language

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories
    Created     : 02.01.2013 16:56:15
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsLanguage

{ Consultingwerk/SmartFramework/System/eSmartLanguage.i }


DEFINE {&ACCESS} DATASET dsLanguage {&REFERENCE-ONLY} FOR eSmartLanguage 

    .    
