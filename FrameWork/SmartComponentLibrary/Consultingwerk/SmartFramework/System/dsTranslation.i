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
    File        : dsTranslation.i
    Purpose     : Business Entity for Translation

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 10.01.2013 14:40:05
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsTranslation

{ Consultingwerk/SmartFramework/System/eSmartTranslation.i }


DEFINE {&ACCESS} DATASET dsTranslation {&REFERENCE-ONLY} FOR eSmartTranslation 

    .    
