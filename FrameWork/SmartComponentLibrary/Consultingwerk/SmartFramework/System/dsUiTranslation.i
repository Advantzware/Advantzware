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
    File        : dsUiTranslation.i
    Purpose     : Business Entity for UiTranslation

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 12.05.2013 18:40:10
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsUiTranslation

{ Consultingwerk/SmartFramework/System/eSmartUiTranslation.i }


DEFINE {&ACCESS} DATASET dsUiTranslation {&REFERENCE-ONLY} FOR eSmartUiTranslation 

    .    
