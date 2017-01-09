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
    File        : dsModule.i
    Purpose     : Business Entity for Module

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 06.04.2014 13:09:37
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsModule

{ Consultingwerk/SmartFramework/Menu/eSmartModule.i }


DEFINE {&ACCESS} DATASET dsModule {&REFERENCE-ONLY} FOR eSmartModule 

    .    
