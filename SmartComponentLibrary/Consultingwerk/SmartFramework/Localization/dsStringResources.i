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
    File        : dsStringResources.i
    Purpose     : Business Entity for StringResources

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 16.05.2013 22:47:02
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsStringResources

{ Consultingwerk/SmartFramework/Localization/eStringResources.i }


DEFINE {&ACCESS} DATASET dsStringResources {&REFERENCE-ONLY} FOR eStringResources 

    .    
