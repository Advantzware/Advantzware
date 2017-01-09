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
    File        : dsAuthorizationMaintenance.i
    Purpose     : Business Entity for dsAuthorizationMaintenance

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 21.05.2014 11:17:03
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsAuthorizationMaintenance

{ Consultingwerk/SmartFramework/Authorization/ttAuthorizationMaintenance.i &NO-BEFORE=YES }


DEFINE {&ACCESS} DATASET dsAuthorizationMaintenance {&REFERENCE-ONLY} FOR ttAuthorizationMaintenance 

    .    
