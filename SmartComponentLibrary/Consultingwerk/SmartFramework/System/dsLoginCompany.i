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
    File        : dsLoginCompany.i
    Purpose     : Business Entity for LoginCompany

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 08.01.2013 20:09:32
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsLoginCompany

{ Consultingwerk/SmartFramework/System/eSmartLoginCompany.i }


DEFINE {&ACCESS} DATASET dsLoginCompany {&REFERENCE-ONLY} FOR eSmartLoginCompany 

    .    
