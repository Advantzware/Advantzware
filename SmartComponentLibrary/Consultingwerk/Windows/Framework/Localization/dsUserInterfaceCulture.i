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
    File        : dsUserInterfaceCulture.i
    Purpose     : Business Entity for UserInterfaceCulture

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 30.05.2013 21:47:56
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsUserInterfaceCulture

{ Consultingwerk/Windows/Framework/Localization/eUserInterfaceCulture.i &NO-BEFORE=YES }


DEFINE {&ACCESS} DATASET dsUserInterfaceCulture {&REFERENCE-ONLY} FOR eUserInterfaceCulture 

    .    
