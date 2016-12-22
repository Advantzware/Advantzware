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
    File        : dsContext.i
    Purpose     : Business Entity for Context

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 26.09.2014 15:48:43
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsContext

{ Consultingwerk/OERA/Context/eContextProperties.i &NO-BEFORE=YES }
{ Consultingwerk/OERA/Context/eSessionContext.i &NO-BEFORE=YES }


DEFINE {&ACCESS} DATASET dsContext{&SUFFIX} {&REFERENCE-ONLY} FOR eContextProperties{&SUFFIX}, eSessionContext{&SUFFIX} 

    .    
