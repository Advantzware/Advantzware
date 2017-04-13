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
    File        : dsObjectDefNames.i
    Purpose     : Business Entity for ObjectDefNames

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 03.11.2013 13:19:55
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsObjectDefNames

{ Consultingwerk/RollbaseAdapter/MetaData/eObjectDefName.i &NO-BEFORE=YES }


DEFINE {&ACCESS} DATASET dsObjectDefNames {&REFERENCE-ONLY} FOR eObjectDefName 

    .    
