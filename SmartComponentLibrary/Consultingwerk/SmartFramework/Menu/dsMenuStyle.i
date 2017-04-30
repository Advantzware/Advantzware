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
    File        : dsMenuStyle.i
    Purpose     : Business Entity for MenuStyle

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 09.08.2013 08:55:19
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsMenuStyle

{ Consultingwerk/SmartFramework/Menu/eSmartMenuStyle.i }


DEFINE {&ACCESS} DATASET dsMenuStyle {&REFERENCE-ONLY} FOR eSmartMenuStyle 

    .    
