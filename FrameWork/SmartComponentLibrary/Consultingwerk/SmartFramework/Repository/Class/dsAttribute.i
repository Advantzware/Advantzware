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
    File        : dsAttribute.i
    Purpose     : Business Entity for Attribute

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 21.05.2014 12:50:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsAttribute

{ Consultingwerk/SmartFramework/Repository/Class/eSmartAttribute.i }


DEFINE {&ACCESS} DATASET dsAttribute {&REFERENCE-ONLY} FOR eSmartAttribute 

    .    
