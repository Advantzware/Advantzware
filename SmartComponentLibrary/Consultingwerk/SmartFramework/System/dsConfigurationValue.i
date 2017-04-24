/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dsConfigurationValue.i
    Purpose     : Business Entity for ConfigurationValue

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 07.01.2017 12:05:53
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsConfigurationValue

{ Consultingwerk/SmartFramework/System/eSmartConfigurationValue.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ConfigurationValueBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsConfigurationValue{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartConfigurationValue{&SUFFIX} 

    .    
