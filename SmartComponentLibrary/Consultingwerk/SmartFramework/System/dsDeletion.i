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
    File        : dsDeletion.i
    Purpose     : Business Entity for SmartDeletion

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.04.2015 21:37:03
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsDeletion

{ Consultingwerk/SmartFramework/System/eSmartDeletion.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.DeletionBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsDeletion{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartDeletion{&SUFFIX} 

    .    
