/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dsRelation.i
    Purpose     : Business Entity for Relation

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 24.03.2016 13:07:15
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsRelation

{ Consultingwerk/SmartFramework/System/eSmartRelation.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.RelationBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsRelation{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartRelation{&SUFFIX} 

    .    
