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
    File        : dsRelation.i
    Purpose     : Business Entity for Relation

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 07.04.2013 12:36:20
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsRelation

{ Consultingwerk/SmartFramework/System/eSmartRelation.i }


DEFINE {&ACCESS} DATASET dsRelation {&REFERENCE-ONLY} FOR eSmartRelation 

    .    
