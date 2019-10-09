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
    File        : dsMissingRelations.i
    Purpose     : Dataset to return missing database relations

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 24 13:17:06 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

DEFINE {&ACCESS} TEMP-TABLE ttRelation{&SUFFIX} {&REFERENCE-ONLY} NO-UNDO
    FIELD ParentTable AS CHARACTER
    FIELD ChildTable AS CHARACTER
    FIELD IndexName AS CHARACTER
    FIELD IndexFields AS CHARACTER
    INDEX ParentTable ParentTable ChildTable .

DEFINE {&ACCESS} DATASET dsMissingRelations{&SUFFIX} {&REFERENCE-ONLY} FOR ttRelation .
