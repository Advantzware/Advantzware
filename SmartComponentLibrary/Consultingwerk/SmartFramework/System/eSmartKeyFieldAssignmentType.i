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
    File        : eSmartKeyFieldAssignmentType.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.10.2015 10:32:35
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.KeyFieldAssignmentTypeBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartKeyFieldAssignmentType{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartKeyFieldAssignmentTypeB{&SUFFIX} &ENDIF
    FIELD KeyFieldAssignmentGUID AS CHARACTER FORMAT "x(36)":U LABEL "KeyFieldAssignmentGUID":T
    FIELD KeyFieldAssignmentKey AS CHARACTER FORMAT "x(8)":U LABEL "Key":T
    FIELD KeyFieldAssignmentDescription AS CHARACTER FORMAT "x(60)":U LABEL "Description":T

    INDEX KeyFieldAssignmentDescription KeyFieldAssignmentDescription ASCENDING
    INDEX KeyFieldAssignmentGUID AS UNIQUE PRIMARY KeyFieldAssignmentGUID ASCENDING
    INDEX KeyFieldAssignmentKey AS UNIQUE KeyFieldAssignmentKey ASCENDING

    .

    