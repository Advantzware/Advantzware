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
    File        : eSmartClassType.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 04.09.2015 23:45:00
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.ClassTypeBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartClassType{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartClassTypeBefore{&SUFFIX} &ENDIF
    FIELD ClassTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "Class Type":T
    FIELD ClassTypeName AS CHARACTER FORMAT "x(20)":U LABEL "Class Type Name":T
    FIELD ClassTypeDescription AS CHARACTER FORMAT "x(300)":U LABEL "Description":T
    FIELD AvmType AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "AVM Type":T

    INDEX ClassTypeGuid AS UNIQUE PRIMARY ClassTypeGuid ASCENDING
    INDEX ClassTypeName AS UNIQUE ClassTypeName ASCENDING

    .

    