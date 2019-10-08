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
    File        : eSmartDeletion.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.04.2015 21:37:03
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.DeletionBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartDeletion{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartDeletionBefore{&SUFFIX} &ENDIF
    FIELD DeletionGuid AS CHARACTER FORMAT "x(36)":U LABEL "DeletionGuid":T
    FIELD DeletionTable AS CHARACTER FORMAT "x(22)":U LABEL "Table":T
    FIELD DeletionKeyFieldValues AS CHARACTER FORMAT "x(40)":U LABEL "Key Field Values":T
    FIELD DeletionTimeStamp AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "Time Stamp":T
    FIELD DeletionUser AS CHARACTER FORMAT "x(20)":U LABEL "User Name":T

    INDEX DeletionGuid AS UNIQUE PRIMARY Deletionguid ASCENDING

    .

    