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
    File        : eSmartModifiedState.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 02.01.2017 11:08:30
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Object.Export.ObjectMasterBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="ModifiedStateGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartModifiedState{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartModifiedStateBefore{&SUFFIX} &ENDIF
    FIELD ModifiedStateGuid AS CHARACTER FORMAT "x(36)":U LABEL "ModifiedStateGuid":T SERIALIZE-HIDDEN XML-NODE-TYPE "hidden":U
    FIELD ModifiedStateTable AS CHARACTER FORMAT "x(22)":U LABEL "Table":T SERIALIZE-HIDDEN XML-NODE-TYPE "hidden":U
    FIELD ModifiedStateKeyFieldValues AS CHARACTER FORMAT "x(40)":U LABEL "Key Field Values":T SERIALIZE-HIDDEN XML-NODE-TYPE "hidden":U
    FIELD ModifiedStateTimeStamp AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INITIAL ? LABEL "Time Stamp":T XML-NODE-TYPE "attribute":U
    FIELD ModifiedStateUser AS CHARACTER FORMAT "x(20)":U LABEL "User Name":T XML-NODE-TYPE "attribute":U

    INDEX ModifiedStateGuid AS UNIQUE PRIMARY ModifiedStateGuid ASCENDING
    INDEX ModifiedStateTableKeyFieldValues AS UNIQUE ModifiedStateTable ASCENDING ModifiedStateKeyFieldValues ASCENDING
    INDEX TimeStamp ModifiedStateTimeStamp ASCENDING

    .

