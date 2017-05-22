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
    File        : ListItem.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.09.2014 11:58:31
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ListItem NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE ListItemBefore &ENDIF
    FIELD fieldid AS CHARACTER SERIALIZE-NAME "objectid":U XML-NODE-TYPE "HIDDEN":U
    FIELD id AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "id":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD origid AS CHARACTER FORMAT "x(20)":U LABEL "origid":T SERIALIZE-NAME "origid":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD orderNo AS INTEGER SERIALIZE-NAME "orderNo":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD source AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "source":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD name AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "name":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD code AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "code":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD mainItemId AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "mainItemId":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD isDefault AS LOGICAL FORMAT "x(20)":U SERIALIZE-NAME "isDefault":U XML-NODE-TYPE "ATTRIBUTE":U

    INDEX parentid AS UNIQUE PRIMARY fieldid ASCENDING orderNo ASCENDING

    .
