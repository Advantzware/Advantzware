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
    File        : RelationshipDef.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 03.11.2013 17:57:06
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE RelationshipDef NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE RelationshipDefBefore &ENDIF
    FIELD objectid AS CHARACTER SERIALIZE-NAME "objectid":U XML-NODE-TYPE "HIDDEN":U
    FIELD id AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "id":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD origid AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "origid":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD relName AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "relName":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD isMultiple1 AS LOGICAL SERIALIZE-NAME "isMultiple1":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD isMultiple2 AS LOGICAL SERIALIZE-NAME "isMultiple2":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD isSystem AS LOGICAL SERIALIZE-NAME "isSystem":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD objDef1 AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "objDef1":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD objDef2 AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "objDef2":U XML-NODE-TYPE "ATTRIBUTE":U

    INDEX parentid AS UNIQUE PRIMARY objectid ASCENDING relName ASCENDING

    .
