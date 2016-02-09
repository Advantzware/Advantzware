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
    File        : DataFieldDef.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 03.11.2013 17:57:06
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE DataFieldDef NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE DataFieldDefBefore &ENDIF
    FIELD objectid AS CHARACTER SERIALIZE-NAME "objectid":U XML-NODE-TYPE "HIDDEN":U
    FIELD id AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "id":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD origid AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "origid":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD fieldName AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "fieldName":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD groupName AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "groupName":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD dataName AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "dataName":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD uiClass AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "uiClass":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD isRequired AS LOGICAL SERIALIZE-NAME "isRequired":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD isReadOnly AS LOGICAL SERIALIZE-NAME "isReadOnly":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD maxLength AS INTEGER SERIALIZE-NAME "maxLength":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD DisplayLabel AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "DisplayLabel":U

    INDEX parentid AS UNIQUE PRIMARY objectid ASCENDING fieldName ASCENDING

    .
