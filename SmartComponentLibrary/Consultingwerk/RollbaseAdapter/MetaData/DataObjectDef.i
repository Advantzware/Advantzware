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
    File        : DataObjectDef.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 03.11.2013 17:57:06
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE DataObjectDef NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE DataObjectDefBefore &ENDIF
    FIELD id AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "id":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD origId AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "origId":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD objDefName AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "objDefName":U XML-NODE-TYPE "ATTRIBUTE":U
    FIELD SingularName AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "SingularName":U
    FIELD PluralName AS CHARACTER FORMAT "x(20)":U SERIALIZE-NAME "PluralName":U

    INDEX id AS UNIQUE PRIMARY id ASCENDING
    INDEX objDefName AS UNIQUE objDefName ASCENDING

    .
