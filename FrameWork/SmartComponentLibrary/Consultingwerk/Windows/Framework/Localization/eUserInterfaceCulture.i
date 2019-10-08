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
    File        : eUserInterfaceCulture.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 30.05.2013 21:47:56
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eUserInterfaceCulture NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eUserInterfaceCultureBefore &ENDIF
    FIELD DisplayName AS CHARACTER FORMAT "X(30)":U LABEL "Display Name":T
    FIELD EnglishName AS CHARACTER FORMAT "X(30)":U LABEL "English Name":T
    FIELD Name AS CHARACTER FORMAT "X(8)":U LABEL "Name":T

    INDEX DisplayName DisplayName ASCENDING
    INDEX EnglishName EnglishName ASCENDING
    INDEX Name AS UNIQUE PRIMARY Name ASCENDING

    .
