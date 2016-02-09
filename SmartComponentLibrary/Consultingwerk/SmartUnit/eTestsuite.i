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
    File        : eTestsuite.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 24.03.2013 01:46:19
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eTestsuite NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eTempTableBefore &ENDIF
    FIELD TestsuiteName AS CHARACTER FORMAT "X(75)":U
    FIELD Tests AS INT64 FORMAT ">>>,>>>,>>9":U INIT "0":U
    FIELD Failures AS INT64 FORMAT ">>>,>>>,>>9":U INIT "0":U
    FIELD TimeStamp AS DATETIME-TZ INIT "NOW":U
    FIELD SystemOut AS CLOB FORMAT "X(8)":U COLUMN-CODEPAGE "UTF-8":U
    FIELD SystemErr AS CLOB FORMAT "X(8)":U COLUMN-CODEPAGE "UTF-8":U

    INDEX Testsuite AS UNIQUE PRIMARY TestsuiteName ASCENDING

    .
