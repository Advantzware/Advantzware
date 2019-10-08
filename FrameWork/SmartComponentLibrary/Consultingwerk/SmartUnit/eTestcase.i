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
    File        : eTestcase.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 25.03.2013 19:01:39
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eTestcase NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eTempTable1Before &ENDIF
    FIELD TestsuiteName AS CHARACTER FORMAT "X(75)":U
    FIELD TestcaseName AS CHARACTER FORMAT "X(75)":U
    FIELD ClassName AS CHARACTER FORMAT "X(75)":U
    FIELD ErrorMessage AS CHARACTER FORMAT "X(75)":U
    FIELD ErrorType AS CHARACTER FORMAT "X(75)":U
    FIELD ErrorStacktrace AS CHARACTER FORMAT "X(75)":U
    FIELD FailureMessage AS CHARACTER FORMAT "X(75)":U
    FIELD FailureType AS CHARACTER FORMAT "X(75)":U
    FIELD FailureStacktrace AS CHARACTER FORMAT "X(75)":U
    FIELD ExecutionTime AS DECIMAL FORMAT ">>>,>>>,>>9.999":U INIT "0":U

    INDEX Testcase AS UNIQUE PRIMARY TestsuiteName ASCENDING ClassName ASCENDING TestcaseName ASCENDING

    .
