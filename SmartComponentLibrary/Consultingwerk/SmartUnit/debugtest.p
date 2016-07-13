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
    File        : debugtest.p
    Purpose     : SmartUnit Debug Launcher Procedure

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 22 10:42:49 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartUnit.*            FROM PROPATH . 
USING Consultingwerk.SmartUnit.TestRunner.* FROM PROPATH . 
USING Consultingwerk.Util.*                 FROM PROPATH . 
USING Progress.Lang.*                       FROM PROPATH . 

{Consultingwerk/products.i}

DEFINE VARIABLE oTestSuite  AS TestSuite   NO-UNDO .
DEFINE VARIABLE oTestRunner AS ITestRunner NO-UNDO .

/* Mike Fechner, Consultingwerk Ltd. 25.09.2014
   SCL-475: Ability to opt-in custom include file into runtest.p
            to allow NEW SHARED definitions etc. prior to executing
            tests (if DB triggers etc. rely on those variables) */
&IF DEFINED (TestRunnerDefinition) &THEN
{{&TestRunnerDefinition}}
&ENDIF

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 25.03.2013
   For UnitTesting should be enforcing the error stack trace */
SESSION:ERROR-STACK-TRACE = TRUE . 

ASSIGN SmartUnit:Active  = TRUE
       SmartUnit:Verbose = TRUE 
       .

FILE-INFO:FILE-NAME = SESSION:PARAMETER .

IF FILE-INFO:FULL-PATHNAME = ? OR 
   NOT FILE-INFO:FILE-TYPE MATCHES "*F*":U THEN 

    UNDO, THROW NEW AppError ("Can only debug a selected class file in Progress Developer Studio.":U, 0) .
       
oTestSuite = NEW TestSuite () . 

ASSIGN 
    oTestSuite:Tests  = ClassHelper:FileNameToClassName (SESSION:PARAMETER) .

IF SESSION:PARAMETER MATCHES "*.cls":U THEN 
    oTestSuite:Output = SUBSTRING (SESSION:PARAMETER, 
                                   1, 
                                   LENGTH (SESSION:PARAMETER) - 3) + "xml":U  .     

oTestRunner = NEW DebugRunner () .

IF oTestRunner:Execute (oTestSuite) = TRUE THEN
    RETURN "0":U .

RETURN "1":U . /* signal error, as above no success was signalled */

/* Mike Fechner, Consultingwerk Ltd. 24.03.2013
   Catch unhandled error, this would be a SmartUnit
   runtime error */
CATCH err AS Progress.Lang.Error :
    ErrorHelper:ShowErrorMessage (err, "Unhandled SmartUnit error":U) .
	
	RETURN "1":U . /* signal erorr */	
END CATCH.

FINALLY:
    ASSIGN SmartUnit:Active  = FALSE .
END FINALLY.	
