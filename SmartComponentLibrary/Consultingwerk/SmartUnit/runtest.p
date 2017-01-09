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
    File        : runtest.p
    Purpose     : SmartUnit Main Launcher Procedure

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 22 10:42:49 CET 2013
    Notes       : See https://code.google.com/p/pct/wiki/PCTRun
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartUnit.*            FROM PROPATH . 
USING Consultingwerk.SmartUnit.TestRunner.* FROM PROPATH . 
USING Consultingwerk.Util.*                 FROM PROPATH . 

{Consultingwerk/products.i}

/* Mike Fechner, Consultingwerk Ltd. 22.03.2013
   Set by the PCT environment based on the verbose attribute */
DEFINE SHARED VARIABLE pctVerbose AS LOGICAL NO-UNDO .

DEFINE VARIABLE oTestSuite  AS ITestSuite  NO-UNDO .
DEFINE VARIABLE oTestRunner AS ITestRunner NO-UNDO .
DEFINE VARIABLE cTestRunner AS CHARACTER   NO-UNDO .

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
       SmartUnit:Verbose = pctVerbose 
       
       oTestSuite        = SmartUnit:ParseTestParameters (SOURCE-PROCEDURE) 
       
       cTestRunner       = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                             "testrunner":U) .
              
IF cTestRunner > "":U THEN DO:
    MESSAGE "Testrunner:":U cTestRunner .       
    
    oTestRunner = DYNAMIC-NEW (cTestRunner) () .
END.
ELSE 
    oTestRunner       = NEW TestRunner () . 
  
IF oTestRunner:Execute (oTestSuite) = TRUE THEN 
    RETURN "0":U .
  
RETURN "1":U . /* signal error, as above no success was signalled */  
    
/* Mike Fechner, Consultingwerk Ltd. 24.03.2013
   Catch unhandled error, this would be a SmartUnit
   runtime error */
CATCH err AS Progress.Lang.Error :
    MESSAGE "Unhandled SmartUnit error:":U SKIP 
            ErrorHelper:FormattedErrorMessagesExt (err) . 
    
    RETURN "1":U . /* signal erorr */    
END CATCH.    
    
FINALLY:
    ASSIGN SmartUnit:Active  = FALSE .
END FINALLY.    
