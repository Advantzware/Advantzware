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
    File        : dump-user.p
    Purpose     : Dump of _user records, to be executed using a PCTRun task

    Syntax      : 

    Description : Dumps the sequence current values of the current DICTDB
                  The DICTDB alias should be set using the PCTAlias option
                  in the PCT DBConnection tag

                  <DBConnection dbName="sports2000" singleUser="true">
                      <PCTAlias name="dictdb" />
                  </DBConnection>

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 20 07:01:36 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* Dictionary variables */

{Consultingwerk/Studio/user-env.i}

DEFINE NEW SHARED STREAM logfile.

DEFINE VARIABLE cTargetFile AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

ASSIGN cTargetFile = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "TargetFile":U) .

Consultingwerk.Assertion.Assert:NotNullOrEmpty (cTargetFile) .

ASSIGN user_env[2] = cTargetFile .
       /*user_env[5] = pcCodePage.*/
       user_env[6] = "no-alert-boxes":U.

RUN prodict/dump/_dmpuser.p.

ERROR-STATUS:ERROR = NO .

RETURN "0":U .

/* Mike Fechner, Consultingwerk Ltd. 07.07.2013
   Catch unhandled error, this would be a runtime error */
CATCH err AS Progress.Lang.Error :
    MESSAGE "Unhandled error:":U SKIP 
            Consultingwerk.Util.ErrorHelper:FormattedErrorMessagesExt (err) . 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.   
