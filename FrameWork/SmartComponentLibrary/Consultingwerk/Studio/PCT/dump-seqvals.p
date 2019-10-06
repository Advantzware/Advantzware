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
    File        : dump-seqvals.p
    Purpose     : Dump of sequence current values, to be executed using 
                  a PCTRun task

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

DEFINE NEW SHARED VARIABLE user_dbname AS CHARACTER NO-UNDO .

DEFINE NEW SHARED STREAM logfile.

DEFINE NEW SHARED VARIABLE drec_db AS RECID INITIAL ? NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tt_cache_file NO-UNDO
    FIELD nPos        AS INTEGER
    FIELD cName       AS CHARACTER
    FIELD p_flag      AS LOGICAL
    
/* Mike Fechner, Consultingwerk Ltd. 20.02.2013
   new tt field in 11.0 */
&IF NOT PROVERSION BEGINS "10":U &THEN
    FIELD multitenant  AS LOGICAL
&ENDIF    
    INDEX nPos IS UNIQUE PRIMARY nPos
    INDEX cName cName.

DEFINE VARIABLE cTargetFile AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

ASSIGN cTargetFile = DYNAMIC-FUNCTION("getParameter":U IN SOURCE-PROCEDURE, INPUT "TargetFile":U) .

Consultingwerk.Assertion.Assert:NotNullOrEmpty (cTargetFile) .

ASSIGN drec_db     = Consultingwerk.Util.DatabaseHelper:GetDictDbRecid() 
       user_dbname = LDBNAME ("DICTDB":U)
       
       user_env[2] = cTargetFile 
       user_env[5] = SESSION:CPSTREAM   
       /*user_env[5] = pcCodePage.*/
       user_env[6] = "no-alert-boxes":U.

RUN prodict/dump/_dmpseqs.p.

ERROR-STATUS:ERROR = NO .

RETURN "0":U .

/* Mike Fechner, Consultingwerk Ltd. 07.07.2013
   Catch unhandled error, this would be a runtime error */
CATCH err AS Progress.Lang.Error :
    MESSAGE "Unhandled error:":U SKIP 
            Consultingwerk.Util.ErrorHelper:FormattedErrorMessagesExt (err) . 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.   
