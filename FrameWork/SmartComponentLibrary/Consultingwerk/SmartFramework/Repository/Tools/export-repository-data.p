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
    File        : export-repository-data.p
    Purpose     : PCTRun procedure that dumps all modified objects

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 01 12:29:53 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.Repository.Tools.* FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .

DEFINE VARIABLE cBaseFolder          AS CHARACTER NO-UNDO .
DEFINE VARIABLE lResetModifiedStatus AS LOGICAL   NO-UNDO .

DEFINE VARIABLE oExporter AS RepositoryObjectExporter NO-UNDO .

/* ***************************  Main Block  *************************** */

SESSION:ERROR-STACK-TRACE = TRUE .

ASSIGN cBaseFolder          = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE,
                                                "BaseFolder":U)
       lResetModifiedStatus = DataTypeHelper:ToLogical (DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE,
                                                        "ResetModifiedStatus":U)) .

oExporter = NEW RepositoryObjectExporter () .
oExporter:DumpModifiedRepositoryObjects (cBaseFolder,
                                         lResetModifiedStatus) .

RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled error:"{&TRAN} SKIP (0)
            ErrorHelper:FormattedErrorMessagesExt (err) SKIP (1).

    RETURN "1":U . /* signal erorr */
END CATCH.
