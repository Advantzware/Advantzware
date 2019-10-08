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
    File        : release-menu-structure.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 12 12:29:53 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.SmartFramework.Tools.Import.* FROM PROPATH . 
USING Consultingwerk.Util.*                        FROM PROPATH . 

DEFINE VARIABLE cBusinessEntity AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO .
DEFINE VARIABLE cSkipFields     AS CHARACTER NO-UNDO .

DEFINE VARIABLE oImporter AS GenericDataImporter NO-UNDO . 
DEFINE VARIABLE oResult   AS ImportResult        NO-UNDO .

/* ***************************  Main Block  *************************** */

SESSION:ERROR-STACK-TRACE = TRUE .

ASSIGN cBusinessEntity = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "BusinessEntity":U) 
       cFileName       = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "FileName":U) 
       cSkipFields     = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "SkipFields":U) 
                                           .

Consultingwerk.Assertion.FileAssert:Exists (cFileName) .

FILE-INFORMATION:FILE-NAME = cFileName . 
cFileName = FILE-INFORMATION:FULL-PATHNAME . 

PUT UNFORMATTED "Importing data: "{&TRAN} cBusinessEntity SKIP (0). 
PUT UNFORMATTED "Importing from: "{&TRAN} cFileName SKIP (0).
IF cSkipFields > "":U THEN 
    PUT UNFORMATTED "Skipping Fields: "{&TRAN} cSkipFields SKIP (0).

oImporter = NEW GenericDataImporter () . 
oResult   = oImporter:ImportBusinessEntityData (cBusinessEntity, 
                                                cFileName,
                                                cSkipFields) .

IF VALID-OBJECT (oResult) THEN DO:
    PUT UNFORMATTED oResult:CreatedRecords " rows created."{&TRAN} SKIP (0).
    PUT UNFORMATTED oResult:ModifiedRecords " rows updated."{&TRAN} SKIP (1).
END.

Consultingwerk.OERA.ServiceManager:StopAllBusinessServices () . 

oImporter = ? . 
oResult = ? .

RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled error:"{&TRAN} SKIP (0)
            ErrorHelper:FormattedErrorMessagesExt (err) SKIP (1). 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.       
       