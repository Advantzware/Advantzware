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
    File        : import-deletions.p
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

USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH.

DEFINE VARIABLE cFileName     AS CHARACTER          NO-UNDO .
DEFINE VARIABLE hBuffer       AS HANDLE             NO-UNDO.

DEFINE VARIABLE oInfoProvider AS ITableInfoProvider NO-UNDO. 

{Consultingwerk/SmartFramework/System/dsDeletion.i}

/* ***************************  Main Block  *************************** */

SESSION:ERROR-STACK-TRACE = TRUE .

ASSIGN cFileName       = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "FileName":U) 
                                           .

Consultingwerk.Assertion.FileAssert:Exists (cFileName) .

FILE-INFO:FILE-NAME = cFileName . 
cFileName = FILE-INFO:FULL-PATHNAME . 

PUT UNFORMATTED "Importing deletions from: "{&TRAN} cFileName SKIP (0).

oInfoProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ITableInfoProvider
                                              "NEW SmartTableInfoProvider()"} .

DATASET dsDeletion:READ-XML ("FILE":U, cFileName, ?, ?, ?) .

FOR EACH eSmartDeletion TRANSACTION ON ERROR UNDO, THROW:
    
    CREATE BUFFER hBuffer FOR TABLE eSmartDeletion.DeletionTable .
    
    oInfoProvider:FindRecordByKeyValues (hBuffer, 
                                         eSmartDeletion.DeletionKeyFieldValues,
                                         TRUE) .
    
    hBuffer:BUFFER-DELETE () .
    
    CATCH notavailex AS RecordNotAvailableException:
    	/* ignore */	
    END CATCH.
    
    FINALLY:
        GarbageCollectorHelper:DeleteObject (hBuffer) .		
    END FINALLY.
END.


RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled error:"{&TRAN} SKIP (0)
            ErrorHelper:FormattedErrorMessagesExt (err) SKIP (1). 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.       
       