/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dump-deletions.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Apr 05 22:59:34 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.SmartFramework.Tools.Export.* FROM PROPATH . 
USING Consultingwerk.Util.*                        FROM PROPATH . 

DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO .

DEFINE VARIABLE oExporter AS GenericDataExporter NO-UNDO . 
DEFINE VARIABLE iRows     AS INTEGER             NO-UNDO .

/* ***************************  Main Block  *************************** */

ASSIGN cFileName       = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "FileName":U) 
                                           .

PUT UNFORMATTED "Dumping deletions. "{&TRAN} SKIP (0). 

oExporter = NEW GenericDataExporter () . 
iRows     = oExporter:ExportBusinessEntityData ("Consultingwerk.SmartFramework.System.DeletionBusinessEntity":U, 
                                                "eSmartDeletion":U,
                                                "":U,
                                                cFileName) .

PUT UNFORMATTED iRows " rows exported."{&TRAN} SKIP (1).

RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled error:"{&TRAN} SKIP (0)
            ErrorHelper:FormattedErrorMessagesExt (err) SKIP (1). 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.       
       