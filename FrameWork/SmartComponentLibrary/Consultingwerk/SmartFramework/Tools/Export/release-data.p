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

USING Consultingwerk.SmartFramework.Tools.Export.* FROM PROPATH . 
USING Consultingwerk.Util.*                        FROM PROPATH . 

DEFINE VARIABLE cBusinessEntity AS CHARACTER NO-UNDO .
DEFINE VARIABLE cTables         AS CHARACTER NO-UNDO .
DEFINE VARIABLE cQueryString    AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO .

DEFINE VARIABLE oExporter AS GenericDataExporter NO-UNDO . 
DEFINE VARIABLE iRows     AS INTEGER             NO-UNDO .

/* ***************************  Main Block  *************************** */

ASSIGN cBusinessEntity = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "BusinessEntity":U) 
       cTables         = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "Tables":U) 
       cQueryString    = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "QueryString":U) 
       cFileName       = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                           "FileName":U) 
                                           .

IF cTables > "":U THEN . 
ELSE cTables = "*":U .

PUT UNFORMATTED "Dumping data: "{&TRAN} cBusinessEntity SKIP (0). 

oExporter = NEW GenericDataExporter () . 
iRows     = oExporter:ExportBusinessEntityData (cBusinessEntity, 
                                                cTables,
                                                cQueryString,
                                                cFileName) .

PUT UNFORMATTED iRows " rows exported."{&TRAN} SKIP (1).

RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled error:"{&TRAN} SKIP (0)
            ErrorHelper:FormattedErrorMessagesExt (err) SKIP (1). 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.       
       