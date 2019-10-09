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
    File        : import-menu-structure.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 12 13:48:13 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Tools.Import.MenuStructure.* FROM PROPATH . 

DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO FORMAT "x(80)":U  LABEL "Source File":U                INIT "c:/temp/menustructure.xml":U .
DEFINE VARIABLE lOverwriteExisting AS LOGICAL   NO-UNDO FORMAT "yes/no":U LABEL "Overwrite existing records":U INIT TRUE.

DEFINE VARIABLE oImporter AS MenuStructureImporter NO-UNDO . 

/* ***************************  Main Block  *************************** */

DEFAULT-WINDOW:WIDTH = 120 . 

UPDATE cFileName SKIP 
       lOverwriteExisting 
       WITH WIDTH 100 1 DOWN . 

oImporter = NEW MenuStructureImporter () . 
oImporter:ImportMenuStructure (cFileName, lOverwriteExisting) .

CATCH err AS Progress.Lang.Error:
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
END CATCH.       
       