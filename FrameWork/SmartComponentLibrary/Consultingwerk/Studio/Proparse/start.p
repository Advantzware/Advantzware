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
    File        : start.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 16 19:07:13 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*       FROM PROPATH .
USING Consultingwerk.OERA.*            FROM PROPATH .
USING Consultingwerk.OERA.Enum.*       FROM PROPATH .
USING Consultingwerk.OERA.Context.*    FROM PROPATH .  
USING Consultingwerk.Studio.Proparse.* FROM PROPATH.
USING Consultingwerk.Util.*            FROM PROPATH .

DEFINE VARIABLE oForm AS ProparseTreeViewForm NO-UNDO . 

DEFINE VARIABLE oTaskbarManager AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager NO-UNDO . 

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
SESSION:APPL-ALERT-BOXES = TRUE . 
SESSION:DEBUG-ALERT = TRUE . 
SESSION:ERROR-STACK-TRACE = TRUE  .
SESSION:SYSTEM-ALERT-BOXES = TRUE . 

IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO:
    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance . 
    oTaskbarManager:ApplicationId = "de.consultingwerk.proparsetreeview":U . 
END.

oForm = NEW ProparseTreeViewForm () . 

WAIT-FOR System.Windows.Forms.Application:Run (oForm) .

QUIT . 

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .      
END CATCH.
