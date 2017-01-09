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
    File        : LockWindowUpdate.p
    Purpose     : The LockWindowUpdate function disables or enables drawing 
                  in the specified window. Only one window can be locked at 
                  a time.

    Syntax      :

    Description : http://msdn.microsoft.com/en-us/library/dd145034(v=vs.85).aspx    

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Nov 10 10:52:23 CET 2010
    Notes       : Facade to Consultingwerk.Windows.API.Win32:LockWindowUpdate
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER piHwnd   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER piLocked AS INTEGER NO-UNDO.

/* ***************************  Main Block  *************************** */

Consultingwerk.Windows.API.Win32:LockWindowUpdate 
    (piHwnd, OUTPUT piLocked) .
