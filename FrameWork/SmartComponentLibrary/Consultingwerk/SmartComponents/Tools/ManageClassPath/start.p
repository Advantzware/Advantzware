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
    Created     : Mon Apr 18 08:04:01 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

DEFINE VARIABLE oForm AS Consultingwerk.SmartComponents.Tools.ManageClassPath.ManageClassPathForm NO-UNDO . 

/* ***************************  Main Block  *************************** */

Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "Software~\Consultingwerk Ltd.~\SmartComponent Library~\":U .
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE . 
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
Consultingwerk.SmartComponents.Implementation.SmartDataBrowser:SaveColumnSettings = TRUE . 
&ELSE 
Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SaveColumnSettings = TRUE . 
&ENDIF

oForm = NEW Consultingwerk.SmartComponents.Tools.ManageClassPath.ManageClassPathForm () . 

WAIT-FOR System.Windows.Forms.Application:RUN (oForm) .

DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager NO-ERROR .

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .      
END CATCH.
