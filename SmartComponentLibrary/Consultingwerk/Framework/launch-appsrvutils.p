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
    File        : launch-appsrvutils.p
    Purpose     : Loads adecomm/as-utils.w when not already loaded. 
                  Procedural wrapper to adecomm/appserv.i as the class
                  AppServerServiceManager cannot use include files that
                  define GLOBAL SHARED VARIABLES
    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 26 17:58:09 CEST 2009
    Notes       : Renamed from LauchAppSrvUtils.p in SCL-721
                    - fixed typo
                    - renamed to all lower case to simplify case for Unix Deployments
                  Customers should remove any locally present copy of 
                  LauchAppSrvUtils.p and LauchAppSrvUtils.r in this 
                  folder
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT-OUTPUT PARAMETER phAppSrvUtils AS HANDLE NO-UNDO . 

/* ***************************  Main Block  *************************** */

IF VALID-HANDLE(phAppSrvUtils)
    THEN RETURN .

{adecomm/appserv.i}

ASSIGN phAppSrvUtils = appSrvUtils . 
