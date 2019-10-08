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
    File        : agentstart-log.i
    Purpose     : Writes a startup header to the AppServer logfile 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jan 12 22:30:43 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

LOG-MANAGER:WRITE-MESSAGE (SUBSTITUTE ("########################################################################~n":U +
                                       "### SmartComponent Library AppServer Agent Startup                      ~n":U +
                                       "### Framework Version: &1~n":U +
                                       "### OpenEdge Version:  &2 &3~n":U + 
                                       "### Agent Type: &4~n":U + 
                                       "### &5~n":U +
                                       "########################################################################":U,
                                       (NEW Consultingwerk.FrameworkVersion()):GetVersionString(),
                                       PROVERSION,
                                       Consultingwerk.Util.SessionHelper:GetPatchLevel(),
                                       SESSION:CLIENT-TYPE, 
                                       (NEW Consultingwerk.FrameworkVersion()):GetCopyRights()),
                                       "SCL":U) .
