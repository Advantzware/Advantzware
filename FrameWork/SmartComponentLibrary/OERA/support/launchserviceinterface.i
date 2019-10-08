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
    File        : launchserviceinterface.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 04 15:50:14 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE NEW GLOBAL SHARED VARIABLE gshServiceInterface AS HANDLE NO-UNDO .  

/* ***************************  Main Block  *************************** */

IF NOT VALID-HANDLE(gshServiceInterface) THEN 
    RUN {&OERASI}/service.p PERSISTENT SET gshServiceInterface . 
    