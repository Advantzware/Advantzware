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
    File        : crSSchedulerJob.p
    Purpose     : Create trigger for SmartSchedulerJob table of SmartDB

    Syntax      :

    Description : 

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed May 25 17:29:06 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

TRIGGER PROCEDURE FOR CREATE OF SmartSchedulerJob.

ASSIGN SmartSchedulerJob.SchedulerJobGuid = GUID.