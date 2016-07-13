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
    File        : crSWStatus.p
    Purpose     : Create trigger for SmartFunction table of SmartDB

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thi Oct 17 12:19:57 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

TRIGGER PROCEDURE FOR CREATE OF SmartWorkflowStatus.

ASSIGN SmartWorkflowStatus.WorkflowStatusGuid = GUID.

