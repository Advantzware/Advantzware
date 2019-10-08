/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ttServiceLifeCycleDefinition.i
    Purpose     : Temp-Table to hold definition of service life cycle

    Syntax      :

    Description :

    Author(s)   :
    Created     : Mon Mar 28 09:58:32 CEST 2016
    Notes       : Only one value of StopAfterEachRequest, TimeoutAfterLastRequest,
                  TimeoutAfterStart should be used per entry
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttServiceLifeCycleDefinition NO-UNDO
    FIELD ServiceNamePattern      AS CHARACTER
    FIELD StopAfterEachRequest    AS LOGICAL INITIAL ?
    FIELD TimeoutAfterLastRequest AS INTEGER INITIAL ?
    FIELD TimeoutAfterStart       AS INTEGER INITIAL ?
    .
