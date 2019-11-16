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
    File        : verify-connect.p
    Purpose     : Verifies connection to SmartDB 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Oct 14 08:44:46 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Main Block  *************************** */

PUT UNFORMATTED "SmartDB migration utility.":U SKIP .
PUT UNFORMATTED "OpenEdge version: ":U PROVERSION SKIP . 

IF NOT CONNECTED ("smartdb":U) THEN DO:
    PUT UNFORMATTED SKIP (0) "ERROR: SmartDB is not connected! " SKIP (1).
    RETURN "1":U .     
END.    

PUT UNFORMATTED "Connected to SmartDB: " DBPARAM ("smartdb":U) SKIP (1) .

RETURN "0":U
