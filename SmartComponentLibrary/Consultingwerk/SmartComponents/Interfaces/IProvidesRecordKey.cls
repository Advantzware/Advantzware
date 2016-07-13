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
    File        : IProvidesRecordKey
    Purpose     : Interface for SmartDataAdapter classes that can provide
                  a value for the key of a record
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 16 11:46:24 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .  
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Interfaces.IProvidesRecordKey: 

    /*------------------------------------------------------------------------------
        Purpose: Returns a key identifying the current record 
        Notes:   Key values may be concatenated, but must be returned as a single character value
        @return The single character key that represents the current record
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC CHARACTER GetCurrentRecordKey () .

END INTERFACE .
