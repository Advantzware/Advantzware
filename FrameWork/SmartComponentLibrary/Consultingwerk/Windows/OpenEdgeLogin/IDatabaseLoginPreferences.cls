/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IDatabaseLoginPreferences
    Purpose     : Service Interface describing methods required for maintaining
                  Database connections opt-out'ed from the OpenEdge login
                  dialog 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 20 08:05:24 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Windows.OpenEdgeLogin.* FROM PROPATH . 
USING Progress.Lang.*                        FROM PROPATH .

INTERFACE Consultingwerk.Windows.OpenEdgeLogin.IDatabaseLoginPreferences:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList of all database opted-out from the login dialog
        Notes:
        @return The CharacterList of all database opted-out from the login dialog
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CharacterList GetAllOptOutedDbParam ().

    /*------------------------------------------------------------------------------
        Purpose: Returns TRUE when the user has opted-out from the DB Connection
                 dialog for the given database 
        Notes:
        @param pcDbParam The DBPARAM value for the DB to opt out from
        @return Logical value indicating if the user has opted out from the given DB
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsDisabled (pcDbParam AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Opts in for the given database connection
        Notes:   Based on DBPARAM () function
        @param pcDbParam The DBPARAM value for the DB to opt in for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OptIn (pcDbParam AS CHARACTER).

    /*------------------------------------------------------------------------------
        Purpose: Opts out for the given database connection
        Notes:   Based on DBPARAM () function
        @param pcDbParam The DBPARAM value for the DB to opt out for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OptOut (pcDbParam AS CHARACTER).

END INTERFACE.
