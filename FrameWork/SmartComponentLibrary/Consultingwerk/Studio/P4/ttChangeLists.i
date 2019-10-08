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
    File        : ttChangeLists.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Fri Nov 13 11:53:32 CET 2015 
    Created     : Fri Nov 13 11:56:58 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttChangeLists NO-UNDO 
    FIELD ChangeList        AS INTEGER 
    FIELD DateString        AS CHARACTER FORMAT "x(20)"
    FIELD TimeString        AS CHARACTER FORMAT "x(20)"
    FIELD UserName          AS CHARACTER FORMAT "x(40)" 
    FIELD ChangeDescription AS CHARACTER FORMAT "x(60)"
    
    INDEX ChangeList IS PRIMARY UNIQUE ChangeList .
    