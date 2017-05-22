/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ttEntries.i
    Purpose     : Temp-Table of internal entries of a procedure

    Syntax      :

    Description : Used by the LibraryProcedureParser class

    Author(s)   :
    Created     : Wed Jul 06 14:08:57 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEntries NO-UNDO
    FIELD EntryType /* PROCEDURE / FUNCTION */ AS CHARACTER
    FIELD EntryName                            AS CHARACTER
    FIELD EntryParameters                      AS CHARACTER
    FIELD EntryReturn                          AS CHARACTER

    INDEX EntryName EntryName .
