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
    File        : ISupportsReposition
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 28 16:53:41 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISupportsReposition:

    /**
     * Purpose: Returns the handle to the Query used to navigate the selected tables
     *          of the ProDataset instance managed by the DataAdapter
     * Notes:
     */
    DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO
    GET .

    /**
     * Purpose: Repositions to a single Rowid
     * Notes:
     * @param prRowid The ROWID to position the query to
     */
    METHOD PUBLIC VOID RepositionToRowid (prRowid AS ROWID).

    /**
     * Purpose: Repositions to a Rowid list from a character
     * Notes:
     * @param pcRowids The comma delimited list of ROWIDs to position the query to
     */
    METHOD PUBLIC VOID RepositionToRowid (pcRowids AS CHARACTER).

    /**
     * Purpose: Repositions to a Rowid Array
     * Notes:
     * @param prRowid The array of ROWIS's to reposition the query to
     */
    METHOD PUBLIC VOID RepositionToRowid (prRowid AS ROWID EXTENT).

END INTERFACE.
