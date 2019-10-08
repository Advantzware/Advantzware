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
    File        : ttInclude.i
    Purpose     : Definition for temp-table of include file references

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 18 07:11:32 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttIncludeFile NO-UNDO 
    FIELD SourceFileName  AS CHARACTER FORMAT "x(60)":U 
    FIELD IncludeFileName AS CHARACTER FORMAT "x(60)":U
    FIELD LineNum         AS INTEGER 
    INDEX SourceFileName IS PRIMARY UNIQUE SourceFileName IncludeFileName LineNum .
