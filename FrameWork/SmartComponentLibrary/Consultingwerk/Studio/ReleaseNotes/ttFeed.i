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
    File        : ttFeed.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Sat Oct 03 18:28:38 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttFeed NO-UNDO 
    FIELD EntryTitle AS CHARACTER 
    FIELD EntryLink  AS CHARACTER
    FIELD Categories AS CHARACTER
    FIELD Published  AS CHARACTER 
    INDEX EntryTitle EntryTitle  . 
