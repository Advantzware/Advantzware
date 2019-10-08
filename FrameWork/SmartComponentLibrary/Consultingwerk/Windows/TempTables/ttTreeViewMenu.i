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
    File        : ttTreeViewMenu.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 16 13:48:48 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttTreeViewMenu NO-UNDO {&REFERENCE-ONLY}  
    FIELD NodeKey       AS CHARACTER 
    FIELD ParentNodeKey AS CHARACTER
    FIELD NodeSort      AS INTEGER  
    FIELD NodeText      AS CHARACTER
    FIELD NodeTag       AS CHARACTER 
    FIELD Active        AS LOGICAL INIT TRUE 
    FIELD ImageKey      AS CHARACTER 
    INDEX NodeKey IS PRIMARY UNIQUE NodeKey 
    INDEX ParentNodeKey ParentNodeKey Active NodeSort .
     