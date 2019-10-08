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
    File        : ttSmartBusinessEntityLookup.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 07 12:09:14 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttSmartBusinessEntityLookup NO-UNDO {&REFERENCE-ONLY}

    FIELD AdapterType                   AS CHARACTER
    FIELD AppServerPartition            AS CHARACTER 
    FIELD LookupBrowserFields           AS CHARACTER 
    FIELD LookupControls                AS CHARACTER 
    FIELD LookupDialogClassName         AS CHARACTER 
    FIELD LookupDialogFilterFields      AS CHARACTER 
    FIELD LookupDialogFilterOperators   AS CHARACTER 
    FIELD LookupDialogQuerySort         AS CHARACTER 
    FIELD LookupDialogQueryString       AS CHARACTER 
    FIELD LookupDialogTitle             AS CHARACTER 
    FIELD LookupEntityName              AS CHARACTER 
    FIELD LookupEntityTable             AS CHARACTER  
    FIELD LookupEntityView              AS CHARACTER 
    FIELD LookupFields                  AS CHARACTER 
    FIELD LookupKeyField                AS CHARACTER 
    FIELD LookupKeyValueColumn          AS CHARACTER 
    FIELD LookupQuerySort               AS CHARACTER 
    FIELD LookupQueryString             AS CHARACTER 
    FIELD LookupTimerDelay              AS INTEGER 
    
    INDEX LookupEntityTable LookupEntityTable . 