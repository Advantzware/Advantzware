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
    File        : dsDockManagerSettings.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 05 20:24:28 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttDockManager NO-UNDO {&REFERENCE-ONLY} 
    FIELD LayoutStyle AS CHARACTER 
    FIELD WindowStyle AS CHARACTER 
    
    .
    
DEFINE {&ACCESS} TEMP-TABLE ttDockAreas NO-UNDO {&REFERENCE-ONLY}
    FIELD PaneType AS CHARACTER 
    FIELD InternalId AS CHARACTER
    FIELD ParentInternalId AS CHARACTER
    FIELD PaneIndex AS INTEGER
    FIELD Order AS INTEGER  
    FIELD DockedLocation AS CHARACTER
    FIELD Key AS CHARACTER
    FIELD AreaText AS CHARACTER 
    FIELD AreaTextTab AS CHARACTER 
    FIELD ChildPaneStyle AS CHARACTER
    FIELD DockedBefore AS CHARACTER
    FIELD Size AS CHARACTER    
    INDEX InternalId IS UNIQUE PRIMARY InternalId  
    INDEX ParentInternalId ParentInternalId Order
    . 
    
DEFINE {&ACCESS} DATASET dsDockManagerSettings {&REFERENCE-ONLY}
    FOR ttDockManager, ttDockAreas 
    DATA-RELATION ParentRelation  
        FOR ttDockAreas, ttDockAreas RECURSIVE
            RELATION-FIELDS (InternalId, ParentInternalId).
         
