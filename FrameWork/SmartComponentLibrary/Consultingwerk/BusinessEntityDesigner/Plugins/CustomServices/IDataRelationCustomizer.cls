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
    File        : IDataRelationCustomizer
    Purpose     : Allows customization of an eDataRelation record after
                  it has been created 
    Syntax      : Service Interface
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 20 22:48:38 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH . 

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.IDataRelationCustomizer:  
  
    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }  

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Data Relation Record
        Notes:   Use the proDataRelation ROWID to locate the added ROWID
        @param proDataRelation The ROWID of the Data Relation record
        @param dsBusinessEntity Dataset with the current Business Entity model  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CustomizeDataRelation (proDataRelation AS ROWID,    
                                              DATASET FOR dsBusinessEntity).

END INTERFACE.