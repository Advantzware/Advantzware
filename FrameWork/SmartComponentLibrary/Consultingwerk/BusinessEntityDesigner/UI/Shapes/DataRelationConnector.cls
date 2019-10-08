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
    File        : DataRelationConnector
    Purpose     : Represents an eDataRelation record on the model
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 16 21:16:25 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Crainiate.ERM4.Connector.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.BusinessEntityDesigner.UI.Shapes.DataRelationConnector INHERITS Connector: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE BUFFER b_eDataRelation FOR eDataRelation . 

    DEFINE VARIABLE oModel AS Crainiate.ERM4.Model NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Data-Relation is active                                                                         
        Notes:                                                  
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Active AS LOGICAL INITIAL TRUE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataRelationConnector class                                                                         
        Notes:                                                  
        @param dsBusinessEntity The Business Entity Dataset to BIND to
        @param poRowid The ROWID of the eDataRelation record that is represented by this shape
        @param poModel The reference to the Crainiate Model class                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DataRelationConnector (DATASET dsBusinessEntity BIND, 
                                              poRowid AS ROWID,
                                              poModel AS Crainiate.ERM4.Model):
        SUPER ().

        oModel = poModel .
        
        FIND b_eDataRelation WHERE ROWID (b_eDataRelation) = poRowid .
        
        THIS-OBJECT:DrawShapeDetails () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Draws the Shape                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DrawShapeDetails ():

        DEFINE BUFFER b_ParentTable FOR eTable .
        DEFINE BUFFER b_ChildTable FOR eTable . 

        FIND CURRENT b_eDataRelation NO-ERROR. 
        IF NOT AVAILABLE b_eDataRelation THEN 
            FIND b_eDataRelation WHERE b_eDataRelation.DataRelationName = CAST (THIS-OBJECT:Label, Crainiate.ERM4.TextLabel):Text.  

        FIND b_ParentTable WHERE b_ParentTable.TempTableName = b_eDataRelation.ParentTempTableName . 
        FIND b_ChildTable WHERE b_ChildTable.TempTableName = b_eDataRelation.ChildTempTableName . 

        THIS-OBJECT:Start:Shape = CAST(oModel:Shapes [STRING(ROWID (b_ParentTable))],
                                       Crainiate.ERM4.Shape) .
        THIS-OBJECT:Start:AllowMove = FALSE . 
                                       
        THIS-OBJECT:End:Shape = CAST(oModel:Shapes [STRING(ROWID (b_ChildTable))],
                                     Crainiate.ERM4.Shape) .
        THIS-OBJECT:End:AllowMove = FALSE . 
        THIS-OBJECT:End:Marker = NEW Crainiate.ERM4.Marker (Crainiate.ERM4.MarkerStyle:Rectangle) .

        THIS-OBJECT:Label = NEW Crainiate.ERM4.TextLabel (b_eDataRelation.DataRelationName) .

        THIS-OBJECT:End:Marker = NEW Crainiate.ERM4.Arrow () .
        THIS-OBJECT:End:Marker:Height = 16 .
        THIS-OBJECT:End:Marker:Width = 16 .
        
        IF b_eDataRelation.DataRelationNotActive = TRUE THEN 
            ASSIGN THIS-OBJECT:BorderColor            = System.Drawing.Color:Red 
                   THIS-OBJECT:End:Marker:BackColor   = System.Drawing.Color:Red
                   THIS-OBJECT:End:Marker:BorderColor = System.Drawing.Color:Red
                   THIS-OBJECT:Active                 = FALSE 
                   . 
        ELSE 
            ASSIGN THIS-OBJECT:BorderColor            = System.Drawing.Color:Black 
                   THIS-OBJECT:End:Marker:BackColor   = System.Drawing.Color:Black
                   THIS-OBJECT:End:Marker:BorderColor = System.Drawing.Color:Black
                   THIS-OBJECT:Active                 = TRUE 
                   . 
        
/*        THIS-OBJECT:End:Marker:DrawBackground = TRUE .*/

    END METHOD.

END CLASS.
