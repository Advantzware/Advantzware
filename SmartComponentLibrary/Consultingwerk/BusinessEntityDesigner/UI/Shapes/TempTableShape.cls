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
    File        : TempTableShape
    Purpose     : Extended Shape for Temp-Tables on the Design canvas
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 19:39:17 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Crainiate.ERM4.Table FROM ASSEMBLY .
USING Progress.Lang.*      FROM PROPATH .
USING System.Drawing.*     FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.UI.Shapes.TempTableShape 
    INHERITS Table: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE BUFFER b_eTable FOR eTable . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TempTableShape class                                                                      
        Notes:                                           
        @param dsBusinessEntity DATASET dsBusinessEntity to BIND to, from Consultingwerk\BusinessEntityDesigner\Services\dsBusinessEntity.i
        @param poRowid The ROWID of the eTable record for this shape instance           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TempTableShape (DATASET dsBusinessEntity BIND, 
                                       poRowid AS ROWID):
        SUPER ().
        
        FIND b_eTable WHERE ROWID (b_eTable) = poRowid .
        
        THIS-OBJECT:BackColor     = System.Drawing.Color:White .
        THIS-OBJECT:GradientColor = System.Drawing.Color:FromArgb (96,System.Drawing.SystemColors:Highlight) .
        THIS-OBJECT:Location      = NEW System.Drawing.PointF (b_eTable.LayoutColumn,
                                                               b_eTable.LayoutRow) .
        THIS-OBJECT:Size          = NEW System.Drawing.SizeF (MAX(b_eTable.LayoutWidth, 100),
                                                              b_eTable.LayoutHeight) .
        THIS-OBJECT:Indent = 10.
        THIS-OBJECT:DrawExpand = TRUE .        
        THIS-OBJECT:Tag = STRING (poRowid) .
        THIS-OBJECT:MaximumSize = NEW System.Drawing.SizeF (250, 5000) . 
            
        THIS-OBJECT:DrawShapeDetails () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Drass the details of the Shape                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DrawShapeDetails ():
        
        DEFINE BUFFER b_eField FOR eField . 
        DEFINE BUFFER b_eIndex FOR eIndex . 
        
        DEFINE VARIABLE oFieldGroup AS Crainiate.ERM4.TableGroup NO-UNDO . 
        DEFINE VARIABLE oIndexGroup AS Crainiate.ERM4.TableGroup NO-UNDO .
        DEFINE VARIABLE oRow        AS Crainiate.ERM4.TableRow   NO-UNDO .
        
        DEFINE VARIABLE cBoundFieldIcon AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cCalcFieldIcon  AS CHARACTER NO-UNDO .
        
        IF VALID-OBJECT (THIS-OBJECT:Container) THEN 
            CAST (THIS-OBJECT:Container, Crainiate.ERM4.Model):Suspend () .
        
        FIND CURRENT b_eTable NO-ERROR .
        IF NOT AVAILABLE b_eTable THEN 
            FIND b_eTable WHERE b_eTable.TempTableName = THIS-OBJECT:Heading .  
        
        THIS-OBJECT:Heading    = b_eTable.TempTableName .
        THIS-OBJECT:SubHeading = b_eTable.TempTablePurpose .
        
        THIS-OBJECT:Groups:Clear () .
            
        oFieldGroup = NEW Crainiate.ERM4.TableGroup () .
        oFieldGroup:Text = "Fields"{&TRAN} .
        THIS-OBJECT:Groups:Add (oFieldGroup) .

        oIndexGroup = NEW Crainiate.ERM4.TableGroup () .
        oIndexGroup:Text = "Indexes"{&TRAN} .
        THIS-OBJECT:Groups:Add (oIndexGroup) .

        FILE-INFO:FILE-NAME = "Consultingwerk\BusinessEntityDesigner\UI\Images\table2_selection_column.png":U .

        ASSIGN cBoundFieldIcon = FILE-INFO:FULL-PATHNAME . 
               
        FILE-INFO:FILE-NAME = "Consultingwerk\BusinessEntityDesigner\UI\Images\text_formula.png":U .

        ASSIGN cCalcFieldIcon = FILE-INFO:FULL-PATHNAME . 

        FOR EACH b_eField OF b_eTable BY b_eField.FieldOrder:
            oRow = NEW Crainiate.ERM4.TableRow () .
            
            IF b_eField.FieldExtent > 1 THEN 
                oRow:Text = SUBSTITUTE ("&1 [&2]":U, b_eField.FieldName, b_eField.FieldExtent) .
            ELSE 
                oRow:Text = b_eField.FieldName .
            
            IF b_eField.FieldSource > "":U THEN 
                oRow:Image = NEW Crainiate.ERM4.Image (cBoundFieldIcon) . 
            ELSE  
                oRow:Image = NEW Crainiate.ERM4.Image (cCalcFieldIcon) . 

            oRow:Tag = "eField,":U + STRING (ROWID (b_eField)) .
            oFieldGroup:Rows:Add (oRow) .
        END.       

        FILE-INFO:FILE-NAME = "Consultingwerk\BusinessEntityDesigner\UI\Images\sort_az_descending.png":U . 

        FOR EACH b_eIndex OF b_eTable BY b_eIndex.IndexOrder:
            oRow = NEW Crainiate.ERM4.TableRow () .
            oRow:Text = b_eIndex.IndexName .

            IF b_eIndex.IndexPrimary OR 
               b_eIndex.IndexUnique OR 
               b_eIndex.IndexWordIndex OR 
               b_eIndex.DefaultSearchCodeGeneration THEN 
               
               oRow:Text = oRow:Text + "    ":U + REPLACE (SUBSTITUTE ("(&1&2&3&4)":U,
                                                                       STRING (b_eIndex.IndexPrimary, "P/":U),
                                                                       STRING (b_eIndex.IndexUnique, "U/":U), 
                                                                       STRING (b_eIndex.IndexWordIndex, "W/":U),
                                                                       STRING (b_eIndex.DefaultSearchCodeGeneration, "S/":U)), " ":U, "":U) .   

            IF b_eIndex.IndexPrimary THEN 
                oRow:FontStyle = System.Drawing.FontStyle:Bold .
            ELSE 
                oRow:FontStyle = System.Drawing.FontStyle:Regular .

            IF FILE-INFO:FULL-PATHNAME <> ? THEN 
                oRow:Image = NEW Crainiate.ERM4.Image (FILE-INFO:FULL-PATHNAME) . 

            oRow:Tag = "eIndex,":U + STRING (ROWID (b_eIndex)) .
            oIndexGroup:Rows:Add (oRow) .
        END.       

        IF VALID-OBJECT (THIS-OBJECT:Container) THEN DO:
            CAST (THIS-OBJECT:Container, Crainiate.ERM4.Model):Resume () .
            CAST (THIS-OBJECT:Container, Crainiate.ERM4.Model):Refresh () .
        END.

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Raises the LocationChanged event
	    Notes:
	    @param poLocation The location for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLocationChanged (poLocation AS PointF):
		
		SUPER:OnLocationChanged (poLocation) .

        FIND CURRENT b_eTable NO-ERROR .
        IF NOT AVAILABLE b_eTable THEN 
            FIND b_eTable WHERE b_eTable.TempTableName = THIS-OBJECT:Heading .  

        ASSIGN b_eTable.LayoutRow    = THIS-OBJECT:Y
               b_eTable.LayoutColumn = THIS-OBJECT:X
               .

	END METHOD.

END CLASS.
