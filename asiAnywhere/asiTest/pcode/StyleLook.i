
/*------------------------------------------------------------------------
    File        : StyleLook Look
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Item Inquiry

    Author(s)   : Kuldeep
    Created     : Feb 20,
     2008
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */       
                           
DEFINE TEMP-TABLE ttStyleLook NO-UNDO 
     FIELD style LIKE style.style
     FIELD StyleDscr LIKE style.dscr 
     FIELD design_no  LIKE  box-design-hdr.design-no 
     FIELD BoxDscr LIKE box-design-hdr.description
     FIELD est_no like box-design-hdr.est-no  
     FIELD form_no LIKE  box-design-hdr.form-no 
     FIELD blank_no  LIKE  box-design-hdr.blank-no 
     FIELD box_3d_image LIKE box-design-hdr.box-3d-image
     FIELD box_design   AS CHAR 
                           .


DEFINE DATASET dsStyleLook FOR ttStyleLook.

DEFINE QUERY q-StyleLookQuery FOR ttStyleLook.

DEFINE DATA-SOURCE src-StyleLook FOR QUERY q-StyleLookQuery.

BUFFER ttStyleLook:ATTACH-DATA-SOURCE(DATA-SOURCE src-StyleLook:HANDLE).

