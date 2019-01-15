<%@ Page Language="C#" AutoEventWireup="true" Debug="true" MasterPageFile="~/MasterPagestimate.master" Inherits="view_entry_estimate" Title="View Entry Estimate" Codebehind="~/view_item_estimate.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script language="javascript"  src="include/CalendarControl.js" > </script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>

<script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmAdd() {
        var retVal = makeMsgBox("Confirmation", "Do you want to Add another Record?", 48, 4, 256, 4096);     
        if (retVal == 6) {
            window.location.href = 'view_item_estimate.aspx?mode=insert';
        }
        else {
            window.location.href = 'view_item_estimate.aspx?mode=mail';
        }            
    }
</script>

<script language="javascript" type="text/javascript">

window.onload=setfocusinit;
function setfocusinit()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox") )
    {      
       if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox").disabled!=true)
        {  
          
        
            if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox") && document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox").value == ""  )
            {
                var quote=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox");
                quote.focus();
            }

        }
        else
        {      
             var fgitem=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox");
                fgitem.focus();
        }
    } 
    else 
    {
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox") &&  document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox").disabled==true )
        {
        
            var custpart=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox");
                custpart.focus();
        }    
           
           
    }   
}
function setqtyfocus()
{
//    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Name1TextBox").disabled==true)
//    {
//        var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
//        qty.focus();
//    }
}
function setcustfocus()
{
//    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_DscrTextBox").disabled==true)
//    {
//        var cpo=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_custpoTextBox");
//        cpo.focus();
//    }
}

function preLeave( fieldObj, fieldType, fieldFormat ){
    fieldObj.style.backgroundColor='Window';
    fieldObj.style.color='WindowText';
    
  fieldType = fieldType.toLowerCase();
 
  if((fieldType == "") || (fieldType == "text")){
     leaveField( fieldObj );
  }

if(fieldType == "date"){
     if(fieldFormat == ""){ var dateFormat = "99/99/9999";
     }else{ var dateFormat = fieldFormat; }
     checkDate(dateFormat,fieldObj,'01/01/1950','12/31/3000',0);
  } 

if(fieldType == "number"){
     if(fieldFormat == ""){ var numFormat = "(>>>>9)";
     }else{ var numFormat = fieldFormat; }
     checkNum(numFormat,fieldObj,'?','?',0);
  }      
}

function preEnter( fieldObj, canEdit ){
    fieldObj.style.backgroundColor='blue';
    fieldObj.style.color = 'white';
  if(canEdit == "no"){
     fieldObj.blur();
     leaveField( fieldObj );      
  }
 
  enterField( fieldObj );
  return;
}

function focusval(obj)
{
    obj.style.backgroundColor='blue';
    obj.style.color = 'white';
}
function blurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
}
function duedatecal()
{
    var date = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox");
     date.style.backgroundColor='blue';
     date.style.color = 'white';
     showCalendarControl(date);
}
function quoteblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';


}
function inameblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';  
}

function descblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    var price=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_priceTextBox");
    price.focus();
}
function underblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
   
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox") )
    {
      
       if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox").disabled!=true)
        {   
        
            if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox"))
            {
                var quote=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox");
                quote.focus();
            }
            else if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox"))
            {
                var custpart=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox");
                custpart.focus();
            }
        }
        else
        {              
            var fgitem=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox");
            fgitem.focus();
        }
    } 
    else 
    {
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox"))
        {
            var custpart=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox");
                custpart.focus();
        }               
    }   
      
}
function duedateval()
{
    var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox").value;
    
    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox").value = duedate + "/";
    }
    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox").value = duedate + "/";
    }
}


function grater(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    var caldis = 0 ;  
    var val = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
    if(val.value<=0)
   {
     alert("Quantity must be greater than 0");
     val.focus();        
   } 
   else 
   {
        var discount = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_discountTextBox");
        var uom = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_TextBox1");
        var casunit  = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_counterTextBox");
        var hidtot = document.getElementById("ctl00_ContentPlaceHolder1_Hiddentotal");
        var price = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_priceTextBox");    
        var total = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox");
        if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1)
        {
            if(uom.value == "CS" || uom.value == "cs"  && casunit.value != 0)
            {        
                total.value = price.value * val.value / casunit.value ; 
                    if(discount.value >= 1)
                    {
                        caldis =  total.value *  discount.value / 100; 
                        total.value = total.value - caldis; 
                    }
                hidtot.value = total.value ; 
                uom.value = "CS";
           } 
           else if(uom.value == "C" || uom.value == "c")
            {
                total.value = price.value * val.value / 100;  
                if(discount.value >= 1)
                    {
                        caldis =  total.value *  discount.value / 100; 
                        total.value = total.value - caldis; 
                    }
                hidtot.value = total.value; 
                uom.value = "C";    
            } 
            else if(uom.value == "M" || uom.value == "m")
                {    
                    total.value = price.value * val.value / 1000; 
                    if(discount.value >= 1)
                        {    
                            caldis =  total.value *  discount.value / 100;        
                            total.value = total.value - caldis; 
                        }
                    hidtot.value = total.value ; 
                    uom.value = "M";
                }  
                else
                    {
                        total.value = price.value * val.value ; 
                        if(discount.value >= 1)
                            {
                                caldis =  total.value *  discount.value / 100; 
                                total.value = total.value - caldis; 
                            }
                       hidtot.value =  total.value ; 
                    }  
          }
          else
            {
                var hiduom= document.getElementById("ctl00_ContentPlaceHolder1_HiddenTextBox1");
                var  hidcounter = document.getElementById("ctl00_ContentPlaceHolder1_Hiddencounter");
                if(hiduom.value == "CS" || hiduom.value == "cs" && hidcounter.value != 0)
                    {
                        total.value = price.value * val.value / hidcounter.value ; 
                        if(discount.value >= 1)
                            {
                                caldis =  total.value *  discount.value / 100; 
                                total.value = total.value - caldis; 
                            }
                        hidtot.value = total.value ; 
                    } 
                else if(hiduom.value == "C" || hiduom.value == "c")
                    {
                        total.value = price.value * val.value / 100;  
                        if(discount.value >= 1) 
                            {
                                caldis =  total.value *  discount.value / 100; 
                                total.value = total.value - caldis; 
                            }
                        hidtot.value = total.value; 
                    } 
                    else if(hiduom.value == "M" || hiduom.value == "m")
                        {
                            total.value = price.value * val.value / 1000; 
                            if(discount.value >= 1) 
                                {
                                    caldis =  total.value *  discount.value / 100; 
                                    total.value = total.value - caldis; 
                                }
                            hidtot.value =total.value; 
                        }  
                        else
                            {
                                total.value = price.value * val.value ; 
                                if(discount.value >= 1)
                                    {
                                        caldis =  total.value *  discount.value / 100; 
                                        total.value = total.value - caldis; 
                                    }
                                hidtot.value = total.value ; 
                             }   
                      }
                      var dotpr = price.value;  
                      if(dotpr.indexOf(".") != -1)
                        {        
                            return;
                        } 
                        else if(dotpr.length > 7 && dotpr.length < 9)
                            dotpr=dotpr + ".";
                       document.getElementById("ctl00_ContentPlaceHolder1_FormView1_priceTextBox").value = dotpr;
                 } 
//                 if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_InsertButton"))
//                 {
//                    var save=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_InsertButton");
//                    save.focus()
//                 }
//                 if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_UpdateButton"))
//                 {
//                    var update=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_UpdateButton");
//                    update.focus()
//                 }    
  }
  
// function blankquantity()
// {
//    var val = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
//    if(val.value<=0)
//    {
//        alert("Quantity must be greater than 0");
//        val.focus(); 
//    } 
// }

 function QuantityLook()
 {
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox"))
    {
        var est1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox").value;
    }
    else
    {
        var est1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoLabel").innerText;        
    }
    var item1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox").value;
    var custpart1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox").value;
    var NewWindow = window.open("item_quantity_lookup.aspx?est="+est1+"&item="+item1+"&custpart="+custpart1+"","typeordLookupWindow","width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function QuantityLookup(ReturnObj1,ReturnObj2,ReturnObj3)
 {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_quantityTextBox.value=ReturnObj1; 
    document.forms[0].ctl00_ContentPlaceHolder1_Hiddenquantity.value = ReturnObj1;  
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value=ReturnObj2; 
    document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = ReturnObj2;  
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1.value=ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenTextBox1.value = ReturnObj3;
    document.getElementById('ctl00_ContentPlaceHolder1_FormView1_quantityTextBox').focus(); 
    
    var discount = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_discountTextBox");
    var hidcounter = document.getElementById("ctl00_ContentPlaceHolder1_Hiddencounter");
    var hidtot = document.getElementById("ctl00_ContentPlaceHolder1_Hiddentotal"); 
    var total = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox");
    var casunit  = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_counterTextBox");
    
    if(ReturnObj3 == "CS" || ReturnObj3 == "cs"  && casunit.value != 0)
            {        
                total.value = ReturnObj2 * ReturnObj1 / casunit.value ; 
                    if(discount.value >= 1)
                    {
                        caldis =  total.value *  discount.value / 100; 
                        total.value = total.value - caldis; 
                    }
                hidtot.value = total.value ;                 
           } 
           else if(ReturnObj3 == "C" || ReturnObj3 == "c")
            {
                total.value = ReturnObj2 * ReturnObj1 / 100;  
                if(discount.value >= 1)
                    {
                        caldis =  total.value *  discount.value / 100; 
                        total.value = total.value - caldis; 
                    }
                hidtot.value = total.value;                    
            } 
            else if(ReturnObj3 == "M" || ReturnObj3 == "m")
                {    
                    total.value = ReturnObj2 * ReturnObj1 / 1000; 
                    if(discount.value >= 1)
                        {    
                            caldis =  total.value *  discount.value / 100;        
                            total.value = total.value - caldis; 
                        }
                    hidtot.value = total.value ;                     
                }  
                else
                    {
                        total.value = ReturnObj2 * ReturnObj1 ; 
                        if(discount.value >= 1)
                            {
                                caldis =  total.value *  discount.value / 100; 
                                total.value = total.value - caldis; 
                            }
                       hidtot.value =  total.value ; 
                    }  
             if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox").value !="")
                {
                    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox").disabled!=true)
                        {
                            var fgitem=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox");
                            fgitem.focus();
                        }
                    else if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox"))
                        {
                            var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
                            qty.focus();
                        }
                }
            else
            {
                if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox").disabled!=true)
                {
                var fgitem=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox");
                fgitem.focus();
                }
                else
                {
                var custpart = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox");
                custpart.focus();
                }
            } 
 }
 
 function PriceLook()
 {
   
   var item1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox").value;
   var uom1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_TextBox1").value;
    var NewWindow = window.open("price_lookup.aspx?item="+item1+"&uom="+uom1+"","typeordLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
 function PriceLookup(ReturnObj1)
 {
   var qty =document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
   var discount = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_discountTextBox");
   var hidtot = document.getElementById("ctl00_ContentPlaceHolder1_Hiddentotal"); 
   var total = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox");
   var uom  = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_TextBox1");
   
   var countprice = 0;
   var caldis =0;
   if( uom.value == "M")
   {
  
   countprice = ReturnObj1 * 1000; 
   
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value=countprice.toFixed(4);
   document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = countprice.toFixed(4);
   document.getElementById('ctl00_ContentPlaceHolder1_FormView1_priceTextBox').focus(); 
                 
                                
                 total.value = (countprice * qty.value / 1000).toFixed(4); 
                    if(discount.value >= 1)
                        {    
                            caldis =  total.value *  discount.value / 100;        
                            total.value = total.value - caldis; 
                        }
                    hidtot.value = total.value ;  
   
   }  
   else if(uom.value == "C")
   {
   countprice = ReturnObj1 * 100;
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value=countprice.toFixed(4);
   document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = countprice.toFixed(4);
   document.getElementById('ctl00_ContentPlaceHolder1_FormView1_priceTextBox').focus(); 
   
    total.value = (countprice * qty.value / 100).toFixed(4);  
                if(discount.value >= 1)
                    {
                        caldis =  total.value *  discount.value / 100; 
                        total.value = total.value - caldis; 
                    }
                hidtot.value = total.value; 
   
   }
   else
   {
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value=ReturnObj1;
   document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = ReturnObj1;
   document.getElementById('ctl00_ContentPlaceHolder1_FormView1_priceTextBox').focus(); 
   
    total.value = (qty.value * ReturnObj1).toFixed(4) ; 
                        if(discount.value >= 1)
                            {
                                caldis =  total.value *  discount.value / 100; 
                                total.value = total.value - caldis; 
                            }
                       hidtot.value =  total.value ; 
   }
   
   
    
 }
  
function estupLook()
{ 
  var NewWindow = window.open("itemupdate_lookup.aspx","typeordLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ItemUpdateLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14,ReturnObj15,ReturnObj16,ReturnObj17,ReturnObj18,ReturnObj19,ReturnObj20,ReturnObj21,ReturnObj22,ReturnObj23,ReturnObj24,ReturnObj25,ReturnObj26,ReturnObj27,ReturnObj28,ReturnObj29,ReturnObj30,ReturnObj31,ReturnObj32)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_est_noTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenestno.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Item1TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFielditem.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustpart.value = ReturnObj3;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Name1TextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFieldname.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DscrTextBox.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Dscr2TextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc2.value = ReturnObj6; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_job_noTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenjob.value = ReturnObj7; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_job_noTextBox2.value = ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenjob2.value = ReturnObj8; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_quantityTextBox.value = ReturnObj9;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenquantity.value = ReturnObj9; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value = ReturnObj11;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = ReturnObj11; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1.value = ReturnObj12;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTextBox1.value = ReturnObj12; 
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox) 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox.value = ReturnObj15;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencasunit.value = ReturnObj15;  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpctTextBox.value = ReturnObj16;
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenspct.value = ReturnObj16;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSmanTextBox.value = ReturnObj17;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensman.value = ReturnObj17;  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vScommTextBox.value = ReturnObj20;
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencomm.value = ReturnObj20;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSnameTextBox.value = ReturnObj21;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensname.value = ReturnObj21;    
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox.value = ReturnObj23;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddentotal.value = ReturnObj23;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_discountTextBox.value = ReturnObj24;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddendiscount.value = ReturnObj24;    
//  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DscrTextBox.value = ReturnObj25;
  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox.value = ReturnObj26;
 // document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpctTextBox.value = ReturnObj27;
  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_custpoTextBox.value = ReturnObj28;
  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox.value = ReturnObj29;  
  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_promisdateTextBox.value = ReturnObj30;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vOverTextBox.value = ReturnObj31;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenover.value = ReturnObj31;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vUnderTextBox.value = ReturnObj32;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenunder.value = ReturnObj32;   
  
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddentax.value = ReturnObj14;  
  if(ReturnObj14 == "Yes")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_taxableCheckBox.checked = true;
//  if(ReturnObj22=="O")
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 0;
//  if(ReturnObj22=="C")
//  document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 1;
//  if(ReturnObj22=="N")
//  document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 2;
//  if(ReturnObj22=="Q")
//  document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 3;
//  if(ReturnObj22=="R")
//  document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 4;
//  if(ReturnObj22=="T")
//  document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 5;
//  if(ReturnObj22=="X")
//  document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 6;
 }

function FGItemLook()
{ 
  var qty1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox").value;
  var quote1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox").value;
  var NewWindow = window.open("fg_lookup.aspx?qty="+qty1+"&quote="+quote1+"","typeordLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function FGItemLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14,ReturnObj15)
{ 
if(ReturnObj1.indexOf(":"))
{
    var val=ReturnObj1;    
    ReturnObj1=val.replace(":", "\"");    
}
if(ReturnObj2.indexOf(":"))
{
    var val2=ReturnObj2;    
    ReturnObj2=val2.replace(":", "\"");    
}

if(ReturnObj4.indexOf(":"))
{
    var val4=ReturnObj4;    
    ReturnObj4=val4.replace(":", "\"");    
}
if(ReturnObj5.indexOf(":"))
{
    var val5=ReturnObj5;    
    ReturnObj5=val5.replace(":", "\"");    
}
if(ReturnObj6.indexOf(":"))
{
    var val6=ReturnObj6;    
    ReturnObj6=val6.replace(":", "\"");    
}
if(ReturnObj7.indexOf(":"))
{
    var val7=ReturnObj7;    
    ReturnObj7=val7.replace(":", "\"");    
}

  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Item1TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFielditem.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Name1TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFieldname.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustpart.value = ReturnObj4;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DscrTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Dscr2TextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc2.value = ReturnObj7; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value = ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = ReturnObj8; 
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1)
      document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1.value = ReturnObj9;   
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTextBox1.value = ReturnObj9; 
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_counterTextBox)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_counterTextBox.value = ReturnObj10; 
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencounter.value = ReturnObj10; 
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox) 
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox.value = ReturnObj11; 
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencasunit.value = ReturnObj11;  
  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox.value = ReturnObj12; 
  //document.forms[0].ctl00_ContentPlaceHolder1_Hiddentotal.value = ReturnObj12;  
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_CostTextBox)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_CostTextBox.value = ReturnObj13;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencost.value = ReturnObj13;   
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_discountTextBox.value = ReturnObj15;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddendiscount.value = ReturnObj15;       
   document.forms[0].ctl00_ContentPlaceHolder1_HiddenDropdown1.value = ReturnObj14;
  if(ReturnObj14=="O")
  {
    if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList1)
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 0;
  }
  if(ReturnObj14=="C")
  {
    if(document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1)
        document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 1;
  }
  if(ReturnObj14=="N")
  {
    if(document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1)
        document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 2;
  }
  if(ReturnObj14=="Q")
  {
    if(document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1)
        document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 3;
  }
  if(ReturnObj14=="R")
  {
    if(document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1)
        document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 4;
  }
  if(ReturnObj14=="T")
  {
    if(document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1)
        document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 5;
  }
  if(ReturnObj14=="X")
  {
    if(document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1)
        document.forms[0].ct100_ContentPlaceHolder1_FormView1_DropDownList1.SelectedIndex = 6;
  }
   var caldis = 0 ; 
   var val = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
   var hidtot = document.getElementById("ctl00_ContentPlaceHolder1_Hiddentotal");    
   var total = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox");
  
   if(ReturnObj9 == "CS" && ReturnObj10 != 0)
   {
     total.value =(ReturnObj8 * val.value / ReturnObj10)  ;
        if(ReturnObj15 >= 1)
            {
                caldis = total.value * ReturnObj15 /100 ;  
                total.value = total.value - caldis;     
            }
            hidtot.value = total.value ; 
   } 
   else if(ReturnObj9 == "C" )
    {
        total.value = ReturnObj8 * val.value / 100 ;
        if(ReturnObj15 >= 1)
        {
            caldis = total.value * ReturnObj15 /100 ;  
            total.value = total.value - caldis;       
        }
        hidtot.value = total.value; 
    } 
    else if(ReturnObj9 == "M" )
    {
        total.value = ReturnObj8 * val.value / 1000 ; 
        if(ReturnObj15 >= 1)
        {
            caldis = total.value * ReturnObj15 /100 ;  
            total.value = total.value - caldis;     
        }
        hidtot.value = total.value; 
    } 
    else
    {
        total.value = ReturnObj8 * val.value ; 
        if(ReturnObj15 >= 1)
            {
                caldis = total.value * ReturnObj15 /100 ;  
                total.value = total.value - caldis;     
            }
            hidtot.value = total.value ; 
    }
    var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
    qty.focus();
 }
 
 function ItemUpdateLook()
 { 
     var NewWindow = window.open("itemupdate_lookup.aspx","ItemUpdateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 } 
 
 function BoardPoLook()
 { 
     var NewWindow = window.open("boardpo_lookup.aspx","BoardPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function BoardPoLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vPonoTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vBoardVenTextBox.value = ReturnObj2;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_vPonoTextBox').focus();
}
  
function salesreplook()
{
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSmanTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensman.value = ReturnObj1;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSnameTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensname.value = ReturnObj2;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_vSmanTextBox').focus();    
}

function smancopylook1()
{ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSman2TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensman2.value = ReturnObj1;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSname2TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensname2.value = ReturnObj2;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_vSman2TextBox').focus();  
}

function salesmanlook()
{ 
  var NewWindow = window.open("salesman_lookup.aspx","SalesManLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup1(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSman3TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensman3.value = ReturnObj1;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSname3TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddensname3.value = ReturnObj2;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_vSman3TextBox').focus();   
}
function uomlook()
{ 
  var NewWindow = window.open("Uom_lookup.aspx","UomLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function UomLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTextBox1.value = ReturnObj1;  
 // document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSname3TextBox.value = ReturnObj2;
}

function Datelook()
{ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox.value="";
  Datelook();
}
function Date2look()
{ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup2(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_promisdateTextBox.value=obj;
}

function Datelook2()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_promisdateTextBox.value="";
  Date2look();
} 
 
 function custpartlook()
 { 
    var fgitem = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Item1TextBox").value;
    var NewWindow = window.open("custpart_lookup.aspx?item="+fgitem+"","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }

function CustPartLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12)
{ 
  if(ReturnObj1.indexOf(":"))
  {
    var val=ReturnObj1;    
    ReturnObj1=val.replace(":", "\"");
    }    

  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustpart.value = ReturnObj1;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox').focus();  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DscrTextBox.value = ReturnObj2;
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc.value = ReturnObj2;  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Dscr2TextBox.value = ReturnObj3; 
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc2.value = ReturnObj3;   
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Item1TextBox.value = ReturnObj4;
//  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFielditem.value = ReturnObj4;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Name1TextBox.value = ReturnObj5;
//  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFieldname.value = ReturnObj5;
//  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1)
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1.value = ReturnObj6;
//  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTextBox1.value = ReturnObj6;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value = ReturnObj7;
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = ReturnObj7;
//  
//  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Dscr2TextBox.value = ReturnObj8;  
//  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_est_noTextBox)
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_est_noTextBox.value = ReturnObj9;
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenestno.value = ReturnObj9;
//  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Name1TextBox.value = ReturnObj10;
//  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_counterTextBox)
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_counterTextBox.value = ReturnObj11;
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencounter.value = ReturnObj11;
//  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox)
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox.value = ReturnObj12;
//  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencasunit.value = ReturnObj12;
//  
//   var val = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
//   var hidtot = document.getElementById("ctl00_ContentPlaceHolder1_Hiddentotal");    
//    var total = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox");
//  
//   if(ReturnObj6 == "CS" && ReturnObj11 != 0)
//   {
//    total.value = ReturnObj7 * val.value / ReturnObj11 ;       
//    hidtot.value = ReturnObj7 * val.value / ReturnObj11 ; 
//    } 
//   else if(ReturnObj6 == "C" )
//   {
//    total.value = ReturnObj7 * val.value / 100 ;    
//    hidtot.value = ReturnObj7 * val.value / 100; 
//    } 
//   else if(ReturnObj6 == "M" )
//   {
//    total.value = ReturnObj7 * val.value / 1000 ;    
//    hidtot.value = ReturnObj7 * val.value / 1000; 
//    } 
//    else
//    {
//    total.value = ReturnObj7 * val.value ;    
//    hidtot.value = ReturnObj7 * val.value ; 
//    }
  
}

function estlook()
{
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_est_noTextBox.value="";
    estupLook();
}

function fglook()
{
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Item1TextBox.value="";
    FGItemLook();
}

function valea(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    
    var val = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
    var ea = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vTypeTextBox");
    var hidtot = document.getElementById("ctl00_ContentPlaceHolder1_Hiddentotal");
    var price = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_priceTextBox");
    var total = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox");
    if(ea.value == "M" || ea.value == "m")
    {
        val.value = val.value * 1000;
        total.value = price.value * val.value;
        hidtot.value = price.value * val.value;
        ea.value = "EA";
        val.focus(); 
    }
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Name1TextBox").disabled==true)
    {
        var custpo=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_custpoTextBox");
        custpo.focus(); 
    }
    else
    {
        var iname=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Name1TextBox");
        iname.focus();
    }
}
function valoverrun()
{
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vOverTextBox").value;
    if(frontback.indexOf(".") != -1)
    {        
        return;
    } 
    else if(frontback.length > 1 && frontback.length < 3)
        frontback=frontback + ".";
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vOverTextBox").value = frontback;
}
function valunderrun()
{
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vUnderTextBox").value;
    if(frontback.indexOf(".") != -1)
    {        
        return;
    } 
    else if(frontback.length > 1 && frontback.length < 3)
        frontback=frontback + ".";
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vUnderTextBox").value = frontback;
}
    
function setfocus(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';    
//         var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");        
//         qty.focus();        
}
function setfocus2(obj)
{
     obj.style.backgroundColor='Window';
     obj.style.color='WindowText';
    //var desc=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Name1TextBox");        
    //desc.focus();
}
function setfocus3(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    var po=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_custpoTextBox");
    //var desc=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Name1TextBox");
    po.focus();
}
function setfocus4(obj)
{
     obj.style.backgroundColor='Window';
     obj.style.color='WindowText';
     var pr1=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_DropDownList2");
     //var desc=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Name1TextBox");
     pr1.focus();
 }
 function setfocus5(obj)
 {
     obj.style.backgroundColor='Window';
     obj.style.color='WindowText';
     var ove=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vOverTextBox");
     //var desc=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Name1TextBox");
     ove.focus();
 }
  
function ItemQuoteLook()
{ 
  var quote1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox").value;
  var cust1 = document.getElementById("ctl00_ContentPlaceHolder1_cust_label").innerText;
  
  var NewWindow = window.open("item_qut_look.aspx?quote="+quote1+"&cust="+cust1+"","QuoteLookupWindow","width=720,height=450,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  
}
function ItemQuoteLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14,ReturnObj15,ReturnObj16,ReturnObj17,ReturnObj18)
{ 
if(ReturnObj2.indexOf(":"))
{
    var val=ReturnObj2;    
    ReturnObj2=val.replace(":", "\"");    
}
if(ReturnObj6.indexOf(":"))
{
    var val6=ReturnObj6;    
    ReturnObj6=val6.replace(":", "\"");
     
}
if(ReturnObj7.indexOf(":"))
{
    var val2=ReturnObj7;    
    ReturnObj7=val2.replace(":", "\"");    
}

if(ReturnObj9.indexOf(":"))
{
    var val4=ReturnObj9;    
    ReturnObj9=val4.replace(":", "\"");    
}
if(ReturnObj10.indexOf(":"))
{
    var val5=ReturnObj10;    
    ReturnObj10=val5.replace(":", "\"");    
}



  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vQnoTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenQuoteNum.value = ReturnObj1;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Item1TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFielditem.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Name1TextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFieldname.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_CustPartTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustpart.value = ReturnObj6;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DscrTextBox.value = ReturnObj9;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc.value = ReturnObj9;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_Dscr2TextBox.value = ReturnObj10;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddencustdesc2.value = ReturnObj10; 
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenEstmate.value = ReturnObj8;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_priceTextBox.value = ReturnObj11;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddenprice.value = ReturnObj11; 
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1)
      document.forms[0].ctl00_ContentPlaceHolder1_FormView1_TextBox1.value = ReturnObj12;   
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTextBox1.value = ReturnObj12; 
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_quantityTextBox)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_quantityTextBox.value = ReturnObj15;  
  
  //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox.value = ReturnObj12; 
  //document.forms[0].ctl00_ContentPlaceHolder1_Hiddentotal.value = ReturnObj12;  
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_discountTextBox.value = ReturnObj14;
  document.forms[0].ctl00_ContentPlaceHolder1_Hiddendiscount.value = ReturnObj14;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_counterTextBox.value = ReturnObj16;
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox)
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcasUnitTextBox.value = ReturnObj17;
  /*document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vPartialTextBox.value = ReturnObj18;  */
       
  
   var caldis = 0 ; 
   
   var hidtot = document.getElementById("ctl00_ContentPlaceHolder1_Hiddentotal");    
   var total = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_extpriceTextBox");
  
//   if(ReturnObj12 == "CS" && ReturnObj10 != 0)
//   {
//     total.value =(ReturnObj11 * ReturnObj15 / ReturnObj10)  ;
//        if(ReturnObj14 >= 1)
//            {
//                caldis = total.value * ReturnObj14 /100 ;  
//                total.value = total.value - caldis;     
//            }
//            hidtot.value = total.value ; 
//   } 
   if(ReturnObj12 == "C" )
    {
        total.value = ReturnObj11 * ReturnObj15 / 100 ;
        if(ReturnObj14 >= 1)
        {
            caldis = total.value * ReturnObj14 /100 ;  
            total.value = total.value - caldis;       
        }
        hidtot.value = total.value; 
    } 
    else if(ReturnObj12 == "M" )
    {
        total.value = ReturnObj11 * ReturnObj15 / 1000 ; 
        if(ReturnObj14 >= 1)
        {
            caldis = total.value * ReturnObj14 /100 ;  
            total.value = total.value - caldis;     
        }
        hidtot.value = total.value; 
    } 
    else
    {
        total.value = ReturnObj8 * ReturnObj15 ; 
        if(ReturnObj15 >= 1)
            {
                caldis = total.value * ReturnObj15 /100 ;  
                total.value = total.value - caldis;     
            }
            hidtot.value = total.value ; 
    }
    
    var NewWindow = window.open("quantity_look.aspx?est="+ReturnObj1+"&item="+ReturnObj2+"&custpart="+ReturnObj6+"","typeordLookupWindow","width=500,height=200,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    //var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_quantityTextBox");
    //qty.focus();
 }
  
</script>

<div>
    <asp:HiddenField ID="HiddenField1" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenField2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenFielditem" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenFieldname" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencustpart" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencustdesc" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencustdesc2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenprice" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenTextBox1" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencounter" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddentotal" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencost" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenDropdown1" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenestno" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenjob" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenjob2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenquantity" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencasunit" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenspct" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensman" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddencomm" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensname" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddendiscount" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenover" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddenunder" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddentax" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensman2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensname2" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensman3" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="Hiddensname3" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenEstmate" runat="server"></asp:HiddenField>
    <asp:HiddenField ID="HiddenQuoteNum" runat="server"></asp:HiddenField>
    <input id="hiHidden" type="hidden" runat="server"  />
<fieldset id="showinfo" runat="server" style="width:800px;" class="shade">
    <table>
        <tr>
            <td nowrap align="right" style="padding-top:5px;"><b>Order#:</b></td>
            <td nowrap>
                <asp:Label ID="ord_label" runat="server" Font-Bold="true" BackColor="turquoise" Width="80px" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
            </td>
            <td nowrap align="right" style="padding-top:5px;"><b>Cust#:</b></td>
            <td nowrap>
                <asp:Label ID="cust_label" runat="server" Font-Bold="true" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
            </td>            
            <td nowrap align="right" style="padding-top:5px;"><b>Type:</b></td>
            <td nowrap>
                <asp:Label ID="type_label" runat="server" Font-Bold="true" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
            </td>
            <%--<td nowrap align="right" style="padding-top:5px;"><b>Quote:</b></td>
            <td nowrap><asp:Label ID="quote_label" runat="server" Font-Bold="true" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>--%>
            <td nowrap align="right" style="padding-top:5px;"><b>LastUser:</b></td>
            <td nowrap>
                <asp:Label ID="last_label" runat="server" Font-Bold="true" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
            </td>
            <td nowrap align="right" style="padding-top:5px;"><b>Status:</b></td>
            <td nowrap>
                <asp:Label ID="status_label" runat="server" Font-Bold="true" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
            </td>
         </tr>
    </table>
</fieldset>
    
    <asp:FormView ID="FormView1" Font-Bold="true" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload">
        <EditItemTemplate>
            <asp:Panel ID="update_panel" runat="server" DefaultButton="UpdateButton">
                <table width="800px">
                    <tr>
                        <td>
                            <table width="800px" class="shade">
                                <tr>
                                    <td nowrap align="right" style="padding-top:5px;">Order#:</td>
                                    <td nowrap>
                                        <asp:Label ID="ord_label" runat="server" BackColor="turquoise" Width="80px" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                    </td>
                                    <td nowrap align="right" style="padding-top:5px;" >Cust#:</td>
                                    <td nowrap>
                                        <asp:Label ID="cust_label" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                    </td>
                                    <%--<td align="right" style="padding-right:5px"><b>Estimate#:</b></td><td><b><asp:Label ID="est_noTextBox" Width="100px" runat="server" Text='<%# Bind("[est-no]") %>'>
                                    </asp:Label></b></td>--%>
                                    <td align="right" style="padding-right:5px"><b>Quote:</b></td>
                                     <td>
                                       <asp:TextBox ID="vQnoTextBox" Width="70px" ReadOnly="true" BackColor="turquoise" runat="server" Text='<%# Bind("[vQno]") %>'></asp:TextBox>                                      
                                       
                                       </td>
                                    <td align="right" style="padding-right:5px"><b>Job Number:</b></td>
                                    <td><b>
                                        <asp:Label ID="job_noTextBox" Width="70px" runat="server" Text='<%# Bind("[job-no]") %>'></asp:Label>
                                        <asp:Label ID="Jobno2Label" Width="30px"  runat="server" Text='<%# Bind("[job-no2]") %>'></asp:Label>
                                     </b></td>
                                    <td></td>
                                    <td></td>
            
                                    <td align="right" style="padding-right:5px"><b>
                                        <asp:Label ID="Manlabel" runat="server" Text="Managed Inventory"></asp:Label></b></td><td><asp:CheckBox ID="vManagCheckBox" runat="server" Checked='<%# Bind("vManag") %>' />
                                    </td>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="orlabel" runat="server"></asp:Label> </b></td>            
                            </tr>
                    </table>
                </td>
            </tr>            
            <tr>
                <td>
                    <table>
                        <tr>
                            <td>
                                <fieldset style="width:280px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>FG Item:</b></td>
                                            <td nowrap><b>
                                                <asp:TextBox ID="Item1TextBox" Enabled="false" MaxLength="15" Width="170px" onfocus= "javascript:focusval(this)" onblur="setfocus(this)"   runat="server" TabIndex="0" Text='<%# Bind("Item1") %>'></asp:TextBox></b>
                                                <%--<a href="#" onClick="FGItemLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
                                            <td nowrap><b>
                                                <asp:TextBox ID="CustPartTextBox" MaxLength="15" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" AutoPostBack="true" OnTextChanged="custpart_Click" Text='<%# Bind("CustPart") %>'></asp:TextBox></b>
                                                <a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="CustPartLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                             </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Item Name:</b></td>
                                            <td>
                                                <asp:TextBox ID="Name1TextBox" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:inameblurval(this)" runat="server" Text='<%# Bind("Name1") %>'></asp:TextBox>
                                            </td>
                                            
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                            <td>
                                                <asp:TextBox ID="DscrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="30" Width="170px" runat="server" Text='<%# Bind("Dscr") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                            <td>
                                                <asp:TextBox ID="Dscr2TextBox" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:descblurval(this)" runat="server" Text='<%# Bind("Dscr2") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                    </table>
                                 </fieldset>
                               </td>
                               <td>
                                <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                                    <table>
                                        <tr>
                                            <td nowrap align="right"  style="padding-right:5px"><b>Quantity:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="quantityTextBox" MaxLength="9" onfocus= "javascript:focusval(this)" AutoPostBack="true" OnTextChanged="Quantity_Change_Click" onblur="javascript:blurval(this)" Width="60px" runat="server" Text='<%# Bind("quantity") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="QuantityLook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                <asp:TextBox ID="vTypeTextBox" Width="20px" onfocus= "javascript:focusval(this)" onblur="valea(this)" runat="server" Text=''></asp:TextBox>
                                                <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="quantityTextBox" Display="dynamic" runat="server" SetFocusOnError="true" ErrorMessage="Enter The Quantity"></asp:RequiredFieldValidator>
                                                <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="quantityTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <%--<tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="qtyunitLabel"  runat="server" Text="Qty/Unit:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="counterTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("counter") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="QtyCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="counterTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>--%>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Price:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="priceTextBox" MaxLength="11" Width="100px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("price") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="PriceLook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                                                
                                                <asp:TextBox ID="TextBox1" Width="40px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("uom") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="priceTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Discount:</b></td>
                                            <td>
                                                <asp:TextBox ID="discountTextBox" MaxLength="2" Width="30px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("discount") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="DiscountCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="discountTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Total Price:</b></td>
                                            <td>
                                                <asp:TextBox ID="extpriceTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("extprice") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="extpriceTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="cost1Label" runat="server" Text="Cost/M:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="CostTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("vCost") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator20" runat="server" ErrorMessage="Only Numbers" ControlToValidate="CostTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        
                                    </table>
                                 </fieldset>
                              </td>
                              <td>
                                <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                                    <table>
                                    <tr></tr>
                                        <tr>
                                            <td>&nbsp;</td>
                                            <td>
                                                <asp:DropDownList ID="DropDownList1" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("vType") %>' DataTextField='<%# Bind("vType") %>' runat="server">
                                                    <asp:ListItem Value="O">O- Original</asp:ListItem>
                                                    <asp:ListItem Value="C">C- Change</asp:ListItem>
                                                    <asp:ListItem Value="N">N- New</asp:ListItem>
                                                    <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                                    <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                                    <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                                    <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                                    <asp:ListItem Value=""></asp:ListItem>
                                                </asp:DropDownList>
                                            </td>
                                        </tr>
                                        <tr>
                                            <%--<td align="right" style="padding-right:5px"><b><asp:Label ID="uomLabel2" runat="server" Text="UOM:"></asp:Label></b></td>--%>
                                            <td colspan="2"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;               
                                                <b>Tax:&nbsp;&nbsp;</b><asp:CheckBox ID="taxableCheckBox" runat="server" Checked='<%# Bind("taxable") %>' />
                                            </td>
                                        </tr>
                                        
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="qtyunitLabel"  runat="server" Text="Qty/Unit:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="counterTextBox" MaxLength="6" Width="100px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("counter") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="QtyCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="counterTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="partLabel" runat="server" Text="Partial:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="vPartialTextBox" MaxLength="7" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vPartial") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="PartialCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vPartialTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="unitpalletLabel" runat="server" Text="Units/Pallet:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="VcasUnitTextBox" MaxLength="4" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("VcasUnit") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="UnitsCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VcasUnitTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                        <%--<tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="cost1Label" runat="server" Text="Cost/M:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="CostTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("vCost") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator20" runat="server" ErrorMessage="Only Numbers" ControlToValidate="CostTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>--%>
                                     </table>
                                  </fieldset>
                               </td>
                             </tr>
                           </table>
                         </td>
                       </tr>
                       <tr>
                            <td>
                                <table>
                                    <tr>
                                        <td>
                                             <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:70px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Cust Po#:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="custpoTextBox" MaxLength="15" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server" Text='<%# Bind("custpo") %>'></asp:TextBox>
                                                        </td>
                                                        <td align="right" style="padding-right:5px"><b>Ln#:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="vLineTextBox" Width="50px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="setfocus4(this)" runat="server" Text='<%# Bind("vEnum") %>'></asp:TextBox>
                                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="vLineTextBox" ErrorMessage="Only Integer Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator> 
                                                        </td>
                                                     </tr>
                                                     <tr>
                                                        <td nowrap align="right" style="padding-right:5px"><b><asp:Label ID="bpolabel" runat="server" Text="Board PO#:"></asp:Label></b></td>
                                                        <td>
                                                            <asp:TextBox ID="vPonoTextBox" Width="80px" MaxLength="6" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vPono") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="BoardPoLook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="vPonoTextBox" ErrorMessage="Only  Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                                        </td>
                                                        <td nowrap align="right" style="padding-right:5px"><b><asp:Label ID="bvendlabel" runat="server" Text="Board Vendor#:"></asp:Label></b></td>
                                                        <td>
                                                            <asp:TextBox ID="vBoardVenTextBox"  Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vBoardVen") %>'></asp:TextBox>
                                                        </td>
                                                     </tr>
                                                  </table>
                                               </fieldset>
                                             </td>
                                             <td>
                                                <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:70px;">
                                                    <table>
                                                        <tr>
                                                            <td nowrap align="right" style="padding-right:5px"><b>Priority:</b>
                                                                <asp:DropDownList ID="DropDownList2" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("promised") %>' DataTextField='<%# Bind("promised") %>' runat="server">
                                                                    <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                                    <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                                    <asp:ListItem Value="MUST"></asp:ListItem>
                                                                    <asp:ListItem Value="HOT"></asp:ListItem>
                                                                    <asp:ListItem Value="RUSH"></asp:ListItem>
                                                                    <asp:ListItem Value="WO"></asp:ListItem>
                                                                    <asp:ListItem Value="HOLD"></asp:ListItem>
                                                                    <asp:ListItem Value="CR"></asp:ListItem>
                                                                    <asp:ListItem Value="BY"></asp:ListItem>
                                                                    <asp:ListItem Value="ON">ON</asp:ListItem>
                                                                    <asp:ListItem Value="NH"></asp:ListItem>
                                                                    <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                                    <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                                    <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                                    <asp:ListItem Value=""></asp:ListItem>
                                                                </asp:DropDownList>
                                                            </td>
                                                            <td align="right" style="padding-right:5px"><b>Due Date:</b>
                                                                <asp:TextBox ID="requestdateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="80px" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("requestdate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <a href="#" onblur="ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox.focus()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                            </td>
                                                        </tr>
                                                        <tr>
                                                            <td nowrap align="right" style="padding-right:5px"><b>Priority:</b>
                                                                <asp:DropDownList ID="DropDownList3" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("requested") %>' DataTextField='<%# Bind("requested") %>' runat="server">
                                                                <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                                 <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                                 <asp:ListItem Value="MUST"></asp:ListItem>
                                                                 <asp:ListItem Value="HOT"></asp:ListItem>
                                                                 <asp:ListItem Value="RUSH"></asp:ListItem>
                                                                 <asp:ListItem Value="WO"></asp:ListItem>
                                                                 <asp:ListItem Value="HOLD"></asp:ListItem>
                                                                 <asp:ListItem Value="CR"></asp:ListItem>
                                                                 <asp:ListItem Value="BY"></asp:ListItem>
                                                                 <asp:ListItem Value="ON">ON</asp:ListItem>
                                                                 <asp:ListItem Value="NH"></asp:ListItem>
                                                                 <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                                 <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                                 <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                                 <asp:ListItem Value=""></asp:ListItem>
                                                              </asp:DropDownList>
                                                           </td>
                                                           <td nowrap align="right" style="padding-right:5px"><b>Promise Date:</b>
                                                                <asp:TextBox ID="promisdateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY"  Width="80px" runat="server" Text='<%# Bind("promisdate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_promisdateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                           </td>
                                                          </tr>
                                                        </table>
                                                     </fieldset>
                                                </td>
                                             </tr>
                                          </table>
                                       </td>
                                    </tr>
                                    <tr>
                                        <td>
                                            <table>
                                                <tr>
                                                    <td>
                                                        <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                                            <table>
                                                                <tr>
                                                                    <td><b>Sales Rep </b></td>
                                                                    <td><b>Sales Rep Name</b></td>
                                                                    <%--<td><b>% of Sales</b></td>
                                                                    <td><b>Comm%</b></td>--%>
                                                                </tr>
                                                                <tr>
                                                                    <td>
                                                                        <asp:TextBox ID="vSmanTextBox" MaxLength="3" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSman") %>'></asp:TextBox>
                                                                        <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" />
                                                                    </td>
                                                                    <td>
                                                                        <asp:TextBox ID="vSnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname") %>'></asp:TextBox>
                                                                    </td>
                                                                    <%--<td><asp:TextBox ID="vSpctTextBox" runat="server" Width="50px" Text='<%# Bind("vSpct") %>'></asp:TextBox>
                                                                        <asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vSpctTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                                                    <td><asp:TextBox ID="vScommTextBox" runat="server" Width="50px" Text='<%# Bind("vScomm") %>'> </asp:TextBox>
                                                                    <asp:CompareValidator ID="CompareValidator19" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vScommTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>--%>
                                                                </tr>
                                                                <tr>
                                                                <td>
                                                                     <asp:TextBox ID="vSman2TextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("vSman2") %>'></asp:TextBox>
                                                                     <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                 </td>
                                                                 <td>
                                                                    <asp:TextBox ID="vSname2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname2") %>'></asp:TextBox>
                                                                 </td>
                                                                 <%--<td><asp:TextBox ID="vSpct2TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct2") %>'></asp:TextBox>
                                                                    <asp:CompareValidator ID="CompareValidator9" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vSpct2TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                                                    <td><asp:TextBox ID="vScomm2TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm2") %>'> </asp:TextBox>
                                                                    <asp:CompareValidator ID="CompareValidator18" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vScomm2TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>--%>
                                                            </tr>
                                                            <tr>
                                                                <td>
                                                                    <asp:TextBox ID="vSman3TextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("vSman3") %>'></asp:TextBox>
                                                                        <a href="#" tabindex="1" onClick="salesmanlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                </td>
                                                                <td>
                                                                    <asp:TextBox ID="vSname3TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname3") %>'></asp:TextBox>
                                                                </td>
                                                                <%--<td><asp:TextBox ID="vSpct3TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct3") %>'></asp:TextBox>
                                                                    <asp:CompareValidator ID="CompareValidator10" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vSpct3TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                                                    <td><asp:TextBox ID="vScomm3TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm3") %>'> </asp:TextBox>
                                                                    <asp:CompareValidator ID="CompareValidator17" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vScomm3TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>--%>
                                                            </tr>
                                                        </table>
                                                    </fieldset>
                                                  </td>
                                                  <td>
                                                    <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                                      <table>
                                                        <tr>
                                                            <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                                            <td>
                                                                <asp:TextBox ID="vOverTextBox" Width="50px" MaxLength="5" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="valoverrun()" runat="server"  Text='<%# Bind("vOver") %>'></asp:TextBox>
                                                                <asp:CompareValidator ID="CompareValidator7" runat="server"  ErrorMessage="Only Numbers" ControlToValidate="vOverTextBox"  Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                            </td>
                                                            <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                                            <td>
                                                                <asp:TextBox ID="vUnderTextBox" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:underblurval(this)" MaxLength="5" onkeyup="valunderrun()" runat="server" Text='<%# Bind("vUnder") %>'></asp:TextBox>
                                                                <asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vUnderTextBox"  Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                            </td>
                                                        </tr>
                                                    </table>
                                                  </fieldset>
                                                </td>
                                             </tr>
                                             <tr>
                                                <td colspan="2">
                                                     <asp:Button ID="UpdateButton" CssClass="button" runat="server" OnClientClick="grater(this)" CausesValidation="True" Text="Save" OnClick="UpdateButton_Click"></asp:Button>
                                                     <asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" OnClick="UpdateCancelButton_Click" CommandName="Cancel" Text="Cancel"></asp:Button>
                                                </td>
                                     </tr>
                               </table>            
                     </asp:Panel>
        </EditItemTemplate>


 <InsertItemTemplate>
    <asp:Panel ID="insert_panel" runat="server" DefaultButton="InsertButton">
        <table  width="800px">
            <tr>
                <td>
                    <table class="shade" width="800px" height="5px">
                        <tr style="height:5px">
                            <%--<td align="right" style="padding-right:5px"><b>Estimate#:</b></td>
                            <td nowrap><asp:TextBox ID="est_noTextBox" onkeyup="estlook()" Width="100px" runat="server" Text='<%# Bind("[est-no]") %>'>
                            </asp:TextBox>
                            <a href="#" onClick="ItemUpdateLook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>--%>
                            <td align="right" style="padding-right:5px"><b>Quote:</b></td>
                            <td>
                                <asp:TextBox ID="vQnoTextBox" MaxLength="9" AutoPostBack="true" OnTextChanged="quote_text_changed" onfocus= "javascript:focusval(this)" onblur="javascript:quoteblurval(this)" Width="70px" runat="server" Text='<%# Bind("[vQno]") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="ItemQuoteLook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                <asp:CompareValidator ID="CompareValidator12" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vQnoTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Job Number:</b></td>
                            <td>
                                <asp:Label ID="job_noTextBox" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="70px" runat="server" Text='<%# Bind("[job-no]") %>'></asp:Label>
                                <asp:Label ID="job_noTextBox2" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Width="30px" runat="server" Text='<%# Bind("[job-no2]") %>'></asp:Label>
                            </td>
                            <td></td>
                            <td align="right" style="padding-right:5px"><b>
                                <asp:Label ID="Manlabel" runat="server" Text="Managed Inventory"></asp:Label></b></td>
                            <td><asp:CheckBox ID="vManagCheckBox" runat="server" Checked='<%# Bind("vManag") %>' /></td>
                            <td align="right" style="padding-right:5px"><b><asp:Label ID="orlabel" runat="server"></asp:Label> </b></td>
                       </tr>
                </table>
            </td>         
         </tr>
         <tr>
              <td>
                  <table>
                      <tr>
                          <td>
                                <fieldset style="width:280px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>FG Item:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="Item1TextBox" MaxLength="15" Width="170px" onfocus= "javascript:focusval(this)" onblur="setfocus(this)"  AutoPostBack="true" OnTextChanged="fgitem_value_Click" runat="server" Text='<%# Bind("Item1") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="FGItemLook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="CustPartTextBox" MaxLength="15" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" AutoPostBack="true" OnTextChanged="custpart_Click" Text='<%# Bind("CustPart") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="custpartlook(); return false"><asp:Image ID="CustPartLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Item Name:</b></td>
                                            <td>
                                                <asp:TextBox ID="Name1TextBox" MaxLength="30" Width="170px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:inameblurval(this)" Text='<%# Bind("Name1") %>'></asp:TextBox>
                                            </td>
                                            
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                            <td>
                                                <asp:TextBox ID="DscrTextBox" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("Dscr") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                            <td>
                                                <asp:TextBox ID="Dscr2TextBox" MaxLength="30" Width="170px" onfocus= "javascript:focusval(this)" onblur="javascript:descblurval(this)" runat="server" Text='<%# Bind("Dscr2") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                </table>
                        </fieldset>
                    </td>
                    <td>
                        <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                            <table>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Quantity:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="quantityTextBox" MaxLength="9" onfocus= "javascript:focusval(this)" AutoPostBack="true" OnTextChanged="Quantity_Change_Click" Width="80px" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("quantity") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick="QuantityLook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        <asp:TextBox ID="vTypeTextBox" Width="30px" onfocus= "javascript:focusval(this)" onblur="valea(this)" runat="server" Text='<%# Bind("vType") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="quantityTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
                                        <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="quantityTextBox" Display="dynamic" runat="server" SetFocusOnError="true" ErrorMessage="Enter The Quantity"></asp:RequiredFieldValidator>
                                    </td>
                                </tr>
                                <%--<tr>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="qtyunitLabel" runat="server" Text="Qty/Unit:"></asp:Label></b></td>
                                    <td>
                                        <asp:TextBox ID="counterTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("counter") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="QtyCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="counterTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>--%>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Price:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="priceTextBox" MaxLength="11" Width="100px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("price") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick="PriceLook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                        <asp:TextBox ID="TextBox1" Width="40px" runat="server" onfocus= "javascript:focusval(this)" onblur="grater(this)" Text='<%# Bind("uom") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="priceTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Discount:</b></td>
                                    <td>
                                        <asp:TextBox ID="discountTextBox" MaxLength="2" Width="30px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("discount") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="DiscountCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="discountTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Total Price:</b></td>
                                    <td>
                                        <asp:TextBox ID="extpriceTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("extprice") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator4" runat="server" ErrorMessage="Only Numbers" ControlToValidate="extpriceTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="cost1Label" runat="server" Text="Cost/M:"></asp:Label></b></td>
                                    <td>
                                        <asp:TextBox ID="CostTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vCost") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator16" runat="server" ErrorMessage="Only Numbers" ControlToValidate="CostTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                
                        </table>
                    </fieldset>
                </td>
                <td>
                    <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:140px;">
                        <table>
                            <tr></tr>
                            <tr>
                                <td></td>
                                    <td>
                                        <asp:DropDownList ID="DropDownList1" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" DataTextField='<%# Bind("vType") %>' runat="server">
                                            <asp:ListItem Value="O">O- Original</asp:ListItem>
                                            <asp:ListItem Value="C">C- Change</asp:ListItem>
                                            <asp:ListItem Value="N">N- New</asp:ListItem>
                                            <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                            <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                            <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                            <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                         </asp:DropDownList>
                                    </td>
                                </tr>
                                <tr>
                                    <%--<td align="right" style="padding-right:5px"><b><asp:Label ID="uomLabel2" runat="server" Text="UOM:"></asp:Label></b></td>--%>
                                    <td colspan="2"> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;               
                                    <b>Tax:&nbsp;&nbsp;</b><asp:CheckBox ID="taxableCheckBox" runat="server" Checked='<%# Bind("taxable") %>' /></td>
                                </tr>
                                
                                <tr>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="qtyunitLabel" runat="server" Text="Qty/Unit:"></asp:Label></b></td>
                                    <td>
                                        <asp:TextBox ID="counterTextBox" MaxLength="6" Width="100px" onfocus= "javascript:focusval(this)" onblur="grater(this)" runat="server" Text='<%# Bind("counter") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="QtyCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="counterTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                
                                <tr>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="partLabel" runat="server" Text="Partial:"></asp:Label></b></td>
                                    <td>
                                        <asp:TextBox ID="vPartialTextBox" MaxLength="7" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vPartial") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="PartialCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vPartialTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="unitpalletLabel" runat="server" Text="Units/Pallet:"></asp:Label></b></td>
                                    <td>
                                        <asp:TextBox ID="VcasUnitTextBox" MaxLength="4" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("VcasUnit") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="UnitsCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VcasUnitTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <%--<tr>
                                    <td align="right" style="padding-right:5px"><b><asp:Label ID="cost1Label" runat="server" Text="Cost/M:"></asp:Label></b></td>
                                    <td>
                                        <asp:TextBox ID="CostTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vCost") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator16" runat="server" ErrorMessage="Only Numbers" ControlToValidate="CostTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>--%>
                            </table>
                        </fieldset>
                    </td>
              </tr>
           </table>
        </td>
      </tr>
      <tr>
            <td>
                  <table>
                        <tr>
                            <td>
                                <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:70px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Cust Po#:</b></td>
                                            <td nowrap>
                                                <asp:TextBox ID="custpoTextBox" Width="120px" MaxLength="15" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("custpo") %>'></asp:TextBox>
                                            </td>
                                           <td align="right" style="padding-right:5px"><b>Ln#:</b></td>
                                           <td>
                                                <asp:TextBox ID="vLineTextBox" Width="50px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="setfocus4(this)" runat="server" Text='<%# Bind("vEnum") %>'></asp:TextBox>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="vLineTextBox" ErrorMessage="Only Integer Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>                                  
                                            </td>
                                        </tr>
                                        <tr>
                                            <td nowrap align="right" style="padding-right:5px"><b><asp:Label ID="bpolabel" runat="server" Text="Board PO#:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox Width="80px" MaxLength="6" ID="vPonoTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vPono") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="BoardPoLook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                <asp:RegularExpressionValidator ID="RegularExpressionValidator2" runat="server" SetFocusOnError="true" ControlToValidate="vPonoTextBox" ErrorMessage="Only  Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                            </td>
                                            <td nowrap align="right" style="padding-right:5px"><b><asp:Label ID="bvendlabel" runat="server" Text="Board Vendor#:"></asp:Label></b></td>
                                            <td>
                                                <asp:TextBox ID="vBoardVenTextBox" Width="80px" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vBoardVen") %>'></asp:TextBox>
                                            </td>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                            <td>
                                <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:70px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Priority:</b>
                                                <asp:DropDownList ID="DropDownList2" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("promised") %>' DataTextField='<%# Bind("promised") %>' runat="server">
                                                    <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                    <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                    <asp:ListItem Value="MUST"></asp:ListItem>
                                                    <asp:ListItem Value="HOT"></asp:ListItem>
                                                    <asp:ListItem Value="RUSH"></asp:ListItem>
                                                    <asp:ListItem Value="WO"></asp:ListItem>
                                                    <asp:ListItem Value="HOLD"></asp:ListItem>
                                                    <asp:ListItem Value="CR"></asp:ListItem>
                                                    <asp:ListItem Value="BY"></asp:ListItem>
                                                    <asp:ListItem Value="ON">ON</asp:ListItem>
                                                    <asp:ListItem Value="NH"></asp:ListItem>
                                                    <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                    <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                    <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                </asp:DropDownList>
                                            </td>
                                            <td align="right" style="padding-right:5px"><b>Due Date:</b>
                                                <asp:TextBox ID="requestdateTextBox" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="80px" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("requestdate") %>'></asp:TextBox>

                                                <a href="#" onblur="ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox.focus()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_requestdateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Priority:</b>
                                                <asp:DropDownList ID="DropDownList3" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" SelectedValue='<%# Bind("requested") %>' DataTextField='<%# Bind("requested") %>' runat="server">
                                                    <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                    <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                    <asp:ListItem Value="MUST"></asp:ListItem>
                                                    <asp:ListItem Value="HOT"></asp:ListItem>
                                                    <asp:ListItem Value="RUSH"></asp:ListItem>
                                                    <asp:ListItem Value="WO"></asp:ListItem>
                                                    <asp:ListItem Value="HOLD"></asp:ListItem>
                                                    <asp:ListItem Value="CR"></asp:ListItem>
                                                    <asp:ListItem Value="BY"></asp:ListItem>
                                                    <asp:ListItem Value="ON">ON</asp:ListItem>
                                                    <asp:ListItem Value="NH"></asp:ListItem>
                                                    <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                    <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                    <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                </asp:DropDownList>
                                            </td>
                                            <td nowrap align="right" style="padding-right:5px"><b>Promise Date:</b>
                                                <asp:TextBox ID="promisdateTextBox" Width="80px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("promisdate") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_promisdateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                
                                            </td>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                         </tr>
                    </table>
                </td>
              </tr>
              <tr>
                    <td>
                        <table>
                            <tr>
                                <td>
                                    <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                        <table>
                                            <tr>
                                                <td><b>Sales Rep </b></td>
                                                <td><b>Sales Rep Name</b></td>
                                                <%--<td><b>% of Sales</b></td>
                                                <td><b>Comm%</b></td>--%>
                                            </tr>
                                            <tr>
                                                <td>
                                                    <asp:TextBox ID="vSmanTextBox" MaxLength="3" Width="50px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSman") %>'></asp:TextBox>
                                                    <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" />
                                                </td>
                                                <td>
                                                    <asp:TextBox ID="vSnameTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("vSname") %>'></asp:TextBox></td>
                                                    <%--<td><asp:TextBox ID="vSpctTextBox" runat="server" Width="50px" Text='<%# Bind("vSpct") %>'></asp:TextBox>
                                                        <asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vSpctTextBox" Display="dynamic"  SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                                        <td><asp:TextBox ID="vScommTextBox" runat="server"  Width="50px" Text='<%# Bind("vScomm") %>'> </asp:TextBox>
                                                        <asp:CompareValidator ID="CompareValidator15" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vScommTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                        </td>--%>
                                          </tr>
                                          <tr>
                                            <td>
                                                 <asp:TextBox ID="vSman2TextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("vSman2") %>'></asp:TextBox>
                                                 <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                            <td>
                                                <asp:TextBox ID="vSname2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname2") %>'></asp:TextBox>
                                            </td>
                                                <%--<td><asp:TextBox ID="vSpct2TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct2") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator11" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vSpct2TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                                <td><asp:TextBox ID="vScomm2TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm2") %>'> </asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator14" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vScomm2TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>--%>
                                        </tr>
                                        <tr>
                                            <td>
                                                <asp:TextBox ID="vSman3TextBox" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("vSman3") %>'></asp:TextBox>
                                                <a href="#" tabindex="1" onClick="salesmanlook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                            </td>
                                            <td>
                                                <asp:TextBox ID="vSname3TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vSname3") %>'></asp:TextBox>
                                            </td>
                                            <%--<td><asp:TextBox ID="vSpct3TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct3") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator12" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vSpct3TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                                <td><asp:TextBox ID="vScomm3TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm3") %>'> </asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator13" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vScomm3TextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>--%>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                            <td>
                                <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                            <td>
                                                <asp:TextBox ID="vOverTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="5" onkeyup="valoverrun()" runat="server" Text='<%# Bind("vOver") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator5" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vOverTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                            <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                            <td>
                                                <asp:TextBox ID="vUnderTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:underblurval(this)" Width="50px" MaxLength="5" onkeyup="valunderrun()" runat="server" Text='<%# Bind("vUnder") %>'></asp:TextBox>
                                                <asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vUnderTextBox" SetFocusOnError="true" Display="dynamic"  Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                            </td>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                         </tr>
                    </table>
                </td>
              </tr>
              <tr>
                    <td>
                        <asp:Button ID="InsertButton" runat="server" CssClass="button" OnClientClick="grater(this)" CausesValidation="True" Text="Save" OnClick="InsertButton_Click"></asp:Button>
                        <asp:Button ID="InsertCancelButton" CssClass="button" runat="server" CausesValidation="False" OnClick="InsertCancelButton_Click"
                         CommandName="Cancel" Text="Cancel" ></asp:Button>
                    </td>
              </tr>
           </table>
         </asp:Panel>
     </InsertItemTemplate>
     
     
        <ItemTemplate>
           <table width="800px">
              <tr>
                <td>
                    <table class="shade" width="800px">
                        <tr>
                            <td nowrap align="right" style="padding-top:5px;">Order#:</td>
                            <td nowrap>
                                <asp:Label ID="ord_label" runat="server" BackColor="turquoise" Width="80px" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                            </td>
                            <td nowrap align="right" style="padding-top:5px;" >Cust#:</td>
                            <td nowrap>
                                <asp:Label ID="cust_label" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                            </td>
                              <%--<td align="right" style="padding-right:5px"><b><asp:Label ID="Estlabel" runat="server" Text="Estimate#:"></asp:Label></b></td>
                                <td><asp:Label ID="est_noLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[est-no]") %>'></asp:Label></td>--%>
                              <td nowrap align="right" style="padding-top:5px;" >Quote:</td>
                            <td nowrap>
                                <asp:Label ID="vQnoLabel" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" Text='<%# Bind("[vQno]") %>' BorderWidth="1px"></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Job Number:</b></td>
                            <td>
                                <asp:Label ID="job_noLabel" Width="70px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[job-no]") %>'></asp:Label>
                                <asp:Label ID="Jobno2Label" Width="30px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("[job-no2]") %>'></asp:Label>
                            </td>
                          <td align="right" style="padding-right:5px"><b><asp:Label ID="Manlabel" runat="server" Text="Managed Inventory"></asp:Label></b></td>
                          <td>
                            <asp:CheckBox ID="vManagCheckBox" runat="server"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Checked='<%# Bind("vManag") %>' Enabled="false" />
                          </td>
                          <td align="right" style="padding-right:5px"><b>Type:</b></td>
                          <td>
                            <asp:Label ID="Label2" runat="server" Width="40px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vType") %>'></asp:Label>
                          </td>
                      </tr>
                   </table>
               </td>
           </tr>
           <tr>
              <td>
                  <table>
                    <tr>
                        <td>
                            <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:130px;">
                                <table>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>FG Item:</b></td>
                                        <td>
                                            <asp:Label ID="Item1Label" runat="server" Width="180px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("Item1") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
                                        <td>
                                            <asp:Label ID="CustPartLabel" runat="server" Width="180px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("CustPart") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Item Name:</b></td>
                                        <td>
                                            <asp:Label ID="Name1Label" Width="180px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("Name1") %>'></asp:Label>
                                        </td>
                                        
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                        <td>
                                            <asp:Label ID="DscrLabel" runat="server" Width="180px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("Dscr") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Description:</b></td>
                                        <td>
                                            <asp:Label ID="Dscr2Label" runat="server" Width="180px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("Dscr2") %>'></asp:Label>
                                        </td>
                                    </tr>
                                </table>
                            </fieldset>
                        </td>
                        <td>
                            <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:130px;">
                                <table >
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Quantity:</b></td>
                                        <td>
                                            <asp:Label ID="quantityLabel" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("quantity","{0:###,###,##0}") %>'></asp:Label>
                                            <asp:Label ID="vTypeLabel" runat="server" Width="20px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text="EA"></asp:Label>
                                        </td>
                                     </tr>
                                      <%--<tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="qtyunitLabel" runat="server" Text="Qty/Unit:"></asp:Label></b></td>
                                            <td>
                                                <asp:Label ID="counterLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("counter","{0:###,###,##0}") %>'></asp:Label>
                                            </td>
                                        </tr>--%>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Price:</b></td>
                                            <td>
                                                <asp:Label ID="priceLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("price","{0:C}") %>'></asp:Label>
                                            </td>
                                             <%--<td  align="right" style="padding-right:5px"><b><asp:Label ID="uomLabel2" runat="server" Text="UOM:"></asp:Label></b></td>--%>
                                            <td>
                                                <asp:Label ID="uomLabel" runat="server" Width="40px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("uom") %>'></asp:Label>
                                            </td>
                                      </tr>
                                      <tr>
                                            <td align="right" style="padding-right:5px"><b>Discount:</b></td>
                                            <td>
                                                <asp:Label ID="discountLabel" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("discount","{0:###,###,##0.00}") %>'></asp:Label>
                                            </td>
                                      </tr>
                                      <tr>
                                            <td align="right" style="padding-right:5px"><b>Total Price:</b></td>
                                            <td>
                                                <asp:Label ID="extpriceLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("extprice","{0:C}") %>'></asp:Label>
                                            </td>
                                      </tr>
                                      <tr id="costrow" runat="server">
                                            <td><b><asp:Label ID="cost2Label" runat="server" Text="Cost/M:"></asp:Label></b></td>
                                            <td>
                                                <asp:Label ID="costLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCost") %>'></asp:Label>
                                            </td>
                                      </tr>
                                      
                                </table>
                            </fieldset>
                        </td>
                        <td>
                            <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:130px;">
                                <table>
                                    <tr><td>&nbsp;</td></tr>
                                    <tr>
                                         <td>
                                            <b>Tax:</b>
                                                <asp:CheckBox ID="taxableCheckBox"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("taxable") %>' Enabled="false" />
                                         </td>
                                    </tr>
                                    <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="qtyunitLabel" runat="server" Text="Qty/Unit:"></asp:Label></b></td>
                                            <td>
                                                <asp:Label ID="counterLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("counter","{0:###,###,##0}") %>'></asp:Label>
                                            </td>
                                        </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b><asp:Label ID="partLabel" runat="server" Text="Partial:"></asp:Label></b></td>
                                        <td>
                                            <asp:Label ID="vPartialLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vPartial") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b><asp:Label ID="unitpalletLabel" runat="server" Text="Units/Pallet:"></asp:Label></b></td>
                                        <td>
                                            <asp:Label ID="VcasUnitLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VcasUnit") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <%--<tr id="costrow" runat="server">
                                            <td><b><asp:Label ID="cost2Label" runat="server" Text="Cost/M:"></asp:Label></b></td>
                                            <td>
                                                <asp:Label ID="costLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCost") %>'></asp:Label>
                                            </td>
                                      </tr>--%>
                                </table>
                            </fieldset>
                         </td>
                      </tr>
                  </table>
                </td>
              </tr>
              <tr>
                 <td>
                    <table>
                        <tr>
                            <td>
                                <fieldset style="width:395px; border:solid 1px black; background-color:#EFF3FB; height:60px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Cust Po#:</b></td>
                                            <td>
                                                <asp:Label ID="custpoLabel" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server"  Text='<%# Bind("custpo") %>'></asp:Label>
                                            </td>
                                            <td align="right" style="padding-right:5px"><b>Ln#:</b></td>
                                            <td>
                                                <asp:Label ID="vLineLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vEnum") %>'></asp:Label>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="bpolabel" runat="server" Text="Board PO#:"></asp:Label></b></td>
                                            <td>
                                                <asp:Label ID="vPonoLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vPono") %>'></asp:Label>
                                            </td>
                                            <td align="right" style="padding-right:5px"><b><asp:Label ID="bvendlabel" runat="server" Text="Board Vendor#:"></asp:Label></b></td>
                                            <td>
                                                <asp:Label ID="vBoardVenLabel" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vBoardVen") %>'></asp:Label>
                                            </td>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                            <td>
                                <fieldset style="width:390px; border:solid 1px black; background-color:#EFF3FB; height:60px;">
                                    <table>
                                        <tr>
                                            <td><b>Priority:</b></td>
                                            <td>
                                                <asp:Label ID="promisedLabel" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("promised") %>'></asp:Label>
                                            </td>
                                            <td align="right" style="padding-right:5px"><b>Due Date:</b></td>
                                            <td>
                                                <asp:Label ID="requestdateLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("requestdate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                            </td>
                                        </tr>
                                        <tr>
                                              <td ><b>Priority:</b></td>
                                              <td>
                                                  <asp:Label ID="requestedLabel" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("requested") %>'></asp:Label>
                                              </td>
                                              <td align="right" style="padding-right:5px"><b>Promise Date:</b></td>
                                              <td>
                                                    <asp:Label ID="promisdateLabel" Width="80px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("promisdate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                              </td>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                        </tr>
                    </table>
                  </td>
                </tr>
                <tr>
                      <td>
                        <table>
                            <tr>
                                <td>
                                    <fieldset style="width:395px; border:solid 1px black; background-color:#EFF3FB; height:90px;">
                                        <table>
                                            <tr>
                                                <td><b>Sales Rep</b></td>
                                                <td><b>Sales Rep Name</b></td>
                                                <%--<td><b>% of Sales</b></td>
                                                    <td><b>Comm%</b></td>--%>
                                            </tr>
                                            <tr>
                                                <td>
                                                    <asp:Label ID="vSmanLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSman") %>'></asp:Label>
                                                </td>
                                                <td>
                                                    <asp:Label ID="vSnameLabel" runat="server" Width="150px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSname") %>'></asp:Label>
                                                </td>
                                                <%--<td><asp:Label ID="vSpctLabel" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSpct") %>'></asp:Label></td>
                                                    <td><asp:Label ID="vScommLabel" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vScomm") %>'></asp:Label></td>--%>
                                          </tr>
                                          <tr>
                                                <td>
                                                    <asp:Label ID="vSman2Label" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSman2") %>'></asp:Label>
                                                </td>
                                                <td>
                                                    <asp:Label ID="vSname2Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSname2") %>'></asp:Label>
                                                </td>
                                                <%--<td><asp:Label ID="vSpct2Label" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSpct2") %>'></asp:Label></td>
                                                    <td><asp:Label ID="vScomm2Label" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vScomm2") %>'></asp:Label></td>--%>
                                         </tr>
                                         <tr>
                                                <td>
                                                    <asp:Label ID="vSman3Label" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSman3") %>'></asp:Label>
                                                </td>
                                                <td>
                                                    <asp:Label ID="vSname3Label" runat="server" Width="150px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSname3") %>'></asp:Label>
                                                </td>
                                                <%--<td><asp:Label ID="vSpct3Label" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSpct3") %>'></asp:Label></td>
                                                    <td><asp:Label ID="vScomm3Label" runat="server" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vScomm3") %>'></asp:Label></td>--%>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                            <td>
                                <fieldset style="width:390px; border:solid 1px black; background-color:#EFF3FB; height:90px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                            <td>
                                                <asp:Label ID="vOverLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vOver") %>'></asp:Label>
                                            </td>
                                            <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                            <td>
                                                <asp:Label ID="vUnderLabel" runat="server" Width="100px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vUnder") %>'></asp:Label>
                                            </td>
                                        </tr>
                                    </table>
                                </fieldset>
                            </td>
                        </tr>
                    </table>
                </td>
              </tr>
           </table>
           <table class="shade" width="750px"> 
                <tr><td>
                <asp:Label ID="Label_rec_key" runat="server" Visible="false" Text='<%# Bind("vReckey") %>'></asp:Label>
                <asp:Label ID="est_Label_est" runat="server" Visible="false" Text='<%# Eval("est-no") %>' ></asp:Label>
                </td></tr>
                <tr>
                    <td colspan="3">
                        <asp:Label ID="vlineLabe" Visible="false" runat="server" Text='<%# Bind("vLine") %>'></asp:Label>
                        <asp:Button ID="AddButton" runat="server" CssClass="button" CausesValidation="False" CommandName="new" Text="Add"></asp:Button>
                        <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit" Text="Update"></asp:Button>
                        <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')" Text="Delete"></asp:Button>
                    </td>
                </tr>
            </table>
     </ItemTemplate>
</asp:FormView>

    <asp:Button ID="NewAddButton"  runat="server" CssClass="button" Text="Add" OnClick="newAddButton_Click" />
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectViewItemEstimate" TypeName="orderentry">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction"  Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmOrderNum" SessionField="order_est" Type="Int32" />
            <asp:SessionParameter Name="prmLine" SessionField="view_line_est" Type="Int32" />
            <asp:Parameter Name="prmEstimate" Type="String" />
            <asp:Parameter Name="prmItemNum" Type="String" />
            <asp:Parameter Name="prmPartNum" Type="String" />
            <asp:Parameter Name="prmQty" Type="Decimal" />
            <asp:Parameter Name="prmItemName" Type="String" />
            <asp:Parameter Name="prmPartdscr" Type="String" />
            <asp:Parameter Name="prmPartdscr1" Type="String" />
            <asp:Parameter Name="prmPartdscr2" Type="String" />
            <asp:Parameter Name="prmPrice" Type="Decimal" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmTax" Type="String" />
            <asp:Parameter Name="prmPoNum" Type="String" />
            <asp:Parameter Name="prmJob" Type="String" />
            <asp:Parameter Name="prmJob2" Type="Int32" />
            <asp:Parameter Name="prmDiscount" Type="Decimal" />
            <asp:Parameter Name="prmCode" Type="String" />
            <asp:Parameter Name="prmReqDate" Type="DateTime" />
            <asp:Parameter Name="prmTPrice" Type="Decimal" />
            <asp:Parameter Name="prmPromCode" Type="String" />
            <asp:Parameter Name="prmPromDate" Type="DateTime" />
            <asp:Parameter Name="prmShip" Type="Decimal" />
            <asp:Parameter Name="prmCas" Type="Int32" />
            <asp:Parameter Name="prmPartial" Type="Decimal" />
            <asp:Parameter Name="prmUnit" Type="Int32" />
            <asp:Parameter Name="prmEnum" Type="Int32" />
            <asp:Parameter Name="prmPrevOrder" Type="Int32" />
            <asp:Parameter Name="prmSman" Type="String" />
            <asp:Parameter Name="prmSman2" Type="String" />
            <asp:Parameter Name="prmSman3" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmOver" Type="Decimal" />
            <asp:Parameter Name="prmUnder" Type="Decimal" />
            <asp:Parameter Name="prmVend" Type="String" />
            <asp:Parameter Name="prmManag" Type="String" />
            <asp:Parameter Name="prmLn" Type="String" />
            <asp:Parameter Name="prmSname" Type="String" />
            <asp:Parameter Name="prmSname2" Type="String" />
            <asp:Parameter Name="prmSname3" Type="String" />
            <asp:Parameter Name="prmSpct" Type="Decimal" />
            <asp:Parameter Name="prmSpct2" Type="Decimal" />
            <asp:Parameter Name="prmSpct3" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />
            <asp:Parameter Name="prmComm2" Type="Decimal" />
            <asp:Parameter Name="prmComm3" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmQno" Type="int32" />
            <asp:Parameter Name="prmNewItemCreated" Type="String" />
        </SelectParameters>     
    </asp:ObjectDataSource>
    &nbsp;&nbsp;  
    
   </div> 
   
             
      
</asp:Content>


