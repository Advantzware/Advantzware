<%@ Page Language="C#" Debug="true" AutoEventWireup="true" MasterPageFile="~/MasterPagestimate.master" Inherits="order_estimate" Title="View Order Estimate" Codebehind="~/view_order_estimate.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
 <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>

<script language = "JavaScript" src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>


<script language="javascript">

window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox") && document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox").value == "" )
    {
        var quote= document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox");        
        quote.focus();
       
    }
//    else if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox"))
//    {
//        var bill=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox");
//        bill.focus();
//    }
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox") && document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox").value == "" )
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox").value != "")
        {        
            var ponum=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VPonumTextBox");
            ponum.focus();
        }    
    }    
}
function focusonsave()
{
    if(event.keyCode==13)
    {
        var save=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_InsertButton");
        save.focus();
    }
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
function dateshowcal()
{
    var date = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel");
     date.style.backgroundColor='blue';
     date.style.color = 'white';
     showCalendarControl(date);
}
function duedatecal()
{
    var date = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox");
     date.style.backgroundColor='blue';
     date.style.color = 'white';
     showCalendarControl(date);
}
function expdatecal()
{
    var date = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox");
     date.style.backgroundColor='blue';
     date.style.color = 'white';
     showCalendarControl(date);
}
function dateblurCal()
{

    var date = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel");
     var due = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_DropDownList1");
     date.style.backgroundColor='Window';
     date.style.color = 'WindowText';
     due.focus();
}
function duedateblurcal()
{
    var date = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox");
    var pono = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VPonumTextBox");
     date.style.backgroundColor='Window';
     date.style.color = 'WindowText';
     pono.focus();
}
function expdateblurcal()
{
    var date = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox");
    var account = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCnumTextBox");
     date.style.backgroundColor='Window';
     date.style.color = 'WindowText';
     account.focus();
}
 

function underblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';   
    
    var tax=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox");
    var pay=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox");
    var frt=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RD1");
    var ptype=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCtypeTextBox");
    
    if(tax.disabled==true && pay.disabled==true && frt.disabled ==true)
    {
        ptype.focus();
    }
}
function updateunderblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText'; 
    var tax=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox");
    var pay=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox");
    var frt=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RD1");
    var acc=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCnumTextBox");
    
    if(tax.disabled==true && pay.disabled==true && frt.disabled==true)
    {
        acc.focus();
    }
}

function contactblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText'; 
    
    var sold=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox");
    var due=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_DropDownList1");
    
    if(sold.disabled==true)
    {
        due.focus();
    }
    
}
function quoteblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox"))
    {
        var quote= document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox");
        
        if(quote.value!="" && quote.value!="0")
        {
            var bill=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox");
            var soldto= document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox");
            var bill_img= document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustomerLook");
            var sold_img= document.getElementById("ctl00_ContentPlaceHolder1_FormView1_Image13");
            
            document.forms[0].ctl00_ContentPlaceHolder1_Hiddensold.value=soldto.value;
            
            bill.disabled=true;
            soldto.disabled=true;
            
            bill_img.style.visibility='hidden';
            sold_img.style.visibility='hidden';
            
            if(bill.disabled==true)
            {
               var contact=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox");
                contact.focus();
            }
            
            //var NewWindow = window.open("quantity_lookup.aspx?est="+quote.value+"","typeordLookupWindow","width=500,height=200,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        else
        {
            var quote= document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox");
            var bill=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox");
            if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_estimateTextBox") && (quote.value=="" || quote.value=="0") )
            {
                var est=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_estimateTextBox");
                est.focus();
            }
           if( bill.disabled !=true )
           {
                var bill=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox");
                bill.focus();
            } 
            else
            {
                var contact=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox");
                contact.focus();
            }           
         
        }
         
    }
}
function refblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
//    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_DropDownList2"))
//    {
//        var type=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_DropDownList2");
//        type.focus();
//    }
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox"))
    {
        var quote= document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox");
        quote.focus();
    }
    else if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox"))
    {
        var contact=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox");
        contact.focus();
    }
}


function accblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCauthTextBox").disabled == true)
    {
        var cont=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox");
        cont.focus();
    }
}


function duedateval()
{
    var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox").value;
    
    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox").value = duedate + "/";
    }
    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox").value = duedate + "/";
    }
}
function expdateval()
{
    var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox").value;
    
    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox").value = duedate + "/";
    }
    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox").value = duedate + "/";
    }
}

function orderdateval()
{
    
   
var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel").value;
    
    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel").value = duedate + "/";
    }
    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel").value = duedate + "/";
    }

}
function setcust(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
//    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VPonumTextBox"))
//    {
//        var cust=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VPonumTextBox");
//        cust.focus();
//    }
}
function setsave(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
//    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_InsertButton"))
//    {
//        var save=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_InsertButton");
//        save.focus();
//    }
//    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_UpdateButton"))
//    {
//        var update=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_UpdateButton");
//        update.focus();
//    }
}


function contactcustomerlook()
{ 
  var NewWindow = window.open("bill_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function BillCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14,ReturnObj15,ReturnObj16,ReturnObj17,ReturnObj18,ReturnObj19,ReturnObj20, ReturnObj21,ReturnObj22,ReturnObj23,ReturnObj24,ReturnObj25,ReturnObj26,ReturnObj27,ReturnObj28,ReturnObj29,ReturnObj30,ReturnObj31,ReturnObj32,ReturnObj33,ReturnObj34,ReturnObj35)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustNameTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustAddrTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcustAddr2TextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCityTextBox.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VStateTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VZipTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldNameTextBox.value = ReturnObj30;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddrTextBox.value = ReturnObj31;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddr2TextBox.value = ReturnObj32;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldCityTextBox.value = ReturnObj33;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldStateTextBox.value = ReturnObj34;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldZipTextBox.value = ReturnObj35;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox').focus();
  
  
  if(ReturnObj14=="Prepaid")
  {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD1.checked=true;
  }
  else if(ReturnObj14=="Bill")
  {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD2.checked=true;
  }
  else if(ReturnObj14=="Collect")
  {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD3.checked=true;
  }
  else
  {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD4.checked=true;
  }
  if(ReturnObj17=="DEST")
  {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD5.checked=true;
  }
  else
  {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD6.checked=true;
  }
  
  //var sname=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VSnameTextBox");
  //sname.innerText = ReturnObj10;
  //document.forms[0].ctl00_ContentPlaceHolder1_HiddenField11.value = ReturnObj10;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSnameTextBox.value = ReturnObj10;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSmanTextBox.value = ReturnObj9;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCarrierTextBox.value = ReturnObj18;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenCarr.value = ReturnObj18;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VContactTextBox.value = ReturnObj19;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VOverpctTextBox.value = ReturnObj20;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VUnderpctTextBox.value = ReturnObj21;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox.value = ReturnObj22;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTerm.value = ReturnObj22;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermdscrTextBox.value = ReturnObj23;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTermdesc.value = ReturnObj23;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VProdTextBox.value = ReturnObj24;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox.value = ReturnObj25;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox.value = ReturnObj26;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.value = ReturnObj27;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VLastDateTextBox.value = ReturnObj28;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenField10.value = ReturnObj28;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList1.value = ReturnObj29;  
   
   if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VPonumTextBox"))
    {
        var cust=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VPonumTextBox");
        cust.focus();
    }
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox").onchange();
}

function estlook()
{
    if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox.value="";
    estimatelook();
}
function estlook12()
{
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RfqTextBox.value="";
    quotelook();
}

//function estimatelookcopy()
//{
//  var NewWindow = window.open("estup_lookupcopy.aspx","ordestimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}
//function ordestimateLookupcopy(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14,ReturnObj15,ReturnObj16,ReturnObj17,ReturnObj18,ReturnObj19,ReturnObj20,ReturnObj21,ReturnObj22,ReturnObj23,ReturnObj24,ReturnObj25,ReturnObj26,ReturnObj27,ReturnObj28,ReturnObj29,ReturnObj30,ReturnObj31,ReturnObj32,ReturnObj33,ReturnObj34,ReturnObj35,ReturnObj36,ReturnObj37,ReturnObj38)
//{ 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_estimateTextBox.value = ReturnObj1;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VCustomerTextBox.value = ReturnObj2;
//  
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VCustNameTextBox.value = ReturnObj3;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSoldNameTextBox.value = ReturnObj25;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSoldTextBox.value = ReturnObj4;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSmanTextBox.value = ReturnObj5;
//   
// 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VCarrierTextBox.value = ReturnObj6;
//  
//  if(ReturnObj7=="P")
//document.forms[0].ctl00_ContentPlaceHolder1_FormView2_RD1.checked=true;
//if(ReturnObj7=="B")
//document.forms[0].ctl00_ContentPlaceHolder1_FormView2_RD2.checked=true;
//if(ReturnObj7=="C")
//document.forms[0].ctl00_ContentPlaceHolder1_FormView2_RD3.checked=true;
//if(ReturnObj7=="T")
//document.forms[0].ctl00_ContentPlaceHolder1_FormView2_RD4.checked=true;

//   document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vSpctTextBox.value = ReturnObj8;
//   
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList1.value =ReturnObj9;

//    

//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VJobTextBox.value = ReturnObj10;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VJob2TextBox.value = ReturnObj11;
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VCustAddrTextBox.value = ReturnObj12;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSoldAddrTextBox.value = ReturnObj26;
//  
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VcustAddr2TextBox.value = ReturnObj13;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSoldAddr2TextBox.value = ReturnObj27;
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VCityTextBox.value = ReturnObj14;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSoldCityTextBox.value = ReturnObj28;
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VStateTextBox.value = ReturnObj15;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSoldStateTextBox.value = ReturnObj29;
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VZipTextBox.value = ReturnObj16;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSoldZipTextBox.value = ReturnObj16;

//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VContactTextBox.value = ReturnObj17;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VTermsTextBox.value = ReturnObj18;
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VOverpctTextBox.value = ReturnObj19;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VUnderpctTextBox.value = ReturnObj20;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VTaxgrTextBox.value = ReturnObj22;
//  
//  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vScommTextBox!=null)
//  {
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vScommTextBox.value = ReturnObj24;
//   }
// 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSnameTextBox.value = ReturnObj31;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VTermdscrTextBox.value = ReturnObj32;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VLastDateTextBox.value = ReturnObj33;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VDueDateTextBox.value = ReturnObj34;
//  
//  if(document.forms[0].ctl00_ContentPlaceHolder1_VnewordernoTextbox_vScommTextBox!=null)
//  {
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VnewordernoTextbox.value = ReturnObj35;
//  }
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSnameTextBox.value = ReturnObj36;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VProdTextBox.value = ReturnObj37;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_RfqTextBox.value = ReturnObj38;
//  
//  if(ReturnObj21=="DEST")
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_RD5.checked=true;
//   else
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_RD6.checked=true;
// if(ReturnObj23=="O")
// document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.SelectedIndex=0;
// if(ReturnObj23=="C")
// document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.SelectedIndex=1;
// if(ReturnObj23=="N")
// document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.SelectedIndex=2;
// if(ReturnObj23=="Q")
// document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.SelectedIndex=3;
// if(ReturnObj23=="R")
// document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.SelectedIndex=4;
// if(ReturnObj23=="T")
// document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.SelectedIndex=5;
// if(ReturnObj23=="X")
// document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.SelectedIndex=6;


// }


//function estimatelook()
//{ 
//  var NewWindow = window.open("estup_lookup.aspx","ordestimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}
//function ordestimateLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14,ReturnObj15,ReturnObj16,ReturnObj17,ReturnObj18,ReturnObj19,ReturnObj20,ReturnObj21,ReturnObj22,ReturnObj23,ReturnObj24,ReturnObj25,ReturnObj26,ReturnObj27,ReturnObj28,ReturnObj29,ReturnObj30,ReturnObj31,ReturnObj32,ReturnObj33,ReturnObj34,ReturnObj35,ReturnObj36,ReturnObj37,ReturnObj38)
//{ 
//  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox)
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox.value = ReturnObj1;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox.value = ReturnObj2;
//  
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustNameTextBox.value = ReturnObj3;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldNameTextBox.value = ReturnObj25;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox.value = ReturnObj4;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSmanTextBox.value = ReturnObj5;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCarrierTextBox.value = ReturnObj6;
//  
//    if(ReturnObj7=="P")
//        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD1.checked=true;
//    if(ReturnObj7=="B")
//        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD2.checked=true;
//    if(ReturnObj7=="C")
//        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD3.checked=true;
//    if(ReturnObj7=="T")
//        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD4.checked=true;
//        
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpctTextBox.value = ReturnObj8;   
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList1.value =ReturnObj9;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VJobTextBox.value = ReturnObj10;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VJob2TextBox.value = ReturnObj11;  
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustAddrTextBox.value = ReturnObj12;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddrTextBox.value = ReturnObj26;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcustAddr2TextBox.value = ReturnObj13;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddr2TextBox.value = ReturnObj27;  
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCityTextBox.value = ReturnObj14;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldCityTextBox.value = ReturnObj28;  
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VStateTextBox.value = ReturnObj15;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldStateTextBox.value = ReturnObj29;  
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VZipTextBox.value = ReturnObj16;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldZipTextBox.value = ReturnObj16;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VContactTextBox.value = ReturnObj17;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox.value = ReturnObj18;  
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VOverpctTextBox.value = ReturnObj19;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VUnderpctTextBox.value = ReturnObj20;
//   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox.value = ReturnObj22;
//  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSnameTextBox.value = ReturnObj36;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermdscrTextBox.value = ReturnObj32;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VLastDateTextBox.value = ReturnObj33;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.value = ReturnObj34;  
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VnewordernoTextbox.value = ReturnObj35;
//  sname.innerText = ReturnObj36;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VProdTextBox.value = ReturnObj37;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RfqTextBox.value = ReturnObj38;
//  
//  if(ReturnObj21=="DEST")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD5.checked=true;
//   else
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD6.checked=true;
// if(ReturnObj23=="O")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=0;
// if(ReturnObj23=="C")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=1;
// if(ReturnObj23=="N")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=2;
// if(ReturnObj23=="Q")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=3;
// if(ReturnObj23=="R")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=4;
// if(ReturnObj23=="T")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=5;
// if(ReturnObj23=="X")
//    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=6;
// } 

function typelook()
{ 
  var NewWindow = window.open("type_ordlookup.aspx","typeordLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function typeordLookup(ReturnObj1)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_ordtypeTextBox.value = ReturnObj1;
}
function carrierlook()
{ 
  var NewWindow = window.open("Carrier_lookup.aspx","CarrierLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrierlookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCarrierTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenCarr.value = ReturnObj1;
}

 
function salesreplook()
{ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSmanTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSnameTextBox.value = ReturnObj2;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_VSmanTextBox').focus();
}

//function salesreplookcopy(){ 
//  var NewWindow = window.open("salesrep_lookupcopy.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}
//function SalesRepLookupcopy(ReturnObj1,ReturnObj2){ 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSmanTextBox.value = ReturnObj1;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSnameTextBox.value = ReturnObj2;
//}

function smancopylook1()
{ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSman2TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSname2TextBox.value = ReturnObj2;
}

//function smancopylook1copy(){ 
//  var NewWindow = window.open("sman_copylookupcopy.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}
//function smancopyLookupcopy(ReturnObj1,ReturnObj2){ 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSman2TextBox.value = ReturnObj1;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSname2TextBox.value = ReturnObj2;
//}

function salesmanlook()
{ 
  var NewWindow = window.open("salesman_lookup.aspx","SalesManLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup1(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSman3TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSname3TextBox.value = ReturnObj2; 
}

//function salesmanlookcopy(){ 
//  var NewWindow = window.open("salesman_lookupcopy.aspx","SalesManLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}
//function smancopyLookup1copy(ReturnObj1,ReturnObj2){ 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSman3TextBox.value = ReturnObj1;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VSname3TextBox.value = ReturnObj2;
// 
//}

function termslook()
{ 
  var NewWindow = window.open("terms_lookup.aspx","TermsLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function termsLookup(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTerm.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermdscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTermdesc.value = ReturnObj2;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox').focus();
}

//function termslookcopy(){ 
//  var NewWindow = window.open("terms_lookupcopy.aspx","TermsLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}
//function termsLookupcopy(ReturnObj1,ReturnObj2){ 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VTermsTextBox.value = ReturnObj1;
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VTermdscrTextBox.value = ReturnObj2;
//}

function taxlook()
{ 
  var NewWindow = window.open("tax_lookup.aspx","TaxLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TaxLookup(ReturnObj1)
{
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox.value = ReturnObj1;
    document.getElementById('ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox').focus();
 }
 
// function taxlookcopy(){ 
//  var NewWindow = window.open("tax_lookupcopy.aspx","TaxLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//}
//function TaxLookupcopy(ReturnObj1){ 
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VTaxgrTextBox.value = ReturnObj1;
// }
 
function duelook()
{ 
  var NewWindow = window.open("Due_lookup.aspx","DueCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function dueLookup(ReturnObj1)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VDueCodeTextBox.value = ReturnObj1;
}

function Date5look()
{ 
  var NewWindow = window.open("date_lookup5.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup5(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel.value=obj;
}
function Datelook5()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel.value="";
  Date5look();
}

function Datelook()
{ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.value=obj;
}

//function Datelookcopy(){ 
//  var NewWindow = window.open("date_lookupcopy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//  }
//function Datelookupcopy(obj)
//{
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VDueDateTextBox.value=obj;
//}

function Datelook1()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.value="";
  Datelook();
}
function Date2look()
{ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup2(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VLastDateTextBox.value=obj;
}

//function Date2lookcopy(){ 
//  var NewWindow = window.open("date_lookup2copy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//  }
//function Datelookup2copy(obj)
//{
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VLastDateTextBox.value=obj;
//}

function Datelook2()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VLastDateTextBox.value="";
  Date2look();
}
function Date3look()
{ 
  var NewWindow = window.open("date3_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup3(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VProdDateTextBox.value=obj;
}

//function Date3lookcopy(){ 
//  var NewWindow = window.open("date3_lookupcopy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//  }
//function Datelookup3copy(obj)
//{
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VProdDateTextBox.value=obj;
//}

function Datelook3()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VProdDateTextBox.value="";
  Date3look();
}
function Date4look()
{ 
  var NewWindow = window.open("date4_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Datelookup4(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox.value=obj;
}

//function Date4lookcopy(){ 
//  var NewWindow = window.open("date4_lookupcopy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
//  }
//function Datelookup4copy(obj)
//{
//  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_VcExpTextBox.value=obj;
//}

function Datelook4()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox.value="";
  Date4look();
}
function estimatelook()
{ 
  var NewWindow = window.open("est_order_lookup.aspx","ordestimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function estimateLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13, ReturnObj14, ReturnObj15, ReturnObj16, ReturnObj17, ReturnObj18, ReturnObj19, ReturnObj20, ReturnObj21, ReturnObj22, ReturnObj23, ReturnObj24, ReturnObj25, ReturnObj26, ReturnObj27, ReturnObj28, ReturnObj29, ReturnObj30, ReturnObj31, ReturnObj32, ReturnObj33, ReturnObj34, ReturnObj35, ReturnObj36, ReturnObj37, ReturnObj38, ReturnObj39)
{
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox)
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox.value = ReturnObj2;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustNameTextBox.value = ReturnObj3;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldNameTextBox.value = ReturnObj25;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox.value = ReturnObj4; 
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSmanTextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCarrierTextBox.value = ReturnObj6;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenCarr.value = ReturnObj6;
    document.getElementById('ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox').focus();

    if (ReturnObj7 == "P")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD1.checked = true;
    if (ReturnObj7 == "B")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD2.checked = true;
    if (ReturnObj7 == "C")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD3.checked = true;
    if (ReturnObj7 == "T")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD4.checked = true;
 
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList1.value = ReturnObj9;

    var job1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VJobTextBox");
    var job2 = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VJob2TextBox");
    job1.innerText = ReturnObj10;
    job2.innerText = ReturnObj11;

    document.forms[0].ctl00_ContentPlaceHolder1_HiddenField8.value = ReturnObj10;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenField9.value = ReturnObj11;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustAddrTextBox.value = ReturnObj12;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddrTextBox.value = ReturnObj26;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcustAddr2TextBox.value = ReturnObj13;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddr2TextBox.value = ReturnObj27;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCityTextBox.value = ReturnObj14;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldCityTextBox.value = ReturnObj28;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VStateTextBox.value = ReturnObj15;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldStateTextBox.value = ReturnObj29;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VZipTextBox.value = ReturnObj16;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldZipTextBox.value = ReturnObj16;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VContactTextBox.value = ReturnObj17;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox.value = ReturnObj18;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenTerm.value = ReturnObj18;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VOverpctTextBox.value = ReturnObj19;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VUnderpctTextBox.value = ReturnObj20;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox.value = ReturnObj22;
    
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSnameTextBox.value = ReturnObj36;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermdscrTextBox.value = ReturnObj32;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenTermdesc.value = ReturnObj32;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VLastDateTextBox.value = ReturnObj33;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenField10.value = ReturnObj33;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.value = ReturnObj34;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VnewordernoTextbox.value = ReturnObj35;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VProdTextBox.value = ReturnObj37;
    if (ReturnObj38 != "" && ReturnObj38 != "0") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RfqTextBox.value = ReturnObj38;
        document.forms[0].ctl00_ContentPlaceHolder1_HiddenQuoteNum.value = ReturnObj38;
    }
    else {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RfqTextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_HiddenQuoteNum.value = null;
    }
    if (ReturnObj21 == "DEST")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD5.checked = true;
    else
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD6.checked = true;
    if (ReturnObj23 == "O")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 0;
    if (ReturnObj23 == "C")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 1;
    if (ReturnObj23 == "N")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 2;
    if (ReturnObj23 == "Q")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 3;
    if (ReturnObj23 == "R")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 4;
    if (ReturnObj23 == "T")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 5;
    if (ReturnObj23 == "X")
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 6;

    if (ReturnObj39 == 4 || ReturnObj39 == 8) {
        var contect = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox");
        contect.focus();
    }

    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_estimateTextBox").onchange(); 
} 


function quotelook()
{ 
  var NewWindow = window.open("quote_lookup.aspx","ordestimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function quoteLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14,ReturnObj15,ReturnObj16,ReturnObj17,ReturnObj18,ReturnObj19,ReturnObj20,ReturnObj21,ReturnObj22,ReturnObj23,ReturnObj24,ReturnObj25,ReturnObj26,ReturnObj27,ReturnObj28,ReturnObj29,ReturnObj30,ReturnObj31,ReturnObj32,ReturnObj33,ReturnObj34,ReturnObj35,ReturnObj36,ReturnObj37,ReturnObj38,ReturnObj39)
{ 
  if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox)
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox.value = ReturnObj2;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustNameTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldNameTextBox.value = ReturnObj25;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox.value = ReturnObj4;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSmanTextBox.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCarrierTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenCarr.value = ReturnObj6;
  if (document.forms[0].ctl00_ContentPlaceHolder1_FormView1_estimateTextBox)
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_estimateTextBox').focus();
  
  if(ReturnObj7=="P")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD1.checked=true;
  if(ReturnObj7=="B")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD2.checked=true;
  if(ReturnObj7=="C")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD3.checked=true;
  if(ReturnObj7=="T")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD4.checked=true;

   
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList1.value =ReturnObj9;  
    
    
    var job1=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VJobTextBox");
    var job2=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VJob2TextBox");
    job1.innerText=ReturnObj10;
    job2.innerText=ReturnObj11;
    
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenField8.value = ReturnObj10;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenField9.value = ReturnObj11;  
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustAddrTextBox.value = ReturnObj12;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddrTextBox.value = ReturnObj26;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VcustAddr2TextBox.value = ReturnObj13;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddr2TextBox.value = ReturnObj27;  
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCityTextBox.value = ReturnObj14;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldCityTextBox.value = ReturnObj28;  
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VStateTextBox.value = ReturnObj15;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldStateTextBox.value = ReturnObj29;  
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VZipTextBox.value = ReturnObj16;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldZipTextBox.value = ReturnObj16;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VContactTextBox.value = ReturnObj17;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermsTextBox.value = ReturnObj18; 
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenTerm.value = ReturnObj18;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VOverpctTextBox.value = ReturnObj19;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VUnderpctTextBox.value = ReturnObj20;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTaxgrTextBox.value = ReturnObj22;
  

  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSnameTextBox.value = ReturnObj36;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTermdscrTextBox.value = ReturnObj32;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenTermdesc.value = ReturnObj32;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VLastDateTextBox.value = ReturnObj33;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenField10.value = ReturnObj33;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.value = ReturnObj34;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VnewordernoTextbox.value = ReturnObj35;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VProdTextBox.value = ReturnObj37;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RfqTextBox.value = ReturnObj38;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenQuoteNum.value = ReturnObj38;
  if(ReturnObj21=="DEST")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD5.checked=true;
  else
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_RD6.checked=true;
  if(ReturnObj23=="O")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=0;
  if(ReturnObj23=="C")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=1;
  if(ReturnObj23=="N")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=2;
  if(ReturnObj23=="Q")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=3;
  if(ReturnObj23=="R")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=4;
  if(ReturnObj23=="T")
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex=5;
  if(ReturnObj23=="X")
      document.forms[0].ctl00_ContentPlaceHolder1_FormView1_DropDownList2.SelectedIndex = 6;
 
      
    var NewWindow = window.open("quantity_lookup.aspx?est="+ReturnObj38+"","typeordLookupWindow","width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");

    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_RfqTextBox").onchange();       
 } 

 function QuantityLookup(ReturnObj1,ReturnObj2,ReturnObj3)
 {
 
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenQuoteQty.value=ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenQuotePrice.value=ReturnObj2; 
    document.forms[0].ctl00_ContentPlaceHolder1_HiddenQuoteUom.value=ReturnObj3; 
    
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_estimateTextBox"))
    {
        var est=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_estimateTextBox");
        est.focus();
    }
    else if(document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox"))
        {
            var contact=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VContactTextBox");
            contact.focus();
        } 
 }
 
 
 
 function ShipTOLook()
 { 
    var lookHidden = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VCustomerTextBox").value;
    var NewWindow = window.open("soldto_lookup.aspx?look="+lookHidden +"","SoldToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function SoldToLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldNameTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddrTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldAddr2TextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldCityTextBox.value = ReturnObj5;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldStateTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VSoldZipTextBox.value = ReturnObj7;
  document.getElementById('ctl00_ContentPlaceHolder1_FormView1_VSoldTextBox').focus();   
}
function overrun()
{
    var over=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VOverpctTextBox").value;
    
    if(over.indexOf(".") != -1)
    {        
        return;
    } 
    else if(over.length > 2 && over.length < 4)
        over=over + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VOverpctTextBox").value = over;
}
function underrun()
{
    var under=document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VUnderpctTextBox").value;
    if(under.indexOf(".") != -1)
    {        
        return;
    } 
    else if(under.length > 2 && under.length < 4)
        under=under + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView1_VUnderpctTextBox").value = under;
}

</script>


    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />
    <asp:HiddenField ID="HiddenField4" runat="server" />
    <asp:HiddenField ID="HiddenField5" runat="server" />
    <asp:HiddenField ID="HiddenField6" runat="server" />
    <asp:HiddenField ID="HiddenField7" runat="server" />
    <asp:HiddenField ID="HiddenField8" runat="server" />
    <asp:HiddenField ID="HiddenField9" runat="server" />
    <asp:HiddenField ID="HiddenField10" runat="server" />
    <asp:HiddenField ID="HiddenField11" runat="server" />
    <asp:HiddenField ID="HiddenCarr" runat="server" />
    <asp:HiddenField ID="HiddenTerm" runat="server" />
    <asp:HiddenField ID="HiddenTermdesc" runat="server" />
    <asp:HiddenField ID="Hiddensold" runat="server" />
    
    <asp:HiddenField ID="HiddenQuoteQty" runat="server" />
    <asp:HiddenField ID="HiddenQuotePrice" runat="server" />
    <asp:HiddenField ID="HiddenQuoteUom" runat="server" />
    <asp:HiddenField ID="HiddenQuoteNum" runat="server" />
    
    
    <asp:Button ID="NewAddButton"  runat="server" CssClass="button" Text="Add" OnClick="newAddButton_Click" />
    
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" Font-Bold="true" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender" OnUnload="FormView1_Unload">
        <ItemTemplate>
            <table width="800px">
                <tr>
                    <td colspan="2">
                        <table class="shade" width="800px">
                            <tr>
                                <td><b>Order:</b></td>
                                <td>
                                    <asp:Label ID="VOrderNumLabel11" runat="server" Width="100px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VOrderNum") %>'></asp:Label>
                                </td>
                                <td><b>Type:</b></td>
                                <td>
                                    <asp:Label ID="ordtypeLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ordtype") %>'></asp:Label> 
                                </td>
                                <td><b>Quote:</b></td>
                                <td>
                                    <asp:Label ID="vRfqLabel" Width="50px" runat="server" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vRfq") %>'></asp:Label>
                                </td>
            
                                <td><b><asp:Label ID="est_label" runat="server" Text="Estimate:"></asp:Label></b></td>
                                <td><asp:Label ID="EstimateTextBox" runat="server"  Width="70px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VEstimate") %>'></asp:Label></td>
                                <td><b>Job:</b></td>
                                <td><asp:Label ID="Label3" runat="server" Width="70px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VJob") %>'></asp:Label>
                                <asp:Label ID="Label4" runat="server" Width="30px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VJob2") %>'></asp:Label></td>
                                
                                <td nowrap><b>Last User:</b></td>
                                <td>
                                    <asp:Label ID="VUseridLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VUserid") %>'></asp:Label>
                                </td>
                                <td><b>Status:</b></td>
                                <td>
                                    <asp:Label ID="VStatLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VStat") %>'></asp:Label>
                                </td>
                           </tr>
                      </table>
                  </td>
             </tr>
         
             <tr>
                <td colspan="2">        
                    <table>
                        <tr>
                            <td>
                                <fieldset style="width:270px; border:solid 1px black; background-color:#EFF3FB; height:150px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Bill To:</b></td>
                                            <td>
                                                <asp:Label ID="VCustomerLabel" runat="server" Width="212px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustomer") %>'></asp:Label>
                                            </td>
                                         </tr>
                                         <tr>
                                            <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                            <td>
                                                <asp:Label ID="VCustNameLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustName") %>'></asp:Label>
                                            </td>
                                         </tr>
                                         <tr>
                                            <td></td>
                                            <td>
                                                <asp:Label ID="VCustAddrLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustAddr") %>'></asp:Label>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td></td>
                                            <td>
                                                <asp:Label ID="VcustAddr2Label" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VcustAddr2") %>'></asp:Label>
                                            </td>
                                        </tr>
                                        <tr>
                                            <td></td>
                                            <td nowrap>           
                                                <asp:Label ID="VCityLabel" runat="server" Width="127px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCity") %>'></asp:Label>
                                                <asp:Label ID="VStateLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VState") %>'></asp:Label>
                                                <asp:Label ID="VZipLabel" runat="server" Width="54px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VZip") %>'></asp:Label></td>
                                        </tr>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                                            <td>
                                                <asp:Label ID="VContactLabel" Width="212px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VContact") %>'></asp:Label>
                                            </td>
                                        </tr>
                                  </table>
                              </fieldset>
                          </td>
                          <td>
                            <fieldset style="width:270px; border:solid 1px black; background-color:#EFF3FB; height:150px;">
                                <table>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Sold To:</b></td>
                                        <td>
                                            <asp:Label ID="VSoldLabel" runat="server"  Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSold") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                        <td>
                                            <asp:Label ID="VSoldNameLabel" runat="server" Width="212px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldName") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td></td>
                                        <td>
                                            <asp:Label ID="VSoldAddrLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldAddr") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td></td>
                                        <td>
                                            <asp:Label ID="VSoldAddr2Label" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldAddr2") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td></td>
                                        <td nowrap>             
                                            <asp:Label ID="VSoldCityLabel" runat="server" Width="127px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldCity") %>'></asp:Label>
                                            <asp:Label ID="VSoldStateLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldState") %>'></asp:Label>
                                            <asp:Label ID="VSoldZipLabel" runat="server" Width="54px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldZip") %>'></asp:Label>
                                        </td>
                                    </tr>
                                </table>
                             </fieldset>
                          </td>
                          <td>
                            <fieldset style="width:258px; border:solid 1px black; background-color:#EFF3FB; height:150px;">
                                <table>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Date:</b></td>
                                        <td>
                                            <asp:Label ID="VOrdateLabel" runat="server" Width="160px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VOrdate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Due Date:</b></td>
                                        <td>
                                            <asp:Label ID="VDueCodeLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VDueCode") %>'></asp:Label>
                                            <asp:Label ID="VDueDateLabel" Width="105px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VDueDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                     </tr>
                                     <tr>
                                        <td align="right" style="padding-right:5px"><b>Last Ship:</b></td>
                                        <td>
                                            <asp:Label ID="VLastDateLabel" Width="160px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VLastDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                     </tr>
                                     <tr>
                                        <td align="right" style="padding-right:5px"><b><asp:Label ID="prd_label" runat="server" Text="Prod Date:"></asp:Label></b></td>
                                        <td>
                                            <asp:Label ID="VProdDateLabel" runat="server" Width="160px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VProdDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                     </tr>
                                     <tr>
                                        <td align="right" style="padding-right:5px" nowrap><b>Customer Po#:</b></td>
                                        <td>
                                            <asp:Label ID="VPonumLabel" Width="160px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VPonum") %>'></asp:Label>
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
                                    <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:120px;">
                                        <table>
                                            <tr>
                                                <td align="right" style="padding-right:5px"><b>Previous Order#:</b></td>
                                                <td>
                                                    <asp:Label ID="VProdLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VProd") %>'></asp:Label>
                                                </td>
                                            </tr>
                                            <tr>
                                                <td align="right" style="padding-right:5px"><b>Overrun%</b></td>
                                                <td>
                                                    <asp:Label ID="VOverpctLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VOverpct","{0:##0.00}") %>'></asp:Label>
                                                </td>
                                                <td style="padding-right:5px"><b>Underrun%:</b>
                                                    <asp:Label ID="VUnderpctLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VUnderpct","{0:##0.00}") %>'></asp:Label>
                                                </td>
                                             </tr>
                                             <tr>
                                                <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                                                <td>
                                                    <asp:Label ID="VTaxgrLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VTaxgr") %>'></asp:Label>
                                                </td>
                                             </tr>
                                             <tr>
                                                <td align="right" style="padding-right:5px" nowrap><b>Pay Terms:</b></td>
                                                <td>
                                                    <asp:Label ID="VTermsLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VTerms") %>'></asp:Label>
                                                </td>
                                                <td>
                                                    <asp:Label ID="VTermdscrLabel" runat="server" Width="180px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VTermdscr") %>'></asp:Label>
                                                </td>
                                             </tr>
                                          </table>
                                       </fieldset>
                                    </td>
                                    <td>
                                        <fieldset style="width:405px; border:solid 1px black; background-color:#EFF3FB; height:120px;">
                                            <table>
                                                <tr>
                                                    <td align="right" style="padding-right:5px"><b>Freight Charge:</b></td>
                                                    <td><b>
                                                        <asp:RadioButton ID="RD1" Enabled="false" runat="server" />Prepaid
                                                        <asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("VFreight") %>'></asp:Label>
                                                        <asp:RadioButton ID="RD2" Enabled="false" runat="server" />Bill
                                                        <asp:RadioButton ID="RD3" Enabled="false" runat="server" />Collect
                                                        <asp:RadioButton ID="RD4" Enabled="false" runat="server" />3rd Party
                                                     </b></td>
                                                 </tr>
                                                 <tr>
                                                    <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                                    <td>
                                                        <asp:Label ID="VCarrierLabel" runat="server" Width="100px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCarrier") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td align="right" style="padding-right:5px;"><b>FOB:</b></td>
                                                    <td style="height: 23px"><b>
                                                        <asp:RadioButton ID="RD5" Enabled="false" runat="server" />Destination
                                                        <asp:Label ID="Label5" Visible="false" runat="server" Text='<%# Bind("VFob") %>'></asp:Label>
                                                        <asp:RadioButton ID="RD6" Enabled="false" runat="server" />Origin</b>
                                                    </td>
                                                 </tr>
                                              </table>
                                           </fieldset>
                                        </td>
                                     </tr>
                                  </table>
                                  <%--<table class="shade" width="200px">
                                          <tr>
                                          </tr>
                                          <tr>
                                            <td align="right" style="padding-right:5px"><b>Manage Inventory:</b></td>
                                            <td>
                                                <asp:CheckBox ID="VWhisCheckBox"   BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("VWhis") %>' Enabled="false" />
                                            </td>
                                          </tr>
                                          <tr>
                                            <td></td>
                                          </tr>
                                       </table>--%>
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
                                                        <td>&nbsp;</td>
                                                    </tr>
                                                    <tr>
                                                    <td nowrap ><b>Sales Rep</b></td>
                                                    <td>
                                                        <asp:Label ID="VSmanLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSman") %>'></asp:Label>
                                                    </td>
                                                    <td>
                                                        <asp:Label ID="VSnameLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSname") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                              </table>
                                           </fieldset>
                                        </td>
                                     <td>
                                        <fieldset style="width:405px; border:solid 1px black; background-color:#EFF3FB; height:70px;">
                                            <table>
                                                <tr>
                                                    <td align="right" style="padding-right:5px"><b>Payment Type:</b></td>
                                                    <td>
                                                        <asp:Label ID="VCtypeLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCtype") %>'></asp:Label>
                                                    </td>
                                                    <td align="right" style="padding-right:5px"><b>Expire</b></td>
                                                    <td>
                                                        <asp:Label ID="VcExpLabel" runat="server" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VcExp","{0:MM/dd/yyyy}") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td align="right" style="padding-right:5px"><b>Account#:</b></td>
                                                    <td>
                                                        <asp:Label ID="VCnumLabel" runat="server" Width="125px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCnum") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td align="right" style="padding-right:5px"><b>Ref#:</b></td>
                                                    <td>
                                                        <asp:Label ID="VCauthLabel" runat="server" Width="190px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCauth") %>'></asp:Label>                                                        
                                                    </td>
                                                    <td>
                                                        <asp:Label ID="lbl_rec_key" Visible="false" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vRecKey") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                              </table>
                                           </fieldset>
                                        </td>
                                     </tr>
                                  </table>
                               </td>
                            </tr>
                            <%--<tr><td colspan="2"><table  width="700px">
                                <tr>
                                    <td><b>Name</b>&nbsp;&nbsp;&nbsp;&nbsp;</td><td><b>% of Sales</b></td>
                                    <td><b>Comm %</b>&nbsp;&nbsp;&nbsp;&nbsp;</td>        
                                </tr>
                                <tr>
                                    <td>           
                                    <td>           
                                        <td><asp:Label ID="vSpctLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSpct") %>'></asp:Label></td>
                                         <td><asp:Label ID="vScommLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vScomm") %>'></asp:Label></td>
                               </tr>
                                <tr> <td> 
                                <asp:Label ID="VSman2Label" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSman2") %>'></asp:Label></td>
                                <td> 
                                <asp:Label ID="VSname2Label" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSname2") %>'></asp:Label></td>
                                 <td><asp:Label ID="vSpct2Label" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSpct2") %>'></asp:Label></td>
                                <td><asp:Label ID="vScomm2Label" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vScomm2") %>'></asp:Label></td>         
                                </tr>
                                <tr><td> 
                             <asp:Label ID="VSman3Label" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSman3") %>'></asp:Label></td>
                            <td>
                            <asp:Label ID="VSname3Label" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSname3") %>'></asp:Label></td>
            
                            <td><asp:Label ID="vSpct3Label" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vSpct3") %>'></asp:Label></td>
                            <td><asp:Label ID="vScomm3Label" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vScomm3") %>'></asp:Label></td>

                        </tr>--%>
        </table>
    </td>
 </tr>
 <tr>
    <td colspan="2">
        <asp:Button ID="addButton" runat="server" CommandName="new"  CssClass="button" Text="Add"></asp:Button>
        <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" CssClass="buttonM" Text="Update" />
        <%--<asp:Button ID="copybutton" runat="server" CommandName="Edit" OnClick="CopyButton_click" CssClass="buttonM" Text="Copy" />--%>
        <asp:Button ID="deleteButton" runat="server" CssClass="button" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>
    </td>
  </tr>
  <tr><td style="display:none"> <asp:Label ID="LineTotalLabel" runat="server" Text='<%# Bind("vLineTotal") %>'></asp:Label> </td></tr>
 </table>   
</ItemTemplate>

<EditItemTemplate>
    <asp:Panel ID="Panel1" DefaultButton="UpdateButton" runat="server">
        <table width="800">
            <tr>
                <td colspan="2">
                    <table class="shade" width="800">
                        <tr>
                            <td> <b>Order#:</b></td>
                            <td>
                                <asp:Label ID="VOrderNumLabel" Width="80px" BackColor="turquoise" runat="server" Text='<%# Bind("VOrderNum") %>'></asp:Label>
                            </td>
                            <td><b>Type:</b> </td>
                            <td><%--<asp:TextBox ID="ordtypeTextBox" Width="30px" runat="server" Text='<%# Bind("ordtype") %>'></asp:TextBox>--%>
                                <asp:DropDownList ID="DropDownList2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" DataTextField='<%# Bind("ordtype") %>' runat="server" SelectedValue='<%# Bind("ordtype") %>'>
                                    <asp:ListItem Value="O">O- Original</asp:ListItem>
                                    <asp:ListItem Value="C">C- Change</asp:ListItem>
                                    <asp:ListItem Value="N">N- New</asp:ListItem>
                                    <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                    <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                    <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                    <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                 </asp:DropDownList>
                                 <%--<a href="#" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                             </td>
                             <td><b>Quote:</b></td>
                                    <td nowrap>
                                    
                                        <asp:Label ID="quoteTextBox" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("VEstimate") %>'> </asp:Label>                                        
                                    </td>
                                    <td> <b><asp:Label ID="est_Label" runat="server" Text="Estimate:"></asp:Label></b></td>
                                    <td nowrap>
                                        <asp:Label ID="estimateTextBox" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("VEstimate") %>'> </asp:Label>
                                        <%--<a href="#" onClick="quotelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                    </td>
                            <td nowrap> <b>Job#:</b></td>
                                <td><asp:label ID="Label8" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("VJob") %>'></asp:Label>
                                <td><asp:label ID="Label9" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("VJob2") %>'></asp:Label></td>  
                            <td nowrap> <b>Last User:</b></td>
                            <td>
                                <asp:label ID="VUseridLabel" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("VUserid") %>'></asp:Label>
                            </td>
                            <td nowrap><b>Status:</b></td>
                            <td>
                                <asp:Label ID="VStatLabel" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("VStat") %>'></asp:Label>
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
                                    <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:160px;">
                                        <table>
                                            <tr>
                                                <td align="right" style="padding-right:5px" ><b>Bill To:</b></td>
                                                <td nowrap>
                                                    <asp:TextBox Enabled="false" ID="VCustomerTextBox2" Width="120px" MaxLength="8" runat="server" Text='<%# Bind("VCustomer") %>'></asp:TextBox>
                                                    <%--<a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                </td>
                                            </tr>
                                            <tr>
                                                <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                                <td>
                                                    <asp:TextBox ID="VCustNameTextBox" Enabled="false" Width="190px"  runat="server" Text='<%# Bind("VCustName") %>'></asp:TextBox>
                                                </td>
                                            </tr>
                                            <tr>
                                                <td></td>
                                                <td>
                                                    <asp:TextBox ID="VCustAddrTextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VCustAddr") %>'></asp:TextBox>
                                                </td>
                                            </tr>
                                            <tr>
                                                <td></td>
                                                <td>
                                                    <asp:TextBox ID="VcustAddr2TextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VcustAddr2") %>'></asp:TextBox>
                                                </td>
                                             </tr>
                                             <tr>
                                                <td></td>
                                                <td> 
                                                    <asp:TextBox ID="VCityTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("VCity") %>'></asp:TextBox>
                                                    <asp:TextBox ID="VStateTextBox" runat="server" Enabled="false" Width="20px" Text='<%# Bind("VState") %>'></asp:TextBox>
                                                    <asp:TextBox ID="VZipTextBox" runat="server" Enabled="false" Width="50px" Text='<%# Bind("VZip") %>'></asp:TextBox>
                                                </td>
                                             </tr>
                                             <tr>
                                                <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                                                <td>
                                                    <asp:TextBox ID="VContactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="190px" MaxLength="25" runat="server" Text='<%# Bind("VContact") %>'></asp:TextBox>
                                                </td>
                                             </tr>
                                          </table>
                                       </fieldset>
                                    </td>
                                    <td>
                                        <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:160px;">
                                            <table>
                                                <tr>
                                                    <td align="right" style="padding-right:5px"><b>Sold To:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="VSoldTextBox" Width="120px" MaxLength="8" Enabled="false"  runat="server" Text='<%# Bind("VSold") %>'></asp:TextBox>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="VSoldNameTextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VSoldName") %>'></asp:TextBox>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td></td>
                                                    <td>
                                                        <asp:TextBox ID="VSoldAddrTextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VSoldAddr") %>'></asp:TextBox>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td></td>
                                                    <td>
                                                        <asp:TextBox ID="VSoldAddr2TextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VSoldAddr2") %>'></asp:TextBox>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td></td>
                                                    <td>
                                                        <asp:TextBox ID="VSoldCityTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("VSoldCity") %>'></asp:TextBox>            
                                                        <asp:TextBox ID="VSoldStateTextBox" Width="20px" Enabled="false" runat="server" Text='<%# Bind("VSoldState") %>'></asp:TextBox>           
                                                        <asp:TextBox ID="VSoldZipTextBox" Width="50px" Enabled="false" runat="server" Text='<%# Bind("VSoldZip") %>'></asp:TextBox>
                                                    </td>
                                                 </tr>
                                              </table>
                                           </fieldset>
                                        </td>
                                        <td>
                                            <fieldset style="width:280px; border:solid 1px black; background-color:#EFF3FB; height:160px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Date:</b></td>
                                                        <td nowrap><asp:TextBox ID="VOrdateLabel" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" Width="80px" runat="server" Text='<%# Bind("VOrdate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                   <a href="#" onblur="ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel.focus()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_VOrdateLabel); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                   
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Due Date:</b></td>
                                                        <td nowrap><%--<asp:TextBox ID="VDueCodeTextBox"  Width="50px"  runat="server" Text='<%# Bind("VDueCode") %>'></asp:TextBox>--%>
                                                            <asp:DropDownList ID="DropDownList1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" SelectedValue='<%# Bind("VDueCode") %>' DataTextField='<%# Bind("VDueCode") %>' runat="server">
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
                                                             <%--<a href="#" onClick="duelook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>    --%>
                                                             <asp:TextBox ID="VDueDateTextBox" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="53px" runat="server" ToolTip="MM/DD/YYYY" Text='<%# Bind("VDueDate","{0:MM/dd/yyyy}") %>'></asp:TextBox> 
                                                             <a href="#" onblur="ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.focus()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                             
                                                          </td>
                                                       </tr>
                                                       <tr>
                                                            <td align="right" style="padding-right:5px" nowrap><b>Last Ship:</b></td>
                                                            <td nowrap>
                                                                <asp:TextBox ID="VLastDateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Enabled="false" Width="80px" runat="server" Text='<%# Bind("VLastDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <%--<a href="#" onClick="Date2look(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                            </td>
                                                       </tr>
                                                       <tr>
                                                            <td align="right" style="padding-right:5px" nowrap><b><asp:Label ID="prd_label" runat="server" Text="Prod Date:"></asp:Label></b></td>
                                                            <td nowrap>
                                                                <asp:TextBox Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" ID="VProdDateTextBox" Enabled="false" runat="server" Text='<%# Bind("VProdDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="VProdDateTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Date" SetFocusOnError="true" ErrorMessage="Invalid Date"></asp:CompareValidator>
                                                                <%--<a href="#" onClick="Date3look(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>    --%>
                                                            </td>
                                                       </tr>
                                                       <tr>
                                                            <td align="right" style="padding-right:5px" nowrap><b>Customer Po#:</b></td>
                                                            <td nowrap>
                                                                <asp:TextBox ID="VPonumTextBox" onkeyup="focusonsave()" onfocus= "javascript:focusval(this)" onblur="setsave(this)" Width="80px" MaxLength="15" runat="server" Text='<%# Bind("VPonum") %>'></asp:TextBox>
                                                                
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
                                                                <td align="right" style="padding-right:5px" nowrap><b>Previous Order#:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VProdTextBox" Width="50px" MaxLength="6" Enabled="false" runat="server" Text='<%# Bind("VProd") %>'></asp:TextBox>
                                                                    <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="VProdTextBox" ErrorMessage="Only Integer Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                                                </td>
                                                            </tr>
                                                            <tr>
                                                                <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VOverpctTextBox" MaxLength="6" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="overrun()" Width="50px"  runat="server" Text='<%# Bind("VOverpct") %>'></asp:TextBox>
                                                                    <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VOverpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                                </td>
                                                                <td align="right" style="padding-right:5px"><b>Underrun%:</b>
                                                                    <asp:TextBox ID="VUnderpctTextBox" MaxLength="6" onfocus= "javascript:focusval(this)" onblur="javascript:updateunderblurval(this)" onkeyup="underrun()" Width="50px" runat="server" Text='<%# Bind("VUnderpct") %>'></asp:TextBox>
                                                                    <asp:CompareValidator ID="UnderCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VUnderpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                                </td>
                                                            </tr>
                                                            <tr>
                                                                <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VTaxgrTextBox" Width="50px" MaxLength="3" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("VTaxgr") %>'></asp:TextBox>
                                                                    <a href="#" tabindex="1" onClick="taxlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                </td>
                                                            </tr>
                                                            <tr>
                                                                <td align="right" style="padding-right:5px"><b>Pay Terms:</b></td>
                                                                <td>
                                                                    <asp:TextBox  ID="VTermsTextBox" Enabled="false" Width="80px" MaxLength="5" runat="server" Text='<%# Bind("VTerms") %>'></asp:TextBox>
                                                                    <%--<a href="#" onClick="termslook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                                </td>
                                                                <td>
                                                                    <asp:TextBox ID="VTermdscrTextBox" Enabled="false" Width="150px" runat="server" Text='<%# Bind("VTermdscr") %>'></asp:TextBox>
                                                                </td>
                                                            </tr>
                                                         </table>
                                                      </fieldset>
                                                   </td>
                                                   <td>
                                                    <fieldset style="width:405px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                                        <table>
                                                            <tr >       
                                                                <td align="right" style="padding-right:5px"><b>Freight Charge:</b></td>
                                                                <td>
                                                                    <asp:RadioButton ID="RD1" Enabled="false" GroupName="editstatus1" runat="server" />Prepaid
                                                                    <asp:TextBox ID="FRTextBox"  Visible="false" runat="server" Text='<%# Bind("VFreight") %>'></asp:TextBox>
                                                                    <asp:RadioButton ID="RD2" Enabled="false" GroupName="editstatus1"  runat="server" />Bill
                                                                    <asp:RadioButton ID="RD3" Enabled="false" GroupName="editstatus1" runat="server" />Collect
                                                                    <asp:RadioButton ID="RD4" Enabled="false" GroupName="editstatus1" runat="server" />3rd Party
                                                                 </td>
                                                             </tr>
                                                             <tr>
                                                                <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VCarrierTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("VCarrier") %>'></asp:TextBox>
                                                                    <%--<a href="#" onClick="carrierlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                                </td>
                                                            </tr>
                                                            <tr>
                                                                <td align="right" style="padding-right:5px"><b>FOB:</b></td>
                                                                <td><b>
                                                                    <asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("VFob") %>'></asp:TextBox>
                                                                    <asp:RadioButton ID="RD5" Enabled="false" GroupName="editstatus" runat="server" />Destination
                                                                    <asp:RadioButton ID="RD6" Enabled="false" GroupName="editstatus" runat="server" />Origin
                                                                </b></td>
                                                            </tr>
                                                            <tr>
                                                                <%--<td align="right" style="padding-right:5px"><b>Manage Inventory:</b></td>
                                                                <td><asp:CheckBox ID="VWhisCheckBox" runat="server" Checked='<%# Bind("VWhis") %>' /></td>--%>
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
                                                            <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:90px;">
                                                                <table>
                                                                    <tr>
                                                                        <td><b>Sales Rep</b></td>
                                                                        <td><b>Sales Rep Name</b></td>
                                                                        <%--<td><b>% of Sales</b></td>--%>
                                                                        <td><b><%--<asp:Label ID="commLabel" runat="server" Text="Comm"></asp:Label>--%></b></td>
                                                                     </tr>
                                                                     <tr>
                                                                        <td> 
                                                                            <asp:TextBox ID="VSmanTextBox" Enabled="false" Width="50px" runat="server" Text='<%# Bind("VSman") %>'></asp:TextBox>
                                                                            <%--<a href="#" onClick="salesreplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                                        </td>
                                                                        <td>
                                                                            <asp:TextBox ID="VSnameTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("VSname") %>'></asp:TextBox>
                                                                        </td>
                                                                        <td><%--<asp:TextBox ID="vSpctTextBox" runat="server" Width="50px" Text='<%# Bind("vSpct") %>'></asp:TextBox>--%></td>
                                                                        <td><%--<asp:TextBox ID="vScommTextBox" runat="server" Width="50px" Text='<%# Bind("vScomm") %>'> </asp:TextBox>--%></td>
                                                                     </tr>
                                                                     <tr>
                                                                        <td>
                                                                            <%--<asp:TextBox ID="VSman2TextBox" Width="50px" runat="server" Text='<%# Bind("VSman2") %>'></asp:TextBox>
                                                                            <a href="#" onClick="smancopylook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                                        </td>
                                                                        <td>
                                                                            <%--<asp:TextBox ID="VSname2TextBox" Width="100px" runat="server" Text='<%# Bind("VSname2") %>'></asp:TextBox>--%>
                                                                        </td>
                                                                        <td><%--<asp:TextBox ID="vSpct2TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct2") %>'></asp:TextBox>--%></td>
                                                                        <td><%--<asp:TextBox ID="vScomm2TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm2") %>'> </asp:TextBox>--%></td>
                                                                     </tr>
                                                                     <tr>
                                                                        <td> 
                                                                            <%--<asp:TextBox ID="VSman3TextBox" Width="50px" runat="server" Text='<%# Bind("VSman3") %>'></asp:TextBox>
                                                                            <a href="#" onClick="salesmanlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                                        </td>
                                                                        <td> 
                                                                            <%--<asp:TextBox ID="VSname3TextBox" Width="100px" runat="server" Text='<%# Bind("VSname3") %>'></asp:TextBox>--%>
                                                                        </td>
                                                                        <td><%--<asp:TextBox ID="vSpct3TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct3") %>'></asp:TextBox>--%></td>
                                                                        <td><%--<asp:TextBox ID="vScomm3TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm3") %>'> </asp:TextBox>--%></td>
                                                                     </tr>
                                                                  </table>
                                                               </fieldset>
                                                            </td>
                                                            <td>
                                                                <fieldset style="width:405px; border:solid 1px black; background-color:#EFF3FB; height:90px;">
                                                                    <table>
                                                                        <tr>
                                                                            <td nowrap><b>Payment Type:</b></td>
                                                                            <td>
                                                                                <asp:TextBox ID="VCtypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="8" Width="150px" runat="server" Text='<%# Bind("VCtype") %>'></asp:TextBox>
                                                                            </td>
                                                                            <td align="right" style="padding-right:5px" nowrap><b>Expire:</b>
                                                                                <asp:TextBox ID="VcExpTextBox" MaxLength="10"  Width="53px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("VcExp","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                                <a href="#" onblur="ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox.focus()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                                
                                                                            </td>
                                                                        </tr>
                                                                        <tr>
                                                                            <td align="right" style="padding-right:5px"><b>Account#:</b></td>
                                                                            <td>
                                                                                <asp:TextBox ID="VCnumTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:accblurval(this)" MaxLength="20" Width="150px" runat="server" Text='<%# Bind("VCnum") %>'></asp:TextBox>
                                                                            </td>
                                                                            <td></td>
                                                                        </tr>
                                                                        <tr>
                                                                            <td align="right" style="padding-right:5px"><b>Ref#:</b></td>
                                                                            <td>
                                                                                <asp:TextBox ID="VCauthTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:refblurval(this)" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("VCauth") %>'></asp:TextBox>
                                                                            </td>
                                                                        </tr>
                                                                     </table>
                                                                  </fieldset>
                                                               </td>
                                                            </tr>
                                                         </table>
                                                      </td>
                                                   </tr>
                                                   <%-- <tr>
                                                            <td></td></tr></table>
                                                            </td></tr>
                                                            <tr>
                                                            <td> <b>Name</b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td><td nowrap><b>% of Sales</b></td><td><b>Comm %</b></td></tr>
                                                            <tr>
                                                            </tr>
                                                            <tr>
                                                            </tr>
                                                            <tr>
                                                            </tr></table></td>
                                                            <td><table class="shade" width="400">
                                                            <tr>
                                                            </tr>
                                                            <td></td></tr></table></td></tr>--%>
                                                            <%--<input type="hidden"  name="VRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.VRowid"))) %>' />--%>
                                                   <tr>
                                                        <td>
                                                            <asp:Button ID="UpdateButton" runat="server" Font-Bold="true" CssClass="buttonM" CausesValidation="true" OnClick="UpdateButon_click" Text="Save"></asp:Button>
                                                            <asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel" OnClick="UpdateButton_Cancel_Click" Font-Bold="true"></asp:Button>
                                                        </td>
                                                </tr>
                                      </table>
                         </asp:Panel>
      </EditItemTemplate>
        
        <InsertItemTemplate>
            <asp:Panel DefaultButton="InsertButton" runat="server">
                
                <table width="800">
                    <tr>
                        <td colspan="2">
                            <table class="shade" width="830">
                                <tr>
                                    <td><b>&nbsp;</b>
                                        <asp:Label ID="VOrderNumLabel"  Visible="false"  Width="20px" runat="server" Text='<%# Bind("VOrderNum") %>'></asp:Label> 
                                        <b>Order: &nbsp;</b>
                                            <asp:TextBox BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" ID="VnewordernoTextbox" Enabled="false" runat="server" Width="50px"></asp:TextBox>
                                    </td>
                                    <td><b>Type:</b> </td>
                                    <td><%--<asp:TextBox ID="ordtypeTextBox" Width="30px" runat="server" Text='<%# Bind("ordtype") %>'></asp:TextBox>--%>
                                        <asp:DropDownList ID="DropDownList2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" DataTextField='<%# Bind("ordtype") %>'  runat="server" SelectedValue='<%# Bind("ordtype") %>'>
                                            <asp:ListItem Value="O">O- Original</asp:ListItem>
                                            <asp:ListItem Value="C">C- Change</asp:ListItem>
                                            <asp:ListItem Value="N">N- New</asp:ListItem>
                                            <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                            <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                            <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                            <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                        </asp:DropDownList>
                                        <%--<a href="#" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                    </td>
                                    <td> <b>Quote:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="RfqTextBox" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="javascript:quoteblurval(this)" OnTextChanged="Quote_TextChanged" Width="70px" runat="server" Text='<%# Bind("VEstimate") %>'> </asp:TextBox>
                                        <a href="#" tabindex="1" onClick="quotelook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td> <b><asp:Label ID="est_Label" runat="server" Text="Estimate:"></asp:Label></b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="estimateTextBox" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Width="70px" runat="server" OnTextChanged="Estimate_TextChanged" Text='<%# Bind("VEstimate") %>'> </asp:TextBox>                                                                                
                                        <a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td> <b>Job#:</b></td>
                                        <td><asp:Label ID="VJobTextBox" Width="40px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VJob") %>'></asp:Label>
                                        <td><asp:Label ID="VJob2TextBox" Width="20px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VJob2") %>'></asp:Label></td> 
                                   <td nowrap> <b>Last User:</b></td>
                                   <td>
                                      <asp:label ID="VUseridLabel" Width="70px" runat="server" Text='<%# Bind("VUserid") %>'></asp:label>
                                   </td>
                                   <td><b>Status:</b></td>
                                   <td>
                                        <asp:Label ID="VStatLabel" Width="20px" runat="server" Text='<%# Bind("VStat") %>'></asp:Label>
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
                                            <fieldset style="width:270px; border:solid 1px black; background-color:#EFF3FB; height:160px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Bill To:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VCustomerTextBox" onfocus= "javascript:focusval(this)" onblur="setcust(this)" Width="120px" MaxLength="8" AutoPostBack="true" OnTextChanged="BillTo_TextChanged" runat="server" Text='<%# Bind("VCustomer") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                                        <td> 
                                                            <asp:TextBox ID="VCustNameTextBox" Enabled="false" Width="190px"  runat="server" Text='<%# Bind("VCustName") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td></td>
                                                        <td>
                                                            <asp:TextBox ID="VCustAddrTextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VCustAddr") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td></td>
                                                        <td>
                                                            <asp:TextBox ID="VcustAddr2TextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VcustAddr2") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td></td>
                                                        <td> 
                                                            <asp:TextBox ID="VCityTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("VCity") %>'></asp:TextBox>
                                                            <asp:TextBox ID="VStateTextBox" Enabled="false" runat="server" Width="20px" Text='<%# Bind("VState") %>'></asp:TextBox>
                                                            <asp:TextBox ID="VZipTextBox" Enabled="false" runat="server" Width="50px" Text='<%# Bind("VZip") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VContactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:contactblurval(this)" Width="190px" MaxLength="25" runat="server" Text='<%# Bind("VContact") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                </table>
                                            </fieldset>
                                         </td>
                                         <td>
                                            <fieldset style="width:270px; border:solid 1px black; background-color:#EFF3FB; height:160px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Sold To:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VSoldTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="120px" MaxLength="8"  runat="server" Text='<%# Bind("VSold") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                                        <td> 
                                                            <asp:TextBox ID="VSoldNameTextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VSoldName") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td></td>
                                                        <td>
                                                            <asp:TextBox ID="VSoldAddrTextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VSoldAddr") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td></td>
                                                        <td>
                                                            <asp:TextBox ID="VSoldAddr2TextBox" Enabled="false" Width="190px" runat="server" Text='<%# Bind("VSoldAddr2") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td></td>
                                                        <td>
                                                            <asp:TextBox ID="VSoldCityTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("VSoldCity") %>'></asp:TextBox>            
                                                            <asp:TextBox ID="VSoldStateTextBox" Width="20px" Enabled="false" runat="server" Text='<%# Bind("VSoldState") %>'></asp:TextBox>           
                                                            <asp:TextBox ID="VSoldZipTextBox" Width="50px" Enabled="false" runat="server" Text='<%# Bind("VSoldZip") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                 </table>
                                             </fieldset>
                                          </td>
                                          <td>
                                            <fieldset style="width:280px; border:solid 1px black; background-color:#EFF3FB; height:160px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Date:</b></td>
                                                        <td nowrap>
                                                            <asp:Label ID="VOrdateLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Width="80px"  runat="server" Text='<%# Bind("VOrdate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Due Date:</b></td>
                                                        <td nowrap><%--<asp:TextBox ID="VDueCodeTextBox"  Width="50px"  runat="server" Text='<%# Bind("VDueCode") %>'></asp:TextBox>--%>
                                                            <asp:DropDownList ID="DropDownList1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" SelectedValue='<%# Bind("VDueCode") %>' DataTextField='<%# Bind("VDueCode") %>' runat="server">
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
                                                             <%--<a href="#" onClick="duelook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>    --%>
                                                             <asp:TextBox ID="VDueDateTextBox" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" Width="53px" runat="server" Text='<%# Bind("VDueDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>                 
                                                                <a href="#" onblur="ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox.focus()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_VDueDateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                         </td>
                                                     </tr>
                                                     <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Last Ship:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VLastDateTextBox" Enabled="false" Width="80px" runat="server" Text='<%# Bind("VLastDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                            <%--<a href="#" onClick="Date2look(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>
                                                            <asp:Label ID="prd_label" runat="server" Text="Prod Date:"></asp:Label></b>
                                                        </td>
                                                        <td nowrap>
                                                            <asp:TextBox Width="80px" ID="VProdDateTextBox" Enabled="false" runat="server" Text='<%# Bind("VProdDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                            <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="VProdDateTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Date" SetFocusOnError="true" ErrorMessage="Invalid Date"></asp:CompareValidator>
                                                            <%--<a href="#" onClick="Date3look(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>  --%>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Customer Po#:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VPonumTextBox" onkeyup="focusonsave()" onfocus= "javascript:focusval(this)" onblur="javascript:setsave(this)" Width="80px" MaxLength="15" runat="server" Text='<%# Bind("VPonum") %>'></asp:TextBox>
                                                            
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
                                                        <td align="right" style="padding-right:5px" nowrap><b>Previous Order#:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VProdTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="6" runat="server" Text='<%# Bind("VProd") %>'></asp:TextBox>
                                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="VProdTextBox" ErrorMessage="Only Integer Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>        
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VOverpctTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="6" onkeyup="overrun()" Width="50px" runat="server" Text='<%# Bind("VOverpct") %>'></asp:TextBox>
                                                            <asp:CompareValidator ID="OverCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VOverpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                        </td>
                                                        <td align="right" style="padding-right:5px"><b>Underrun%:</b>
                                                            <asp:TextBox ID="VUnderpctTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:underblurval(this)" MaxLength="6" onkeyup="underrun()" Width="50px" runat="server" Text='<%# Bind("VUnderpct") %>'></asp:TextBox>
                                                            <asp:CompareValidator ID="UnderCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VUnderpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VTaxgrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="3" runat="server" Text='<%# Bind("VTaxgr") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="taxlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Pay Terms:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox  ID="VTermsTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" MaxLength="5" runat="server" Text='<%# Bind("VTerms") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                         </td>
                                                         <td>
                                                            <asp:TextBox ID="VTermdscrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" runat="server" Text='<%# Bind("VTermdscr") %>'></asp:TextBox>
                                                         </td>
                                                    </tr>
                                               </table>
                                           </fieldset>
                                        </td>
                                        <td>
                                            <fieldset style="width:416px; border:solid 1px black; background-color:#EFF3FB; height:110px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Freight Charge:</b></td>
                                                        <td>
                                                            <asp:RadioButton ID="RD1" GroupName="editstatus1" runat="server" />Prepaid
                                                            <asp:TextBox ID="FRTextBox"  Visible="false" runat="server" Text='<%# Bind("VFreight") %>'></asp:TextBox>
                                                            <asp:RadioButton ID="RD2" GroupName="editstatus1"  runat="server" />Bill
                                                            <asp:RadioButton ID="RD3" GroupName="editstatus1" runat="server" />Collect
                                                            <asp:RadioButton ID="RD4" GroupName="editstatus1" runat="server" />3rd Party
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VCarrierTextBox" Width="100px" Enabled="false" runat="server" Text='<%# Bind("VCarrier") %>'></asp:TextBox>
                                                            <%--<a href="#" onClick="carrierlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>FOB:</b></td>
                                                        <td><b>
                                                            <asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("VFob") %>'></asp:TextBox>
                                                            <asp:RadioButton ID="RD5" Enabled="false" GroupName="editstatus" runat="server" />Destination
                                                            <asp:RadioButton ID="RD6" Enabled="false" GroupName="editstatus" runat="server" />Origin
                                                        </b></td>
                                                    </tr>
                                                    <tr>
                                                        <%--<td align="right" style="padding-right:5px"><b>Manage Inventory:</b></td>
                                                        <td><asp:CheckBox ID="VWhisCheckBox" runat="server" Checked='<%# Bind("VWhis") %>' /></td>--%>
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
                                            <fieldset style="width:400px; border:solid 1px black; background-color:#EFF3FB; height:85px;">
                                                <table>
                                                    <tr>
                                                        <td><b>Sales Rep</b></td>
                                                        <td><b>Sales Rep Name</b></td>
                                                        <td><b><%--% of Sales--%></b></td>
                                                        <td><b><%--<asp:Label ID="commLabel" runat="server" Text="Comm"></asp:Label>--%></b></td>
                                                    </tr>
                                                    <tr>
                                                        <td> 
                                                            <asp:TextBox ID="VSmanTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="3" runat="server" Text='<%# Bind("VSman") %>'></asp:TextBox>
                                                            <a href="#"  tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                        <td>
                                                            <asp:TextBox ID="VSnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" runat="server" Text='<%# Bind("VSname") %>'></asp:TextBox>
                                                        </td>
                                                        <td>
                                                            <%--<asp:TextBox ID="vSpctTextBox" runat="server" Width="50px" Text='<%# Bind("vSpct") %>'></asp:TextBox>--%>
                                                        </td>
                                                        <td>
                                                            <%--<asp:TextBox ID="vScommTextBox" runat="server" Width="50px" Text='<%# Bind("vScomm") %>'> </asp:TextBox>--%>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td>
                                                            <%--<asp:TextBox ID="VSman2TextBox" Width="50px" runat="server" Text='<%# Bind("VSman2") %>'></asp:TextBox>
                                                            <a href="#" onClick="smancopylook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                        </td>
                                                        <td>
                                                            <%--<asp:TextBox ID="VSname2TextBox" Width="100px" runat="server" Text='<%# Bind("VSname2") %>'></asp:TextBox>--%>
                                                        </td>
                                                        <td>
                                                            <%--<asp:TextBox ID="vSpct2TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct2") %>'></asp:TextBox>--%>
                                                        </td>
                                                        <td>
                                                            <%--<asp:TextBox ID="vScomm2TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm2") %>'> </asp:TextBox>--%>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td> 
                                                            <%--<asp:TextBox ID="VSman3TextBox" Width="50px" runat="server" Text='<%# Bind("VSman3") %>'></asp:TextBox>
                                                            <a href="#" onClick="salesmanlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                        </td>
                                                        <td> 
                                                            <%--<asp:TextBox ID="VSname3TextBox" Width="100px" runat="server" Text='<%# Bind("VSname3") %>'></asp:TextBox>--%>
                                                        </td>
                                                        <td>
                                                            <%--<asp:TextBox ID="vSpct3TextBox" runat="server" Width="50px" Text='<%# Bind("vSpct3") %>'></asp:TextBox>--%>
                                                        </td>
                                                        <td>
                                                            <%--<asp:TextBox ID="vScomm3TextBox" runat="server" Width="50px" Text='<%# Bind("vScomm3") %>'> </asp:TextBox>--%>
                                                        </td>
                                                    </tr>
                                                </table>
                                            </fieldset>
                                        </td>
                                        <td>
                                            <fieldset style="width:416px; border:solid 1px black; background-color:#EFF3FB; height:85px;">
                                                <table>
                                                    <tr>
                                                        <td nowrap><b>Payment Type:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VCtypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="8" Width="150px" runat="server" Text='<%# Bind("VCtype") %>'></asp:TextBox>
                                                        </td>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Expire:</b>
                                                            <asp:TextBox ID="VcExpTextBox" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="53px" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("VcExp","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                            <a href="#" onblur="ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox.focus()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_VcExpTextBox); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                            
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                    <td align="right" style="padding-right:5px"><b>Account#:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="VCnumTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="20" Width="150px" runat="server" Text='<%# Bind("VCnum") %>'></asp:TextBox>
                                                    </td>
                                                </tr>
                                                <tr>
                                                    <td align="right" style="padding-right:5px"><b>Ref#:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="VCauthTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:refblurval(this)" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("VCauth") %>'></asp:TextBox>
                                                    </td>
                                                </tr>
                                            </table>
                                        </fieldset>
                                    </td>
                                </tr>
                            </table>
                        </td>
                    </tr>
                 </td>
              </tr>
           </table>
            <%--</td></tr>
                <tr>
                    <td> <b>Name</b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
                    <td><b>% of Sales</b></td>
                    <td><b>Comm %</b></td>
                </tr>
             </table></td>
             <td><table class="shade" width="400">
           <tr>
           </tr>
           <tr>
           <td></td></tr>
           <tr>
           <td></td></tr></table></td></tr>--%>
           
           <%-- <input type="hidden"  name="VRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.VRowid"))) %>' />--%>
           
          
           <tr>
                <td>
                    <asp:Button ID="InsertButton" runat="server" Font-Bold="true" CssClass="buttonM" OnClick="InsertButon_click" Text="Save"></asp:Button>
                    <asp:Button ID="UpdateCancelButton" OnClick="Btn_Insert_cancel" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel" Font-Bold="true" ></asp:Button>
        </asp:Panel>
     </InsertItemTemplate>
  </asp:FormView>
      <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectEstEntry" TypeName="orderentry">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmExt" Type="String" />
                <asp:SessionParameter Name="prmOrderNum" SessionField="order_est" Type="Int32" />           
                <asp:Parameter Name="prmCustomer" Type="String" />
                <asp:Parameter Name="prmUserid" Type="String" />
                <asp:Parameter Name="prmStat" Type="String" />
                <asp:Parameter Name="prmSold" Type="String" />
                <asp:Parameter Name="prmOrdate" Type="DateTime" />
                <asp:Parameter Name="prmSoldName" Type="String" />
                <asp:Parameter Name="prmDueCode" Type="String" />
                <asp:Parameter Name="prmDueDate" Type="DateTime" />
                <asp:Parameter Name="prmCustAddr" Type="String" />
                <asp:Parameter Name="prmSoldAddr" Type="String" />        
                <asp:Parameter Name="prmLastDate" Type="DateTime" />
                <asp:Parameter Name="prmcustAddr2" Type="String" />
                <asp:Parameter Name="prmSoldAddr2" Type="String" />
                <asp:Parameter Name="prmProdDate" Type="string" />
                <asp:Parameter Name="prmCity" Type="String" />
                <asp:Parameter Name="prmState" Type="String" />
                <asp:Parameter Name="prmZip" Type="String" />
                <asp:Parameter Name="prmSoldCity" Type="String" />
                <asp:Parameter Name="prmSoldState" Type="String" />
                <asp:Parameter Name="prmSoldZip" Type="String" />
                <asp:Parameter Name="prmPonum" Type="String" />
                <asp:Parameter Name="prmContact" Type="String" />
                <asp:Parameter Name="prmOverpct" Type="Decimal" />
                <asp:Parameter Name="prmUnderpct" Type="Decimal" />
                <asp:Parameter Name="prmTerms" Type="String" />
                <asp:Parameter Name="prmTermdscr" Type="String" />
                <asp:Parameter Name="prmProd" Type="Int32" />
                <asp:Parameter Name="prmTaxgr" Type="String" />
                <asp:Parameter Name="prmFreight" Type="String" />
                <asp:Parameter Name="prmCarrier" Type="String" />
                <asp:Parameter Name="prmFob" Type="String" />
                <asp:Parameter Name="prmSman" Type="String" />
                <asp:Parameter Name="prmSname" Type="String" />               
                <asp:Parameter Name="prmSman2" Type="String" />
                <asp:Parameter Name="prmSname2" Type="String" />                
                <asp:Parameter Name="prmSman3" Type="String" />
                <asp:Parameter Name="prmSname3" Type="String" />                
                <asp:Parameter Name="prmCtype" Type="String" />
                <asp:Parameter Name="prmcExp" Type="String" />
                <asp:Parameter Name="prmCnum" Type="String" />
                <asp:Parameter Name="prmCauth" Type="String" />
                <asp:Parameter Name="prmCustName" Type="String" />
                <asp:Parameter Name="prmType" Type="String" />
                <asp:Parameter Name="prmLine" Type="int32" />
                <asp:Parameter Name="prmWhis" Type="string" />
                <asp:Parameter Name="VRowid" Type="int64" />
                <asp:Parameter Name="prmJob" Type="String" />
                <asp:Parameter Name="prmJob2" Type="int32" />
                <asp:Parameter Name="prmEst" Type="string" />
                <asp:Parameter Name="prmSales1" Type="Decimal" />
                <asp:Parameter Name="prmSales2" Type="Decimal" />
                <asp:Parameter Name="prmSales3" Type="Decimal" />
                <asp:Parameter Name="prmComm1" Type="Decimal" />
                <asp:Parameter Name="prmComm2" Type="Decimal" />
                <asp:Parameter Name="prmComm3" Type="Decimal" />
                <asp:Parameter Name="prmQuote" Type="int32" />                
                <asp:Parameter Name="prmQty" Type="int32" />   
                <asp:Parameter Name="prmPrice" Type="decimal" /> 
                <asp:Parameter Name="prmUom" Type="string" />            
        </SelectParameters>
    </asp:ObjectDataSource>
    


</asp:Content>
