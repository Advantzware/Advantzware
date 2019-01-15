<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="cust_inv_reorder_report" Codebehind="cust_inv_reorder_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customer Inventory Reorder Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = JavaScript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }
    
 function samevalue()
    {
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    if(endc.value!=beginc.value)
    {
    alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    }
    }
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].TextBox1.value = ReturnObj1;
  document.forms[0].TextBox2.value = ReturnObj1;
  }
  function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  
  document.forms[0].TextBox2.value = ReturnObj1;
  }


 
 
function Relook(){ 
  var NewWindow = window.open("reorder_item_lookup.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].BeitemTextBox.value = ReturnObj1;
}
function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].EnditemTextBox.value = ReturnObj1;
}
function ShipTOLook(){ 
  var lookHidden = document.getElementById("TextBox1").value;
  var NewWindow = window.open("ShipIdCustLook.aspx?look="+lookHidden +"","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipToLookup(ReturnObj1){ 
  document.forms[0].BelocTextBox.value = ReturnObj1;
}
function ShipTOLook2(){ 
 var lookHidden = document.getElementById("TextBox1").value;
  var NewWindow = window.open("ShipToCustLook.aspx?look="+ lookHidden +"","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipTo2Lookup(ReturnObj1){ 

  document.forms[0].EndlocTextBox.value = ReturnObj1;
}
function procatlook(){ 
  var NewWindow = window.open("procat_lookup.aspx","ProcatLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1){ 
  document.forms[0].BecatTextBox.value = ReturnObj1;
}

function prolook(){ 
  var NewWindow = window.open("procat_lookup2.aspx","ProcatLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procat2Lookup(ReturnObj1){ 
  document.forms[0].EndCatTextBox.value = ReturnObj1;
}


 

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].DateTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].DateTextBox.value="";
  Datelook();
}
function datevalidate()
{
    var date=document.getElementById("DateTextBox").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("DateTextBox").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("DateTextBox").value = date + "/";
    }
    
}

    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
         <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         <asp:HiddenField ID="HiddenField5" runat="server" />
         <asp:HiddenField ID="HiddenField6" runat="server" />
                
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
           <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  
                  <asp:Label ID="CustLabel" Visible="false" runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
         
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Customer Inventory Reorder Report &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table class="shade" width="700px">
       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1" onkeyup="samevalue()"   width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox2"  width="100px" runat="server"></asp:TextBox>
      <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        
        <tr><td align="right" style="padding-right:5px"><b>Beginning Ship-To</b></td>
          <td>
              <asp:TextBox ID="BelocTextBox"  Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px"><b>Ending Ship-To</b></td>
          <td>
              <asp:TextBox ID="EndlocTextBox" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="ShipTOLook2(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
        
              <tr><td align="right" style="padding-right:5px"><b>Begining Item#</b></td>
          <td>
              <asp:TextBox ID="BeitemTextBox" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px"><b>Ending Item#</b></td>
          <td>
              <asp:TextBox ID="EnditemTextBox" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
           <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Category:</b></td>
          <td><asp:TextBox ID="BecatTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right: 5px; " nowrap><b>Ending Category:</b></td>
          <td>
              <asp:TextBox ID="EndCatTextBox" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="prolook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        </tr>
         
        </table>
          
        
        <table class="shade" style="width: 700px">
         <tr>
         <td align="right" style="padding-right:5px"><b><asp:CheckBox ID="CheckBox1" Text="Use Customer Inventory" runat="server"></asp:CheckBox></b></td>
         </tr>  
         <tr>
         <td align="right" style="padding-right:5px">
         <b><asp:CheckBox ID="CheckBox2" Text="Include Quantity on Order with Quantity on Hand" runat="server"></asp:CheckBox></b></td>
         </tr><tr>
         <td align="right" style="padding-right:5px"><b>Use
          <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server" >
             
                 
                 <asp:ListItem      Text="Total Allocated" />
                  <asp:ListItem     Text="Release Qty" />
                  <asp:ListItem     Text="All" />
                 
         </asp:RadioButtonList></b></td>
           <td> <b> as of </b>
             <asp:TextBox ID="DateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="12" ToolTip="MM/DD/YYYY" runat="server"></asp:TextBox>
             <a href="#" tabindex="1" onblur="document.getElementById('DateTextBox').focus()"  onClick="showCalendarControl(DateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
             
         </td>
         </tr>
         <tr>
         <td align="right" style="padding-right:5px"> 
         <b>Print</b><asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server">
             
                  <asp:ListItem     Text="Purchased" />
                  <asp:ListItem      Text="Manufactured" />
                  <asp:ListItem     Text="Both" />
                                   
         </asp:RadioButtonList></td></tr>
         <tr>
         <td align="right" style="padding-right:5px"> 
         <b>Print Lot Controller</b><asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server">
              
                  <asp:ListItem     Text="Level" />
                  <asp:ListItem      Text="Reorder" />
                  <asp:ListItem     Text="Both" />
                                   
         </asp:RadioButtonList></td></tr>
         <tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;</td></tr>
         
          
         </table>     
          
          <asp:FormView ID="FormView1" Visible="false" runat="server" DataSourceID="ObjectDataSource1">
                           
              <ItemTemplate>
                  vCustRep:
                  <asp:Label ID="vCustRepLabel" runat="server" Text='<%# Bind("vCustRep") %>'></asp:Label><br />
                  vCustRepFile:
                  <asp:Label ID="vCustRepFileLabel" runat="server" Text='<%# Bind("vCustRepFile") %>'>
                  </asp:Label><br />
              </ItemTemplate>
              
          </asp:FormView>   
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectCustInvReReport2" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmReAct" Type="String" />
                  <asp:Parameter Name="prmBegCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBegShipto" Type="String" />
                  <asp:Parameter Name="prmEndShitto" Type="String" />
                  <asp:Parameter Name="prmBegItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBegCat" Type="String" />
                  <asp:Parameter Name="prmEndCat" Type="String" />
                  <asp:Parameter Name="prmCustInv" Type="String" />
                  <asp:Parameter Name="prmQOnHand" Type="String" />
                  <asp:Parameter Name="prmTotalAlloc" Type="String" />
                  <asp:Parameter Name="prmAsOf" Type="DateTime" />
                  <asp:Parameter Name="prmPrint" Type="String" />
                  <asp:Parameter Name="prmPrintLotCtl" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

