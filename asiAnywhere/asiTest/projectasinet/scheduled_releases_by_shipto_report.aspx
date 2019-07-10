<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="scheduled_releases_by_shipto_report" Codebehind="~/scheduled_releases_by_shipto_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Scheduled Releases By ShipTo</title>
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
    
    
    function Datelook11()
{
document.forms[0].TextBox5.value="";
Datelook1();
}

function Datelook1(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].TextBox5.value=obj;
}

function Datelook12()
{
document.forms[0].TextBox6.value="";
Datelook2();
}

function Datelook2(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].TextBox6.value=obj;
}
    
    
    
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].TextBox1.value = ReturnObj1;
  
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  
  document.forms[0].TextBox2.value = ReturnObj1;
  }

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].BeginSalesmanTextBox.value = ReturnObj1;
  }


function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].EndSalesmanTextBox.value = ReturnObj1;
  }

  function Relook() {
      var item1 = document.getElementById("TextBox1").value;
  var NewWindow = window.open("reorder_item_lookup.aspx?item="+ item1+"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox3.value = ReturnObj1;
}
function Relook2() {
    var item2 = document.getElementById("TextBox2").value; 
  var NewWindow = window.open("reorder_item_lookup2.aspx?item1="+ item2 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox4.value = ReturnObj1;
}


function orderlook(){ 
  var NewWindow = window.open("order_lookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(ReturnObj1){ 
  document.forms[0].BeginOrderTextBox.value = ReturnObj1;
}
function orderlook2(){ 
  var NewWindow = window.open("order2_lookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Order2Lookup(ReturnObj1){ 
  document.forms[0].EndOrderTextBox.value = ReturnObj1;
}


function ShipIdlook(){ 
  var NewWindow = window.open("ShipIdLook.aspx","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ShipIdLookup(ReturnObj1){ 
  document.forms[0].BeginShiptoTextBox.value = ReturnObj1;
  
}

function ShipIdlook2(){ 
  var NewWindow = window.open("ShipIdLook2.aspx","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ShipIdLookup2(ReturnObj1){ 
  document.forms[0].EndShiptoTextBox.value = ReturnObj1;
  
}
function datevalidate()
{
    var date=document.getElementById("TextBox5").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("TextBox5").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("TextBox5").value = date + "/";
    }
    var date2=document.getElementById("TextBox6").value;
    
    if(date2.length>1 && date2.length<3 && date2.indexOf('/')!=1)
    {
        document.getElementById("TextBox6").value = date2 + "/";
    }
    if(date2.length>4 && date2.length<6 && date2.indexOf('/')!=3)
    {
        document.getElementById("TextBox6").value = date2 + "/";
    }
}
    </script> 
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
         <asp:HiddenField ID="HiddenField7" runat="server" />
         <asp:HiddenField ID="HiddenField8" runat="server" />
         <asp:HiddenField ID="HiddenField9" runat="server" />
         <asp:HiddenField ID="HiddenField10" runat="server" />
                 
         
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
            
          <TD align=center nowrap><font size=+0><b>Scheduled Releases By ShipTo Report &nbsp;</b></font></TD>
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
      
      <table class="shade" width="750px">
       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()"  width="100px" AutoPostBack="true" OnTextChanged="cust_text_Click" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
      <td>
            <asp:TextBox ID="TextBox2" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        
         <tr><td align="right" style="padding-right: 5px"><b>Begining Ship To#:</b></td>
        <td><asp:TextBox ID="BeginShiptoTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="ShipIdlook(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          
        <td align="right" style="padding-right: 5px"><b>Ending Ship To#:</b></td>
          <td><asp:TextBox ID="EndShiptoTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="ShipIdlook2(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          
           <tr><td align="right" style="padding-right: 5px"><b>Beginning Order#:</b></td>
        <td><asp:TextBox ID="BeginOrderTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="orderlook(); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          
        <td align="right" style="padding-right: 5px"><b>Ending Order#:</b></td>
          <td><asp:TextBox ID="EndOrderTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="orderlook2(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          
          
        <tr><td align="right" style="padding-right: 5px"><b>Begining Item #:</b></td>
          <td nowrap><asp:TextBox ID="TextBox3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox4"  Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
          </tr>
          
           <tr><td align="right" style="padding-right: 5px"><b>Beginning Sales Rep#:</b></td>
        <td><asp:TextBox ID="BeginSalesmanTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          
        <td align="right" style="padding-right: 5px"><b>Ending Sales Rep#:</b></td>
          <td><asp:TextBox ID="EndSalesmanTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          </tr>
          
          <tr><td align="right" style="padding-right:5px"><b>Begining Date:</b></td>
          <td><asp:TextBox ID="TextBox5" MaxLength="12" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onblur="document.getElementById('TextBox5').focus()"  onClick="showCalendarControl(TextBox5); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
          </td>
         
          <td align="right" style="padding-right:5px"><b>Ending Date</b></td>
           <td nowrap>
              <asp:TextBox ID="TextBox6"  MaxLength="12" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" Width="100px" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onblur="document.getElementById('TextBox6').focus()"  onClick="showCalendarControl(TextBox6); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              
              </td>
          </tr>
       
        
          
        
        </table>
          
        
        <table class="shade" width="750px">
           
         
         <tr><td align="left" style="padding-left:10px" nowrap ><b>Releases Types 
         &nbsp;
             <asp:CheckBox ID="CheckBox1" Text="Scheduled" runat="server" />
             <asp:CheckBox ID="CheckBox2" Text="Actual" runat="server" />
             <asp:CheckBox ID="CheckBox3" Text="Late" runat="server" />
             <asp:CheckBox ID="CheckBox4" Text="Backorder" runat="server" />
             <asp:CheckBox ID="CheckBox5" Text="Past Last Ship Date" runat="server" />
             <asp:CheckBox ID="CheckBox6" Text="Posted" runat="server" />
             <asp:CheckBox ID="CheckBox7" Text="Completed" runat="server" />
             <asp:CheckBox ID="CheckBox8" Text="Invoice" runat="server" />
             </b>
         </td>
         <td>
         
         </td></tr>
         <tr>
         <td align="left" style="padding-left:10px"><b>
         Sort By: &nbsp;
         <asp:RadioButtonList ID="RadioButtonList1" runat="server" RepeatColumns="6" RepeatLayout="flow">
         <asp:ListItem Text="Customer"></asp:ListItem>
         <asp:ListItem Text="Release Date"></asp:ListItem>
         <asp:ListItem Text="Item#"></asp:ListItem>
         <asp:ListItem Text="Item Name"></asp:ListItem>
         <asp:ListItem Text="Order"></asp:ListItem>
         <asp:ListItem Text="Sales Rep"></asp:ListItem>
         </asp:RadioButtonList></b>
         </td>
         </tr>
         
         
         
        <tr><td colspan="2">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>         
          </tr>     
         </table>
       
          
       <asp:FormView ID="FormView1" Visible="false"  runat="server" DataSourceID="ObjectDataSource1">
             
              
              <ItemTemplate>
                  shipFile:
                  <asp:Label ID="shipFileLabel" runat="server" Text='<%# Bind("shipFile") %>'></asp:Label><br />
                 
              </ItemTemplate>
            
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
               SelectMethod="SelectShip" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmShipAct" Type="String" />
                  <asp:Parameter Name="prmBeginCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />         
                  <asp:Parameter Name="prmBeginItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBeginorder" Type="Int32" />
                  <asp:Parameter Name="prmEndorder" Type="Int32" />
                  <asp:Parameter Name="prmBeginship" Type="String" />
                  <asp:Parameter Name="prmEndship" Type="String" />
                  <asp:Parameter Name="prmBeginsales" Type="String" />
                  <asp:Parameter Name="prmEndsales" Type="String" />
                  <asp:Parameter Name="prmBegindate" Type="DateTime" />
                  <asp:Parameter Name="prmEnddate" Type="DateTime" />
                  <asp:Parameter Name="prmScheduled" Type="String" />
                  <asp:Parameter Name="prmActual" Type="String" />
                  <asp:Parameter Name="prmLate" Type="String" />
                  <asp:Parameter Name="prmBackOrd" Type="String" />
                  <asp:Parameter Name="prmpastdate" Type="String" />
                  <asp:Parameter Name="prmPost" Type="String" />
                  <asp:Parameter Name="prmComplete" Type="String" />
                  <asp:Parameter Name="prmInvoice" Type="String" />
                  <asp:Parameter Name="prmSort" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
          
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

