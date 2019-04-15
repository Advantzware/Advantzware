<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="cust_inv_report_list" Codebehind="cust_inv_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customer Inventory Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
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
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  document.forms[0].TextBox1.value = ReturnObj1;
  document.forms[0].TextBox2.value = ReturnObj1;
  document.forms[0].TextBox2.focus();    
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){

    document.forms[0].TextBox2.value = ReturnObj1;
    document.forms[0].TextBox2.focus();
  }
 function Inventorylook(){ 
  var NewWindow = window.open("InventoryClass.aspx","InventoryClassLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function InventoryLookup(ReturnObj1){ 
  document.forms[0].TextBox3.value = ReturnObj1;
  document.forms[0].TextBox4.value = ReturnObj1;
  document.forms[0].TextBox4.focus();   
}
function Inventoryclasslook(){ 
  var NewWindow = window.open("InventoryClassLook.aspx","InventoryClassLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function InventoryClassLookup(ReturnObj1){

    document.forms[0].TextBox4.value = ReturnObj1;
    document.forms[0].TextBox4.focus();   
}
function sameinv()
    {
    var beginc=document.getElementById("TextBox3");
    var endc=document.getElementById("TextBox4");
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
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
         <asp:HiddenField ID="HiddenField1" runat="server" />
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Customer Inventory Report&nbsp;</b></font></TD>
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
      <asp:Label ID="Label1" ForeColor="red" Font-Bold="true" runat="server" ></asp:Label>
      
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
      <table class="shade" >
     
      <tr><td align="right" style="padding-right :5px"><b>Begining Customer#:</b></td>
          <td><asp:TextBox ID="TextBox1" onkeyup="samevalue()" MaxLength="8" width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right :5px"><b>Ending Customer#:</b></td>
      <td><asp:TextBox ID="TextBox2" MaxLength="8" width="100px" runat="server"></asp:TextBox>
      <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
      <tr><td align="right" style="padding-right :5px"><b>From Inventory Class:</b></td><td>
          <asp:TextBox ID="TextBox3" onkeyup="sameinv()" Width="100px" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="Inventorylook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
         <td align="right" style="padding-right :5px"> <b>To Inventory Class:</b></td>
          <td><asp:TextBox ID="TextBox4" Width="100px" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="Inventoryclasslook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
          <tr><td >
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <b>Print To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Pdf" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>
          
          <tr><td colspan="2">
              <asp:Button ID="submitbutton" runat="server" class="buttonM" Text="Submit" OnClick="submitbutton_click" />
              &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Print File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> 
              </td>
          </tr>
       </table>
      <asp:FormView ID="FormView1" runat="server" Visible="false" OnPreRender="FormView1_PreRender" DataSourceID="ObjectDataSource1">
            
            <ItemTemplate>
                <%--abc:
                <asp:Label ID="abcLabel" runat="server" Text='<%# Bind("abc") %>'></asp:Label><br />--%>
                
                <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="CustInvoice" TypeName="reports">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="PrmAction" Type="String" />
                <asp:Parameter Name="prmOut" Type="String" />
                <asp:Parameter Name="vBeginCust" Type="String" />
                <asp:Parameter Name="vEndCust" Type="String" />
                <asp:Parameter Name="vFromlistclass" Type="String" />
                <asp:Parameter Name="vTolistclass" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

