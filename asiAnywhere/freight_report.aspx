<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="freight_report_list" Codebehind="freight_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<HTML xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Freight Report</title>
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




function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox4.value = ReturnObj1;
}
function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].bedateTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].bedateTextBox.value="";
  Datelook();
}
function Date2look(){ 
  var NewWindow = window.open("date_lookup2.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup2(obj)
{
  document.forms[0].enddateTextBox.value=obj;
}

function Datelook2()
{
  document.forms[0].enddateTextBox.value="";
  Date2look();
}

    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='bedateTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
         
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
              
         
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Freight Report&nbsp;</b></font></TD>
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
      <table class="shade" style="width: 652px; height: 124px">
       <tr><td align="right" style="padding-right: 5px"><b>Begining InvoiceDate:</b></td>
          <td><asp:TextBox ID="bedateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="showCalendarControl(bedateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending InvoiceDate:</b></td>
          <td><asp:TextBox ID="enddateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="showCalendarControl(enddateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1" onkeyup="samevalue()"   width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox2" onkeyup="samevalue2()" width="100px" runat="server"></asp:TextBox></td>
        </tr>
        <tr> <td align="right" style="padding-right: 5px"><b>Begining Job#:</b></td>
          <td><asp:TextBox ID="BeJobTextBox" Width="100px" runat="server"></asp:TextBox>
          <asp:TextBox ID="BeJob2TextBox" Width="30px" runat="server"></asp:TextBox>
          
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Job#:</b></td>
          <td><asp:TextBox ID="EndJobTextBox" Width="100px" runat="server"></asp:TextBox>
          <asp:TextBox ID="EndJob2TextBox" Width="30px" runat="server"></asp:TextBox>
          </td></tr>
          
           <tr><td colspan="2" align="left" style="padding-left:10px">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <b>Output to?  <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                 <asp:ListItem   Value="No"   Text="Text File" />
                 <asp:ListItem  Value="Yes"  Text="Excel" />                 
         </asp:RadioButtonList></b></td></tr>
      
        <tr><td colspan="3">
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
              &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>  
        
        </table>
          
        
        
        <asp:FormView ID="FormView1" Visible="false"  runat="server" DataSourceID="ObjectDataSource1">
            
            <ItemTemplate>
                vFright:
                <asp:Label ID="vFrightLabel" runat="server" Text='<%# Bind("vFright") %>'></asp:Label><br />
            </ItemTemplate>                        
             
             
            
        </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SedsFreightRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmFreight" Type="String" />
                  <asp:Parameter Name="prmBeInvDate" Type="DateTime" />
                  <asp:Parameter Name="prmEndInvDate" Type="DateTime" />
                  <asp:Parameter Name="prmBeCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBeJob1" Type="String" />
                  <asp:Parameter Name="prmEndJob1" Type="String" />
                  <asp:Parameter Name="prmBeJob2" Type="String" />
                  <asp:Parameter Name="prmEndJob2" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          &nbsp;&nbsp;&nbsp;&nbsp;
          
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
        
    </form>
  </body>
</HTML>

