<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="prep_mat" Codebehind="prep_mat_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Prep List by Material Type</title>
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

    function contactcustomerlook() {
        var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].BegCustTextBox.value = ReturnObj1;
        document.forms[0].BegCustTextBox.focus();
    }
    function contactcustomerlook2() {
        var NewWindow = window.open("contact_customer_copylookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {

        document.forms[0].EndCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.focus();
    }

    function salesreplook() {
        var NewWindow = window.open("salesrep_lookup.aspx", "SalesRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function SalesRepLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].BegSalrepTextBox.value = ReturnObj1;
        document.forms[0].BegSalrepTextBox.focus();
    }

    function smancopylook1() {
        var NewWindow = window.open("sman_copylookup.aspx", "smancopyLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function smancopyLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].EndSalrepTextBox.value = ReturnObj1;
        document.forms[0].EndSalrepTextBox.focus();
    }
    
    var pval = "";
    function preplook(val) {
        pval = val;
        var NewWindow = window.open("prep_lookup.aspx", "PrepLookup", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function PrepLookup(ReturnObj1) {
        if (pval == "1") {
            document.forms[0].BegPrepTextBox.value = ReturnObj1;
            document.forms[0].BegPrepTextBox.focus();
        }
        else if (pval == "2") {
            document.forms[0].EndPrepTextBox.value = ReturnObj1;
            document.forms[0].EndPrepTextBox.focus();
        }
    }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='BegSalrepTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        <asp:HiddenField ID="HiddenField4" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" />   
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Prep List by Material Type&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
      <table class="shade" width="520px">
      
      <tr><td align="right" style="padding-right: 5px"><b>&nbsp;</b></td>
          <td align="right">
                <b>Material Type?</b>
          </td>   
          <td colspan="2">
                <asp:CheckBox ID="CheckBox1" Text="Die" runat="server"></asp:CheckBox> <br />
                <asp:CheckBox ID="CheckBox2" Text="Plate" runat="server"></asp:CheckBox>
          </td>      
      </tr>
      
      <tr><td align="right" style="padding-right: 5px"><b>&nbsp;</b></td>
          <td align="right">
                <b>Industry Type?</b>
          </td>   
          <td colspan="2">
                <asp:CheckBox ID="CheckBox3" Text="Folding" runat="server"></asp:CheckBox> <br />
                <asp:CheckBox ID="CheckBox4" Text="Corrugated" runat="server"></asp:CheckBox>
          </td>      
      </tr>      
      
      <tr><td align="right" nowrap style="padding-right: 5px"><b>Begining Sales Rep#:</b></td>
          <td nowrap><asp:TextBox ID="BegSalrepTextBox" Width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
           </td>
        <td align="right" nowrap style="padding-right: 5px"><b>Ending Sales Rep#:</b></td>
          <td nowrap><asp:TextBox ID="EndSalrepTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      </tr>
      
      <tr><td align="right" nowrap style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="BegCustTextBox"  onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
            <td align="right" nowrap style="padding-right: 5px"><b>Ending Customer#:</b></td>
            <td><asp:TextBox ID="EndCustTextBox"  width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
      </tr>
      
      <tr><td nowrap align="right" style="padding-right: 5px"><b> Beginning Last Mod Date:</b></td><td>
          <asp:TextBox ID="BegLastModdateTextBox"  width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('BegLastModdateTextBox').focus()" tabindex="1" onClick="showCalendarControl(BegLastModdateTextBox); return false"><asp:Image ID="datelook" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
          </td>
      <td nowrap align="right" style="padding-right: 5px"><b>Ending Last Mod Date:</b></td><td>
          <asp:TextBox ID="EndLastModdateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" width="100px"  runat="server"></asp:TextBox>
           <a href="#" onblur="document.getElementById('EndLastModdateTextBox').focus()" tabindex="1" onClick="showCalendarControl(EndLastModdateTextBox); return false"><asp:Image id="look2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
            
      <tr><td align="right" nowrap style="padding-right: 5px"><b> Beginning Last Ord Date:</b></td><td>
          <asp:TextBox ID="BegLastOrddateTextBox"  width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('BegLastOrddateTextBox').focus()" tabindex="1" onClick="showCalendarControl(BegLastOrddateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
          </td>
      <td align="right" nowrap style="padding-right: 5px"><b>Ending Last Ord Date:</b></td><td>
          <asp:TextBox ID="EndLastOrddateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" width="100px"  runat="server"></asp:TextBox>
           <a href="#" onblur="document.getElementById('EndLastOrddateTextBox').focus()" tabindex="1" onClick="showCalendarControl(EndLastOrddateTextBox); return false"><asp:Image id="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
      
      <tr>
          <td align="right" nowrap style="padding-right: 5px"><b>Beginning Prep Code:</b></td><td>
            <asp:TextBox ID="BegPrepTextBox"  width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onclick="preplook('1'); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
            <td align="right" nowrap style="padding-right: 5px"><b>Ending Prep Code:</b></td>
            <td><asp:TextBox ID="EndPrepTextBox" width="100px"  runat="server"></asp:TextBox>
                <a href="#" tabindex="1" onclick="preplook('2'); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
      </tr>
        
      <tr>
        <td>&nbsp;</td>
        <td colspan="3">            
                <b>Sort by?  
                    <asp:RadioButtonList ID="RadioButtonList1"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                        <asp:ListItem   Value="Prep Code"   Text="Prep Code" />
                        <asp:ListItem  Value="SalesRep/Cust"  Text="SalesRep/Cust" />                 
                        <asp:ListItem  Value="Order Date"  Text="Order Date" />
                    </asp:RadioButtonList>
                </b>
         </td></tr>  
     
         <tr></tr> 
        <tr>
            <td>&nbsp;</td>           
            <td align="left" colspan="3" style="padding-left:50px">
                <b><asp:CheckBox ID="CheckBox5" Text="Print Description?" runat="server"></asp:CheckBox></b>
            </td>            
        </tr>                    
          <tr><td colspan="2" align="left" style="padding-left:10px">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Output to?  
                    <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                        <asp:ListItem   Value="No"   Text="Text File" />
                        <asp:ListItem  Value="Yes"  Text="Excel" />                 
                    </asp:RadioButtonList>
                </b>
         </td></tr>                    
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                  
              <ItemTemplate>
                  pMatFile:
                  <asp:Label ID="pMatFileLabel" runat="server" Text='<%# Bind("pMatFile") %>'></asp:Label><br />                 
                  dfjzvbsj:
                  <asp:Label ID="dfjzvbsjLabel" runat="server" 
                      Text='<%# Bind("dfjzvbsj") %>' />
                  <br />
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectPrepMatRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmBegCustomer" Type="String" />
                  <asp:Parameter Name="prmEndCustomer" Type="String" />
                  <asp:Parameter Name="prmBegLastModDate" Type="String" />
                  <asp:Parameter Name="prmEndLastModDate" Type="String" />
                  <asp:Parameter Name="prmBegLastOrdDate" Type="String" />
                  <asp:Parameter Name="prmEndLastOrdDate" Type="String" />
                  <asp:Parameter Name="prmBegSalrep" Type="String" />
                  <asp:Parameter Name="prmEndSalrep" Type="String" />
                  <asp:Parameter Name="prmBegPrepCode" Type="String" />
                  <asp:Parameter Name="prmEndPrepCode" Type="String" />
                  <asp:Parameter Name="prmDie" Type="String" />
                  <asp:Parameter Name="prmPlate" Type="String" />
                  <asp:Parameter Name="prmFolding" Type="String" />
                  <asp:Parameter Name="prmCorrugated" Type="String" />
                  <asp:Parameter Name="prmSortBy" Type="String" />
                  <asp:Parameter Name="prmprintDscr" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


