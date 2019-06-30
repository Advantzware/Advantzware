<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="machine_file" Codebehind="machine_file_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Machine File</title>
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

    var mval = "";
    var dval = "";
    function machine(val) {
        mval = val;
        var NewWindow = window.open("machine_lookup.aspx", "MachineLookup", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function MachineLookup(ReturnObj1, ReturnObj2) {
        if (mval == "1") {
            document.forms[0].BegMachTextBox.value = ReturnObj1;
            document.forms[0].BegMachTextBox.focus();
        }
        else if (mval == "2") {
            document.forms[0].EndMachTextBox.value = ReturnObj1;
            document.forms[0].EndMachTextBox.focus();
        }        
    }


    function deptlook(val) {
        dval = val;
        var NewWindow = window.open("dept_lookup.aspx", "DepartmentLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function DepartmentLookup(ReturnObj1) {
        if (dval == "1") {
            document.forms[0].BegDeptTextBox.value = ReturnObj1;
            document.forms[0].BegDeptTextBox.focus();
        }
        else if (dval == "2") {
            document.forms[0].EndDeptTextBox.value = ReturnObj1;
            document.forms[0].EndDeptTextBox.focus();
        }     
    }
   
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='BegMachTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />   
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Machine File&nbsp;</b></font></TD>
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
     
      <tr><td align="right" style="padding-right: 5px"><b> Beginning Machine#:</b></td><td>
          <asp:TextBox ID="BegMachTextBox"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="machine('1'); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Machine#:</b></td><td>
          <asp:TextBox ID="EndMachTextBox" width="100px"  runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="machine('2'); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        </tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Dept. Code:</b></td>
          <td nowrap><asp:TextBox ID="BegDeptTextBox" Width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="deptlook('1'); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
           </td>
        <td align="right" style="padding-right: 5px"><b>Ending Dept. Code:</b></td>
          <td nowrap><asp:TextBox ID="EndDeptTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="deptlook('2'); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr> 
         <tr></tr> 
        <tr>
            <td align="right" colspan="4" style="padding-right:15px">
                <b><asp:CheckBox ID="CheckBox1" Text="Show Standards?" runat="server"></asp:CheckBox></b>
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
                  vFile:
                  <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />                 
                  dfjdskj:
                  <asp:Label ID="dfjdskjLabel" runat="server" Text='<%# Bind("dfjdskj") %>' />
                  <br />
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectMachFileRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmBegMachine" Type="String" />
                  <asp:Parameter Name="prmEndMachine" Type="String" />
                  <asp:Parameter Name="prmBegDeptCode" Type="String" />
                  <asp:Parameter Name="prmEndDeptCode" Type="String" />
                  <asp:Parameter Name="prmShowStandard" Type="String" />
                  <asp:Parameter Name="prmOutexcel" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


