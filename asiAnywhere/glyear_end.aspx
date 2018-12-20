<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="glyear_end" Codebehind="glyear_end.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>G/L Year-End Closing</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    
    <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmPost() {
        var retVal = makeMsgBox("Confirmation", "This year end function will transfer all current balances to the previous year, Make sure you have printed all required year-end reports. ", 48, 4, 256, 4096);
        if (retVal == 6) {
            document.forms[0].HiddenFieldPost.value = "Yes";
        }
        else {
            document.forms[0].HiddenFieldPost.value = "No";
        }
    }
</script>
<script language="javascript">
    
</script>
    
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

    
    


function job1look() {
    var NewWindow = window.open("job1_lookup.aspx", "JobLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    document.forms[0].TextBox7.value = ReturnObj1;
}
function jobReplook() {
    var NewWindow = window.open("jobRep_lookup.aspx", "JobLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function JobRepLookup(ReturnObj1) {
    document.forms[0].TextBox8.value = ReturnObj1;
}


function Relook(){ 
  var NewWindow = window.open("reorder_item_lookup.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox9.value = ReturnObj1;
}
function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox10.value = ReturnObj1;
}
var comp;
function companylook(obj7) {
    comp = obj7;
    var NewWindow = window.open("company_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CompanyLookup(ReturnObj1) {
    if (comp == "1") {
        document.forms[0].begcomTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].endcomTextBox.value = ReturnObj1;
    }
    
} 
 </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='whyearTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
         <asp:HiddenField ID="HiddenField1" runat="server" />          
         <asp:HiddenField ID="HiddenFieldPost" runat="server" /> 
                 
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>                     
         
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>G/L Year-End Closing &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table class="shade" style="width: 500px">                    
         <tr><td valign="top">                                     
                
      <table class="shade" width="500px">      
      
      <tr><td align="right" style="padding-right: 5px"><b>Which Year?</b></td><td>
          <asp:TextBox ID="whyearTextBox"   width="100px" runat="server"></asp:TextBox>          
          </td>      
        </tr>        
        </table>                                                       
          
        
        <table class="shade" style="width: 500px">                    
         <tr><td valign="top" align="center">                  
        <table>         
         <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Move Current Year Budget to Last Year?" runat="server"></asp:CheckBox></b></td></tr>                        
         </table>     
         </td></tr></table>
         
         </td>
          </tr>                                      
         
         <tr><td  colspan="3">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>
          </tr>
           
         </table> 
             
          <asp:FormView ID="FormView1" Visible="False"   runat="server" 
              DataSourceID="ObjectDataSource1">                                                                                    
              <ItemTemplate>
                   
                  <asp:Label ID="yrcloLabel" runat="server" 
                      Text='<%# Bind("yrclo") %>'></asp:Label><br />                                 
                  
              </ItemTemplate>
          </asp:FormView>
          
         
             
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="GLYearClose" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmyear" Type="Int32" />
                  <asp:Parameter Name="prmmove" Type="String" />                                   
                  <asp:Parameter Name="prmOut" Type="String" />
                                                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

