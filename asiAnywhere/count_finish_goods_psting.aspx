<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Count_finish_goods_psting" Codebehind="count_finish_goods_psting.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Post Physical Counts</title>
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
        var retVal = makeMsgBox("Confirmation", "Are you ready to post to finished goods?", 48, 4, 256, 4096);
        if (retVal == 6) {
            document.forms[0].HiddenFieldPost.value = "Yes";
        }
        else {
            document.forms[0].HiddenFieldPost.value = "No";
                       
        }
    }
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

var lookup = "";

function fgtaglook(var1) {
    lookup = var1;
    var NewWindow = window.open("fgtaglookup.aspx","fgtagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function fgtaglookup(ReturnObj1) {
            if (lookup == "1") {
                document.forms[0].TextBox1.value = ReturnObj1;
                document.forms[0].TextBox1.focus();
            }
            else {
                document.forms[0].TextBox2.value = ReturnObj1;
                document.forms[0].TextBox2.focus();
            }
        }

 </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
       
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
         <asp:HiddenField ID="HiddenField11" runat="server" />
         <asp:HiddenField ID="HiddenField12" runat="server" />         
         <asp:HiddenField ID="HiddenField13" runat="server" />         
         <asp:HiddenField ID="HiddenField14" runat="server" />         
         <asp:HiddenField ID="HiddenField15" runat="server" />         
         <asp:HiddenField ID="HiddenField16" runat="server" /> 
         <asp:HiddenField ID="HiddenField17" runat="server" /> 
         <asp:HiddenField ID="HiddenFieldPost" runat="server" /> 
                 
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
         
            
      <TABLE id="tblTop" cellSpacing="3" border="0" Width="100%">
        <TR>
            
          <TD nowrap><font size=+0><b>This procedure will post all selected finished goods physical count transactions &nbsp;</b></font></TD>      
          
         
          
        </TR>
      </TABLE>
      <table class="shade" style="width: 600px">                    
         <tr><td valign="top">          
        <fieldset>        
      <table class="shade" width="600px">
       
       <tr><td align="right" style="padding-right:5px"><b>Post Date:</b></td>
          <td>
              <asp:TextBox ID="TextBox5" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onblur="TextBox5.focus()" onClick="showCalendarControl(TextBox5); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td>
              
        <tr><td align="right" style="padding-right: 5px"><b>Begining Tag#:</b></td>
          <td><asp:TextBox ID="TextBox1" Width="115px" MaxLength="20" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="fgtaglook(1); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Tag#:</b></td>
          <td><asp:TextBox ID="TextBox2" Width="115px" MaxLength="20" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="fgtaglook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>      
      
        <tr><td align="right" style="padding-right:5px"><b>Begining User ID#:</b></td>
          <td>
              <asp:TextBox ID="TextBox3" Width="115px" MaxLength="8" runat="server"></asp:TextBox>               
          <td align="right" style="padding-right:5px"><b>Ending User ID#:</b></td>
          <td>
              <asp:TextBox ID="TextBox4" Width="115px" MaxLength="8" runat="server"></asp:TextBox>              
              </td></tr>
                  
        
        </table>
          
        
        <table class="shade" style="width: 600px">                    
         <tr>
         <td align="center" valign="top">
                
                <br />
                <tr><td align="center" nowrap><b><asp:CheckBox ID="CheckBox1" Text="Print GL Account Numbers?" runat="server"></asp:CheckBox></b></td></tr>
                <tr><td align="center" nowrap><b><asp:CheckBox ID="CheckBox2" Text="Total Cost/Value         " runat="server"></asp:CheckBox></b></td></tr>
                

          </tr>
          </fieldset></td>
          </tr>
          <tr><td colspan="2" align="left" style="padding-left:10px">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <b>Output to?  <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="No"   Text="Text File" />
                 
                 
         </asp:RadioButtonList></b></td></tr>
         
         <tr><td  colspan="3">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" Width="50px" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="OK" />
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:Button ID="Button1"  OnClientClick="window.close()" runat="server" class="buttonM" Text="Cancel" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>
          </tr>   
         </table>     
          
         
          <asp:FormView ID="FormView1" Visible="False"  runat="server" 
              DataSourceID="ObjectDataSource1">
             
              
                           
              <ItemTemplate>
                  vtrnspost:
                  <asp:Label ID="vtrnspostLabel" runat="server" Text='<%# Bind("vtrnspost") %>'></asp:Label><br />
                  vpstfg:
                  <asp:Label ID="vpstfgLabel" runat="server" Text='<%# Bind("vpstfg") %>' />
                  <br />
              </ItemTemplate>
              
              
          </asp:FormView>   
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="dsdsfgpost" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmtrnspost" Type="String" />
                  <asp:Parameter Name="prmBeginTag" Type="String" />
                  <asp:Parameter Name="prmEndTag" Type="String" />                  
                  <asp:Parameter Name="prmBeginUsrid" Type="String" />
                  <asp:Parameter Name="prmEndUsrid" Type="String" />
                  <asp:Parameter Name="prmPstDate" Type="String" />
                  <asp:Parameter Name="prmGlActNm" Type="String" />                  
                  <asp:Parameter Name="prmshwinv" Type="String" />                  
                  <asp:Parameter Name="prmOut" Type="String" />
                                                
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    
    </form>
  </body>
</HTML>

