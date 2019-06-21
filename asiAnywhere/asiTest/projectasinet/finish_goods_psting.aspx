<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="finish_goods_psting" Codebehind="finish_goods_psting.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Finished Goods Posting</title>
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
         
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Finished Goods Posting &nbsp;</b></font></TD>
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
      <table class="shade" style="width: 600px">                    
         <tr><td valign="top">          
        <fieldset>        
      <table class="shade" width="600px">
       
       <tr><td align="right" style="padding-right:5px"><b>Post Date:</b></td>
          <td>
              <asp:TextBox ID="TextBox14" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onblur="TextBox14.focus()" onClick="showCalendarControl(TextBox14); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Seq#:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()"  width="100px" MaxLength="8" runat="server"></asp:TextBox>          
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Seq#:</b></td>
      <td><asp:TextBox ID="TextBox2"  width="100px" MaxLength="8" runat="server"></asp:TextBox>        
      </td>
        </tr>
        <tr><td align="right" style="padding-right:5px"><b>Begining User ID#:</b></td>
          <td>
              <asp:TextBox ID="TextBox3" Width="100px" MaxLength="8" runat="server"></asp:TextBox>               
          <td align="right" style="padding-right:5px"><b>Ending User ID#:</b></td>
          <td>
              <asp:TextBox ID="TextBox4" Width="100px" MaxLength="8" runat="server"></asp:TextBox>              
              </td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Begining Date:</b></td>
          <td>
              <asp:TextBox ID="TextBox5" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onblur="TextBox5.focus()" onClick="showCalendarControl(TextBox5); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td>
          <td align="right" style="padding-right:5px"><b>Ending Date:</b></td>
          <td>
              <asp:TextBox ID="TextBox6" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onblur="TextBox6.focus()" onClick="showCalendarControl(TextBox6); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
              <tr><td align="right" style="padding-right:5px"><b>Beginning Job#:</b></td>
          <td>
              <asp:TextBox ID="TextBox7" Width="100px" MaxLength="6" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px"><b>Ending Job#:</b></td>
          <td>
              <asp:TextBox ID="TextBox8" Width="100px" MaxLength="6" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="jobReplook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
              <tr><td align="right" style="padding-right: 5px"><b>Begining FG Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox9" Width="100px" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending FG Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox10"  Width="100px" MaxLength="15" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
      
        <tr><td align="right" style="padding-right: 5px"><b>From Warehouse:</b></td>
          <td><asp:TextBox ID="TextBox11" Width="100px" MaxLength="5" runat="server"></asp:TextBox>
          </td>
        <td align="right" style="padding-right: 5px"><b>To Warehouse:</b></td>
          <td><asp:TextBox ID="TextBox12" Width="100px" MaxLength="5" runat="server"></asp:TextBox>
          </td></tr>
                  
        
        </table>
          
        
        <table class="shade" style="width: 600px">                    
         <tr><td valign="top">          
        <fieldset>
        <table class="shade" style="height:230px">         
         <tr><td align="left" valign="top" style="padding-left:10px" ><b>Trans Typ:</b></td></tr>
         <tr><td nowrap><asp:TextBox ID="TextBox13" Width="100px" runat="server"></asp:TextBox></td></tr>
         <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Receipts" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td><b><asp:CheckBox ID="CheckBox2" Text="Shipment" runat="server"></asp:CheckBox></b></td></tr>
         <tr><td><b><asp:CheckBox ID="CheckBox3" Text="Transfer" runat="server"  ></asp:CheckBox></b></td></tr>
         <tr><td> <b><asp:CheckBox ID="CheckBox4" Text="Adjustments" runat="server"></asp:CheckBox></b>  </td></tr> 
         <tr><td><b><asp:CheckBox ID="CheckBox5" Text="Credit Returns" runat="server"></asp:CheckBox></b></td></tr>         
         <tr><td style="height:90px"><b>&nbsp;</b></td></tr>
         </table></fieldset></td>
         <td valign="top">
         <fieldset> <table style="height:258px">
          <tr><td nowrap align="Left" valign="top" style="padding-left:10px"  ><b>Sort Options:</b></td></tr>
                 <tr><td nowarp><b><asp:CheckBox ID="CheckBox11" Enabled="false" Text="Item Code" runat="server"></asp:CheckBox></b></td></tr>             
         
            <tr><td style="height:93px"><b>&nbsp;</b></td></tr>
         </table></fieldset></td>
         
        
         <td valign="top">         
            <fieldset> <table style="height:245px">
                <tr><td nowrap align="Left" valign="top" style="padding-left:10px"  ><b>Print Options:</b></td></tr>
                
                <tr><td valign="top">
                    <fieldset><table cellpadding="0" cellspacing="0"><tr><td>
                        <asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"  Font-Bold ="true" runat="server" >                              
                            <asp:ListItem  Value="Cost"    Text="Cost" /> 
                            <asp:ListItem   Value="Sell Value" Text="Sell Value" />                                                                
                            </asp:RadioButtonList></td></tr></table></fieldset>
                            
                
                
                    <fieldset><table><tr><td>
                        <asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"  Font-Bold ="true" runat="server" >                              
                            <asp:ListItem  Value="FG Item#"    Text="FG Item#" /> 
                            <asp:ListItem   Value="Customer Part#"  Text="Customer Part#" />                                                                
                            </asp:RadioButtonList></td></tr></table></fieldset>
                            
                
                
                    <fieldset><table><tr><td>
                        <asp:RadioButtonList ID="RadioButtonList4" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"  Font-Bold ="true" runat="server" >                              
                            <asp:ListItem  Value="Item Name"    Text="Item Name" /> 
                            <asp:ListItem   Value="P.O. #/Vendor" Text="P.O. #/Vendor" />                                                                
                            </asp:RadioButtonList></td></tr></table></fieldset>
                                          
                
                
                
                    <fieldset><table><tr><td>
                        <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="2" Font-Bold ="true" runat="server" >                              
                            <asp:ListItem  Value="UOM"    Text="UOM" /> 
                            <asp:ListItem   Value="Job#"  Text="Job#" />                                                                
                            </asp:RadioButtonList></td></tr></table></fieldset>
                            &nbsp;&nbsp;</td>                
                </tr>
                
                <br />
                <tr><td nowrap><b><asp:CheckBox ID="CheckBox9" Text="Print GL Account Numbers?" runat="server"></asp:CheckBox></b></td></tr>
                <tr><td nowrap><b><asp:CheckBox ID="CheckBox10" Text="Total Cost/Value" runat="server"></asp:CheckBox></b></td>
                <td nowrap><b><asp:CheckBox ID="CheckBox12" Text="Grand Total" runat="server"></asp:CheckBox></b></td></tr>
                
                
                
                
                
                
                 
         </table></fieldset>
      
         </td>

          </tr>
          </fieldset></td>
          </tr>
          <tr><td colspan="2" align="left" style="padding-left:10px">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <b>Output to?  <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="No"   Text="Text File" />
                 <asp:ListItem  Value="Yes"  Text="Excel" />
                 
         </asp:RadioButtonList></b></td></tr>
         
         <tr><td  colspan="3">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>
          </tr>   
         </table>     
          
         
          <asp:FormView ID="FormView1" Visible="False"  runat="server" 
              DataSourceID="ObjectDataSource1">
             
              
                           
              <ItemTemplate>
                  vfgpost:
                  <asp:Label ID="vfgpostLabel" runat="server" Text='<%# Bind("vfgpost") %>'></asp:Label><br />
                  vpstfg:
                  <asp:Label ID="vpstfgLabel" runat="server" Text='<%# Bind("vpstfg") %>' />
                  <br />
              </ItemTemplate>
              
              
          </asp:FormView>   
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="dsdsfgpost" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmfgpost" Type="String" />
                  <asp:Parameter Name="prmBeginSeq" Type="Int32" />
                  <asp:Parameter Name="prmEndSeq" Type="Int32" />
                  <asp:Parameter Name="prmBeginUsrid" Type="String" />
                  <asp:Parameter Name="prmEndUsrid" Type="String" />
                  <asp:Parameter Name="prmBeDate" Type="String" />
                  <asp:Parameter Name="prmEndDate" Type="String" />
                  <asp:Parameter Name="prmBeItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBeJob" Type="String" />
                  <asp:Parameter Name="prmEndJob" Type="String" />
                  <asp:Parameter Name="prmBeWare" Type="String" />
                  <asp:Parameter Name="prmEndWare" Type="String" />
                  <asp:Parameter Name="prmPstDate" Type="String" />
                  <asp:Parameter Name="prmRecept" Type="String" />
                  <asp:Parameter Name="prmShipmnt" Type="String" />
                  <asp:Parameter Name="prmTrnsfr" Type="String" />
                  <asp:Parameter Name="prmAdjstmnt" Type="String" />
                  <asp:Parameter Name="prmCrdRtn" Type="String" />
                  <asp:Parameter Name="prmItmcod" Type="String" />
                  <asp:Parameter Name="prmcostsell" Type="String" />
                  <asp:Parameter Name="prmitmcustp" Type="String" />
                  <asp:Parameter Name="prmNamPoVn" Type="String" />
                  <asp:Parameter Name="prmUomJob" Type="String" />
                  <asp:Parameter Name="prmGlActNm" Type="String" />
                  <asp:Parameter Name="prmTcost" Type="String" />
                  <asp:Parameter Name="prmGrndTotl" Type="String" />
                  <asp:Parameter Name="prmtrnstype" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="prmsetup" Type="String" />                                   
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

