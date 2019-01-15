<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Est_Bysize" Codebehind="est_bysize_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Estimates by Size Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
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
    function samevalue() {
        var beginc = document.getElementById("BegCustTextBox");
        var endc = document.getElementById("EndCustTextBox");
        endc.value = beginc.value;
    }
    

    function contactcustomerlook() {
        var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].BegCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.focus();
    }
    function contactcustomerlook2() {
        var NewWindow = window.open("contact_customer_copylookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {

        document.forms[0].EndCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.focus();
    }
    var diecount = 0;
    var stylecount = 0;
    var flutecount = 0;
    var testcount = 0;
    function dielook(die) {
        diecount = die;
        var NewWindow = window.open("die_lookup.aspx", "DieLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function DieLookup(ReturnObj1) {
        if(diecount == 1) {
            document.forms[0].BegDieTextBox.value = ReturnObj1;
            document.forms[0].BegDieTextBox.focus();
            }
        if(diecount == 2) {
            document.forms[0].EndDieTextBox.value = ReturnObj1;
            document.forms[0].EndDieTextBox.focus();
            }
    }
    function stylelook(style) {
        stylecount = style;
        var NewWindow = window.open("Style_look.aspx", "StyleLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Style_LookUp(ReturnObj1) {
        if (stylecount == 1) {
            document.forms[0].BegStyleTextBox.value = ReturnObj1;
            document.forms[0].BegStyleTextBox.focus();
        }
        if (stylecount == 2) {
            document.forms[0].EndStyleTextBox.value = ReturnObj1;
            document.forms[0].EndStyleTextBox.focus();
        }
    }
    function flutelook(flute) {
        flutecount = flute;
        var NewWindow = window.open("flute_lookup.aspx", "FluteLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function FluteLookup(ReturnObj1) {
        if (flutecount == 1) {
            document.forms[0].BegFluteTextBox.value = ReturnObj1;
            document.forms[0].BegFluteTextBox.focus();
            }
        if (flutecount == 2) {
            document.forms[0].EndFluteTextBox.value = ReturnObj1;
            document.forms[0].EndFluteTextBox.focus();
            }
    }
    function testlook(test) {
        testcount = test;
        var NewWindow = window.open("test_lookup.aspx?look1="+ "all" +"", "testLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function testLookup(ReturnObj1) {
        if (testcount == 1) {
            document.forms[0].BegTestTextBox.value = ReturnObj1;
            document.forms[0].BegTestTextBox.focus();
            }
        if (testcount == 2) {
            document.forms[0].EndTestTextBox.value = ReturnObj1;
            document.forms[0].EndTestTextBox.focus();
            }
    }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='BegCustTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Estimates by Size Report&nbsp;</b></font></TD>
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
       <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>                  
                  <asp:Label ID="CustLabel"  runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
       
      <table class="shade" width="520px">
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="BegCustTextBox"  onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
            <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
            <td><asp:TextBox ID="EndCustTextBox"  width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
      </tr>      
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Style:</b></td>
        <td nowrap><asp:TextBox ID="BegStyleTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="stylelook(1); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Style:</b></td>
        <td nowrap><asp:TextBox ID="EndStyleTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="stylelook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
      </tr>       
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Flute:</b></td>
        <td nowrap><asp:TextBox ID="BegFluteTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="flutelook(1); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Flute:</b></td>
        <td nowrap><asp:TextBox ID="EndFluteTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="flutelook(2); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
      </tr> 
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Test:</b></td>
        <td nowrap><asp:TextBox ID="BegTestTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="testlook(1); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Test:</b></td>
        <td nowrap><asp:TextBox ID="EndTestTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="testlook(2); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
      </tr>
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Die:</b></td>
        <td nowrap><asp:TextBox ID="BegDieTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="dielook(1); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Die:</b></td>
        <td nowrap><asp:TextBox ID="EndDieTextBox" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="dielook(2); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
      </tr>
      
      <tr></tr>  
      <tr>
        <td>&nbsp;</td>
        <td align="right" style="padding-left:10px">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Sort by? </b>
         </td>
         <td>       
                    <asp:RadioButtonList ID="RadioButtonList1"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                        <asp:ListItem  Value="Length" Text="Length" />
                        <asp:ListItem  Value="Part#"  Text="Part#" />                 
                    </asp:RadioButtonList>
                
         </td></tr>  
     
         <tr></tr> 
        <tr>
            <td>&nbsp;</td>
            <td align="right" style="padding-left:10px">
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Industry Type?</b>
            </td>    
            <td>
                   <asp:CheckBox ID="CheckBox1" Text="Folding" runat="server"></asp:CheckBox>
                   <br /> <asp:CheckBox ID="CheckBox2" Text="Corrugated" runat="server"></asp:CheckBox>               
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
                  vEstSizeFile:
                  <asp:Label ID="vEstSizeFileLabel" runat="server" 
                      Text='<%# Bind("vEstSizeFile") %>'></asp:Label><br />                 
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectEstBySizeRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmBegCustomer" Type="String" />
                  <asp:Parameter Name="prmEndCustomer" Type="String" />
                  <asp:Parameter Name="prmBeginStyle" Type="String" />
                  <asp:Parameter Name="prmEndStyle" Type="String" />
                  <asp:Parameter Name="prmBegFlute" Type="String" />
                  <asp:Parameter Name="prmEndFlute" Type="String" />
                  <asp:Parameter Name="prmBegTest" Type="String" />
                  <asp:Parameter Name="prmEndTest" Type="String" />
                  <asp:Parameter Name="prmBegDie" Type="String" />
                  <asp:Parameter Name="prmEndDie" Type="String" />
                  <asp:Parameter Name="prmSortBy" Type="String" />
                  <asp:Parameter Name="prmFold" Type="String" />
                  <asp:Parameter Name="prmCorr" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


