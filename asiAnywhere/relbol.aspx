<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="relbol" MaintainScrollPositionOnPostback="true" Codebehind="relbol.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Release Scan and Create BOL</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = JavaScript>

        var bSelected = false;
        function ChSel() {
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;
            bSelected = !bSelected;
            var i;
            for (i = 0; i < theForm.chDelete.length; ++i) theForm.chDelete[i].checked = bSelected;
        }

        function OnKeyDown() {
            e = window.event;
            if (e.keyCode == 13) {
                e.cancel = true;
                var theForm = document.forms['frmList'];
                if (!theForm) theForm = document.frmList;
                theForm.btnSearch.click();
            }
        }

        function releaselook()
        { 
            var NewWindow = window.open("release_lookup.aspx","ReleaseLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function ReleaseLookUp(ReturnObj1)
        {
            document.forms[0].FormView1$vRelease_TextBox.value = ReturnObj1;
            document.getElementById("FormView1$vRelease_TextBox").onchange(); 
            document.forms[0].FormView1$vTag_TextBox.focus();
        }

        function trailerlook() {
            var NewWindow = window.open("trailer_lookup.aspx", "TrailerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function TrailerLookUp(ReturnObj1) {
            document.forms[0].FormView1$vTrailerTextBox.value = ReturnObj1;
            document.getElementById("FormView1_vCasesTextBox").focus();
        }
        

        function taglook() {
            var releaseval = document.forms[0].FormView1$vRelease_TextBox.value;
            var NewWindow = window.open("tag_lookup.aspx?release=" + releaseval + "", "TagLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");         
        }

        function TagLookUp(ReturnObj1) 
        {
            if (ReturnObj1.indexOf(":")) {
                var val = ReturnObj1;
                ReturnObj1 = val.replace(":", "\"");
            }
            document.forms[0].FormView1$vTag_TextBox.value = ReturnObj1;                       
            document.getElementById("FormView1$vTag_TextBox").onchange();    
        }


        function calc_qty() { 
            /* tt-relbol.qty:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} =
        STRING((DEC(tt-relbol.cases:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) *
                DEC(tt-relbol.qty-case:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})) +
               DEC(tt-relbol.partial:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})).
               */
               
               var caseval = parseFloat(document.forms[0].FormView1$vCasesTextBox.value);
               var qtycaseval = parseFloat(document.forms[0].FormView1$vQtyCaseTextBox.value);
               var partialval = parseFloat(document.forms[0].FormView1$vPartialTextBox.value);
               
               var qty = parseFloat(document.forms[0].FormView1$vCasesTextBox.value) * parseFloat(document.forms[0].FormView1$vQtyCaseTextBox.value) + parseFloat(document.forms[0].FormView1$vPartialTextBox.value);
           }

           function validate() {
//               var relqty   = document.getElementById("RelQtyLabel").innerHTML;
//               var scanqty = document.getElementById("ScanQtyLabel").innerHTML;
//               var nscanqty = document.forms[0].FormView1$vQtyTextBox.value;
//               if ((parseFloat(scanqty) + parseFloat(nscanqty)) > parseFloat(relqty)) {
//                   alert("Qty scanned exceeds qty released for Order# "); 
//                   return false; 
//               }                            
           }
 
    </script> 
    
    <script language="javascript">
        function leaveunit(val) {
            if(val == 1)
                document.getElementById('FormView1_vQtyTextBox').focus();
            else
                document.getElementById('FormView1_vInoTextBox').focus();
                
            window.scroll(800, 900);
        }

        function leavewhouse() {
            document.getElementById('FormView1_vLocBinTextBox').focus();
            window.scroll(1800, 900);
        }
        function leavepartial() {            
            window.scroll(0, 0);
        }
    </script>
    
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='terms_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
                
    <asp:HiddenField ID="JobNoHiddenField" runat="server" />
    <asp:HiddenField ID="JobNo2HiddenField" runat="server" />
    <asp:HiddenField ID="PoNoHiddenField" runat="server" />
    <asp:HiddenField ID="LineHiddenField" runat="server" />    
    <asp:HiddenField ID="WarnedHiddenField" runat="server" />  
    
    
      <div>
                    
          <asp:Label ID="Label1" ForeColor="red" Font-Bold="true" runat="server" ></asp:Label>
                 
       <table><tr><td>
        <TABLE id="tblTop" cellSpacing="3" align="left" border="0">
        <TR>
            
          <TD align="left"><font size=+0><b>Release Scan and Create BOL&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</b></font></TD>
         
          <TD  align="left" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       </td></tr>
       <tr><td>
       <table class="shade" >
     
      
        <tr> <td align="right" style="padding-right: 5px">Release Qty:</td>
        <td>
       <%-- <asp:TextBox ID="RelQtyTextBox" MaxLength="10" Width="100px" runat="server"></asp:TextBox>--%>
        <asp:Label ID="RelQtyLabel" runat="server" Text="Label" Width="100px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" ></asp:Label>
        </td>
        
        <td align="right" style="padding-right: 5px">Scanned Qty:</td>
        <td>
        <%--<asp:TextBox ID="ScanQtyTextBox" AutoPostBack="true" MaxLength="10" Width="100px" runat="server"></asp:TextBox>--%>
        <asp:Label ID="ScanQtyLabel" runat="server" Text="Label" Width="100px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" ></asp:Label>
        </td>
        </tr>
        
        <tr><td align="left" colspan="4">
        <asp:Label ID="graterLabel" runat="server" ForeColor="Red"></asp:Label>
        <%--<asp:Panel ID="gridpanel" runat="server" Width="230px" ScrollBars="vertical" Height="300px">--%>
        <asp:GridView ID="GridView1" runat="server" DataSourceID="SqlDataSource1"
          AutoGenerateColumns="False" AllowSorting="True"  OnSelectedIndexChanged="GridView1_SelectedIndexChanged" 
            EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />            
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <HeaderStyle  BackColor="Teal" ForeColor="White" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
            <RowStyle CssClass="shade"  />
            <Columns>
                            <%--<asp:TemplateField>
                     <ItemTemplate>
                      <asp:CheckBox ID="chk1" runat="server" />
                      </ItemTemplate>
                      </asp:TemplateField>--%>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField> 
                 
                <asp:BoundField DataField="seq" HeaderText="Seq#" SortExpression="seq" />
                <asp:BoundField DataField="release" HeaderText="Release#" SortExpression="release" />
                            <asp:BoundField DataField="tag" HeaderText="Tag#" SortExpression="tag" />
                            <asp:BoundField DataField="trailor" HeaderText="Trailer" 
                                SortExpression="trailor" />
                            <asp:BoundField DataField="i_no" HeaderText="Item#" 
                                SortExpression="i_no" />
                            <asp:BoundField DataField="i_name" HeaderText="Name" 
                                SortExpression="i_name" />
                            <asp:BoundField DataField="ord_no" HeaderText="Order#" 
                                SortExpression="ord_no" />
                            <%--<asp:BoundField DataField="job_no" HeaderText="job_no" 
                                SortExpression="job_no" />
                            <asp:BoundField DataField="job_no2" HeaderText="job_no2" 
                                SortExpression="job_no2" />--%>
                            <asp:BoundField DataField="loc" HeaderText="Warehouse" SortExpression="loc" />
                            <asp:BoundField DataField="loc_bin" HeaderText="Bin" 
                                SortExpression="loc_bin" />
                            <asp:BoundField DataField="cust_no" HeaderText="Customer#" 
                                SortExpression="cust_no" />
                            <asp:BoundField DataField="cases" HeaderText="Units" 
                                SortExpression="cases" />
                            <asp:BoundField DataField="qty_case" HeaderText="Unit Count" 
                                SortExpression="qty_case" />
                            <asp:BoundField DataField="cases_unit" HeaderText="Unit per Pallet" 
                                SortExpression="cases_unit" />
                            <asp:BoundField DataField="partial" 
                                HeaderText="Partial" SortExpression="partial" />
                            <asp:BoundField DataField="qty" HeaderText="Quantity" SortExpression="qty" />
                            <%--<asp:BoundField DataField="t_qty" HeaderText="t_qty" SortExpression="t_qty" />
                            <asp:BoundField DataField="line" HeaderText="line" SortExpression="line" />
                            <asp:BoundField DataField="warned" HeaderText="warned" 
                                SortExpression="warned" />
                            <asp:BoundField DataField="po_no" HeaderText="po_no" SortExpression="po_no" />--%>
            </Columns>
             <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:SqlDataSource ID="SqlDataSource1" runat="server" 
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" 
                SelectCommand="SELECT * FROM [relbol] order by seq asc"></asp:SqlDataSource>
                
                <asp:SqlDataSource ID="SqlDataSource2" runat="server" 
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" 
                SelectCommand="SELECT * FROM [relbol] WHERE ([seq] = @seq)">
                    <SelectParameters>
                        <asp:SessionParameter Name="seq" SessionField="relbol_seq_txt" Type="Int32" />
                    </SelectParameters>
            </asp:SqlDataSource>
        <%--</asp:Panel>--%>
        </td>        
        </tr>
        
        <tr>
            <asp:FormView ID="FormView1" runat="server" DataSourceID="SqlDataSource2" OnDataBound="insertseq">
            <EditItemTemplate>     
                <asp:HiddenField ID="HiddenField1" Value="2" runat="server" />
                <asp:HiddenField ID="TagHiddenField" runat="server" Value='<%# Bind("[tag]") %>' />
                <table class="shade">
                <tr><td nowrap align="right" style="padding-right:5px"><b>Seq#</b></td>
                <td nowrap><asp:TextBox ID="vSeqTextBox" runat="server" Text='<%# Bind("seq") %>' /></td>                
                <td align="right" nowrap style="padding-right:5px"><b>Release#</b></td>                              
                <td nowrap><asp:TextBox ID="vRelease_TextBox" runat="server" AutoPostBack="true" OnTextChanged="ReleaseTextChange" Text='<%# Bind("[release]") %>' />
                <a href="#" tabindex="1" onclick="releaselook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td>
                
                <td align="right" nowrap style="padding-right:5px"><b>Tag#</b></td>
                <td nowrap><asp:TextBox ID="vTag_TextBox" runat="server" AutoPostBack="true" OnTextChanged="TagTextChange" Text='<%# Bind("[tag]") %>' />
                <a href="#" tabindex="1" onclick="taglook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td>        
                <td nowrap align="right" style="padding-right:5px"><b>Trailer</b></td>
                <td nowrap><asp:TextBox ID="vTrailerTextBox" runat="server" onfocus="this.select();" Text='<%# Bind("trailor") %>' />
                    <a href="#" tabindex="1" onclick="trailerlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Units</b></td>
                <td nowrap><asp:TextBox ID="vCasesTextBox" onfocus="this.select();" onblur="leaveunit(2);" runat="server" Text='<%# Bind("cases") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Item#</b></td>
                <td nowrap><asp:TextBox ID="vInoTextBox" runat="server" Text='<%# Bind("i_no") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Name</b></td>
                <td nowrap><asp:TextBox ID="vInameTextBox" runat="server" Text='<%# Bind("i_name") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Order#</b></td>
                <td nowrap><asp:TextBox ID="vOrdNoTextBox" runat="server" Text='<%# Bind("ord_no") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Quantity</b></td>
                <td nowrap><asp:TextBox ID="vQtyTextBox" onfocus="this.select();" runat="server" Text='<%# Bind("qty") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Warehouse</b></td>
                <td nowrap><asp:TextBox ID="vLocTextBox" runat="server" onblur="leavewhouse();" Text='<%# Bind("loc") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Bin</b></td>
                <td nowrap><asp:TextBox ID="vLocBinTextBox" runat="server" Text='<%# Bind("loc_bin") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Customer#</b></td>
                <td nowrap><asp:TextBox ID="vCustNoTextBox" runat="server" Text='<%# Bind("cust_no") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Unit Count</b></td>
                <td nowrap><asp:TextBox ID="vQtyCaseTextBox" runat="server" Text='<%# Bind("qty_case") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Unit per Pallet</b></td>
                <td nowrap><asp:TextBox ID="vCasesUnitTextBox" runat="server" Text='<%# Bind("cases_Unit") %>' /></td>
                <td nowrap align="right" style="padding-right:5px"><b>Partial</b></td>
                <td nowrap><asp:TextBox ID="vPartialTextBox" onblur="leavepartial();" runat="server" Text='<%# Bind("partial") %>' /></td>        
                </tr> 
                                                                                                    
                <%--vJobNo:
                <asp:TextBox ID="vJobNoTextBox" runat="server" Text='<%# Bind("vJobNo") %>' />
                <br />
                vJobNo2:
                <asp:TextBox ID="vJobNo2TextBox" runat="server" Text='<%# Bind("vJobNo2") %>' />
                <br />
                vTQty:
                <asp:TextBox ID="vTQtyTextBox" runat="server" Text='<%# Bind("vTQty") %>' />
                <br />
                vLine:
                <asp:TextBox ID="vLineTextBox" runat="server" Text='<%# Bind("vLine") %>' />
                <br />--%>
                <%--vWarned:
                <asp:CheckBox ID="vWarnedCheckBox" runat="server" 
                    Checked='<%# Bind("vWarned") %>' />
                <br />
                vPoNo:
                <asp:TextBox ID="vPoNoTextBox" runat="server" Text='<%# Bind("vPoNo") %>' />
                <br />
                vRelQty:
                <asp:TextBox ID="vRelQtyTextBox" runat="server" Text='<%# Bind("vRelQty") %>' />
                <br />
                vScanQty:
                <asp:TextBox ID="vScanQtyTextBox" runat="server" 
                    Text='<%# Bind("vScanQty") %>' />
                <br />--%>
                
                <tr><td colspan="15" align="left">
                    <asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="True" OnClick="UpdateButtonClick" Text="Save" />
                &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td></tr>
                </table>
            </EditItemTemplate>
            <InsertItemTemplate>
                <asp:HiddenField ID="HiddenField1" Value="1" runat="server" />
            <table class="shade">                
                <tr><td align="left" nowrap style="padding-right:5px"><b>Seq#</b></td>
                <td align="left"><asp:TextBox ID="vSeqTextBox" runat="server" Width="60px" Text='<%# Bind("seq") %>' ReadOnly="true" BackColor="Turquoise" /></td>                
                <td align="left" nowrap style="padding-right:5px"><b>Release#</b></td>                              
                <td nowrap><asp:TextBox ID="vRelease_TextBox" runat="server" AutoPostBack="true" OnTextChanged="ReleaseTextChange" Text='<%# Bind("[release]") %>' />                                    
                <a href="#" onclick="releaselook(); return false" tabindex="1"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td>
                
                <td align="left" nowrap style="padding-right:5px"><b>Tag#</b></td>
                <td nowrap><asp:TextBox ID="vTag_TextBox" runat="server" AutoPostBack="true" OnTextChanged="TagTextChange" Text='<%# Bind("[tag]") %>' />
                <a href="#" tabindex="1" onclick="taglook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td>        
                <td align="left" nowrap style="padding-right:5px"><b>Trailer</b></td>
                <td nowrap><asp:TextBox ID="vTrailerTextBox" runat="server" onfocus="this.select();" Text='<%# Bind("trailor") %>' />
                    <a href="#" tabindex="1" onclick="trailerlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                </td>
                
                <td align="left" nowrap style="padding-right:5px"><b>Units</b></td>
                <td nowrap><asp:TextBox ID="vCasesTextBox" runat="server" onfocus="this.select();" onblur="leaveunit(1);" Text='<%# Bind("cases") %>' /></td>        
                <td align="left" nowrap style="padding-right:5px"><b>Item#</b></td>
                <td nowrap><asp:TextBox ID="vInoTextBox" runat="server" Text='<%# Bind("i_no") %>' ReadOnly="true" /></td>
                
                <td align="left" nowrap style="padding-right:5px"><b>Name</b></td>
                <td nowrap><asp:TextBox ID="vInameTextBox" runat="server" Text='<%# Bind("i_name") %>' ReadOnly="true" /></td>        
                <td align="left" nowrap style="padding-right:5px"><b>Order#</b></td>
                <td nowrap><asp:TextBox ID="vOrdNoTextBox" runat="server" Text='<%# Bind("ord_no") %>' ReadOnly="true" /></td>
                
                <td align="left" nowrap style="padding-right:5px"><b>Quantity</b></td>
                <td nowrap><asp:TextBox ID="vQtyTextBox" onfocus="this.select();" runat="server" Text='<%# Bind("qty") %>' /></td>        
                <td align="left" nowrap style="padding-right:5px"><b>Warehouse</b></td>
                <td nowrap><asp:TextBox ID="vLocTextBox" runat="server" onblur="leavewhouse();" Text='<%# Bind("loc") %>' /></td>
                
                <td align="left" nowrap style="padding-right:5px"><b>Bin</b></td>
                <td nowrap><asp:TextBox ID="vLocBinTextBox" runat="server" Text='<%# Bind("loc_bin") %>' /></td>        
                <td align="left" nowrap style="padding-right:5px"><b>Customer#</b></td>
                <td nowrap><asp:TextBox ID="vCustNoTextBox" runat="server" Text='<%# Bind("cust_no") %>' /></td>
                
                <td align="left" nowrap style="padding-right:5px"><b>Unit Count</b></td>
                <td nowrap><asp:TextBox ID="vQtyCaseTextBox" onblur="document.getElementById('FormView1_vPartialTextBox').focus();" runat="server" Text='<%# Bind("qty_case") %>' /></td>        
                <td align="left" nowrap style="padding-right:5px"><b>Unit per Pallet</b></td>
                <td nowrap><asp:TextBox ID="vCasesUnitTextBox" runat="server" Text='<%# Bind("cases_Unit") %>' ReadOnly="true" /></td>
                <td align="left" nowrap style="padding-right:5px"><b>Partial</b></td>
                <td nowrap><asp:TextBox ID="vPartialTextBox" onblur="leavepartial();" runat="server" Text='<%# Bind("partial") %>' /></td>        
                </tr> 
                                                                                                    
                <%--vJobNo:
                <asp:TextBox ID="vJobNoTextBox" runat="server" Text='<%# Bind("vJobNo") %>' />
                <br />
                vJobNo2:
                <asp:TextBox ID="vJobNo2TextBox" runat="server" Text='<%# Bind("vJobNo2") %>' />
                <br />
                vTQty:
                <asp:TextBox ID="vTQtyTextBox" runat="server" Text='<%# Bind("vTQty") %>' />
                <br />
                vLine:
                <asp:TextBox ID="vLineTextBox" runat="server" Text='<%# Bind("vLine") %>' />
                <br />--%>
                <%--vWarned:
                <asp:CheckBox ID="vWarnedCheckBox" runat="server" 
                    Checked='<%# Bind("vWarned") %>' />
                <br />
                vPoNo:
                <asp:TextBox ID="vPoNoTextBox" runat="server" Text='<%# Bind("vPoNo") %>' />
                <br />
                vRelQty:
                <asp:TextBox ID="vRelQtyTextBox" runat="server" Text='<%# Bind("vRelQty") %>' />
                <br />
                vScanQty:
                <asp:TextBox ID="vScanQtyTextBox" runat="server" 
                    Text='<%# Bind("vScanQty") %>' />
                <br />--%>
                <br /><br />
                <tr>
                <td align="left" nowrap colspan="15">
                    <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClientClick="javascript:return validate();" Text="Save" OnClick="InsertButtonClick" />                    
                    
                    &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel"  />
                </td></tr>
                </table>
             </InsertItemTemplate>
                <ItemTemplate>
                
                <table class="shade" width="400px">
                <tr><td nowrap align="right" style="padding-right:5px"><b>Seq#</b></td>
                <td nowrap><asp:Label ID="vSeqLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("seq") %>' /></td>                
                <td nowrap align="right" style="padding-right:5px"><b>Release#</b></td>                              
                <td nowrap><asp:Label ID="vRelease_Label" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("[release]") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Tag#</b></td>
                <td nowrap><asp:Label ID="vTag_Label" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("[tag]") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Trailer</b></td>
                <td nowrap><asp:Label ID="vTrailerLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("trailor") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Units</b></td>
                <td nowrap><asp:Label ID="vCasesLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("cases") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Item#</b></td>
                <td nowrap><asp:Label ID="vInoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("i_no") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Name</b></td>
                <td nowrap><asp:Label ID="vInameLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("i_name") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Order#</b></td>
                <td nowrap><asp:Label ID="vOrdNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("ord_no") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Quantity</b></td>
                <td nowrap><asp:Label ID="vQtyLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("qty") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Warehouse</b></td>
                <td nowrap><asp:Label ID="vLocLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("loc") %>' /></td>
                            
                <td nowrap align="right" style="padding-right:5px"><b>Bin</b></td>
                <td nowrap><asp:Label ID="vLocBinLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("loc_bin") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Customer#</b></td>
                <td nowrap><asp:Label ID="vCustNoLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("cust_no") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Unit Count</b></td>
                <td nowrap><asp:Label ID="vQtyCaseLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("qty_case") %>' /></td>        
                <td nowrap align="right" style="padding-right:5px"><b>Unit per Pallet</b></td>
                <td nowrap><asp:Label ID="vCasesUnitLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("cases_unit") %>' /></td>
                
                <td nowrap align="right" style="padding-right:5px"><b>Partial</b></td>
                <td nowrap><asp:Label ID="vPartialLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("partial") %>' /></td>                        
                                                                                                    
                <%--vJobNo:
                <asp:Label ID="vJobNoLabel" runat="server" Text='<%# Bind("vJobNo") %>' />
                <br />
                vJobNo2:
                <asp:Label ID="vJobNo2Label" runat="server" Text='<%# Bind("vJobNo2") %>' />
                <br />
                vTQty:
                <asp:Label ID="vTQtyLabel" runat="server" Text='<%# Bind("vTQty") %>' />
                <br />
                vLine:
                <asp:Label ID="vLineLabel" runat="server" Text='<%# Bind("vLine") %>' />
                <br />--%>
                <%--vWarned:
                <asp:CheckBox ID="vWarnedCheckBox" runat="server" 
                    Checked='<%# Bind("vWarned") %>' />
                <br />
                vPoNo:
                <asp:Label ID="vPoNoLabel" runat="server" Text='<%# Bind("vPoNo") %>' />
                <br />
                vRelQty:
                <asp:Label ID="vRelQtyLabel" runat="server" Text='<%# Bind("vRelQty") %>' />
                <br />
                vScanQty:
                <asp:Label ID="vScanQtyLabel" runat="server" 
                    Text='<%# Bind("vScanQty") %>' />
                <br />--%>
              
                </table>
                                                   
                </ItemTemplate>
            </asp:FormView>              
        </tr>
           <tr><td align="left" colspan="4">
           <asp:Button ID="addbutton" OnClick="addbutton_click" runat="server"  class="buttonM"  Text="Add" />
           <asp:Button ID="updatebutton" OnClick="updatebutton_click" runat="server"  class="buttonM"  Text="Update" />
           <asp:Button ID="deletebutton" runat="server" OnClick="deletebutton_click"  class="buttonM"  Text="Delete" />
           <asp:Button ID="printbolbutton" runat="server" OnClick="printbolbutton_click" class="buttonM"  Text="Print BOL" />
              
              &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>
        
        
        </table>
       </td></tr>
       </table>       
     
      
        
             
        
        
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectRelBOL" TypeName="bol">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmSeq" Type="Int32" />
                  <asp:Parameter Name="prmRelease" Type="Int32" />
                  <asp:Parameter Name="prmTag" Type="String" />
                  <asp:Parameter Name="prmTrailor" Type="String" />
                  <asp:Parameter Name="prmCases" Type="Int32" />
                  <asp:Parameter Name="prmIno" Type="String" />
                  <asp:Parameter Name="prmIName" Type="String" />
                  <asp:Parameter Name="prmOrdNo" Type="Int32" />
                  <asp:Parameter Name="prmQty" Type="Int32" />
                  <asp:Parameter Name="prmLoc" Type="String" />
                  <asp:Parameter Name="prmLocBin" Type="String" />
                  <asp:Parameter Name="prmCustNo" Type="String" />
                  <asp:Parameter Name="prmQtyCase" Type="Int32" />
                  <asp:Parameter Name="prmCasesUnit" Type="Int32" />
                  <asp:Parameter Name="prmPartial" Type="Int32" />
                  <asp:Parameter Name="prmRelQty" Type="Int32" />
                  <asp:Parameter Name="prmScanQty" Type="Int32" />
                  <asp:Parameter Name="prmFilePath" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
          
          <%--<asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  vDashFile:
                  <asp:Label ID="vDashFileLabel" runat="server" Text='<%# Bind("vDashFile") %>'></asp:Label><br />
                 
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectProdHighReport" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String"  />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmDate" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmMachine1" Type="String" />
                  <asp:Parameter Name="prmMachine2" Type="String" />
                  <asp:Parameter Name="prmMachine3" Type="String" />
                  <asp:Parameter Name="prmMachine4" Type="String" />
                  <asp:Parameter Name="prmMachine5" Type="String" />
                  <asp:Parameter Name="prmMachine6" Type="String" />
                  <asp:Parameter Name="prmMachine7" Type="String" />
                  <asp:Parameter Name="prmMachine8" Type="String" />
                  <asp:Parameter Name="prmMachine9" Type="String" />
                  <asp:Parameter Name="prmMachine10" Type="String" />
                  <asp:Parameter Name="prmMachine11" Type="String" />
                  <asp:Parameter Name="prmMachine12" Type="String" />
                  <asp:Parameter Name="prmMachine13" Type="String" />
                  <asp:Parameter Name="prmMachine14" Type="String" />
                  <asp:Parameter Name="prmMachine15" Type="String" />
                  <asp:Parameter Name="prmOutexcel" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>--%>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

