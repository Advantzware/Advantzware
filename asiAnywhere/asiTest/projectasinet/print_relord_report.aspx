<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="print_relord_report" Codebehind="print_relord_report.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Release Ticket</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    
     <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
//    function confirmPost() {
//        var retVal = makeMsgBox("Confirmation", "Are you ready to  Post Invoices?", 48, 4, 256, 4096);
//        if (retVal == 6) {
//            document.forms[0].HiddenFieldPost.value = "Yes";
//        }
//        else {
//            document.forms[0].HiddenFieldPost.value = "No";
//        }
//    }
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

    function preEnter(fieldObj, canEdit) {
        fieldObj.style.backgroundColor = 'blue';
        fieldObj.style.color = 'white';
        if (canEdit == "no") {
            fieldObj.blur();
            leaveField(fieldObj);
        }

        enterField(fieldObj);
        return;
    }
    
    function preLeave( fieldObj, fieldType, fieldFormat ){
    fieldObj.style.backgroundColor='Window';
    fieldObj.style.color='WindowText';
    fieldType = fieldType.toLowerCase();
    if ((fieldType == "") || (fieldType == "text")) {
        leaveField(fieldObj);
    }
}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}

var polook = "";
function customerpolook(po1) {
    polook = po1;
    var NewWindow = window.open("poitem_lookup.aspx", "CustomerpoWindow", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ItemPoLookup(ReturnObj1) {
    if (polook == 1) {
        document.forms[0].bepoTextBox.value = ReturnObj1;
    }
    else
        document.forms[0].endpoTextBox.value = ReturnObj1;    
    
}


    function vendtext() {
        var vend = document.getElementById("bevendTextBox");
        vend.focus();
    }
    function periodtext() {
        var vend = document.getElementById("postdateTextBox");
        var pertext = document.getElementById("perTextBox");
        pertext.value = (vend.value).substring(0, 2);

    }
    var ordlook = "";
    function orderlook(obj1) {
        ordlook = obj1;
        var cust = document.getElementById("becustTextBox").value;
        var NewWindow = window.open("order_lookup.aspx?customer=" + cust + "", "OrderLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function OrderLookup(ReturnObj1) {
    if(ordlook == 1) {    
        document.forms[0].begordTextBox.value = ReturnObj1;
        document.forms[0].begordTextBox.focus();
        }
    else
    {
        document.forms[0].endordTextBox.value = ReturnObj1;
        document.forms[0].endordTextBox.focus();
        }
}
var custlook = "";
function customerlook(obj2) {
    custlook = obj2;
    var NewWindow = window.open("customer_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1) {
    if (custlook == 1) {
        document.forms[0].becustTextBox.value = ReturnObj1;
        document.forms[0].becustTextBox.focus();
    }
    else {
        document.forms[0].endcustTextBox.value = ReturnObj1;
        document.forms[0].endcustTextBox.focus();
    }
}
var zonlook = "";
function deliveryzonelook(obj3) {
    zonlook = obj3;
    var NewWindow = window.open("zone_lookup.aspx", "DelZoneLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function zoneLookup(ReturnObj1) {
    if (zonlook == 1) {
        document.forms[0].bedelznTextBox.value = ReturnObj1;
        document.forms[0].bedelznTextBox.focus();
    }
    else {
        document.forms[0].enddelznTextBox.value = ReturnObj1;
        document.forms[0].enddelznTextBox.focus();
    }
}
var loclook = "";
function locationlook(obj4) {
    loclook = obj4;
    var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1) {
    if (loclook == 1) {
        document.forms[0].fwhseTextBox.value = ReturnObj1;
        document.forms[0].fwhseTextBox.focus();
    }
    else {
        document.forms[0].twhseTextBox.value = ReturnObj1;
        document.forms[0].twhseTextBox.focus();
    }
}
var binlk = "";
function binlook(obj5) {
    binlk = obj5;
    var loc1 = document.getElementById("fwhseTextBox").value;
    var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustBinLookup(ReturnObj1) {
    if (binlk == 1) {
        document.forms[0].fbinTextBox.value = ReturnObj1;
        document.forms[0].fbinTextBox.focus();
    }
    else {
        document.forms[0].tbinTextBox.value = ReturnObj1;
        document.forms[0].tbinTextBox.focus();
    }
}
var rellook = "";
function releaselook(obj6) {
    rellook = obj6;
    var NewWindow = window.open("release_lookup.aspx", "ReleaseLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ReleaseLookUp(ReturnObj1) {
    if (rellook == 1) {
        document.forms[0].berellTextBox.value = ReturnObj1;
        document.forms[0].berellTextBox.focus();
    }
    else {
        document.forms[0].endrellTextBox.value = ReturnObj1;
        document.forms[0].endrellTextBox.focus();
    }   
}

        
      
   </script> 
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
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Release Ticket&nbsp;</b></font></TD>
          
          
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       
      <fieldset class="shade">
      <table class="shade" >
      <tr><td><table>
      <tr>
         <td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="becustTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onClick="customerlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
         <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="endcustTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onClick="customerlook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        </tr>       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Release#:</b></td><td>
          <asp:TextBox ID="berellTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onClick="releaselook(1); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Release#:</b></td><td>
        <asp:TextBox ID="endrellTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>            
        <a href="#" tabindex="1" onClick="releaselook(2); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>  
      <tr><td align="right" style="padding-right: 5px"><b>Begining Order#:</b></td><td>
          <asp:TextBox ID="begordTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onClick="orderlook(1); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Order#:</b></td><td>
        <asp:TextBox ID="endordTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>            
        <a href="#" tabindex="1" onClick="orderlook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>  
      <tr><td align="right" style="padding-right: 5px"><b>Begining Date:</b></td><td>
          <asp:TextBox ID="bedateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onClick="showCalendarControl(bedateTextBox); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Date:</b></td><td>
        <asp:TextBox ID="enddateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>            
        <a href="#" tabindex="1" onClick="showCalendarControl(enddateTextBox); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr> 
     <tr><td align="right" style="padding-right: 5px"><b>Begining Delivery Zone:</b></td><td>
          <asp:TextBox ID="bedelznTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>          
          <a href="#" tabindex="1" onClick="deliveryzonelook(1); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="right" style="padding-right: 5px"><b>Ending Delivery Zone:</b></td><td>
        <asp:TextBox ID="enddelznTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>            
        <a href="#" tabindex="1" onClick="deliveryzonelook(2); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>        
        </table></td></tr>        
        
        <tr></tr><tr></tr>
        <tr><td><table>
            <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Reprint Release Tickets? " runat="server" /></b>
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox2" Text="Reprint Posted Release? " runat="server" /></b>
            </td>
            <td rowspan="5"><fieldset><table><tr><td colspan="3"><asp:RadioButtonList ID="RadioButtonList1" onfocus= "javascript:focusval(this)" onblur="setfocus(this)"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="1" Font-Bold="true" Enabled="true"    runat="server">
                                                         <asp:ListItem value="I"  Text="Print All Tags in Item Bin Locations"   />
                                                         <asp:ListItem value="R" Text="Print Only Tags on Release" />
                                                         <asp:ListItem value="S" Text="Summary of Bins On Hand" />                                                         
                                                     </asp:RadioButtonList></td></tr>
            <tr><td></td><td align="center"><b>From</b></td><td align="center"><b>To</b></td></tr>                                                                                                                     
            <tr><td align="right" style="padding-right: 5px"><b>Warehouse:</b></td><td>
            <asp:TextBox ID="fwhseTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="70px" runat="server"></asp:TextBox>          
            <a href="#" tabindex="1" onClick="locationlook(1); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="twhseTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="70px" runat="server"></asp:TextBox>            
            <a href="#" tabindex="1" onClick="locationlook(2); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr> 
            <tr><td align="right" style="padding-right: 5px"><b>Bin:</b></td><td>
            <asp:TextBox ID="fbinTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="70px" runat="server"></asp:TextBox>          
            <a href="#" tabindex="1" tabindex="1" onClick="binlook(1); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="tbinTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="70px" runat="server"></asp:TextBox>            
            <a href="#" tabindex="1" tabindex="1" onClick="binlook(2); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>  
            </table></fieldset></td></tr>                         
            <tr><td><b><asp:CheckBox ID="CheckBox3" Text="Print Multiple Releases Per Form?" runat="server" /></b>
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox4" Text="Print Bin Locations? " runat="server" /></b>
            </td></tr>                        
            <tr><td><b><asp:CheckBox ID="CheckBox5" Text="Sort By Delivery Zone? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox6" Text="Print Delivery Zone? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox7" Text="Print Assembled Components? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox8" Text="Print Customer Part#? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox9" Text="Print Pricing? " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox11" Text="Post Release " runat="server" /></b>
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox10" Text="Sort Bin Locations " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox12" Text="Multiple Release " runat="server" /></b>
            </td></tr>             
            <tr><td><b><asp:CheckBox ID="CheckBox13" Text="Exclude Tags and Bins?" runat="server" /></b><br />
            </td></tr>                        
         </table></td></tr>
         
                          
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       </fieldset>
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">               
             
              <ItemTemplate>
                  relprt:
                  <asp:Label ID="relprtLabel" runat="server" 
                      Text='<%# Bind("relprt") %>'></asp:Label><br />                 
                  
                  
                  
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="PrintReleaseOrd" TypeName="release">
              <SelectParameters>
                  <asp:Parameter Name="prmUser"     Type="String" />
                  <asp:Parameter Name="prmrelprt"    Type="String" />
                  <asp:Parameter Name="prmbegcust"    Type="String" />
                  <asp:Parameter Name="prmbegrel"    Type="Int32" />
                  <asp:Parameter Name="prmbegord"  Type="Int32" />
                  <asp:Parameter Name="prmbegdate"  Type="String" />
                  <asp:Parameter Name="prmbegdelz"    Type="String" />
                  <asp:Parameter Name="prmbegloc"  Type="String" />
                  <asp:Parameter Name="prmbeglocbin"   Type="String" />
                  <asp:Parameter Name="prmendcust"   Type="String" />
                  <asp:Parameter Name="prmendrel"     Type="Int32" />
                  <asp:Parameter Name="prmendord"     Type="Int32" />
                  <asp:Parameter Name="prmenddate"   Type="String" />
                  <asp:Parameter Name="prmenddelz"  Type="String" />
                  <asp:Parameter Name="prmendloc"   Type="String" />
                  <asp:Parameter Name="prmendlocbin"   Type="String" />
                  <asp:Parameter Name="prmpsted"   Type="String" />
                  <asp:Parameter Name="prmprinted" Type="String" />                  
                  <asp:Parameter Name="prmrel_prfm"      Type="String" />                  
                  <asp:Parameter Name="prmbinloc"     Type="String" />                   
                  <asp:Parameter Name="prmsrtdelz"   Type="String" />
                  <asp:Parameter Name="prmprtdelz"     Type="String" />
                  <asp:Parameter Name="prmasscomp"     Type="String" />
                  <asp:Parameter Name="prmcustprt"   Type="String" />
                  <asp:Parameter Name="prmprtpric"  Type="String" />
                  <asp:Parameter Name="prmsrtbinloc"   Type="String" />
                  <asp:Parameter Name="prmprtwht"   Type="String" />
                  <asp:Parameter Name="prmpstrel"   Type="String" />
                  <asp:Parameter Name="prmmulrel" Type="String" />                  
                  <asp:Parameter Name="prmextagbin"      Type="String" />                  
                  <asp:Parameter Name="prmextra"     Type="String" />                                                                                
                                         
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    
    </form>
  </body>
</HTML>


