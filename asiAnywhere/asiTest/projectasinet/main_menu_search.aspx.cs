 
#region " using "
using System;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;
#endregion

public partial class Cmain_menu_search : System.Web.UI.Page
{

    private System.Collections.Hashtable htPerammain_menu = new System.Collections.Hashtable();

 protected void Page_Load( object sender,  System.EventArgs e)  
 {
        // 
         UserClass.CheckLogin(Page);
        // 
        //
        if (! func.CheckUserPermissions("[dbo].[main_menu]", "S") ) 
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>") ;
            Response.End();
        }  
        //
        
        if (! Page.IsPostBack ) 
        { 
            Session["dbGrid_main_menu_SearchSQL"] = null;
            Session["dbGrid_main_menu_SearchParams"] = null;

            BindData();
        }        
}

protected void BindData() 
{

    func.LoadDataToLookUp(ref fldparent, "SELECT [menu_id], [menu_label] FROM [dbo].[main_menu]   " + " where " + "main_menu.target_page = 'menuhead'" + " ORDER BY [menu_label]", "");

    htPerammain_menu = (System.Collections.Hashtable)Session["htPerammain_menu"];
    if ( htPerammain_menu !=null ) 
    {
        if (htPerammain_menu.ContainsKey("rb")) { rbAnd.Checked = (bool)htPerammain_menu["rb"]; }

		if ( htPerammain_menu.ContainsKey("ddlOperation_menu_id") ) ddlOperation_menu_id.SelectedValue = (string)htPerammain_menu["ddlOperation_menu_id"];
        if ( htPerammain_menu.ContainsKey("chNot_menu_id") ) chNot_menu_id.Checked = (bool)htPerammain_menu["chNot_menu_id"];

        if ( htPerammain_menu.ContainsKey("fldmenu_id") ) fldmenu_id.Text = (string)htPerammain_menu["fldmenu_id"];
        if ( htPerammain_menu.ContainsKey("fld2menu_id") )fld2menu_id.Text = (string)htPerammain_menu["fld2menu_id"];

		if ( htPerammain_menu.ContainsKey("ddlOperation_menu_label") ) ddlOperation_menu_label.SelectedValue = (string)htPerammain_menu["ddlOperation_menu_label"];
        if ( htPerammain_menu.ContainsKey("chNot_menu_label") ) chNot_menu_label.Checked = (bool)htPerammain_menu["chNot_menu_label"];

        if ( htPerammain_menu.ContainsKey("fldmenu_label") ) fldmenu_label.Text = (string)htPerammain_menu["fldmenu_label"];
        if ( htPerammain_menu.ContainsKey("fld2menu_label") )fld2menu_label.Text = (string)htPerammain_menu["fld2menu_label"];

		if ( htPerammain_menu.ContainsKey("ddlOperation_target_page") ) ddlOperation_target_page.SelectedValue = (string)htPerammain_menu["ddlOperation_target_page"];
        if ( htPerammain_menu.ContainsKey("chNot_target_page") ) chNot_target_page.Checked = (bool)htPerammain_menu["chNot_target_page"];

        if ( htPerammain_menu.ContainsKey("fldtarget_page") ) fldtarget_page.Text = (string)htPerammain_menu["fldtarget_page"];
        if ( htPerammain_menu.ContainsKey("fld2target_page") )fld2target_page.Text = (string)htPerammain_menu["fld2target_page"];

		if ( htPerammain_menu.ContainsKey("ddlOperation_parent") ) ddlOperation_parent.SelectedValue = (string)htPerammain_menu["ddlOperation_parent"];
        if ( htPerammain_menu.ContainsKey("chNot_parent") ) chNot_parent.Checked = (bool)htPerammain_menu["chNot_parent"];

        if ( htPerammain_menu.ContainsKey("fldparent") )fldparent.SelectedValue = (string)htPerammain_menu["fldparent"];

		if ( htPerammain_menu.ContainsKey("ddlOperation_security") ) ddlOperation_security.SelectedValue = (string)htPerammain_menu["ddlOperation_security"];
        if ( htPerammain_menu.ContainsKey("chNot_security") ) chNot_security.Checked = (bool)htPerammain_menu["chNot_security"];

        if ( htPerammain_menu.ContainsKey("fldsecurity") ) fldsecurity.Text = (string)htPerammain_menu["fldsecurity"];
        if ( htPerammain_menu.ContainsKey("fld2security") )fld2security.Text = (string)htPerammain_menu["fld2security"];

		if ( htPerammain_menu.ContainsKey("ddlOperation_description") ) ddlOperation_description.SelectedValue = (string)htPerammain_menu["ddlOperation_description"];
        if ( htPerammain_menu.ContainsKey("chNot_description") ) chNot_description.Checked = (bool)htPerammain_menu["chNot_description"];

        if ( htPerammain_menu.ContainsKey("flddescription") ) flddescription.Text = (string)htPerammain_menu["flddescription"];
        if ( htPerammain_menu.ContainsKey("fld2description") )fld2description.Text = (string)htPerammain_menu["fld2description"];

    }
}

protected void hlBack_Click( object sender,  System.EventArgs e) 
{
    Response.Redirect("main_menu_list.aspx");
}

protected void btnSearch_Click( object sender,  System.EventArgs e)
{
    string  sWhere = "";
    ParameterCollection Params = new ParameterCollection();
    string  sValue = "";
    string  sValue2 = "";
    htPerammain_menu.Add("rb", rbAnd.Checked);

    htPerammain_menu.Add("ddlOperation_menu_id", ddlOperation_menu_id.SelectedValue);
    htPerammain_menu.Add("chNot_menu_id", chNot_menu_id.Checked);

    sValue = fldmenu_id.Text;
    sValue2 = fld2menu_id.Text;

    if(sValue != string.Empty) htPerammain_menu.Add("fldmenu_id", sValue);
    if(sValue2 != string.Empty) htPerammain_menu.Add("fld2menu_id", sValue2);
  
    sWhere = func.WhereBuilder(sWhere, WhereOneField("[menu_id]",TypeCode.Int32, ddlOperation_menu_id.SelectedItem.Value.ToString(), 
            sValue, sValue2, Params, chNot_menu_id.Checked), (rbAnd.Checked?"And":"Or"));            

    htPerammain_menu.Add("ddlOperation_menu_label", ddlOperation_menu_label.SelectedValue);
    htPerammain_menu.Add("chNot_menu_label", chNot_menu_label.Checked);

    sValue = fldmenu_label.Text;
    sValue2 = fld2menu_label.Text;

    if(sValue != string.Empty) htPerammain_menu.Add("fldmenu_label", sValue);
    if(sValue2 != string.Empty) htPerammain_menu.Add("fld2menu_label", sValue2);
  
    sWhere = func.WhereBuilder(sWhere, WhereOneField("[menu_label]",TypeCode.String, ddlOperation_menu_label.SelectedItem.Value.ToString(), 
            sValue, sValue2, Params, chNot_menu_label.Checked), (rbAnd.Checked?"And":"Or"));            

    htPerammain_menu.Add("ddlOperation_target_page", ddlOperation_target_page.SelectedValue);
    htPerammain_menu.Add("chNot_target_page", chNot_target_page.Checked);

    sValue = fldtarget_page.Text;
    sValue2 = fld2target_page.Text;

    if(sValue != string.Empty) htPerammain_menu.Add("fldtarget_page", sValue);
    if(sValue2 != string.Empty) htPerammain_menu.Add("fld2target_page", sValue2);
  
    sWhere = func.WhereBuilder(sWhere, WhereOneField("[target_page]",TypeCode.String, ddlOperation_target_page.SelectedItem.Value.ToString(), 
            sValue, sValue2, Params, chNot_target_page.Checked), (rbAnd.Checked?"And":"Or"));            

    htPerammain_menu.Add("ddlOperation_parent", ddlOperation_parent.SelectedValue);
    htPerammain_menu.Add("chNot_parent", chNot_parent.Checked);

    sValue = fldparent.SelectedValue;

    if(sValue != string.Empty) htPerammain_menu.Add("fldparent", sValue);
    if(sValue2 != string.Empty) htPerammain_menu.Add("fld2parent", sValue2);
  
    sWhere = func.WhereBuilder(sWhere, WhereOneField("[parent]",TypeCode.String, ddlOperation_parent.SelectedItem.Value.ToString(), 
            sValue, sValue2, Params, chNot_parent.Checked), (rbAnd.Checked?"And":"Or"));            

    htPerammain_menu.Add("ddlOperation_security", ddlOperation_security.SelectedValue);
    htPerammain_menu.Add("chNot_security", chNot_security.Checked);

    sValue = fldsecurity.Text;
    sValue2 = fld2security.Text;

    if(sValue != string.Empty) htPerammain_menu.Add("fldsecurity", sValue);
    if(sValue2 != string.Empty) htPerammain_menu.Add("fld2security", sValue2);
  
    sWhere = func.WhereBuilder(sWhere, WhereOneField("[security]",TypeCode.String, ddlOperation_security.SelectedItem.Value.ToString(), 
            sValue, sValue2, Params, chNot_security.Checked), (rbAnd.Checked?"And":"Or"));            

    htPerammain_menu.Add("ddlOperation_description", ddlOperation_description.SelectedValue);
    htPerammain_menu.Add("chNot_description", chNot_description.Checked);

    sValue = flddescription.Text;
    sValue2 = fld2description.Text;

    if(sValue != string.Empty) htPerammain_menu.Add("flddescription", sValue);
    if(sValue2 != string.Empty) htPerammain_menu.Add("fld2description", sValue2);
  
    sWhere = func.WhereBuilder(sWhere, WhereOneField("[description]",TypeCode.String, ddlOperation_description.SelectedItem.Value.ToString(), 
            sValue, sValue2, Params, chNot_description.Checked), (rbAnd.Checked?"And":"Or"));            
    
    if ( sWhere.Trim() == "" ) 
    {
            Session["dbGrid_main_menu_AdvSearch"] = null;
            Session["dbGrid_main_menu_AdvParam"] = null;
            Session["htPerammain_menu"] = null;
    }
    else 
    {
            Session["dbGrid_main_menu_AdvSearch"] = "(" + sWhere +")";
            Session["dbGrid_main_menu_AdvParam"] = Params;
            Session["htPerammain_menu"] = htPerammain_menu;
    }
    Response.Redirect("main_menu_list.aspx");

}

protected string WhereOneField(string  sSearchField, TypeCode FieldType, string sSearchOperation,  string  sValue,  string  sValue2, ParameterCollection Params, bool bNot) 
{

  if (sSearchOperation == "IsNull") return sSearchField + " is null";
  if ( sValue == "" && sValue2 == "") return String.Empty;
/*
  try
  {
      object o = Convert.ChangeType(sValue, FieldType);
      if (sValue2 != string.Empty) o = Convert.ChangeType(sValue2, FieldType);
  }
  catch { return String.Empty; }
  */
  string  sReturn = "";
  string  sSearchType = "";
  string  sParamName = func.BuildParameterName("AdvSearch" + sSearchField);  
  if (!(func.IsDate(sValue) || func.IsNumeric(sValue))) 
  {

    sSearchType = "upper";

    sValue = sValue.ToUpper();
  }

  switch (sSearchOperation)
    {
        case "Contains":
                sReturn = sSearchType + "(" + sSearchField + ") like '%" + sValue + "%'";break;
        case "Starts with ...":
              sReturn = sSearchType + "(" + sSearchField + ") like '" + sValue + "%'";break;            
        case "Equals":
            sReturn = sSearchType + "(" + sSearchField + ") = @" + sParamName;
            break;
        case "More than ...":
            sReturn = sSearchField + ">@" + sParamName;
            break;
        case "Less than ...":
            sReturn = sSearchField + "<@" + sParamName;
            break;
        case "Equal or more than ...":
            sReturn = sSearchField + ">=@" + sParamName;
            break;
        case "Equal or less than ...":
            sReturn = sSearchField + "<=@" + sParamName;
            break;
        case "Between":
            sReturn = sSearchField + ">=@" + sParamName + " And " + sSearchField + "<=@" + sParamName+"2";
            break;
        default:
            return String.Empty;
    }
    if (sReturn != string.Empty && bNot) sReturn = "(Not (" + sReturn + "))";        
    if (sReturn != string.Empty && (sSearchOperation != "Contains" && sSearchOperation != "Starts with ..."))
    {  
	  Params.Add(sParamName, FieldType, sValue);
      if (sSearchOperation == "Between") Params.Add(sParamName+"2", FieldType, sValue2);
    }
  return sReturn;
}

//
   
}

   