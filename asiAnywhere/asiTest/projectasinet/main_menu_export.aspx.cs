
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Xml;
#endregion

public partial class Cmain_menu_export : System.Web.UI.Page 
{
  
    private int iDefaultRecordsPerPage = 100;

protected void Page_Load( object sender,  System.EventArgs e)  
{

    UserClass.CheckLogin(Page);

    if (! (func.CheckUserPermissions("[dbo].[main_menu]", "S") && func.CheckUserPermissions("[dbo].[main_menu]", "P")) ) 
    {
      Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
      Response.End();
    }  

    lblMessage.Text = "";

    string sCulture = ConfigurationManager.AppSettings["LCID"];
    if (!String.IsNullOrEmpty(sCulture)) 
    {
        int nCulture = int.Parse(sCulture);
        System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
    } 
               
    if (! Page.IsPostBack )
    {    

    } //  ! Page.IsPostBack

}

protected void btnExport_Click( object sender,  System.EventArgs e) 
{
    Response.Expires = 0; // prevent caching;
    Response.Buffer = true;
    Server.ScriptTimeout = 120;
  
  dbGrid_main_menu.Visible = true;
    BuildDataSource();

    if ( rbExel.Checked ) ExporToExel();
    if ( rbWord.Checked ) ExporToWord();
    if ( rbCSV.Checked )  ExporToCSV();
    if ( rbXML.Checked )  ExporToXML();
  
  dbGrid_main_menu.Visible = false;

}

private void ExporToExel() 
{
    Response.ContentType = "application/vnd.ms-excel";
    Response.AddHeader("Content-Disposition", "attachment;Filename=main_menu.xls");
    Response.Write ("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=Utf-8\">");
    
    System.IO.StringWriter tw = new System.IO.StringWriter();
    System.Web.UI.HtmlTextWriter hw = new System.Web.UI.HtmlTextWriter(tw);

    dbGrid_main_menu.RenderControl(hw);
    Response.Write(tw.ToString());
    Response.End();
}

private void ExporToWord() 
{
    Response.ContentType = "application/vnd.ms-word";
    Response.AddHeader("Content-Disposition", "attachment;Filename=main_menu.doc");
    Response.Write ("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=Utf-8\">");
    System.IO.StringWriter tw = new System.IO.StringWriter();
    System.Web.UI.HtmlTextWriter hw = new System.Web.UI.HtmlTextWriter(tw);

    dbGrid_main_menu.RenderControl(hw);
    Response.Write(tw.ToString());
    Response.End();
}

private void ExporToCSV() 
{
    Response.ContentType = "application/csv";
    Response.AddHeader("Content-Disposition", "attachment;Filename=main_menu.csv");

    for ( int i = 0 ; i < dbGrid_main_menu.Columns.Count; i++)
    {
        Response.Write("\"" + dbGrid_main_menu.Columns[i].HeaderText + "\"");
        if ( i != dbGrid_main_menu.Columns.Count - 1 )  Response.Write(", ");
    } //
    Response.Write("\n");
    foreach (GridViewRow dgRow in dbGrid_main_menu.Rows)
    {
        for ( int i = 0 ; i < dbGrid_main_menu.Columns.Count; i++)
        {
            Response.Write("\"" + dgRow.Cells[i].Text.Replace("&nbsp;", " ").Trim() + "\"");
            if ( i != dbGrid_main_menu.Columns.Count - 1) Response.Write(", ");
        } //
        Response.Write("\n");
     } //
     Response.End();
}

private void ExporToXML() 
{
    string  sTopNode = "TopNode"; //"TopNode";
    string  sRowNode = "RowNode";

    Response.ContentType = "text/xml";
    Response.AddHeader("Content-Disposition", "attachment;Filename=main_menu.xml");

    XmlDocument  myXmlDocument = new XmlDocument();
    XmlElement xmlRoot = myXmlDocument.CreateElement(sTopNode);
    myXmlDocument.AppendChild(xmlRoot);

    foreach (GridViewRow dgRow in dbGrid_main_menu.Rows)
    {
        XmlElement xmlRow = myXmlDocument.CreateElement(sRowNode);
        xmlRoot.AppendChild(xmlRow);

        for ( int i = 0 ; i < dbGrid_main_menu.Columns.Count; i++)
        {
            XmlElement xmlField = myXmlDocument.CreateElement(XMLNameEncode(dbGrid_main_menu.Columns[i].HeaderText));
            string  str = dgRow.Cells[i].Text;
            str = str.Replace("&nbsp;", " ").Trim();
            if ( str == "" ) str = "<NULL>";
            xmlField.InnerText = str.Replace(System.Convert.ToChar(149), '_');
            xmlRow.AppendChild(xmlField);
         } //
     } //
     Response.Write("<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\n" + myXmlDocument.InnerXml);
     Response.End();
}

private string  XMLNameEncode( string  strValue) 
{
     string sXMLNameEncode = "";
     sXMLNameEncode = strValue.Replace(" ", "");
     sXMLNameEncode = sXMLNameEncode.Replace("#", "");
     sXMLNameEncode = sXMLNameEncode.Replace("//", "");
     sXMLNameEncode = sXMLNameEncode.Replace("/", "");
     sXMLNameEncode = sXMLNameEncode.Replace("\\", "");
     sXMLNameEncode = sXMLNameEncode.Replace("(", "");
     sXMLNameEncode = sXMLNameEncode.Replace(")", "");
     sXMLNameEncode = sXMLNameEncode.Replace(",", "");
     sXMLNameEncode = sXMLNameEncode.Replace("[", "");
     sXMLNameEncode = sXMLNameEncode.Replace("]", "");
     sXMLNameEncode = sXMLNameEncode.Replace("+", "");
     sXMLNameEncode = sXMLNameEncode.Replace("\"", "");
     sXMLNameEncode = sXMLNameEncode.Replace("-", "");
     sXMLNameEncode = sXMLNameEncode.Replace("_", "");
     sXMLNameEncode = sXMLNameEncode.Replace("|", "");
     sXMLNameEncode = sXMLNameEncode.Replace("}", "");
     sXMLNameEncode = sXMLNameEncode.Replace("{", "");
     return sXMLNameEncode;
}
private void BuildDataSource()
{
    try
    {
        string sSqlWhere = "";
        main_menuSqlDataSource.SelectParameters.Clear();

        if (Session["dbGrid_main_menu_SearchSQL"] != null) 
            sSqlWhere =  func.WhereBuilder(sSqlWhere, "(" + Session["dbGrid_main_menu_SearchSQL"].ToString() + ")","And");
        if (Session["dbGrid_main_menu_SearchParams"] != null) 
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_main_menu_SearchParams"];
            foreach (Parameter p in Params) 
            {
                main_menuSqlDataSource.SelectParameters.Add(p);
            }
        }               

        if (Session["dbGrid_main_menu_AdvSearch"] != null)
            sSqlWhere = func.WhereBuilder( sSqlWhere , Session["dbGrid_main_menu_AdvSearch"].ToString());
        if (Session["dbGrid_main_menu_AdvParam"] != null)
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_main_menu_AdvParam"];
            foreach (Parameter p in Params) main_menuSqlDataSource.SelectParameters.Add(p);
        }    

        if ( func.GetsAdvSecurityMethod("[dbo].[main_menu]") == "1" && (! func.IsAdminUser()))
        {
            string sOwnerIDField = func.GetOwnerIDField("[dbo].[main_menu]");
            string userId = ((UserClass)Session["User"]).UserID;
            
            if (sOwnerIDField != string.Empty && userId != string.Empty)
            {
                sSqlWhere = func.WhereBuilder(sSqlWhere, "[" + sOwnerIDField + "]=@" + func.BuildParameterName(sOwnerIDField));            
                main_menuSqlDataSource.SelectParameters.Add(func.BuildParameterName(sOwnerIDField), GetFieldType(sOwnerIDField), userId);
            }
            else
      {
        return;
      }  
        }

    if(sSqlWhere != string.Empty) main_menuSqlDataSource.SelectCommand = func.SqlBuilder(main_menuSqlDataSource.SelectCommand, sSqlWhere);
        
        if (Session["dbGrid_main_menu_SortExpression"] != null)
        {
            dbGrid_main_menu.Sort((string)Session["dbGrid_main_menu_SortExpression"], (SortDirection)Session["dbGrid_main_menu_SortDirection"]);
        }
        
        if ( rbCurr.Checked )
        {
            if (Session["dbGrid_main_menu_CurrentPageCount"] != null )
            {
                dbGrid_main_menu.PageSize = Convert.ToInt32(Session["dbGrid_main_menu_CurrentPageCount"]);
            } 
            else
            {
                dbGrid_main_menu.PageSize = iDefaultRecordsPerPage;
            }

            int iCurrentPageIndex = (Session["dbGrid_main_menu_CurrentPageIndex"] == null)?0:Convert.ToInt32(Session["dbGrid_main_menu_CurrentPageIndex"]);
            if (dbGrid_main_menu.PageCount >= iCurrentPageIndex)
                dbGrid_main_menu.PageIndex = iCurrentPageIndex;
            else 
            {
                dbGrid_main_menu.PageIndex = 0;
                Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
            }
        }
        else dbGrid_main_menu.AllowPaging = false;

        dbGrid_main_menu.DataBind();

      }
    catch (Exception ex)
    {
        lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
        dbGrid_main_menu.Visible = false;
    }  
    finally
    {
/*#if(DEBUG)
{
    lblMessage.Text += "<p>SQL = " + main_menuSqlDataSource.SelectCommand  + "<p>";
    foreach (Parameter p in main_menuSqlDataSource.SelectParameters)
        lblMessage.Text += p.Name + "(" + p.Type.ToString()+")="+p.DefaultValue+"<br>";

}
#endif*/
    }
}

protected void dbGrid_main_menu_RowDataBound(object sender,  GridViewRowEventArgs e)
{
    if (e.Row.RowType == DataControlRowType.DataRow)
    {

        DataRowView rowData;

        rowData = (DataRowView)e.Row.DataItem;

        if (rowData["parent"] != System.DBNull.Value) e.Row.Cells[4].Text = func.GetLookupValue("[menu_label]", "[menu_id]", "[dbo].[main_menu]", Convert.ToString(rowData["parent"]), TypeCode.String);

    }

}    

protected TypeCode GetFieldType(string  sField) 
{
    switch (sField)
    {
      
        case "description": return TypeCode.String;
      
        case "menu_id": return TypeCode.Int32;
      
        case "group_id": return TypeCode.String;
      
        case "menu_label": return TypeCode.String;
      
        case "long_desc": return TypeCode.String;
      
        case "order_num": return TypeCode.Int32;
      
        case "allowedidtypes": return TypeCode.String;
      
        case "target_page": return TypeCode.String;
      
        case "parent": return TypeCode.String;
      
        case "destination": return TypeCode.String;
      
        case "security": return TypeCode.String;
      
        default: return TypeCode.String;
    }    
}

public override void VerifyRenderingInServerForm(System.Web.UI.Control control)
{
// Confirms that an HtmlForm control is rendered for the specified ASP.NET server control at run time.
}
}
