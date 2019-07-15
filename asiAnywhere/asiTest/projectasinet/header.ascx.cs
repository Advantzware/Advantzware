
#region " using "
using System;
using System.Data;
using System.Data.SqlClient;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class HeaderFooter : System.Web.UI.UserControl
{
  
    private int iDefaultRecordsPerPage = 100;

    private bool bSort = true;


private string  sConnectionString = "";


protected void createMenu(string groupID, string parentID) 
{
  //sConnectionString = func.GetConnectionStr();
  ConnectionStringSettings cts = ConfigurationManager.ConnectionStrings["Project1ConnectionString"];    
  try {
	string  sGroupID = ((UserClass)System.Web.HttpContext.Current.Session["User"]).GroupID;	          
        string  sSQLQuery = "select [group_id],   [parent],   [menu_label],   [destination],   [security],   [description],   [menu_id],   [order_num],   [allowedidtypes],   [long_desc],   [target_page]   From [dbo].[main_menu]";
        string sWhere = " where [group_id] = '" + groupID + "' and " + "[parent] = '" + parentID + "' and " + "[security] like '%" + sGroupID + "%' ";
        //sSQLQuery = func.WhereBuilder(sSQLQuery, sWhere);	
	sSQLQuery += sWhere;	
        sSQLQuery += " ORDER BY [parent] ASC, [order_num] ASC ";
	//Response.Write(sSQLQuery);
        SqlConnection  cn = new SqlConnection();
        try {
                //cn.ConnectionString = sConnectionString;
		cn.ConnectionString = cts.ConnectionString;
                SqlDataAdapter  da = new SqlDataAdapter(sSQLQuery, cn);
                DataSet  ds = new DataSet();
                da.Fill(ds);   
                if (ds.Tables[0].Rows.Count == 0) {
                    return;
                }
                for(int i = 0; i<ds.Tables[0].Rows.Count;i++)
                {	                         
                    if (ds.Tables[0].Rows[i]["target_page"].ToString() == "menuhead")   {
                        Response.Write("<li><a href='#' >" + ds.Tables[0].Rows[i]["menu_label"] + "</a>");
	          Response.Write("<ul>");	          
                        createMenu(ds.Tables[0].Rows[i]["group_id"].ToString(), ds.Tables[0].Rows[i]["menu_id"].ToString());
                        Response.Write("</ul>");	          
                        Response.Write("</li>");	          
                    }
                    if (ds.Tables[0].Rows[i]["target_page"].ToString() != "menuhead")	   {
                        Response.Write("<li><a href='" + ds.Tables[0].Rows[i]["target_page"] + "'>" + ds.Tables[0].Rows[i]["menu_label"] + "</a></li>");                        
                    }
                }
        }  
      catch (Exception ex) {  Response.Write("Error description" + ": " + ex.Message + "<p>");}
      finally {cn.Close();}
    }
     catch (Exception ex) {  Response.Write("Error description" + ": " + ex.Message + "<p>");}
}

}
