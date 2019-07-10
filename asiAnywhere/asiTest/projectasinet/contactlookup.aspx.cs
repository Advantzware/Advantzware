using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
/// <summary>
/// Summary description for Class1
/// </summary>
public partial class contactlookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        if (!Page.IsPostBack)
        {
            //Bindgrid();
        }
        this.BuildDataSource();
       
    }
    private void BuildDataSource()
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();           
            string cmd = "select last_name, first_name,  addr1, addr2, city, state, zip, country  from contact";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            GridView1.DataSource = ds;            
            GridView1.DataBind();
           

            conn.Close();

        }
        catch
        { 
            return; 
        }
        finally
        {
            conn.Close();
        }
    }
    protected void Bindgrid()
    {
        //SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        //ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        //ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        //ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();

        string searchfield = ddlSearchField.SelectedValue.Trim();
        string searchop = ddlSearchOperation.SelectedValue.Trim();
        string txtval = txtSearchValue.Text.Trim();

        string cmd = "";

        if (searchfield == "firstname")
        {
            if (searchop == "EQUAL")
            {
                cmd = cmd + "select last_name, first_name,  addr1, addr2, city, state, zip, country  from contact where first_name ='" + txtval + "'";
            }
            if (searchop == "BEGIN")
            {                
                cmd = cmd + "select last_name, first_name,  addr1, addr2, city, state, zip, country  from contact where first_name like '" + txtval + "%'";              
            }
        }
        if (searchfield == "lastname")
        {
            if (searchop == "EQUAL")
            {
                cmd = cmd + "select last_name, first_name,  addr1, addr2, city, state, zip, country  from contact where last_name ='" + txtval + "'";  
            }
            if (searchop == "BEGIN")
            {
                cmd = cmd + "select last_name, first_name,  addr1, addr2, city, state, zip, country  from contact where last_name like '" + txtval + "%'";                              
            }
        }

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            //string cmd = "select last_name, first_name,  addr1, addr2, city, state, zip, country  from contact";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            GridView1.DataSource = ds;
            GridView1.DataBind();


            conn.Close();

        }
        catch
        {
            return;
        }
        finally
        {
            conn.Close();
        }

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        Response.Redirect("contactlookup.aspx");
    }
}
